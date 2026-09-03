#' @title Plot Pairwise Rank Distributions
#'
#' @description
#' Creates pairwise scatter plots comparing rank values across selected ranking
#' methods.
#'
#' @details
#' `plot_rank_distribution()` compares numeric ranking columns by plotting all
#' pairwise method combinations. If `top_k` is supplied, only drugs appearing
#' within the top-k threshold in at least one selected method are retained.
#'
#' @inheritParams plot_top_k_overlap
#' @param methods Optional character vector of ranking-method columns to plot.
#'   If `NULL`, all numeric columns are used.
#' @param point_size Point size. Default is `1.5`.
#' @param alpha Point transparency in `(0, 1]`. Default is `0.6`.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' rank_df <- data.frame(
#'   Drug = c("drug_a", "drug_b", "drug_c"),
#'   Method1 = c(1, 2, 3),
#'   Method2 = c(2, 1, 3),
#'   Method3 = c(3, 1, 2)
#' )
#'
#' plot_rank_distribution(rank_df)
#'
#' plot_rank_distribution(
#'   drug_ranks_df = rank_df,
#'   methods = c("Method1", "Method2"),
#'   top_k = 100
#' )
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_abline facet_grid theme_bw theme element_text ggsave labs coord_cartesian
#' @export
setGeneric(
  "plot_rank_distribution",
  function(object = NULL, drug_ranks_df = NULL,
           methods = NULL, top_k = NULL,
           point_size = 1.5, alpha = 0.6,
           file_type = "pdf", file_name = NULL,
           width = 12, height = 10, units = "in") {

    if (is.null(drug_ranks_df) && is.data.frame(object)) {
      drug_ranks_df <- object
      object <- NULL
    }

    if (is.null(drug_ranks_df) || !is.data.frame(drug_ranks_df)) {
      stop("`drug_ranks_df` must be provided as a data frame.", call. = FALSE)
    }

    numeric_cols <- names(drug_ranks_df)[vapply(drug_ranks_df, is.numeric, logical(1))]
    if (length(numeric_cols) < 2) {
      stop("`drug_ranks_df` must contain at least two numeric ranking columns.", call. = FALSE)
    }

    if (!is.null(methods)) {
      if (!all(methods %in% names(drug_ranks_df))) {
        stop("All values in `methods` must be column names in `drug_ranks_df`.", call. = FALSE)
      }
      non_numeric_methods <- methods[!vapply(drug_ranks_df[methods], is.numeric, logical(1))]
      if (length(non_numeric_methods)) {
        stop("All selected `methods` columns must be numeric.", call. = FALSE)
      }
      if (length(methods) < 2) {
        stop("`methods` must contain at least two numeric ranking columns.", call. = FALSE)
      }
    } else {
      methods <- numeric_cols
    }

    if (!is.null(top_k) && (!is.numeric(top_k) || length(top_k) != 1 || top_k <= 0)) {
      stop("`top_k` must be a single positive number.", call. = FALSE)
    }

    if (!is.numeric(point_size) || length(point_size) != 1 || point_size <= 0) {
      stop("`point_size` must be a single positive number.", call. = FALSE)
    }

    if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha > 1) {
      stop("`alpha` must be a number in (0, 1].", call. = FALSE)
    }

    if (is.null(file_name)) {
      file_name <- tempfile("plot_rank_distribution_")
    }

    if (is.null(object)) {
      object <- PlotObject(
        input_data = drug_ranks_df,
        file_type = file_type,
        file_name = file_name,
        width = width,
        height = height,
        units = units
      )
      object@parameters$methods <- methods
      object@parameters$top_k <- top_k
      object@parameters$point_size <- point_size
      object@parameters$alpha <- alpha
    }

    standardGeneric("plot_rank_distribution")
  }
)

#' @rdname plot_rank_distribution
#' @export
setMethod(
  "plot_rank_distribution",
  signature = "PlotObject",
  function(object, methods, top_k, point_size, alpha) {

    params <- object@parameters
    input_data <- params$input_data

    if (missing(methods) || is.null(methods)) {
      methods <- if (!is.null(params$methods)) params$methods else names(input_data)[vapply(input_data, is.numeric, logical(1))]
    }

    if (missing(top_k)) {
      top_k <- params$top_k
    }

    if (missing(point_size) || is.null(point_size)) {
      point_size <- if (!is.null(params$point_size)) params$point_size else 1.5
    }

    if (missing(alpha) || is.null(alpha)) {
      alpha <- if (!is.null(params$alpha)) params$alpha else 0.6
    }

    work_df <- input_data[, methods, drop = FALSE]

    if (!is.null(top_k)) {
      if ("Drug" %in% names(input_data)) {
        top_drugs <- extract_top_ranked_drugs(
          df = input_data,
          rank_cols = methods,
          top_n = top_k
        )$Drug
        keep <- input_data$Drug %in% top_drugs
      } else {
        keep <- apply(work_df, 1, function(x) any(!is.na(x) & x <= top_k))
      }
      work_df <- work_df[keep, , drop = FALSE]
      if (nrow(work_df) == 0) {
        stop("No rows remain after `top_k` filtering.", call. = FALSE)
      }
    }

    method_pairs <- utils::combn(methods, 2, simplify = FALSE)

    pair_df <- do.call(
      rbind,
      lapply(method_pairs, function(pair) {
        data.frame(
          method_x = pair[1],
          method_y = pair[2],
          rank_x = work_df[[pair[1]]],
          rank_y = work_df[[pair[2]]],
          stringsAsFactors = FALSE
        )
      })
    )

    pair_df <- stats::na.omit(pair_df)
    if (nrow(pair_df) == 0) {
      stop("No pairwise points available to plot after NA filtering.", call. = FALSE)
    }
    global_min <- min(c(pair_df$rank_x, pair_df$rank_y), na.rm = TRUE)
    global_max <- max(c(pair_df$rank_x, pair_df$rank_y), na.rm = TRUE)

    p <- ggplot2::ggplot(pair_df, ggplot2::aes(x = rank_x, y = rank_y)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey70") +
      ggplot2::geom_point(size = point_size, alpha = alpha, color = "steelblue") +
      ggplot2::facet_grid(method_y ~ method_x, scales = "fixed") +
      ggplot2::coord_cartesian(xlim = c(global_min, global_max), ylim = c(global_min, global_max)) +
      ggplot2::labs(x = "Rank (Method X)", y = "Rank (Method Y)") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        strip.text = ggplot2::element_text(size = 10),
        axis.text.x = ggplot2::element_text(size = 9, angle = 45, hjust = 1),
        axis.text.y = ggplot2::element_text(size = 9)
      )

    if (!is.null(params$file_name) && nzchar(params$file_name)) {
      ggplot2::ggsave(
        filename = paste0(params$file_name, ".", params$file_type),
        plot = p,
        width = params$width,
        height = params$height,
        units = params$units
      )
    }

    p
  }
)
