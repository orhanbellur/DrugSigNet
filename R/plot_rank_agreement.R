#' @title Plot Rank Agreement Across Methods
#'
#' @description
#' Computes and visualizes pairwise rank correlations among ranking methods.
#'
#' @details
#' `plot_rank_agreement()` calculates pairwise correlations among numeric
#' ranking columns and displays them as a heatmap. P-value significance is shown
#' with stars. Rows can optionally be filtered to drugs ranked within `top_k` by
#' at least one method.
#'
#' If `cluster = TRUE`, methods are reordered by hierarchical clustering based
#' on `1 - correlation`.
#'
#' @inheritParams plot_top_k_overlap
#' @param cor_method Correlation method. One of `"spearman"`, `"pearson"`, or
#'   `"kendall"`.
#' @param cluster Logical; whether to cluster methods by correlation similarity.
#'   Default is `TRUE`.
#'
#' @return A list containing `cor_matrix`, `p_matrix`, `plot`, `method`, and
#'   `top_k`.
#'
#' @examples
#' \dontrun{
#' rank_df <- data.frame(
#'   Drug = c("drug_a", "drug_b", "drug_c", "drug_d"),
#'   Method1 = c(1, 2, 3, 4),
#'   Method2 = c(1, 3, 2, 4),
#'   Method3 = c(4, 3, 2, 1)
#' )
#'
#' res <- plot_rank_agreement(
#'   drug_ranks_df = rank_df,
#'   cor_method = "spearman",
#'   top_k = 100
#' )
#'
#' res$plot
#' res$cor_matrix
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_tile geom_text geom_point scale_fill_gradient2 scale_shape_manual guides guide_legend coord_fixed theme_minimal theme element_text element_blank labs ggsave
#' @importFrom stringr str_to_title
#' @export
setGeneric(
  "plot_rank_agreement",
  function(object = NULL, drug_ranks_df = NULL,
           cor_method = c("spearman", "pearson", "kendall"),
           top_k = NULL,
           cluster = TRUE,
           label_size = 2,
           file_type = "pdf",
           file_name = NULL,
           width = 10,
           height = 10,
           units = "in") {

    if (is.null(drug_ranks_df) && is.data.frame(object)) {
      drug_ranks_df <- object
      object <- NULL
    }

    if (is.null(drug_ranks_df)) {
      stop(
        "`drug_ranks_df` is missing. Provide a data frame directly or via `drug_ranks_df = ...`.",
        call. = FALSE
      )
    }

    if (!is.data.frame(drug_ranks_df)) {
      stop("`drug_ranks_df` must be a data frame.", call. = FALSE)
    }

    cor_method <- match.arg(cor_method)

    numeric_cols <- names(drug_ranks_df)[vapply(drug_ranks_df, is.numeric, logical(1))]

    if (length(numeric_cols) < 2) {
      stop(
        "`drug_ranks_df` must contain at least two numeric ranking columns.",
        call. = FALSE
      )
    }

    if (!is.null(top_k)) {
      if (!is.numeric(top_k) || length(top_k) != 1 || top_k <= 0) {
        stop("`top_k` must be a positive integer.", call. = FALSE)
      }
    }

    if (!is.numeric(label_size) || length(label_size) != 1 || label_size <= 0) {
      stop("`label_size` must be a positive number.", call. = FALSE)
    }

    if (is.null(file_name)) {
      file_name <- tempfile("plot_rank_agreement_")
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

      object@parameters$cor_method <- cor_method
      object@parameters$top_k <- top_k
      object@parameters$cluster <- cluster
      object@parameters$label_size <- label_size
    }

    standardGeneric("plot_rank_agreement")
  }
)

#' @rdname plot_rank_agreement
#' @keywords internal
setMethod(
  "plot_rank_agreement",
  signature = "PlotObject",
  function(object, cor_method, top_k, cluster, label_size) {

    params <- object@parameters
    drug_ranks_df <- params$input_data

    if (missing(cor_method) || is.null(cor_method)) {
      cor_method <- if (!is.null(params$cor_method)) params$cor_method else "spearman"
    }
    cor_method <- match.arg(cor_method, c("spearman", "pearson", "kendall"))

    if (missing(top_k)) {
      top_k <- params$top_k
    }
    if (!is.null(top_k) && (!is.numeric(top_k) || length(top_k) != 1 || top_k <= 0)) {
      stop("`top_k` must be a positive integer.", call. = FALSE)
    }

    if (missing(cluster)) {
      cluster <- if (!is.null(params$cluster)) params$cluster else TRUE
    }
    cluster <- isTRUE(cluster)

    if (missing(label_size)) {
      label_size <- if (!is.null(params$label_size)) params$label_size else 2
    }
    if (!is.numeric(label_size) || length(label_size) != 1 || label_size <= 0) {
      stop("`label_size` must be a positive number.", call. = FALSE)
    }

    methods <- names(drug_ranks_df)[vapply(drug_ranks_df, is.numeric, logical(1))]
    method_df <- drug_ranks_df[, methods, drop = FALSE]

    # --------------------------------
    # Top-k filtering
    # --------------------------------
    if (!is.null(top_k)) {
      if ("Drug" %in% names(drug_ranks_df)) {
        top_drugs <- extract_top_ranked_drugs(
          df = drug_ranks_df,
          rank_cols = methods,
          top_n = top_k
        )$Drug
        keep <- drug_ranks_df$Drug %in% top_drugs
      } else {
        keep <- apply(method_df, 1, function(x) any(x <= top_k, na.rm = TRUE))
      }
      method_df <- method_df[keep, , drop = FALSE]

      if (nrow(method_df) == 0) {
        stop("No rows remain after top_k filtering.", call. = FALSE)
      }
    }

    # --------------------------------
    # Compute correlation + p-values
    # --------------------------------
    n <- length(methods)

    cor_matrix <- matrix(NA_real_, n, n, dimnames = list(methods, methods))
    p_matrix <- matrix(NA_real_, n, n, dimnames = list(methods, methods))

    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        x <- method_df[[methods[i]]]
        y <- method_df[[methods[j]]]

        ok <- stats::complete.cases(x, y)

        if (sum(ok) > 2) {
          test <- suppressWarnings(
            stats::cor.test(x[ok], y[ok], method = cor_method)
          )

          cor_matrix[i, j] <- unname(test$estimate)
          p_matrix[i, j] <- test$p.value
        }
      }
    }

    diag(cor_matrix) <- 1
    diag(p_matrix) <- 0

    # --------------------------------
    # Convert p-values to significance stars / levels
    # --------------------------------
    p_to_stars <- function(p) {
      ifelse(
        p < 0.001, "***",
        ifelse(
          p < 0.01, "**",
          ifelse(p < 0.05, "*", "")
        )
      )
    }

    p_to_sig_level <- function(p) {
      ifelse(
        p < 0.001, "< 0.001",
        ifelse(
          p < 0.01, "< 0.01",
          ifelse(p < 0.05, "< 0.05", "ns")
        )
      )
    }

    star_matrix <- matrix(
      p_to_stars(p_matrix),
      nrow = n,
      dimnames = dimnames(p_matrix)
    )

    sig_matrix <- matrix(
      p_to_sig_level(p_matrix),
      nrow = n,
      dimnames = dimnames(p_matrix)
    )

    # --------------------------------
    # Optional clustering
    # --------------------------------
    if (isTRUE(cluster)) {
      hc <- stats::hclust(stats::as.dist(1 - cor_matrix))
      ord <- hc$order

      cor_matrix <- cor_matrix[ord, ord, drop = FALSE]
      p_matrix <- p_matrix[ord, ord, drop = FALSE]
      star_matrix <- star_matrix[ord, ord, drop = FALSE]
      sig_matrix <- sig_matrix[ord, ord, drop = FALSE]
    }

    # --------------------------------
    # Prepare plotting dataframe
    # --------------------------------
    cor_df <- as.data.frame(as.table(cor_matrix))
    names(cor_df) <- c("Var1", "Var2", "cor")

    star_df <- as.data.frame(as.table(star_matrix))
    sig_df <- as.data.frame(as.table(sig_matrix))

    cor_df$stars <- star_df$Freq
    cor_df$label <- sprintf("%.2f%s", cor_df$cor, cor_df$stars)
    cor_df$sig_level <- factor(
      sig_df$Freq,
      levels = c("ns", "< 0.05", "< 0.01", "< 0.001")
    )

    cor_df$Var1 <- factor(cor_df$Var1, levels = colnames(cor_matrix))
    cor_df$Var2 <- factor(cor_df$Var2, levels = rownames(cor_matrix))

    # --------------------------------
    # Plot
    # --------------------------------
    p <- ggplot2::ggplot(
      cor_df,
      ggplot2::aes(x = Var1, y = Var2, fill = cor)
    ) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::geom_text(
        ggplot2::aes(label = label),
        size = label_size,
        lineheight = 0.9,
        color = "black"
      ) +
      ggplot2::geom_point(
        ggplot2::aes(shape = sig_level),
        alpha = 0
      ) +
      ggplot2::scale_fill_gradient2(
        low = "blue",
        mid = "white",
        high = "red",
        midpoint = 0,
        limits = c(-1, 1),
        name = paste0(stringr::str_to_title(cor_method), "\nCorrelation")
      ) +
      ggplot2::scale_shape_manual(
        name = "Significance",
        values = c(
          "ns" = 15,
          "< 0.05" = 15,
          "< 0.01" = 15,
          "< 0.001" = 15
        ),
        labels = c(
          "ns" = "ns (p >= 0.05)",
          "< 0.05" = "* (p < 0.05)",
          "< 0.01" = "** (p < 0.01)",
          "< 0.001" = "*** (p < 0.001)"
        )
      ) +
      ggplot2::guides(
        shape = ggplot2::guide_legend(
          override.aes = list(shape = NA, alpha = 0)
        )
      ) +
      ggplot2::coord_fixed() +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1),
        panel.grid = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.position = "right",
        legend.key = ggplot2::element_blank()
      )

    # --------------------------------
    # Save & Return
    # --------------------------------
    if (!is.null(params$file_name) && nzchar(params$file_name)) {
      ggplot2::ggsave(
        filename = paste0(params$file_name, ".", params$file_type),
        plot = p,
        width = params$width,
        height = params$height,
        units = params$units
      )
    }

    list(
      cor_matrix = cor_matrix,
      p_matrix = p_matrix,
      plot = p,
      method = cor_method,
      top_k = top_k
    )
  }
)
