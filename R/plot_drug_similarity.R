#' @title Plot Drug-Drug Similarity Matrix
#'
#' @description
#' Visualizes a drug-drug structural similarity matrix as a heatmap or
#' dendrogram.
#'
#' @details
#' `plot_drug_similarity()` accepts a `DrugAnnotation` object, numeric matrix,
#' or data frame. Heatmap mode returns a `ggplot` object. Dendrogram mode plots
#' hierarchical clustering based on `1 - similarity` and invisibly returns the
#' `hclust` object.
#'
#' @inheritParams plot_top_k_overlap
#' @param similarity_matrix Square numeric similarity matrix.
#' @param plot_type Plot type. One of `"heatmap"` or `"dendrogram"`.
#'
#' @return A `ggplot` object for heatmaps, or an invisible `hclust` object for
#'   dendrograms.
#'
#' @examples
#' \dontrun{
#' sim_mat <- matrix(
#'   c(1, 0.7, 0.2,
#'     0.7, 1, 0.3,
#'     0.2, 0.3, 1),
#'   nrow = 3,
#'   dimnames = list(
#'     c("drug_a", "drug_b", "drug_c"),
#'     c("drug_a", "drug_b", "drug_c")
#'   )
#' )
#'
#' plot_drug_similarity(sim_mat, plot_type = "heatmap")
#' plot_drug_similarity(sim_mat, plot_type = "dendrogram")
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradient theme_minimal theme labs element_text coord_fixed ggsave
#' @importFrom dplyr mutate
#' @export
setGeneric(
  "plot_drug_similarity",
  function(object = NULL,
           similarity_matrix,
           plot_type = c("heatmap", "dendrogram"),
           heatmap_type = c("triangle", "full"),
           file_type = "pdf", file_name = NULL,
           width = 20, height = 15,
           label_size = 3, units = "in") {

    if (missing(similarity_matrix) && !is.null(object) && !methods::is(object, "PlotObject")) {
      if (methods::is(object, "DrugAnnotation")) {
        similarity_matrix <- object@result
      } else if (is.matrix(object) || is.data.frame(object)) {
        similarity_matrix <- object
      } else {
        stop("When `similarity_matrix` is omitted, `object` must be a DrugAnnotation, matrix, data.frame, or PlotObject.")
      }
      object <- NULL
    }

    if (missing(similarity_matrix)) {
      stop("`similarity_matrix` must be supplied unless `object` is a DrugAnnotation/matrix/data.frame.")
    }

    plot_type <- match.arg(plot_type)
    heatmap_type <- match.arg(heatmap_type)

    if (!is.matrix(similarity_matrix) && !is.data.frame(similarity_matrix)) {
      stop("`similarity_matrix` must be a matrix or data frame.")
    }

    sim_mat <- as.matrix(similarity_matrix)
    if (!is.numeric(sim_mat)) {
      stop("`similarity_matrix` must be numeric.")
    }

    if (nrow(sim_mat) != ncol(sim_mat)) {
      stop("`similarity_matrix` must be square.")
    }

    if (nrow(sim_mat) < 2) {
      stop("`similarity_matrix` must contain at least 2 drugs (2x2).")
    }

    if (is.null(rownames(sim_mat)) || any(!nzchar(rownames(sim_mat)))) {
      if (!is.null(colnames(sim_mat)) && length(colnames(sim_mat)) == nrow(sim_mat)) {
        rownames(sim_mat) <- colnames(sim_mat)
      } else {
        rownames(sim_mat) <- paste0("Drug_", seq_len(nrow(sim_mat)))
      }
    }
    if (is.null(colnames(sim_mat)) || any(!nzchar(colnames(sim_mat)))) {
      colnames(sim_mat) <- rownames(sim_mat)
    }

    auto_file_name <- is.null(file_name)
    if (auto_file_name) {
      file_name <- tempfile("plot_drug_similarity_")
    }

    if (is.null(object)) {
      object <- PlotObject(
        input_data = as.data.frame(sim_mat, stringsAsFactors = FALSE),
        file_type = file_type,
        file_name = file_name,
        width = width,
        height = height,
        units = units
      )
      object@parameters$plot_type <- plot_type
      object@parameters$heatmap_type <- heatmap_type
      object@parameters$label_size <- label_size
      object@parameters$auto_file_name <- auto_file_name
    }

    standardGeneric("plot_drug_similarity")
  }
)

#' @rdname plot_drug_similarity
#' @export
setMethod(
  "plot_drug_similarity",
  signature = "PlotObject",
  function(object, plot_type, heatmap_type, label_size) {

    params <- object@parameters
    sim_mat <- as.matrix(params$input_data)

    if (missing(plot_type) || is.null(plot_type)) {
      plot_type <- if (!is.null(params$plot_type)) params$plot_type else "heatmap"
    }
    if (missing(heatmap_type) || is.null(heatmap_type)) {
      heatmap_type <- if (!is.null(params$heatmap_type)) params$heatmap_type else "triangle"
    }
    if (missing(label_size) || is.null(label_size)) {
      label_size <- if (!is.null(params$label_size)) params$label_size else 3
    }

    plot_type <- match.arg(plot_type, c("heatmap", "dendrogram"))
    heatmap_type <- match.arg(heatmap_type, c("triangle", "full"))

    if (plot_type == "heatmap") {
      methods <- colnames(sim_mat)
      plot_df <- as.data.frame(as.table(sim_mat), stringsAsFactors = FALSE)
      names(plot_df) <- c("method_1", "method_2", "similarity")

      plot_df <- plot_df %>%
        dplyr::mutate(
          method_1 = factor(method_1, levels = methods),
          method_2 = factor(method_2, levels = methods)
        )

      if (heatmap_type == "triangle") {
        plot_df <- plot_df %>%
          dplyr::mutate(
            similarity = ifelse(as.integer(method_1) > as.integer(method_2), NA, similarity)
          )
      }


      p <- ggplot2::ggplot(
        plot_df,
        ggplot2::aes(x = method_1, y = method_2, fill = similarity)
      ) +
        ggplot2::geom_tile(color = "white") +
        ggplot2::scale_fill_gradient(
          low = "white",
          high = "steelblue",
          limits = c(0, 1),
          na.value = "white"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
        ) +
        ggplot2::labs(
          x = "Drug",
          y = "Drug",
          fill = "Similarity"
        ) +
        ggplot2::coord_fixed()

      if (!isTRUE(params$auto_file_name) && !is.null(params$file_name) && nzchar(params$file_name)) {
        ggplot2::ggsave(
          filename = paste0(params$file_name, ".", params$file_type),
          plot = p,
          width = params$width,
          height = params$height,
          units = params$units
        )
      }

      return(p)
    }

    # Dendrogram mode
    d <- stats::as.dist(1 - sim_mat)
    hc <- stats::hclust(d)

    graphics::plot(stats::as.dendrogram(hc), main = "Drug Similarity Dendrogram", ylab = "1 - Similarity")

    if (!isTRUE(params$auto_file_name) && !is.null(params$file_name) && nzchar(params$file_name)) {
      out_file <- paste0(params$file_name, ".", params$file_type)
      width <- params$width
      height <- params$height
      units <- params$units

      open_device <- function() {
        if (params$file_type == "pdf") {
          grDevices::pdf(out_file, width = width, height = height)
        } else if (params$file_type == "svg") {
          grDevices::svg(out_file, width = width, height = height)
        } else if (params$file_type %in% c("png", "jpeg")) {
          res <- 300
          px_scale <- switch(
            units,
            "px" = 1,
            "in" = res,
            "cm" = res / 2.54,
            "mm" = res / 25.4,
            res
          )
          w_px <- as.integer(width * px_scale)
          h_px <- as.integer(height * px_scale)
          if (params$file_type == "png") {
            grDevices::png(out_file, width = w_px, height = h_px, res = res)
          } else {
            grDevices::jpeg(out_file, width = w_px, height = h_px, res = res)
          }
        } else {
          stop("Unsupported file type for dendrogram output.")
        }
      }

      open_device()
      on.exit(grDevices::dev.off(), add = TRUE)
      graphics::plot(stats::as.dendrogram(hc), main = "Drug Similarity Dendrogram", ylab = "1 - Similarity")
    }

    invisible(hc)
  }
)
