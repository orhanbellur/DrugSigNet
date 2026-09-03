#' @title Plot Enriched Terms
#'
#' @description
#' Visualizes enrichment results as a dot plot or bar plot.
#'
#' @details
#' `plot_enriched_terms()` displays enriched terms from GO or similar enrichment
#' output. Input data must contain `Term`, `ont`, `Adjusted.P.value`, and
#' `Overlap`. `Overlap` must be in `"x/y"` format and is used to calculate the
#' gene ratio.
#'
#' Terms are cleaned by removing GO identifiers from labels before plotting.
#'
#' @inheritParams plot_top_k_overlap
#' @param data_df Data frame containing enrichment results.
#' @param show_category Number of top terms to show per ontology. Default is
#'   `10`.
#' @param plottype Plot type. One of `"dotplot"` or `"barplot"`.
#' @param size Base text size and dot-size scaling aid. Default is `3`.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' enrich_df <- data.frame(
#'   Term = c("apoptotic process (GO:0006915)", "cell cycle (GO:0007049)"),
#'   ont = c("BP", "BP"),
#'   Adjusted.P.value = c(0.001, 0.02),
#'   Overlap = c("5/100", "3/80")
#' )
#'
#' plot_enriched_terms(enrich_df, plottype = "dotplot")
#' plot_enriched_terms(enrich_df, plottype = "barplot")
#' }
#'
#' @export
setGeneric(
  "plot_enriched_terms",
  function(
    object = NULL,
    data_df,
    show_category = 10,
    plottype = c("dotplot", "barplot"),
    file_type = "pdf",
    file_name = NULL,
    width = 20,
    height = 15,
    size = 3
  ) {
    plottype <- match.arg(plottype)
    file_type <- match.arg(file_type, c("pdf", "png", "jpeg", "svg"))

    data_df_missing <- missing(data_df)
    if (data_df_missing && is.data.frame(object)) {
      data_df <- object
      data_df_missing <- FALSE
      object <- NULL
    }

    if (!data_df_missing && !is.data.frame(data_df)) {
      stop("'data_df' must be a data.frame.")
    }

    if (!is.numeric(show_category) || length(show_category) != 1 || show_category <= 0) {
      stop("'show_category' must be a single positive numeric value.")
    }

    if (!is.numeric(size) || length(size) != 1 || size <= 0) {
      stop("'size' must be a single positive numeric value.")
    }

    if (is.null(object)) {
      if (data_df_missing) {
        stop("'data_df' must be provided when 'object' is NULL.")
      }

      object <- methods::new(
        "PlotObject",
        parameters = list(
          input_data = as.data.frame(data_df),
          show_category = show_category,
          plottype = plottype,
          file_type = file_type,
          file_name = file_name,
          width = width,
          height = height,
          size = size,
          units = "in"
        )
      )
    }

    standardGeneric("plot_enriched_terms")
  }
)

#' @rdname plot_enriched_terms
#' @param object A `PlotObject` containing parameters for the plot.
#' @return A `ggplot` object representing the enriched terms plot.
#' @keywords internal
#' @export
setMethod(
  "plot_enriched_terms",
  signature = "PlotObject",
  function(object) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop("Package 'ggplot2' is required for plot_enriched_terms.")
    }

    params <- object@parameters
    input_data <- params$input_data

    if (is.null(input_data) || !is.data.frame(input_data)) {
      stop("'data_df' must be a data.frame.")
    }

    required_cols <- c("Term", "ont", "Adjusted.P.value", "Overlap")
    missing_cols <- setdiff(required_cols, colnames(input_data))
    if (length(missing_cols) > 0) {
      stop("Missing required column(s): ", paste(missing_cols, collapse = ", "))
    }

    show_category <- if (!is.null(params$show_category)) params$show_category else 10
    plottype <- if (!is.null(params$plottype)) params$plottype else "dotplot"
    plottype <- match.arg(plottype, c("dotplot", "barplot"))
    size <- if (!is.null(params$size)) params$size else 3

    df <- input_data[, required_cols, drop = FALSE]
    df <- df[!is.na(df$Term) & !is.na(df$ont) & !is.na(df$Adjusted.P.value) & !is.na(df$Overlap), , drop = FALSE]

    if (nrow(df) == 0) {
      stop("No valid rows available after removing missing values.")
    }

    df$Term <- sub(" \\(GO:[^)]+\\)", "", as.character(df$Term))
    df$Term <- trimws(df$Term)
    df$ont <- trimws(as.character(df$ont))

    overlap_split <- strsplit(as.character(df$Overlap), "/", fixed = TRUE)
    valid_overlap <- lengths(overlap_split) == 2
    if (!all(valid_overlap)) {
      stop("'Overlap' values must be in 'x/y' format.")
    }

    df$Overlap_count <- suppressWarnings(as.numeric(vapply(overlap_split, `[`, character(1), 1)))
    df$Total_count <- suppressWarnings(as.numeric(vapply(overlap_split, `[`, character(1), 2)))

    if (any(is.na(df$Overlap_count)) || any(is.na(df$Total_count)) || any(df$Total_count <= 0)) {
      stop("'Overlap' must contain numeric counts with positive denominators.")
    }

    df$GeneRatio <- df$Overlap_count / df$Total_count

    split_by_ont <- split(df, df$ont)
    top_list <- lapply(split_by_ont, function(x) {
      x <- x[order(x$Adjusted.P.value, decreasing = FALSE), , drop = FALSE]
      x[seq_len(min(nrow(x), as.integer(show_category))), , drop = FALSE]
    })
    plot_df <- do.call(rbind, top_list)
    rownames(plot_df) <- NULL

    if (nrow(plot_df) == 0) {
      stop("No rows available for plotting after category filtering.")
    }

    if (plottype == "dotplot") {
      p <- ggplot2::ggplot(
        plot_df,
        ggplot2::aes(
          x = GeneRatio,
          y = reorder(Term, GeneRatio),
          size = Overlap_count,
          color = Adjusted.P.value
        )
      ) +
        ggplot2::geom_point(alpha = 0.85) +
        ggplot2::scale_color_gradient(low = "#2C7BB6", high = "#D7191C", name = "Adjusted P-value") +
        ggplot2::scale_size_continuous(name = "Overlap Count", range = c(max(2, size), max(6, size * 2.5))) +
        ggplot2::facet_grid(rows = ggplot2::vars(ont), scales = "free_y") +
        ggplot2::labs(x = "GeneRatio", y = "Term") +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(
          panel.grid.major.y = ggplot2::element_blank(),
          strip.text.y.right = ggplot2::element_text(face = "bold"),
          legend.position = "right"
        )
    } else {
      p <- ggplot2::ggplot(
        plot_df,
        ggplot2::aes(
          x = Overlap_count,
          y = reorder(Term, Overlap_count),
          fill = Adjusted.P.value
        )
      ) +
        ggplot2::geom_col() +
        ggplot2::scale_fill_gradient(low = "#2C7BB6", high = "#D7191C", name = "Adjusted P-value") +
        ggplot2::facet_grid(rows = ggplot2::vars(ont), scales = "free_y") +
        ggplot2::labs(x = "Overlap Count", y = "Term") +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(
          panel.grid.major.y = ggplot2::element_blank(),
          strip.text.y.right = ggplot2::element_text(face = "bold"),
          legend.position = "right",
          axis.text.y = ggplot2::element_text(size = max(8, size * 3))
        )
    }

    if (!is.null(params$file_name) && nzchar(params$file_name)) {
      ggplot2::ggsave(
        filename = paste0(params$file_name, ".", params$file_type),
        plot = p,
        width = params$width,
        height = params$height
      )
    }

    return(p)
  }
)
