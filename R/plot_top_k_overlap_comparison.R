#' Compare Top-K Drug Overlap Between Two Ranking Results
#'
#' Creates a heatmap showing pairwise drug overlap between ranking methods from
#' two data frames. Drug identifiers are trimmed and compared
#' case-insensitively. Cell labels show the total overlap and, when status
#' information is available, the number of overlapping drugs with non-missing
#' status as `total (status)`.
#'
#' @param df_x,df_y Data frames containing a shared drug identifier column and
#'   numeric ranking columns. `df_x` may also be a `PlotObject`. Smaller rank
#'   values are treated as better.
#' @param top_k Maximum rank included in each drug set.
#' @param drug_col Name of the shared drug identifier column.
#' @param methods_x,methods_y Optional ordered ranking columns from `df_x` and
#'   `df_y`. When `NULL`, all numeric columns are used.
#' @param status_df Optional data frame containing `drug_col` and `status_col`.
#'   Drugs with non-missing status values are counted in the status overlap.
#'   When `NULL`, a `"Status"` column in either ranking input is used when
#'   available.
#' @param status_col Name of the status column in `status_df`.
#' @param x_axis_title,y_axis_title Axis titles for the methods from the first
#'   and second ranking results, respectively.
#' @param file_type Output format: `"pdf"`, `"png"`, `"svg"`, or `"jpeg"`.
#' @param file_name Optional output filename without extension. When `NULL`,
#'   the plot is returned without being saved.
#' @param width,height Dimensions of the saved plot.
#' @param label_size Heatmap cell-label size.
#' @param base_size Base theme text size.
#' @param split_grouped_drugs Logical indicating whether grouped drug labels in
#'   `drug_col` should be split before overlap calculation.
#' @param drug_sep Regular expression used to separate grouped drug labels.
#' @param units Units used for `width` and `height`.
#'
#' @return A `ggplot` object. Its `data` component contains the pairwise overlap
#'   table, including total overlap, status overlap, and intersecting drug
#'   identifiers.
#'
#' @examples
#' df_a <- data.frame(
#'   Drug = c("Drug A", "Drug B", "Drug C"),
#'   Method_A1 = c(1, 2, 3),
#'   Method_A2 = c(2, 1, 3)
#' )
#'
#' df_b <- data.frame(
#'   Drug = c("drug a", "drug c", "drug d"),
#'   Method_B1 = c(1, 2, 3)
#' )
#'
#' status_data <- data.frame(
#'   Drug = c("Drug A", "Drug B", "Drug C", "Drug D"),
#'   Status = c("Strong", "Weak", NA, "No effect")
#' )
#'
#' comparison_plot <- plot_top_k_overlap_comparison(
#'   df_x = df_a,
#'   df_y = df_b,
#'   top_k = 100,
#'   status_df = status_data
#' )
#'
#' comparison_plot
#' comparison_plot$data
#'
#' @export
setGeneric(
  "plot_top_k_overlap_comparison",
  function(df_x, df_y, top_k = 100,
           drug_col = "Drug",
           methods_x = NULL, methods_y = NULL,
           file_type = "pdf", file_name = NULL,
           status_df = NULL, status_col = "Status",
           x_axis_title = "Methods in first result",
           y_axis_title = "Methods in second result",
           width = 20, height = 15, label_size = 3, base_size = 11,
           split_grouped_drugs = FALSE, drug_sep = "\\|",
           units = "in") {
    if (is.data.frame(df_x)) {
      .validate_top_k_comparison_inputs(
        df_x, df_y, top_k, drug_col, methods_x, methods_y,
        status_df, status_col, x_axis_title, y_axis_title,
        label_size, base_size, split_grouped_drugs, drug_sep
      )
      auto_file_name <- is.null(file_name)
      if (auto_file_name) {
        file_name <- tempfile("plot_top_k_overlap_comparison_")
      }
      object <- PlotObject(
        input_data = df_x,
        file_type = file_type,
        file_name = file_name,
        width = width,
        height = height,
        units = units
      )
      object@parameters$df_y <- df_y
      object@parameters$drug_col <- drug_col
      object@parameters$methods_x <- methods_x
      object@parameters$methods_y <- methods_y
      object@parameters$status_df <- status_df
      object@parameters$status_col <- status_col
      object@parameters$x_axis_title <- x_axis_title
      object@parameters$y_axis_title <- y_axis_title
      object@parameters$base_size <- base_size
      object@parameters$split_grouped_drugs <- split_grouped_drugs
      object@parameters$drug_sep <- drug_sep
      object@parameters$auto_file_name <- auto_file_name
      df_x <- object
    }

    standardGeneric("plot_top_k_overlap_comparison")
  }
)

#' @rdname plot_top_k_overlap_comparison
#' @export
setMethod(
  "plot_top_k_overlap_comparison",
  signature = c(df_x = "PlotObject"),
  function(df_x, df_y, top_k, drug_col, methods_x, methods_y,
           status_df, status_col, x_axis_title, y_axis_title,
           label_size, base_size, split_grouped_drugs, drug_sep) {
    object <- df_x
    params <- object@parameters
    df_x <- params$input_data
    if (missing(df_y) || is.null(df_y)) df_y <- params$df_y
    if (missing(top_k) || is.null(top_k)) top_k <- 100
    if (missing(drug_col) || is.null(drug_col)) {
      drug_col <- if (is.null(params$drug_col)) "Drug" else params$drug_col
    }
    if (missing(methods_x)) methods_x <- params$methods_x
    if (missing(methods_y)) methods_y <- params$methods_y
    if (missing(status_df)) status_df <- params$status_df
    if (missing(status_col) || is.null(status_col)) {
      status_col <- if (is.null(params$status_col)) "Status" else params$status_col
    }
    if (missing(x_axis_title) || is.null(x_axis_title)) {
      x_axis_title <- if (is.null(params$x_axis_title)) "Methods in first result" else params$x_axis_title
    }
    if (missing(y_axis_title) || is.null(y_axis_title)) {
      y_axis_title <- if (is.null(params$y_axis_title)) "Methods in second result" else params$y_axis_title
    }
    if (missing(label_size) || is.null(label_size)) label_size <- 3
    if (missing(base_size) || is.null(base_size)) {
      base_size <- if (is.null(params$base_size)) 11 else params$base_size
    }
    if (missing(split_grouped_drugs) || is.null(split_grouped_drugs)) {
      split_grouped_drugs <- isTRUE(params$split_grouped_drugs)
    }
    if (missing(drug_sep) || is.null(drug_sep)) {
      drug_sep <- if (is.null(params$drug_sep)) "\\|" else params$drug_sep
    }

    .validate_top_k_comparison_inputs(
      df_x, df_y, top_k, drug_col, methods_x, methods_y,
      status_df, status_col, x_axis_title, y_axis_title,
      label_size, base_size, split_grouped_drugs, drug_sep
    )
    methods_x <- .resolve_top_k_comparison_methods(df_x, methods_x, "methods_x", drug_col)
    methods_y <- .resolve_top_k_comparison_methods(df_y, methods_y, "methods_y", drug_col)
    overlap_table <- .compute_top_k_overlap_comparison(
      df_x, df_y, methods_x, methods_y, top_k, drug_col,
      status_df, status_col, split_grouped_drugs, drug_sep
    )

    plot <- ggplot2::ggplot(
      overlap_table,
      ggplot2::aes(x = Method_x, y = Method_y, fill = n_overlap)
    ) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::geom_text(ggplot2::aes(label = label), size = label_size) +
      ggplot2::scale_fill_gradient(low = "white", high = "steelblue") +
      ggplot2::labs(
        x = x_axis_title,
        y = y_axis_title,
        fill = "Overlap"
      ) +
      ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        axis.title = ggplot2::element_text(face = "bold"),
        legend.title = ggplot2::element_text(face = "bold")
      ) +
      ggplot2::scale_y_discrete(limits = rev(levels(overlap_table$Method_y))) +
      ggplot2::coord_fixed()

    if (!isTRUE(params$auto_file_name) && !is.null(params$file_name) && nzchar(params$file_name)) {
      ggplot2::ggsave(
        filename = paste0(params$file_name, ".", params$file_type),
        plot = plot,
        width = params$width,
        height = params$height,
        units = params$units
      )
    }
    plot
  }
)

.validate_top_k_comparison_inputs <- function(df_x, df_y, top_k, drug_col,
                                              methods_x, methods_y,
                                              status_df, status_col,
                                              x_axis_title, y_axis_title,
                                              label_size, base_size,
                                              split_grouped_drugs, drug_sep) {
  if (!is.character(drug_col) || length(drug_col) != 1L || is.na(drug_col) || !nzchar(drug_col)) {
    stop("`drug_col` must be a single non-empty string.", call. = FALSE)
  }
  for (item in list(df_x = df_x, df_y = df_y)) {
    if (!is.data.frame(item)) stop("Both `df_x` and `df_y` must be data frames.", call. = FALSE)
    if (!drug_col %in% names(item)) stop("Both inputs must contain `", drug_col, "`.", call. = FALSE)
  }
  if (!is.numeric(top_k) || length(top_k) != 1L || !is.finite(top_k) || top_k <= 0) {
    stop("`top_k` must be a single positive finite number.", call. = FALSE)
  }
  for (selection in list(methods_x = methods_x, methods_y = methods_y)) {
    if (!is.null(selection) &&
        (!is.character(selection) || !length(selection) || anyNA(selection) || any(!nzchar(selection)))) {
      stop("Method selections must be NULL or non-empty character vectors.", call. = FALSE)
    }
  }
  if (!is.character(status_col) || length(status_col) != 1L || is.na(status_col) || !nzchar(status_col)) {
    stop("`status_col` must be a single non-empty character string.", call. = FALSE)
  }
  if (!is.null(status_df)) {
    if (!is.data.frame(status_df) || !all(c(drug_col, status_col) %in% names(status_df))) {
      stop("`status_df` must be NULL or a data frame containing `drug_col` and `status_col`.", call. = FALSE)
    }
  }
  axis_titles <- list(x_axis_title = x_axis_title, y_axis_title = y_axis_title)
  invalid_axis_titles <- !vapply(
    axis_titles,
    function(title) is.character(title) && length(title) == 1L && !is.na(title) && nzchar(title),
    logical(1)
  )
  if (any(invalid_axis_titles)) {
    stop(
      "Axis titles must each be a single non-empty character string: ",
      paste(names(axis_titles)[invalid_axis_titles], collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  for (size in list(label_size = label_size, base_size = base_size)) {
    if (!is.numeric(size) || length(size) != 1L || !is.finite(size) || size <= 0) {
      stop("`label_size` and `base_size` must be positive finite numbers.", call. = FALSE)
    }
  }
  if (!is.logical(split_grouped_drugs) || length(split_grouped_drugs) != 1L || is.na(split_grouped_drugs)) {
    stop("`split_grouped_drugs` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.character(drug_sep) || length(drug_sep) != 1L || is.na(drug_sep) || !nzchar(drug_sep)) {
    stop("`drug_sep` must be a single non-empty character string.", call. = FALSE)
  }
}

.resolve_top_k_comparison_methods <- function(data, methods, arg, drug_col) {
  if (is.null(methods)) methods <- names(data)[vapply(data, is.numeric, logical(1))]
  methods <- unique(setdiff(methods, c(drug_col, "Status")))
  missing_methods <- setdiff(methods, names(data))
  if (length(missing_methods)) {
    stop("Columns in `", arg, "` not found: ", paste(missing_methods, collapse = ", "), ".", call. = FALSE)
  }
  non_numeric <- methods[!vapply(data[, methods, drop = FALSE], is.numeric, logical(1))]
  if (length(non_numeric)) {
    stop("Ranking columns must be numeric: ", paste(non_numeric, collapse = ", "), ".", call. = FALSE)
  }
  if (!length(methods)) stop("Each input requires at least one numeric ranking column.", call. = FALSE)
  methods
}

.compute_top_k_overlap_comparison <- function(df_x, df_y, methods_x, methods_y,
                                              top_k, drug_col,
                                              status_df, status_col,
                                              split_grouped_drugs, drug_sep) {
  clean <- function(x) tolower(trimws(as.character(x)))
  drug_sets <- function(data, methods) {
    drugs <- clean(data[[drug_col]])
    stats::setNames(lapply(methods, function(method) {
      rank <- data[[method]]
      selected <- drugs[!is.na(rank) & rank <= top_k & !is.na(drugs) & nzchar(drugs)]
      if (split_grouped_drugs && length(selected)) {
        selected <- unlist(strsplit(selected, split = drug_sep), use.names = FALSE)
        selected <- clean(selected)
      }
      unique(selected[!is.na(selected) & nzchar(selected)])
    }), methods)
  }
  sets_x <- drug_sets(df_x, methods_x)
  sets_y <- drug_sets(df_y, methods_y)

  status_sources <- if (!is.null(status_df)) {
    list(status_df[c(drug_col, status_col)])
  } else {
    lapply(Filter(function(data) "Status" %in% names(data), list(df_x, df_y)),
           function(data) data[c(drug_col, "Status")])
  }
  valid_drugs <- unique(unlist(lapply(status_sources, function(data) {
    values <- data[[2L]]
    drugs <- clean(data[[drug_col]][!is.na(values)])
    if (split_grouped_drugs && length(drugs)) {
      drugs <- clean(unlist(strsplit(drugs, split = drug_sep), use.names = FALSE))
    }
    drugs[!is.na(drugs) & nzchar(drugs)]
  }), use.names = FALSE))
  use_status <- length(status_sources) > 0L

  pairs <- expand.grid(Method_x = methods_x, Method_y = methods_y, stringsAsFactors = FALSE)
  overlaps <- Map(function(method_x, method_y) {
    drugs <- intersect(sets_x[[method_x]], sets_y[[method_y]])
    c(
      n_overlap = length(drugs),
      n_status_overlap = length(intersect(drugs, valid_drugs)),
      overlap_drugs = paste(drugs, collapse = "|")
    )
  }, pairs$Method_x, pairs$Method_y)
  result <- cbind(pairs, as.data.frame(do.call(rbind, overlaps), stringsAsFactors = FALSE))
  result$n_overlap <- as.integer(result$n_overlap)
  result$n_status_overlap <- as.integer(result$n_status_overlap)
  result$Method_x <- factor(result$Method_x, levels = methods_x)
  result$Method_y <- factor(result$Method_y, levels = methods_y)
  result$label <- if (use_status) {
    paste0(result$n_overlap, "\n(", result$n_status_overlap, ")")
  } else {
    as.character(result$n_overlap)
  }
  result
}
