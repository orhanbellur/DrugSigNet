#' @title Plot Drug Hierarchy
#'
#' @description
#' Visualizes hierarchical drug annotations as an interactive sunburst chart or
#' static alluvial plot.
#'
#' @details
#' `plot_drug_hierarchy()` summarizes hierarchical annotation columns such as
#' ATC class, mechanism of action, indication, or drug name. The hierarchy is
#' defined by `hierarchy_cols` in outer-to-inner order by default.
#'
#' If `value_col` is `NULL`, row counts are used unless a numeric count column
#' such as `N` can be inferred. If `value_col` is supplied, numeric values are
#' aggregated across hierarchy levels.
#'
#' Missing or empty hierarchy values are replaced by `na_label`. Values
#' containing multiple terms separated by `"|"` or `";"` are reduced to the
#' first term before plotting. Selected hierarchy columns can also be expanded
#' with `split_cols`.
#'
#' @inheritParams plot_enriched_terms
#' @param hierarchy_cols Character vector of columns defining the hierarchy. If
#'   `NULL`, hierarchy columns are inferred from non-numeric columns.
#' @param value_col Optional numeric column used as the aggregation value. If
#'   `NULL`, row counts are used unless a count column can be inferred.
#' @param split_cols Optional hierarchy columns to split into multiple rows.
#' @param split_pattern Regular expression used to split `split_cols`. Default
#'   is `"\\|"`.
#' @param na_label Label used for missing or empty hierarchy values. Default is
#'   `"Unknown"`.
#' @param plot_type Plot type. One of `"sunburst"` or `"alluvial"`. Default is
#'   `"sunburst"`.
#' @param layer_order Hierarchy order. Use `"outer_to_inner"` to keep
#'   `hierarchy_cols` as supplied or `"inner_to_outer"` to reverse them.
#' @param scale Image export scale factor for sunburst plots. Default is `5`.
#' @param colors Optional vector of colors for sunburst sectors.
#'
#' @return
#' A plotly sunburst plot when `plot_type = "sunburst"`, or a ggplot alluvial
#' plot when `plot_type = "alluvial"`.
#'
#' @examples
#' \dontrun{
#' hierarchy_df <- data.frame(
#'   ATC = c("Nervous system", "Nervous system", "Antineoplastic"),
#'   MoA = c("Calcium channel blocker", "Dopamine antagonist", "Kinase inhibitor"),
#'   Drug = c("nifedipine", "haloperidol", "imatinib"),
#'   N = c(3, 2, 5)
#' )
#'
#' p1 <- plot_drug_hierarchy(
#'   data_df = hierarchy_df,
#'   hierarchy_cols = c("ATC", "MoA", "Drug"),
#'   value_col = "N",
#'   plot_type = "sunburst"
#' )
#'
#' p2 <- plot_drug_hierarchy(
#'   data_df = hierarchy_df,
#'   hierarchy_cols = c("ATC", "MoA", "Drug"),
#'   value_col = "N",
#'   plot_type = "alluvial"
#' )
#' }
#'
#' @export
setGeneric(
  "plot_drug_hierarchy",
  function(
    object = NULL,
    data_df,
    hierarchy_cols = NULL,
    value_col = NULL,
    split_cols = NULL,
    split_pattern = "\\|",
    na_label = "Unknown",
    plot_type = c("sunburst", "alluvial"),
    layer_order = c("outer_to_inner", "inner_to_outer"),
    file_type = "pdf",
    file_name = NULL,
    width = 1200,
    height = 1200,
    scale = 5,
    colors = NULL
  ) {
    plot_type <- match.arg(plot_type)
    layer_order <- match.arg(layer_order)
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

    infer_default_value_col <- function(df, hierarchy_cols) {
      numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      numeric_cols <- setdiff(numeric_cols, hierarchy_cols)

      if ("N" %in% numeric_cols) {
        return("N")
      }

      if (length(numeric_cols) == 1) {
        return(numeric_cols)
      }

      NULL
    }

    if (is.null(hierarchy_cols)) {
      if (data_df_missing) {
        stop("'hierarchy_cols' must be provided when it cannot be inferred from data.")
      }

      non_numeric_cols <- names(data_df)[!vapply(data_df, is.numeric, logical(1))]
      non_numeric_cols <- non_numeric_cols[!vapply(data_df[non_numeric_cols], is.logical, logical(1))]
      hierarchy_cols <- if (length(non_numeric_cols) >= 2) non_numeric_cols else NULL

      if (is.null(hierarchy_cols)) {
        stop(
          "'hierarchy_cols' must be provided or inferable from input data. ",
          "Provide at least two non-numeric hierarchy columns."
        )
      }
    }

    if (is.null(value_col) && !data_df_missing) {
      value_col <- infer_default_value_col(data_df, hierarchy_cols)
    }

    if (!is.character(hierarchy_cols) || length(hierarchy_cols) < 2) {
      stop("'hierarchy_cols' must be a character vector with at least two columns.")
    }

    if (!is.null(value_col) && (!is.character(value_col) || length(value_col) != 1)) {
      stop("'value_col' must be NULL or a single character string.")
    }

    if (!is.null(split_cols) && !is.character(split_cols)) {
      stop("'split_cols' must be NULL or a character vector.")
    }

    if (!is.character(split_pattern) || length(split_pattern) != 1) {
      stop("'split_pattern' must be a single character string.")
    }

    if (!is.character(na_label) || length(na_label) != 1) {
      stop("'na_label' must be a single character string.")
    }

    if (!is.numeric(width) || length(width) != 1 || is.na(width) || width <= 0) {
      stop("'width' must be a single positive number.")
    }

    if (!is.numeric(height) || length(height) != 1 || is.na(height) || height <= 0) {
      stop("'height' must be a single positive number.")
    }

    if (!is.numeric(scale) || length(scale) != 1 || is.na(scale) || scale <= 0) {
      stop("'scale' must be a single positive number.")
    }

    if (is.null(object)) {
      if (data_df_missing) {
        stop("'data_df' must be provided when 'object' is NULL.")
      }

      object <- methods::new(
        "PlotObject",
        parameters = list(
          input_data = as.data.frame(data_df),
          hierarchy_cols = hierarchy_cols,
          value_col = value_col,
          split_cols = split_cols,
          split_pattern = split_pattern,
          na_label = na_label,
          plot_type = plot_type,
          layer_order = layer_order,
          file_type = file_type,
          file_name = file_name,
          width = width,
          height = height,
          units = "px",
          scale = scale,
          colors = colors
        )
      )
    }

    standardGeneric("plot_drug_hierarchy")
  }
)

#' @rdname plot_drug_hierarchy
#' @return A plotly sunburst plot (`plot_type = "sunburst"`) or a ggplot alluvial plot (`plot_type = "alluvial"`).
#' @keywords internal
#' @export
setMethod(
  "plot_drug_hierarchy",
  signature = "PlotObject",
  function(object) {
    params <- object@parameters
    input_data <- params$input_data
    hierarchy_cols <- params$hierarchy_cols
    value_col <- params$value_col
    split_cols <- params$split_cols
    split_pattern <- params$split_pattern
    na_label <- params$na_label
    plot_type <- if (!is.null(params$plot_type)) params$plot_type else "sunburst"
    plot_type <- match.arg(plot_type, c("sunburst", "alluvial"))
    layer_order <- if (!is.null(params$layer_order)) params$layer_order else "outer_to_inner"
    layer_order <- match.arg(layer_order, c("outer_to_inner", "inner_to_outer"))

    infer_default_value_col <- function(df, hierarchy_cols) {
      numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      numeric_cols <- setdiff(numeric_cols, hierarchy_cols)

      if ("N" %in% numeric_cols) {
        return("N")
      }

      if (length(numeric_cols) == 1) {
        return(numeric_cols)
      }

      NULL
    }

    if (is.null(input_data) || !is.data.frame(input_data)) {
      stop("'data_df' must be a data.frame.")
    }

    if (is.null(hierarchy_cols)) {
      non_numeric_cols <- names(input_data)[!vapply(input_data, is.numeric, logical(1))]
      non_numeric_cols <- non_numeric_cols[!vapply(input_data[non_numeric_cols], is.logical, logical(1))]
      hierarchy_cols <- if (length(non_numeric_cols) >= 2) non_numeric_cols else NULL

      if (is.null(hierarchy_cols)) {
        stop(
          "'hierarchy_cols' must be provided or inferable from input data. ",
          "Provide at least two non-numeric hierarchy columns."
        )
      }
    }

    if (is.null(value_col)) {
      value_col <- infer_default_value_col(input_data, hierarchy_cols)
    }

    if (identical(layer_order, "inner_to_outer")) {
      hierarchy_cols <- rev(hierarchy_cols)
    }

    missing_cols <- setdiff(hierarchy_cols, colnames(input_data))
    if (length(missing_cols) > 0) {
      stop("Missing hierarchy column(s): ", paste(missing_cols, collapse = ", "))
    }

    if (!is.null(value_col) && !(value_col %in% colnames(input_data))) {
      stop("Missing 'value_col': ", value_col)
    }

    if (!is.null(split_cols)) {
      bad_split_cols <- setdiff(split_cols, hierarchy_cols)
      if (length(bad_split_cols) > 0) {
        stop("'split_cols' must be a subset of 'hierarchy_cols'. Invalid: ",
             paste(bad_split_cols, collapse = ", "))
      }
    }

    cols_keep <- unique(c(hierarchy_cols, value_col))
    plot_data <- input_data[, cols_keep, drop = FALSE]

    first_term <- function(x) {
      vals <- as.character(x)
      vals <- vapply(vals, function(v) {
        if (is.na(v)) return(NA_character_)
        parts <- trimws(unlist(strsplit(v, "[\\|;]", perl = TRUE), use.names = FALSE))
        parts <- parts[nzchar(parts)]
        if (length(parts) == 0) return(NA_character_)
        parts[[1]]
      }, character(1))
      vals
    }

    for (col_nm in hierarchy_cols) {
      plot_data[[col_nm]] <- first_term(plot_data[[col_nm]])
    }

    hierarchy_display <- ifelse(hierarchy_cols == "level1_description", "ATC", hierarchy_cols)
    if (!identical(hierarchy_display, hierarchy_cols)) {
      idx <- match(hierarchy_cols, names(plot_data))
      names(plot_data)[idx] <- hierarchy_display
      hierarchy_cols <- hierarchy_display
      if (!is.null(split_cols)) {
        split_cols <- ifelse(split_cols == "level1_description", "ATC", split_cols)
      }
    }

    # Fast row expansion for split columns (avoids repeated tidyr overhead)
    if (!is.null(split_cols) && length(split_cols) > 0) {
      for (col_nm in split_cols) {
        raw_vals <- as.character(plot_data[[col_nm]])
        pieces <- strsplit(raw_vals, split_pattern, perl = TRUE)
        pieces <- lapply(pieces, function(x) {
          if (length(x) == 0) return(NA_character_)
          x
        })

        lens <- lengths(pieces)
        idx <- rep.int(seq_len(nrow(plot_data)), lens)
        expanded <- plot_data[idx, , drop = FALSE]
        expanded[[col_nm]] <- unlist(pieces, use.names = FALSE)
        plot_data <- expanded
      }
    }

    for (col_nm in hierarchy_cols) {
      vals <- as.character(plot_data[[col_nm]])
      vals <- trimws(vals)
      vals[is.na(vals) | vals == ""] <- na_label
      plot_data[[col_nm]] <- vals
    }

    if (is.null(value_col)) {
      plot_data$value_internal <- 1
      value_col <- "value_internal"
    } else {
      if (!is.numeric(plot_data[[value_col]])) {
        stop("'value_col' must refer to a numeric column.")
      }
      plot_data[[value_col]][is.na(plot_data[[value_col]])] <- 0
    }

    if (plot_type == "alluvial") {
      if (!requireNamespace("ggalluvial", quietly = TRUE)) {
        stop("Package 'ggalluvial' is required for plot_type = 'alluvial'.")
      }
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package 'ggplot2' is required for plot_type = 'alluvial'.")
      }

      alluvial_df <- plot_data[, c(hierarchy_cols, value_col), drop = FALSE]
      alluvial_df$Freq <- alluvial_df[[value_col]]
      alluvial_df$AlluviumID <- seq_len(nrow(alluvial_df))

      long_df <- tidyr::pivot_longer(
        alluvial_df,
        cols = tidyselect::all_of(hierarchy_cols),
        names_to = "Axis",
        values_to = "Stratum"
      )
      long_df$Axis <- factor(long_df$Axis, levels = hierarchy_cols)

      p <- ggplot2::ggplot(
        long_df,
        ggplot2::aes(
          x = Axis,
          stratum = Stratum,
          alluvium = AlluviumID,
          y = Freq,
          fill = Stratum,
          label = Stratum
        )
      ) +
        ggalluvial::geom_alluvium(alpha = 0.8) +
        ggalluvial::geom_stratum(width = 0.8) +
        ggalluvial::stat_stratum(geom = "text", size = 2.8) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          legend.position = "none",
          axis.text.y = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank()
        )

      if (!is.null(params$file_name) && nzchar(params$file_name)) {
        ggplot2::ggsave(
          filename = paste0(params$file_name, ".", params$file_type),
          plot = p,
          width = params$width / 100,
          height = params$height / 100
        )
      }

      return(p)
    }

    if (!requireNamespace("data.table", quietly = TRUE)) {
      stop("Package 'data.table' is required for plot_type = 'sunburst'.")
    }
    if (!requireNamespace("plotly", quietly = TRUE)) {
      stop("Package 'plotly' is required for plot_type = 'sunburst'.")
    }

    as.sunburstDF.internal <- function(df, hierarchy_cols, valueCol = NULL) {
      if (data.table::is.data.table(df)) {
        DT <- data.table::copy(df)
      } else {
        DT <- data.table::data.table(df, stringsAsFactors = FALSE)
      }

      colNamesDF <- names(df)
      DT$root <- hierarchy_cols[1]

      if (is.null(valueCol)) {
        data.table::setcolorder(DT, c("root", colNamesDF))
      } else {
        data.table::setnames(DT, valueCol, "values", skip_absent = TRUE)
        data.table::setcolorder(DT, c("root", setdiff(colNamesDF, valueCol), "values"))
      }

      colNamesDT <- names(DT)
      hierarchyCols <- setdiff(colNamesDT, "values")
      hierarchyList <- vector("list", length(hierarchyCols))
      DT_df <- as.data.frame(DT, stringsAsFactors = FALSE)

      if (is.null(valueCol)) {
        DT_df$values <- 1
      }

      for (i in seq_along(hierarchyCols)) {
        currentCols <- colNamesDT[1:i]

        currentDT <- stats::aggregate(
          x = DT_df$values,
          by = DT_df[, currentCols, drop = FALSE],
          FUN = function(x) sum(x, na.rm = TRUE)
        )
        colnames(currentDT)[ncol(currentDT)] <- "values"
        colnames(currentDT)[length(currentCols)] <- "labels"
        hierarchyList[[i]] <- currentDT
      }

      hierarchyDT <- data.table::rbindlist(
        lapply(hierarchyList, data.table::as.data.table),
        use.names = TRUE,
        fill = TRUE
      )

      parentCols <- setdiff(names(hierarchyDT), c("labels", "values", valueCol))
      hierarchyDT$parents <- apply(
        hierarchyDT[, parentCols, with = FALSE],
        1,
        function(x) {
          if (all(is.na(x))) {
            NA_character_
          } else {
            paste(x[!is.na(x)], collapse = " - ")
          }
        }
      )

      hierarchyDT$ids <- apply(
        hierarchyDT[, c("parents", "labels"), with = FALSE],
        1,
        function(x) paste(x[!is.na(x)], collapse = " - ")
      )

      data.table::set(hierarchyDT, j = parentCols, value = NULL)

      as.data.frame(hierarchyDT, stringsAsFactors = FALSE)
    }

    sunburst_df <- as.sunburstDF.internal(plot_data, hierarchy_cols, value_col)

    if (is.null(params$colors)) {
      colors <- c(
        "#E64B35B2", "#4DBBD5B2", "#00A087B2", "#3C5488B2",
        "#F39B7FB2", "#8491B4B2", "#91D1C2B2", "#DC0000B2", "#7E6148B2"
      )
    } else {
      colors <- params$colors
    }

    p <- plotly::plot_ly(
      data = sunburst_df,
      ids = ~ids,
      labels = ~labels,
      parents = ~parents,
      values = ~values,
      type = "sunburst",
      branchvalues = "total",
      marker = list(opacity = 0.7, colors = colors),
      textfont = list(size = 16, family = "Arial", color = "#000000"),
      width = params$width,
      height = params$height
    )

    p <- plotly::layout(
      p,
      margin = list(t = 40, b = 40, l = 40, r = 40)
    )

    if (!is.null(params$file_name) && nzchar(params$file_name)) {
      out_file <- paste0(params$file_name, ".", params$file_type)
      save_ok <- TRUE

      tryCatch(
        {
          plotly::save_image(
            p,
            out_file,
            width = params$width,
            height = params$height,
            scale = params$scale
          )
        },
        error = function(e) {
          save_ok <<- FALSE
          warning(
            "Failed to save sunburst plot with 'plotly::save_image()'. ",
            "Please ensure Plotly static export dependencies are installed ",
            "(e.g., kaleido). Original error: ",
            conditionMessage(e),
            call. = FALSE
          )
        }
      )

      if (isTRUE(save_ok) && file.exists(out_file)) {
        file_info <- file.info(out_file)
        if (!is.na(file_info$size) && file_info$size == 0) {
          unlink(out_file)
          save_ok <- FALSE
          warning(
            "Sunburst export created an empty file. Please ensure Plotly static ",
            "export dependencies are installed correctly (e.g., kaleido).",
            call. = FALSE
          )
        }
      }

      if (!isTRUE(save_ok) && requireNamespace("htmlwidgets", quietly = TRUE)) {
        html_out <- paste0(params$file_name, ".html")
        htmlwidgets::saveWidget(
          widget = p,
          file = html_out,
          selfcontained = TRUE
        )
        warning(
          "Saved interactive sunburst as HTML fallback at '", html_out,
          "' because static image export was unavailable.",
          call. = FALSE
        )
      }
    }

    if (interactive()) {
      print(p)
    }

    return(p)
  }
)
