#' @title Plot Drug Indication Distribution
#'
#' @description
#' Visualizes the frequency distribution of drug indications as a bar plot or
#' word cloud.
#'
#' @details
#' `plot_drug_indications()` summarizes values in the `indication` column. The
#' input data must contain `Drug` and `indication` columns.
#'
#' Before plotting, indications are converted to lower case, split on `"|"` or
#' `";"`, trimmed, cleaned for repeated whitespace, and counted by frequency.
#'
#' For `plottype = "barplot"`, the function returns a horizontal `ggplot`
#' object. For `plottype = "wordcloud"`, it returns a recorded base R plot.
#'
#' @inheritParams plot_enriched_terms
#' @param plottype Plot type. One of `"wordcloud"` or `"barplot"`. Default is
#'   `"wordcloud"`.
#' @param max_words Maximum number of indications to include in the word cloud.
#'   Default is `Inf`.
#' @param top_terms Number of top indications to include in the plot. Default is
#'   `100`.
#'
#' @return
#' A `ggplot` object for `plottype = "barplot"`, or a recorded base R plot for
#' `plottype = "wordcloud"`.
#'
#' @examples
#' data <- data.frame(
#'   Drug = c("DrugA", "DrugB", "DrugC", "DrugD"),
#'   indication = c(
#'     "Cancer|Tumor",
#'     "Diabetes ; Metabolic disease",
#'     "cancer",
#'     "Asthma"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' p <- plot_drug_indications(
#'   data_df = data,
#'   plottype = "barplot",
#'   top_terms = 10
#' )
#'
#' print(p)
#'
#' \dontrun{
#' wc <- plot_drug_indications(
#'   data_df = data,
#'   plottype = "wordcloud",
#'   max_words = 50
#' )
#'
#' plot_drug_indications(
#'   data_df = data,
#'   file_name = "drug_indications_plot",
#'   file_type = "pdf"
#' )
#' }
#'
#' @importFrom ggplot2 aes coord_flip geom_col ggsave ggplot labs theme_bw
#' @importFrom grDevices recordPlot replayPlot dev.off pdf png jpeg svg
#' @export
setGeneric(
  "plot_drug_indications",
  function(
    object = NULL,
    data_df,
    file_type = "pdf",
    file_name = NULL,
    width = 20,
    height = 15,
    plottype = c("wordcloud", "barplot"),
    max_words = Inf,
    top_terms = 100
  ) {
    file_type <- match.arg(file_type, c("pdf", "png", "svg", "jpeg"))
    plottype <- match.arg(plottype)

    if (!is.numeric(max_words) || length(max_words) != 1 || is.na(max_words) || max_words <= 0) {
      stop("'max_words' must be a single positive number or Inf.")
    }
    if (!is.infinite(max_words)) {
      max_words <- as.integer(max_words)
    }

    if (!is.numeric(top_terms) || length(top_terms) != 1 || is.na(top_terms) || top_terms <= 0) {
      stop("'top_terms' must be a single positive number or Inf.")
    }
    if (!is.infinite(top_terms)) {
      top_terms <- as.integer(top_terms)
    }

    data_df_missing <- missing(data_df)

    if (data_df_missing && is.data.frame(object)) {
      data_df <- object
      data_df_missing <- FALSE
      object <- NULL
    }

    if (!data_df_missing && !is.data.frame(data_df)) {
      stop("'data_df' must be a data.frame.")
    }

    if (is.null(object)) {
      if (data_df_missing) {
        stop("'data_df' must be provided when 'object' is NULL.")
      }

      object <- new(
        "PlotObject",
        parameters = list(
          input_data = as.data.frame(data_df),
          file_type = file_type,
          file_name = file_name,
          width = width,
          height = height,
          units = "in",
          plottype = plottype,
          max_words = max_words,
          top_terms = top_terms
        )
      )
    }

    standardGeneric("plot_drug_indications")
  }
)

#' @rdname plot_drug_indications
#' @return A `ggplot` object for `plottype = "barplot"`, or a recorded base plot for `plottype = "wordcloud"`.
#' @keywords internal
setMethod(
  "plot_drug_indications",
  signature = "PlotObject",
  function(object, plottype = c("wordcloud", "barplot"), max_words = Inf, top_terms = 100) {
    params <- object@parameters
    input_data <- params$input_data
    selected_plot_type <- if (!is.null(params$plottype)) params$plottype else plottype
    selected_plot_type <- match.arg(selected_plot_type, c("barplot", "wordcloud"))
    selected_max_words <- if (!is.null(params$max_words)) params$max_words else max_words
    selected_top_terms <- if (!is.null(params$top_terms)) params$top_terms else top_terms

    if (is.null(input_data) || !is.data.frame(input_data)) {
      stop("'data_df' must be a data.frame.")
    }

    required_cols <- c("Drug", "indication")
    missing_cols <- setdiff(required_cols, colnames(input_data))
    if (length(missing_cols) > 0) {
      stop(
        "Missing required column(s): ",
        paste(missing_cols, collapse = ", ")
      )
    }

    plot_data <- input_data[, c("Drug", "indication"), drop = FALSE]
    plot_data <- plot_data[!is.na(plot_data$indication), , drop = FALSE]

    if (nrow(plot_data) == 0) {
      stop("No valid rows found in column 'indication'.")
    }

    plot_data$indication <- tolower(plot_data$indication)

    split_indications <- strsplit(plot_data$indication, "[|;]")

    expanded_data <- data.frame(
      Drug = rep(plot_data$Drug, lengths(split_indications)),
      indication = unlist(split_indications, use.names = FALSE),
      stringsAsFactors = FALSE
    )

    expanded_data$indication <- trimws(expanded_data$indication)
    expanded_data$indication <- gsub("\\s+", " ", expanded_data$indication)

    expanded_data <- expanded_data[
      !is.na(expanded_data$indication) &
        nzchar(expanded_data$indication),
      ,
      drop = FALSE
    ]

    if (nrow(expanded_data) == 0) {
      stop("No valid indication values available after splitting and cleaning.")
    }

    indication_df <- as.data.frame(
      table(expanded_data$indication),
      stringsAsFactors = FALSE
    )
    colnames(indication_df) <- c("indication", "Freq")
    indication_df <- indication_df[order(indication_df$Freq, decreasing = TRUE), , drop = FALSE]

    if (!is.infinite(selected_top_terms)) {
      indication_df <- utils::head(indication_df, selected_top_terms)
    }

    if (nrow(indication_df) == 0) {
      stop("No indications available after applying 'top_terms'.")
    }

    indication_df$indication <- factor(indication_df$indication, levels = rev(indication_df$indication))

    if (selected_plot_type == "barplot") {
      p <- ggplot(indication_df, aes(x = indication, y = Freq)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        labs(
          x = "Indications",
          y = "Freq"
        ) +
        theme_bw()

      if (!is.null(params$file_name) && nzchar(params$file_name)) {
        ggsave(
          filename = paste0(params$file_name, ".", params$file_type),
          plot = p,
          width = params$width,
          height = params$height
        )
      }

      return(p)
    }

    if (!requireNamespace("wordcloud", quietly = TRUE)) {
      stop("Package 'wordcloud' is required for plottype = 'wordcloud'.")
    }

    fit_warnings <- 0L
    withCallingHandlers(
      wordcloud::wordcloud(
        words = as.character(indication_df$indication),
        freq = indication_df$Freq,
        random.order = FALSE,
        max.words = if (is.infinite(selected_max_words)) nrow(indication_df) else min(selected_max_words, nrow(indication_df)),
        scale = c(2, 0.4),
        colors = c("#1f77b4", "#4c78a8", "#72b7b2", "#54a24b", "#e45756")
      ),
      warning = function(w) {
        if (grepl("could not be fit on page", conditionMessage(w), fixed = TRUE)) {
          fit_warnings <<- fit_warnings + 1L
          invokeRestart("muffleWarning")
        }
      }
    )
    recorded_plot <- recordPlot()

    if (fit_warnings > 0) {
      warning(
        fit_warnings,
        " term(s) could not be fit in the wordcloud and were omitted. ",
        "Try increasing plot dimensions or lowering 'max_words'."
      )
    }

    if (!is.null(params$file_name) && nzchar(params$file_name)) {
      out_file <- paste0(params$file_name, ".", params$file_type)

      if (params$file_type == "pdf") {
        grDevices::pdf(file = out_file, width = params$width, height = params$height)
      } else if (params$file_type == "png") {
        grDevices::png(filename = out_file, width = params$width, height = params$height, units = params$units)
      } else if (params$file_type == "jpeg") {
        grDevices::jpeg(filename = out_file, width = params$width, height = params$height, units = params$units)
      } else if (params$file_type == "svg") {
        grDevices::svg(filename = out_file, width = params$width, height = params$height)
      } else {
        stop("Unsupported 'file_type' for saving wordcloud plot.")
      }

      grDevices::replayPlot(recorded_plot)
      grDevices::dev.off()
    }

    return(recorded_plot)
  }
)
