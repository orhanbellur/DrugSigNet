#' @title Plot Drug Status Donut Chart
#'
#' @description
#' Creates a nested donut chart showing drug status and condition-specific
#' status distributions.
#'
#' @details
#' `plot_drug_status()` visualizes hierarchical status information using an
#' inner ring for `condition_status` and an outer ring for `highest_status`.
#' The outer ring is computed within inner-ring groups so that category
#' boundaries remain aligned.
#'
#' Input data must contain `drug`, `highest_status`, and `condition_status`.
#'
#' @inheritParams plot_enriched_terms
#' @param exclude_condition_status Optional character vector of
#'   `condition_status` categories to exclude before plotting.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' status_df <- data.frame(
#'   drug = c("drug_a", "drug_b", "drug_c"),
#'   highest_status = c("Approved", "Phase 2", "Pre-clinical"),
#'   condition_status = c("Approved", "Clinical", "Preclinical")
#' )
#'
#' plot_drug_status(status_df)
#' }
#'
#' @export
setGeneric(
  "plot_drug_status",
  function(
    object = NULL,
    data_df,
    file_type = "pdf",
    file_name = NULL,
    width = 20,
    height = 15,
    exclude_condition_status = NULL
  ) {
    file_type <- match.arg(file_type, c("pdf", "png", "svg", "jpeg"))

    data_df_missing <- missing(data_df)
    if (data_df_missing && is.data.frame(object)) {
      data_df <- object
      data_df_missing <- FALSE
      object <- NULL
    }

    if (!data_df_missing && !is.data.frame(data_df)) {
      stop("'data_df' must be a data.frame.")
    }

    if (!is.null(exclude_condition_status) && !is.character(exclude_condition_status)) {
      stop("'exclude_condition_status' must be NULL or a character vector.")
    }

    if (is.null(object)) {
      if (data_df_missing) {
        stop("'data_df' must be provided when 'object' is NULL.")
      }

      object <- methods::new(
        "PlotObject",
        parameters = list(
          input_data = as.data.frame(data_df),
          file_type = file_type,
          file_name = file_name,
          width = width,
          height = height,
          units = "in",
          exclude_condition_status = exclude_condition_status
        )
      )
    }

    standardGeneric("plot_drug_status")
  }
)

#' @rdname plot_drug_status
#' @return A `ggplot` object representing the nested donut chart.
#' @keywords internal
#' @export
setMethod(
  "plot_drug_status",
  signature = "PlotObject",
  function(object, exclude_condition_status = NULL) {
    params <- object@parameters
    input_data <- params$input_data
    excluded_categories <- if (!is.null(params$exclude_condition_status)) params$exclude_condition_status else exclude_condition_status

    if (is.null(input_data) || !is.data.frame(input_data)) {
      stop("'data_df' must be a data.frame.")
    }

    required_cols <- c("drug", "highest_status", "condition_status")
    missing_cols <- setdiff(required_cols, colnames(input_data))
    if (length(missing_cols) > 0) {
      stop("Missing required column(s): ", paste(missing_cols, collapse = ", "))
    }

    plot_data <- input_data[, required_cols, drop = FALSE]
    plot_data <- plot_data[!is.na(plot_data$highest_status) & !is.na(plot_data$condition_status), , drop = FALSE]
    if (nrow(plot_data) == 0) {
      stop("No valid rows available after removing missing values.")
    }

    clean_txt <- function(x) gsub("\\s+", " ", trimws(as.character(x)))
    plot_data$highest_status <- clean_txt(plot_data$highest_status)
    plot_data$condition_status <- clean_txt(plot_data$condition_status)
    plot_data$highest_status[plot_data$highest_status == "Approved(Withdrawn)"] <- "Approved\n(Withdrawn)"


    if (!is.null(excluded_categories) && length(excluded_categories) > 0) {
      excluded_categories <- clean_txt(excluded_categories)
      plot_data <- plot_data[!(plot_data$condition_status %in% excluded_categories), , drop = FALSE]
    }

    if (nrow(plot_data) == 0) {
      stop("No rows available after applying 'exclude_condition_status'.")
    }
    if (!requireNamespace("ggforce", quietly = TRUE)) {
      stop("Package 'ggforce' is required for plot_drug_status.")
    }

    inner <- as.data.frame(table(plot_data$condition_status), stringsAsFactors = FALSE)
    colnames(inner) <- c("condition_status", "n")
    inner <- inner[order(inner$n, inner$condition_status, decreasing = TRUE), , drop = FALSE]
    inner$frac <- inner$n / sum(inner$n)
    inner$ymax <- cumsum(inner$frac)
    inner$ymin <- c(0, head(inner$ymax, -1))
    inner$pos <- (inner$ymin + inner$ymax) / 2

    inner$label_full <- paste0(
      inner$condition_status, "\n", inner$n,
      " (", sprintf("%.1f%%", inner$frac * 100), ")"
    )
    inner$label_small <- paste0(inner$condition_status, " (n=", inner$n, ")")

    # Build matched outer ring from the same contingency table so each outer
    # segment sits within its parent inner segment.
    combo <- as.data.frame(table(plot_data$condition_status, plot_data$highest_status), stringsAsFactors = FALSE)
    colnames(combo) <- c("condition_status", "highest_status", "n")
    combo <- combo[combo$n > 0, , drop = FALSE]

    outer_parts <- list()
    for (i in seq_len(nrow(inner))) {
      parent <- inner[i, , drop = FALSE]
      sub <- combo[combo$condition_status == parent$condition_status, , drop = FALSE]
      sub <- sub[order(sub$n, sub$highest_status, decreasing = TRUE), , drop = FALSE]
      if (nrow(sub) == 0) next

      sub$frac <- (sub$n / sum(sub$n)) * parent$frac
      sub$ymin <- parent$ymin + c(0, cumsum(head(sub$frac, -1)))
      sub$ymax <- parent$ymin + cumsum(sub$frac)
      sub$pos <- (sub$ymin + sub$ymax) / 2
      sub$label <- paste0(
        sub$highest_status, "\n", sub$n,
        " (", sprintf("%.1f%%", sub$frac * 100), ")"
      )
      outer_parts[[length(outer_parts) + 1L]] <- sub
    }
    outer <- do.call(rbind, outer_parts)
    rownames(outer) <- NULL

    add_arc_geom <- function(df, r0, r) {
      df$theta_mid <- pi / 2 - 2 * pi * df$pos
      df$theta_start <- pi / 2 - 2 * pi * df$ymin
      df$theta_end <- pi / 2 - 2 * pi * df$ymax
      df$r0 <- r0
      df$r <- r
      df$r_mid <- (r0 + r) / 2
      df$x_lab <- df$r_mid * sin(df$theta_mid)
      df$y_lab <- df$r_mid * cos(df$theta_mid)
      df
    }

    inner <- add_arc_geom(inner, r0 = 0.55, r = 1.20)
    inner$is_small <- inner$frac < 0.03
    outer <- add_arc_geom(outer, r0 = 1.24, r = 2.10)

    inner_big <- inner[!inner$is_small, , drop = FALSE]

    inner_small <- inner[inner$is_small, , drop = FALSE]
    if (nrow(inner_small) > 0) {
      inner_small$x_anchor <- 1.23 * sin(inner_small$theta_mid)
      inner_small$y_anchor <- 1.23 * cos(inner_small$theta_mid)
      inner_small <- inner_small[order(-inner_small$y_anchor), , drop = FALSE]
      inner_small$x_elbow <- 1.78
      inner_small$x_text <- 2.52
      inner_small$y_text <- seq(
        from = 1.75,
        to = 0.95,
        length.out = nrow(inner_small)
      )
    }

    fill_vals <- c(
      "Approved" = "#F05A56",
      "Approved\n(Withdrawn)" = "#D89000",
      "Investigational" = "#2FB000",
      "Unknown" = "#EB3FA8",
      "Pre-clinical" = "#C745E0",
      "Phase 3" = "#7A6AF0",
      "Phase 2" = "#1F9CF0",
      "Phase 2/3" = "#19B6C5",
      "Phase 1" = "#2AA198",
      "Discontinued" = "#7A8F00"
    )

    p <- ggplot2::ggplot() +
      ggforce::geom_arc_bar(
        data = inner,
        ggplot2::aes(
          x0 = 0, y0 = 0, r0 = r0, r = r,
          start = theta_start, end = theta_end,
          fill = condition_status
        ),
        color = "white",
        linewidth = 0.8
      ) +
      ggforce::geom_arc_bar(
        data = outer,
        ggplot2::aes(
          x0 = 0, y0 = 0, r0 = r0, r = r,
          start = theta_start, end = theta_end,
          fill = highest_status
        ),
        color = "white",
        linewidth = 0.8
      ) +
      ggplot2::geom_text(
        data = outer,
        ggplot2::aes(x = x_lab, y = y_lab, label = ifelse(frac >= 0.03, label, "")),
        size = 4.1,
        lineheight = 0.95
      ) +
      ggplot2::geom_text(
        data = inner_big,
        ggplot2::aes(x = x_lab, y = y_lab, label = label_full),
        size = 3.9,
        lineheight = 0.95
      ) +
      ggforce::geom_circle(
        data = data.frame(x0 = 0, y0 = 0, r = 0.52),
        ggplot2::aes(x0 = x0, y0 = y0, r = r),
        inherit.aes = FALSE,
        fill = "white",
        color = NA
      ) +
      ggplot2::scale_fill_manual(values = fill_vals, drop = FALSE) +
      ggplot2::coord_fixed(clip = "off") +
      ggplot2::xlim(-3.0, 3.1) +
      ggplot2::ylim(-2.6, 2.6) +
      ggplot2::theme_void(base_size = 12) +
      ggplot2::theme(
        legend.position = "none",
        plot.margin = ggplot2::margin(12, 26, 12, 12)
      )

    if (nrow(inner_small) > 0) {
      p <- p +
        ggplot2::geom_segment(
          data = inner_small,
          ggplot2::aes(x = x_anchor, y = y_anchor, xend = x_elbow, yend = y_text),
          linewidth = 0.35,
          color = "grey20"
        ) +
        ggplot2::geom_segment(
          data = inner_small,
          ggplot2::aes(x = x_elbow, y = y_text, xend = x_text - 0.04, yend = y_text),
          linewidth = 0.35,
          color = "grey20"
        ) +
        ggplot2::geom_text(
          data = inner_small,
          ggplot2::aes(x = x_text, y = y_text, label = label_small),
          hjust = 0,
          size = 3.6
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
