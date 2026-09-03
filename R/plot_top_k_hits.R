#' @title Plot Top-K Hits Across Drug Ranking Methods
#'
#' @description
#' Visualizes annotated hits recovered among top-ranked drugs across ranking
#' methods.
#'
#' @details
#' `plot_top_k_hits()` counts drugs with non-missing status annotations within
#' selected top-k thresholds and supports bar, dot, and line plots.
#'
#' Top-ranked rows are selected before status matching or grouped-drug
#' expansion, so annotation does not affect ranking.
#'
#' When `split_grouped_drugs = FALSE`, a grouped label such as
#' `"daratumumab|felzartamab|isatuximab"` is treated as one ranked entity but
#' matched against status annotations using its individual members. It
#' therefore contributes at most one hit, even when multiple members are
#' annotated. If matched members have different status labels, the group is
#' assigned `"Multiple"`.
#'
#' When `split_grouped_drugs = TRUE`, grouped labels are expanded after top-k
#' selection and individual drugs can contribute separately.
#'
#' If `status_df` is not supplied, status annotations are taken from
#' `drug_ranks_df`.
#'
#' @inheritParams plot_top_k_overlap
#' @param plottype Plot type. One of `"Barplot"`, `"dotplot"`, or
#'   `"lineplot"`. Default is `"Barplot"`.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' rank_df <- data.frame(
#'   Drug = c(
#'     "daratumumab|felzartamab|isatuximab",
#'     "drug_b",
#'     "drug_c"
#'   ),
#'   Method1 = c(1, 2, 3),
#'   Method2 = c(1, 3, 2)
#' )
#'
#' status_df <- data.frame(
#'   Drug = c("daratumumab", "felzartamab", "drug_c"),
#'   Status = c("positive", "positive", "positive")
#' )
#'
#' plot_top_k_hits(
#'   drug_ranks_df = rank_df,
#'   status_df = status_df,
#'   split_grouped_drugs = FALSE,
#'   plottype = "Barplot"
#' )
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_col geom_text facet_grid coord_flip geom_point geom_line geom_jitter labs scale_y_continuous theme_bw theme element_text element_blank ggsave expansion position_stack scale_color_manual
#' @importFrom dplyr mutate filter case_when count if_else
#' @importFrom tidyr unnest
#' @importFrom purrr map_dfr
#' @importFrom stringr str_detect str_remove
#' @importFrom ggsci scale_fill_futurama
#' @importFrom scales hue_pal
#' @export
setGeneric(
  "plot_top_k_hits",
  function(object = NULL,
           drug_ranks_df = NULL,
           plottype = c("Barplot", "dotplot", "lineplot"),
           file_type = "pdf",
           file_name = NULL,
           status_df = NULL,
           status_col = "Status",
           split_grouped_drugs = FALSE,
           drug_sep = "\\|",
           width = 10,
           height = 10,
           units = "in") {

    if (is.null(drug_ranks_df) && is.data.frame(object)) {
      drug_ranks_df <- object
      object <- NULL
    }

    plottype <- match.arg(plottype)

    if (!is.character(status_col) || length(status_col) != 1L ||
        is.na(status_col) || !nzchar(status_col)) {
      stop(
        "`status_col` must be a single non-empty character string.",
        call. = FALSE
      )
    }

    if (!is.character(drug_sep) || length(drug_sep) != 1L ||
        is.na(drug_sep) || !nzchar(drug_sep)) {
      stop(
        "`drug_sep` must be a single non-empty character string.",
        call. = FALSE
      )
    }

    split_grouped_drugs <- isTRUE(split_grouped_drugs)

    if (!is.null(status_df)) {
      if (!is.data.frame(status_df)) {
        stop("`status_df` must be NULL or a data frame.", call. = FALSE)
      }

      if (!all(c("Drug", status_col) %in% names(status_df))) {
        stop(
          "`status_df` must contain `Drug` and `", status_col, "` columns.",
          call. = FALSE
        )
      }
    }

    if (is.null(object)) {
      if (is.null(drug_ranks_df)) {
        stop(
          "The `drug_ranks_df` must be provided as input data.",
          call. = FALSE
        )
      }

      if (!is.data.frame(drug_ranks_df)) {
        stop("`drug_ranks_df` must be a data frame.", call. = FALSE)
      }

      if (!"Drug" %in% names(drug_ranks_df)) {
        stop(
          "`drug_ranks_df` must contain a `Drug` column.",
          call. = FALSE
        )
      }

      if (is.null(status_df) &&
          !status_col %in% names(drug_ranks_df)) {
        stop(
          "`drug_ranks_df` must contain `", status_col,
          "` when `status_df` is not supplied.",
          call. = FALSE
        )
      }

      rank_cols <- setdiff(
        names(drug_ranks_df),
        c("Drug", "Status", status_col)
      )

      if (!length(rank_cols)) {
        stop(
          "At least one numeric ranking column is required.",
          call. = FALSE
        )
      }

      if (!all(vapply(
        drug_ranks_df[, rank_cols, drop = FALSE],
        is.numeric,
        logical(1)
      ))) {
        stop(
          "All ranking columns must be numeric, excluding `Drug` and ",
          "the status column.",
          call. = FALSE
        )
      }

      if (is.null(file_name)) {
        file_name <- tempfile("plot_top_k_hits_")
      }

      object <- PlotObject(
        input_data = drug_ranks_df,
        file_type = file_type,
        file_name = file_name,
        width = width,
        height = height,
        units = units
      )

      object@parameters$status_df <- status_df
      object@parameters$status_col <- status_col
      object@parameters$split_grouped_drugs <- split_grouped_drugs
      object@parameters$drug_sep <- drug_sep
    }

    standardGeneric("plot_top_k_hits")
  }
)


#' @rdname plot_top_k_hits
#' @keywords internal
setMethod(
  "plot_top_k_hits",
  signature = "PlotObject",
  function(object, plottype, status_df, status_col,
           split_grouped_drugs, drug_sep) {

    params <- object@parameters
    input_data <- params$input_data

    if (!is.data.frame(input_data) ||
        !"Drug" %in% names(input_data)) {
      stop(
        "`PlotObject` must contain a data frame with a `Drug` column.",
        call. = FALSE
      )
    }

    if (missing(status_df)) {
      status_df <- params$status_df
    }

    if (missing(status_col) || is.null(status_col)) {
      status_col <- if (!is.null(params$status_col)) {
        params$status_col
      } else {
        "Status"
      }
    }

    if (missing(split_grouped_drugs) ||
        is.null(split_grouped_drugs)) {
      split_grouped_drugs <- if (!is.null(params$split_grouped_drugs)) {
        params$split_grouped_drugs
      } else {
        FALSE
      }
    }

    if (missing(drug_sep) || is.null(drug_sep)) {
      drug_sep <- if (!is.null(params$drug_sep)) {
        params$drug_sep
      } else {
        "\\|"
      }
    }

    split_grouped_drugs <- isTRUE(split_grouped_drugs)

    methods <- setdiff(
      names(input_data),
      c("Drug", "Status", status_col)
    )

    if (!length(methods)) {
      stop(
        "At least one numeric ranking column is required.",
        call. = FALSE
      )
    }

    if (!all(vapply(
      input_data[, methods, drop = FALSE],
      is.numeric,
      logical(1)
    ))) {
      stop("All ranking columns must be numeric.", call. = FALSE)
    }

    split_drug_ids <- function(x) {
      x <- as.character(x)
      x <- x[!is.na(x) & nzchar(x)]

      if (!length(x)) {
        return(character())
      }

      ids <- trimws(
        unlist(
          strsplit(x, split = drug_sep, perl = TRUE),
          use.names = FALSE
        )
      )

      unique(ids[!is.na(ids) & nzchar(ids)])
    }

    # Build an atomic drug-to-status lookup without changing ranked entities.
    build_status_lookup <- function(current_data) {
      if (!is.null(status_df)) {
        source_data <- status_df
      } else {
        if (!status_col %in% names(current_data)) {
          stop(
            "`", status_col,
            "` is not present in the ranking data and `status_df` ",
            "was not supplied.",
            call. = FALSE
          )
        }
        source_data <- current_data
      }

      keep <- !is.na(source_data[[status_col]]) &
        !is.na(source_data$Drug)

      status_data <- source_data[
        keep,
        c("Drug", status_col),
        drop = FALSE
      ]

      if (!nrow(status_data)) {
        return(
          data.frame(
            Drug = character(),
            StatusValue = character(),
            stringsAsFactors = FALSE
          )
        )
      }

      split_values <- strsplit(
        as.character(status_data$Drug),
        split = drug_sep,
        perl = TRUE
      )

      status_lookup <- data.frame(
        Drug = trimws(unlist(split_values, use.names = FALSE)),
        StatusValue = rep(
          as.character(status_data[[status_col]]),
          lengths(split_values)
        ),
        stringsAsFactors = FALSE
      )

      status_lookup <- status_lookup[
        !is.na(status_lookup$Drug) &
          nzchar(status_lookup$Drug) &
          !is.na(status_lookup$StatusValue),
        ,
        drop = FALSE
      ]

      unique(status_lookup)
    }

    # Resolve one status per ranked entity to prevent duplicate hit counting.
    resolve_entity_status <- function(drug_id, status_lookup) {
      members <- split_drug_ids(drug_id)

      if (!length(members) || !nrow(status_lookup)) {
        return(NA_character_)
      }

      mapped_status <- unique(
        status_lookup$StatusValue[
          status_lookup$Drug %in% members
        ]
      )

      mapped_status <- mapped_status[!is.na(mapped_status)]

      if (!length(mapped_status)) {
        return(NA_character_)
      }

      if (length(mapped_status) == 1L) {
        return(mapped_status)
      }

      "Multiple"
    }

    compute_hits <- function(input_data, top_k_seq) {
      status_lookup <- build_status_lookup(input_data)

      purrr::map_dfr(methods, function(m) {
        purrr::map_dfr(top_k_seq, function(top) {
          # Select ranked rows before any grouped-drug expansion.
          top_drugs <- extract_top_ranked_drugs(
            df = input_data,
            rank_cols = m,
            top_n = top
          )$Drug

          top_drugs <- unique(
            trimws(as.character(top_drugs))
          )
          top_drugs <- top_drugs[
            !is.na(top_drugs) & nzchar(top_drugs)
          ]

          ranked_entities <- if (split_grouped_drugs) {
            split_drug_ids(top_drugs)
          } else {
            top_drugs
          }

          entity_status <- if (length(ranked_entities)) {
            vapply(
              ranked_entities,
              resolve_entity_status,
              status_lookup = status_lookup,
              FUN.VALUE = character(1)
            )
          } else {
            character()
          }

          mapped <- !is.na(entity_status)

          tibble::tibble(
            method = m,
            top_k = top,
            hits = sum(mapped),
            Groups = list(entity_status[mapped])
          )
        })
      }) %>%
        dplyr::mutate(
          Study = dplyr::case_when(
            stringr::str_detect(method, "CRank|Dowdall|RRA") ~
              "Rank Aggregation",
            stringr::str_detect(method, "CMAP|LINCS2") ~
              "Signature-based",
            TRUE ~ "Network-based"
          ),
          Study = factor(
            Study,
            levels = c(
              "Rank Aggregation",
              "Network-based",
              "Signature-based"
            )
          )
        ) %>%
        tidyr::unnest(Groups, keep_empty = TRUE) %>%
        dplyr::mutate(
          Groups = factor(Groups, levels = unique(Groups)),
          method = dplyr::if_else(
            Study == "Signature-based",
            stringr::str_remove(method, "_DEGs|_DEPs"),
            method
          ),
          method = factor(method, levels = unique(method))
        )
    }

    base_theme <- ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = "right",
        strip.text = ggplot2::element_text(size = 10),
        axis.title = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 10)
      )

    if (plottype == "Barplot") {
      res_df <- compute_hits(
        input_data,
        top_k_seq = c(100, 200, 300)
      )

      count_df <- res_df %>%
        dplyr::filter(!is.na(Groups)) %>%
        dplyr::count(
          Study,
          top_k,
          method,
          Groups,
          name = "count"
        )

      base <- ggplot2::ggplot(
        count_df,
        ggplot2::aes(
          x = method,
          y = count,
          fill = Groups
        )
      ) +
        ggplot2::geom_col(position = "stack") +
        ggplot2::geom_text(
          ggplot2::aes(label = count),
          position = ggplot2::position_stack(vjust = 0.5),
          size = 3,
          color = "white"
        ) +
        ggplot2::facet_grid(
          Study ~ top_k,
          scales = "free",
          space = "free"
        ) +
        ggplot2::labs(
          y = "Hits",
          x = NULL
        ) +
        ggplot2::coord_flip() +
        base_theme +
        ggsci::scale_fill_futurama(
          alpha = 0.64,
          na.translate = FALSE
        )

    } else if (plottype == "dotplot") {
      res_df <- compute_hits(
        input_data,
        top_k_seq = 100
      )

      plot_df <- res_df[
        !duplicated(
          res_df[c("Study", "top_k", "method", "hits")]
        ),
        ,
        drop = FALSE
      ]

      base <- ggplot2::ggplot(
        plot_df,
        ggplot2::aes(
          x = method,
          y = hits,
          size = hits
        )
      ) +
        ggplot2::geom_point(
          color = "steelblue",
          alpha = 0.8
        ) +
        ggplot2::scale_y_continuous(
          breaks = unique(sort(plot_df$hits)),
          limits = c(
            min(plot_df$hits, na.rm = TRUE),
            max(plot_df$hits, na.rm = TRUE)
          ),
          expand = ggplot2::expansion(
            mult = c(0, 0.05)
          )
        ) +
        ggplot2::labs(
          x = "Method",
          y = "Top100 Hits"
        ) +
        base_theme +
        ggplot2::theme(
          legend.position = "none",
          axis.text.x = ggplot2::element_text(
            angle = 45,
            hjust = 1
          ),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()
        )

    } else {
      res_df <- compute_hits(
        input_data,
        top_k_seq = seq(100, 1000, 100)
      )

      plot_df <- res_df[
        !duplicated(
          res_df[c("Study", "top_k", "method", "hits")]
        ),
        ,
        drop = FALSE
      ]

      line_methods <- unique(as.character(plot_df$method))
      line_methods <- line_methods[
        grepl("CRank|Dowdall|RRA", line_methods)
      ]

      base <- ggplot2::ggplot(
        plot_df,
        ggplot2::aes(
          x = top_k,
          y = hits,
          color = method
        )
      ) +
        ggplot2::geom_jitter(
          width = 2,
          height = 0,
          size = 1.5,
          alpha = 0.9
        ) +
        ggplot2::geom_line(
          data = dplyr::filter(
            plot_df,
            method %in% line_methods
          ),
          ggplot2::aes(group = method),
          size = 1.1,
          alpha = 0.9
        ) +
        ggplot2::labs(
          x = "top_k",
          y = "Hits",
          color = "Method"
        ) +
        ggplot2::scale_y_continuous(
          limits = c(
            min(plot_df$hits, na.rm = TRUE),
            max(plot_df$hits, na.rm = TRUE)
          ),
          expand = ggplot2::expansion(
            mult = c(0, 0.05)
          )
        ) +
        ggplot2::scale_color_manual(
          values = scales::hue_pal()(
            length(unique(plot_df$method))
          )
        ) +
        base_theme +
        ggplot2::theme(
          legend.position = "bottom"
        )
    }

    if (!is.null(params$file_name) &&
        nzchar(params$file_name)) {
      ggplot2::ggsave(
        filename = paste0(
          params$file_name,
          ".",
          params$file_type
        ),
        plot = base,
        width = params$width,
        height = params$height,
        units = params$units
      )
    }

    base
  }
)
