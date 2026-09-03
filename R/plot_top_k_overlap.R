#' @title Plot Top-K Overlap Across Drug Ranking Methods
#'
#' @description
#' Visualizes pairwise overlap among top-ranked drugs across ranking methods.
#'
#' @details
#' `plot_top_k_overlap()` creates a heatmap of pairwise overlap among drugs with
#' ranks less than or equal to `top_k`.
#'
#' When status information is available, the value in parentheses gives the
#' number of overlapping entries with at least one status-mapped drug. Top-k
#' sets are selected before status matching or grouped-drug expansion, so
#' annotation does not affect ranking.
#'
#' When `split_grouped_drugs = FALSE`, a grouped label such as
#' `"daratumumab|felzartamab|isatuximab"` remains one overlap unit but is matched
#' to status information using its individual members. It therefore contributes
#' at most one status-mapped hit. When `TRUE`, grouped labels are expanded after
#' top-k selection and individual drugs can contribute separately.
#'
#' If `status_df` is omitted, a `Status` column in `drug_ranks_df` is used when
#' available. Grouped labels in the status reference are expanded internally
#' for matching.
#'
#' @param object Optional `PlotObject`. For pipe-friendly use, a data frame can
#'   also be supplied as `object` when `drug_ranks_df` is omitted.
#' @param drug_ranks_df Data frame containing `Drug`, optional `Status`, optional
#'   facet columns, and numeric rank columns. Smaller ranks are better.
#' @param top_k Positive rank threshold. Default is `100`.
#' @param file_type Output file format. One of `"pdf"`, `"png"`, `"svg"`, or
#'   `"jpeg"`. Default is `"pdf"`.
#' @param file_name Optional output file name without extension. If `NULL`, the
#'   plot is returned without saving.
#' @param status_df Optional data frame containing `Drug` and `status_col`.
#' @param status_col Column in `status_df` containing status labels. Drugs with
#'   non-missing values are counted in parentheses. Default is `"Status"`.
#' @param width,height Plot dimensions. Defaults are `20` and `15`.
#' @param label_size Text size for heatmap-cell labels. Default is `3`.
#' @param base_size Base text size passed to `ggplot2::theme_minimal()`.
#'   Default is `11`.
#' @param heatmap_type Heatmap layout. One of `"triangle"` or `"full"`.
#'   Default is `"triangle"`.
#' @param split_grouped_drugs Logical; if `TRUE`, grouped `Drug` labels are
#'   expanded using `drug_sep` after top-k selection. Default is `FALSE`.
#' @param drug_sep Separator used for grouped drug labels. Default is `"\\|"`.
#' @param facet_cols Optional character vector of columns used for faceting.
#' @param facet_type Faceting mode. One of `"wrap"` or `"grid"`.
#' @param facet_rows,facet_grid_cols Optional row and column facet variables for
#'   `facet_type = "grid"`.
#' @param units Units for saved plot dimensions. One of `"in"`, `"cm"`, `"mm"`,
#'   or `"px"`. Default is `"in"`.
#'
#' @return
#' A `ggplot` object. The underlying overlap table is available in `plot$data`.
#'
#' @examples
#' \dontrun{
#' rank_df <- data.frame(
#'   Drug = c(
#'     "daratumumab|felzartamab|isatuximab",
#'     "drug_b",
#'     "drug_c",
#'     "drug_d"
#'   ),
#'   Method1 = c(1, 2, 3, 4),
#'   Method2 = c(1, 3, 2, 4)
#' )
#'
#' status_df <- data.frame(
#'   Drug = c("daratumumab", "felzartamab", "drug_c"),
#'   Status = c("positive", "positive", "positive")
#' )
#'
#' p <- plot_top_k_overlap(
#'   drug_ranks_df = rank_df,
#'   status_df = status_df,
#'   top_k = 2,
#'   split_grouped_drugs = FALSE
#' )
#'
#' p$data
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradient theme_minimal theme labs element_text coord_fixed scale_y_discrete ggsave facet_wrap facet_grid
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom tidyr separate_rows
#' @export
setGeneric(
  "plot_top_k_overlap",
  function(object = NULL, drug_ranks_df = NULL,
           top_k = 100,
           file_type = "pdf", file_name = NULL,
           status_df = NULL, status_col = "Status",
           width = 20, height = 15,
           label_size = 3,
           base_size = 11,
           heatmap_type = c("triangle", "full"),
           split_grouped_drugs = FALSE,
           drug_sep = "\\|",
           facet_cols = NULL,
           facet_type = c("wrap", "grid"),
           facet_rows = NULL,
           facet_grid_cols = NULL,
           units = "in") {

    if (is.null(drug_ranks_df) && is.data.frame(object)) {
      drug_ranks_df <- object
      object <- NULL
    }

    heatmap_type <- match.arg(heatmap_type)
    facet_type <- match.arg(facet_type)

    if (!is.character(status_col) || length(status_col) != 1L ||
        is.na(status_col) || !nzchar(status_col)) {
      stop(
        "`status_col` must be a single non-empty character string.",
        call. = FALSE
      )
    }

    if (!is.numeric(top_k) || length(top_k) != 1L || is.na(top_k) ||
        !is.finite(top_k) || top_k <= 0) {
      stop(
        "`top_k` must be a single positive finite number.",
        call. = FALSE
      )
    }

    text_sizes <- c(
      label_size = label_size,
      base_size = base_size
    )

    invalid_sizes <- !vapply(
      text_sizes,
      function(x) {
        is.numeric(x) &&
          length(x) == 1L &&
          !is.na(x) &&
          is.finite(x) &&
          x > 0
      },
      logical(1)
    )

    if (any(invalid_sizes)) {
      stop(
        "Plot text-size parameters must be positive finite numbers: ",
        paste(names(text_sizes)[invalid_sizes], collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    split_grouped_drugs <- isTRUE(split_grouped_drugs)

    if (!is.character(drug_sep) || length(drug_sep) != 1L ||
        is.na(drug_sep) || !nzchar(drug_sep)) {
      stop(
        "`drug_sep` must be a single non-empty character string.",
        call. = FALSE
      )
    }

    if (!is.null(status_df)) {
      if (!is.data.frame(status_df)) {
        stop(
          "`status_df` must be NULL or a data frame.",
          call. = FALSE
        )
      }

      if (!all(c("Drug", status_col) %in% names(status_df))) {
        stop(
          "`status_df` must contain `Drug` and `",
          status_col,
          "` columns.",
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
        stop(
          "`drug_ranks_df` must be a data frame.",
          call. = FALSE
        )
      }

      if (!"Drug" %in% names(drug_ranks_df)) {
        stop(
          "`drug_ranks_df` must contain a `Drug` column.",
          call. = FALSE
        )
      }

      facet_spec <- .top_k_overlap_resolve_facets(
        facet_cols = facet_cols,
        facet_type = facet_type,
        facet_rows = facet_rows,
        facet_grid_cols = facet_grid_cols,
        data_names = names(drug_ranks_df)
      )

      rank_cols <- setdiff(
        names(drug_ranks_df),
        c("Drug", "Status", facet_spec$all_cols)
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
          "All ranking columns must be numeric, excluding `Drug`, optional ",
          "`Status`, and facet columns.",
          call. = FALSE
        )
      }

      auto_file_name <- is.null(file_name)

      if (auto_file_name) {
        file_name <- tempfile("plot_top_k_overlap_")
      }

      object <- PlotObject(
        input_data = drug_ranks_df,
        file_type = file_type,
        file_name = file_name,
        width = width,
        height = height,
        units = units
      )

      object@parameters$top_k <- top_k
      object@parameters$status_df <- status_df
      object@parameters$status_col <- status_col
      object@parameters$label_size <- label_size
      object@parameters$base_size <- base_size
      object@parameters$heatmap_type <- heatmap_type
      object@parameters$split_grouped_drugs <- split_grouped_drugs
      object@parameters$drug_sep <- drug_sep
      object@parameters$facet_cols <- facet_spec$wrap_cols
      object@parameters$facet_type <- facet_spec$type
      object@parameters$facet_rows <- facet_spec$rows
      object@parameters$facet_grid_cols <- facet_spec$cols
      object@parameters$auto_file_name <- auto_file_name
    }

    standardGeneric("plot_top_k_overlap")
  }
)


#' @rdname plot_top_k_overlap
#' @export
setMethod(
  "plot_top_k_overlap",
  signature = "PlotObject",
  function(object, top_k, status_df, status_col, label_size, base_size,
           heatmap_type, split_grouped_drugs, drug_sep, facet_cols,
           facet_type, facet_rows, facet_grid_cols) {

    params <- object@parameters
    input_data <- params$input_data

    if (!is.data.frame(input_data) ||
        !"Drug" %in% names(input_data)) {
      stop(
        "`PlotObject` must contain a data frame with a `Drug` column.",
        call. = FALSE
      )
    }

    if (missing(top_k) || is.null(top_k)) {
      top_k <- if (!is.null(params$top_k)) params$top_k else 100
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

    if (missing(label_size) || is.null(label_size)) {
      label_size <- if (!is.null(params$label_size)) {
        params$label_size
      } else {
        3
      }
    }

    if (missing(base_size) || is.null(base_size)) {
      base_size <- if (!is.null(params$base_size)) {
        params$base_size
      } else {
        11
      }
    }

    if (missing(heatmap_type) || is.null(heatmap_type)) {
      heatmap_type <- if (!is.null(params$heatmap_type)) {
        params$heatmap_type
      } else {
        "triangle"
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

    if (missing(facet_cols)) {
      facet_cols <- params$facet_cols
    }

    if (missing(facet_type) || is.null(facet_type)) {
      facet_type <- if (!is.null(params$facet_type)) {
        params$facet_type
      } else {
        "wrap"
      }
    }

    if (missing(facet_rows)) {
      facet_rows <- params$facet_rows
    }

    if (missing(facet_grid_cols)) {
      facet_grid_cols <- params$facet_grid_cols
    }

    heatmap_type <- match.arg(
      heatmap_type,
      c("triangle", "full")
    )

    facet_type <- match.arg(
      facet_type,
      c("wrap", "grid")
    )

    split_grouped_drugs <- isTRUE(split_grouped_drugs)

    facet_spec <- .top_k_overlap_resolve_facets(
      facet_cols = facet_cols,
      facet_type = facet_type,
      facet_rows = facet_rows,
      facet_grid_cols = facet_grid_cols,
      data_names = names(input_data)
    )

    methods <- setdiff(
      names(input_data),
      c("Drug", "Status", facet_spec$all_cols)
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
      stop(
        "All ranking columns must be numeric.",
        call. = FALSE
      )
    }

    # Resolve atomic status IDs without changing ranked entities.
    get_status_drugs <- function(current_data) {
      if (!is.null(status_df)) {
        keep <- !is.na(status_df[[status_col]])
        status_data <- status_df[
          keep,
          "Drug",
          drop = FALSE
        ]
      } else if ("Status" %in% names(current_data)) {
        keep <- !is.na(current_data[["Status"]])
        status_data <- current_data[
          keep,
          "Drug",
          drop = FALSE
        ]
      } else {
        return(NULL)
      }

      if (!nrow(status_data)) {
        return(character())
      }

      status_values <- as.character(status_data$Drug)
      status_values <- status_values[
        !is.na(status_values) &
          nzchar(status_values)
      ]

      if (!length(status_values)) {
        return(character())
      }

      status_ids <- trimws(
        unlist(
          strsplit(
            status_values,
            split = drug_sep,
            perl = TRUE
          ),
          use.names = FALSE
        )
      )

      unique(
        status_ids[
          !is.na(status_ids) &
            nzchar(status_ids)
        ]
      )
    }

    compute_overlap_one <- function(current_data) {
      status_ids <- get_status_drugs(current_data)
      use_status <- !is.null(status_ids)

      # Select original top-k rows before grouped-drug expansion.
      top_sets <- lapply(
        methods,
        function(method_name) {
          rank_values <- current_data[[method_name]]

          keep <- !is.na(rank_values) &
            rank_values <= top_k

          top_data <- current_data[
            keep,
            "Drug",
            drop = FALSE
          ]

          if (split_grouped_drugs && nrow(top_data)) {
            top_data <- tidyr::separate_rows(
              top_data,
              Drug,
              sep = drug_sep
            )
          }

          top_ids <- trimws(
            as.character(top_data$Drug)
          )

          unique(
            top_ids[
              !is.na(top_ids) &
                nzchar(top_ids)
            ]
          )
        }
      )

      names(top_sets) <- methods

      # Pre-compute status-mapped ranked identifiers once per dataset.
      status_match_ids <- character()

      if (use_status) {
        candidate_ids <- unique(
          unlist(
            top_sets,
            use.names = FALSE
          )
        )

        if (length(candidate_ids) &&
            length(status_ids)) {
          candidate_is_mapped <- vapply(
            candidate_ids,
            function(drug_id) {
              members <- trimws(
                strsplit(
                  as.character(drug_id),
                  split = drug_sep,
                  perl = TRUE
                )[[1]]
              )

              members <- members[
                !is.na(members) &
                  nzchar(members)
              ]

              any(members %in% status_ids)
            },
            logical(1)
          )

          status_match_ids <- candidate_ids[
            candidate_is_mapped
          ]
        }
      }

      n_methods <- length(methods)

      overlap_all <- matrix(
        0L,
        nrow = n_methods,
        ncol = n_methods,
        dimnames = list(methods, methods)
      )

      overlap_status <- if (use_status) {
        matrix(
          0L,
          nrow = n_methods,
          ncol = n_methods,
          dimnames = list(methods, methods)
        )
      } else {
        NULL
      }

      for (i in seq_len(n_methods)) {
        for (j in seq_len(n_methods)) {
          pairwise_ids <- intersect(
            top_sets[[i]],
            top_sets[[j]]
          )

          overlap_all[i, j] <- length(
            pairwise_ids
          )

          if (use_status) {
            overlap_status[i, j] <- sum(
              pairwise_ids %in% status_match_ids
            )
          }
        }
      }

      overlap_df <- as.data.frame(
        as.table(overlap_all),
        stringsAsFactors = FALSE
      )

      names(overlap_df) <- c(
        "method_1",
        "method_2",
        "overlap_all"
      )

      overlap_df$method_1 <- factor(
        overlap_df$method_1,
        levels = methods
      )

      overlap_df$method_2 <- factor(
        overlap_df$method_2,
        levels = methods
      )

      overlap_df$overlap_valid <- if (use_status) {
        as.vector(overlap_status)
      } else {
        NA_integer_
      }

      if (identical(heatmap_type, "triangle")) {
        hide_cell <- as.integer(
          overlap_df$method_1
        ) >
          as.integer(
            overlap_df$method_2
          )

        overlap_df$overlap_all[
          hide_cell
        ] <- NA_integer_

        overlap_df$overlap_valid[
          hide_cell
        ] <- NA_integer_
      }

      if (use_status) {
        overlap_df$overlap_label <- ifelse(
          is.na(overlap_df$overlap_all),
          "",
          paste0(
            overlap_df$overlap_all,
            "\n(",
            overlap_df$overlap_valid,
            ")"
          )
        )
      } else {
        overlap_df$overlap_label <- ifelse(
          is.na(overlap_df$overlap_all),
          "",
          as.character(overlap_df$overlap_all)
        )
      }

      overlap_df
    }

    compute_overlap <- function(current_data) {
      if (!length(facet_spec$all_cols)) {
        return(
          compute_overlap_one(
            current_data
          )
        )
      }

      split_keys <- interaction(
        current_data[
          ,
          facet_spec$all_cols,
          drop = FALSE
        ],
        drop = TRUE,
        lex.order = TRUE
      )

      split_rows <- split(
        seq_len(nrow(current_data)),
        split_keys
      )

      overlap_list <- lapply(
        split_rows,
        function(row_index) {
          subset_data <- current_data[
            row_index,
            ,
            drop = FALSE
          ]

          facet_values <- subset_data[
            1,
            facet_spec$all_cols,
            drop = FALSE
          ]

          dplyr::bind_cols(
            facet_values,
            compute_overlap_one(
              subset_data
            )
          )
        }
      )

      dplyr::bind_rows(
        overlap_list
      )
    }

    overlap_df <- compute_overlap(
      input_data
    )

    p <- ggplot2::ggplot(
      overlap_df,
      ggplot2::aes(
        x = method_1,
        y = method_2,
        fill = overlap_all
      )
    ) +
      ggplot2::geom_tile(
        color = "white"
      ) +
      ggplot2::geom_text(
        ggplot2::aes(
          label = overlap_label
        ),
        color = "black",
        size = label_size,
        na.rm = TRUE
      ) +
      ggplot2::scale_fill_gradient(
        low = "white",
        high = "steelblue",
        na.value = "white"
      ) +
      ggplot2::theme_minimal(
        base_size = base_size
      ) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          angle = 45,
          hjust = 1
        )
      ) +
      ggplot2::labs(
        x = "Method",
        y = "Method",
        fill = "Overlap"
      ) +
      ggplot2::scale_y_discrete(
        limits = rev(
          levels(
            overlap_df$method_2
          )
        )
      ) +
      ggplot2::coord_fixed()

    if (length(facet_spec$all_cols)) {
      if (identical(facet_spec$type, "grid")) {
        row_formula <- if (length(facet_spec$rows)) {
          paste(
            facet_spec$rows,
            collapse = "+"
          )
        } else {
          "."
        }

        col_formula <- if (length(facet_spec$cols)) {
          paste(
            facet_spec$cols,
            collapse = "+"
          )
        } else {
          "."
        }

        p <- p +
          ggplot2::facet_grid(
            stats::as.formula(
              paste(
                row_formula,
                "~",
                col_formula
              )
            )
          )
      } else {
        p <- p +
          ggplot2::facet_wrap(
            stats::as.formula(
              paste(
                "~",
                paste(
                  facet_spec$wrap_cols,
                  collapse = "+"
                )
              )
            )
          )
      }
    }

    if (!isTRUE(params$auto_file_name) &&
        !is.null(params$file_name) &&
        nzchar(params$file_name)) {
      ggplot2::ggsave(
        filename = paste0(
          params$file_name,
          ".",
          params$file_type
        ),
        plot = p,
        width = params$width,
        height = params$height,
        units = params$units
      )
    }

    p
  }
)


.top_k_overlap_resolve_facets <- function(
    facet_cols = NULL,
    facet_type = c("wrap", "grid"),
    facet_rows = NULL,
    facet_grid_cols = NULL,
    data_names = character()) {

  facet_type <- match.arg(facet_type)

  validate_cols <- function(x, argument) {
    if (is.null(x)) {
      return(character())
    }

    if (!is.character(x) ||
        anyNA(x) ||
        any(!nzchar(x))) {
      stop(
        "`", argument,
        "` must be NULL or a character vector of column names.",
        call. = FALSE
      )
    }

    missing_cols <- setdiff(
      x,
      data_names
    )

    if (length(missing_cols)) {
      stop(
        "`", argument,
        "` not found in input data: ",
        paste(missing_cols, collapse = ", "),
        call. = FALSE
      )
    }

    unique(x)
  }

  facet_cols <- validate_cols(
    facet_cols,
    "facet_cols"
  )

  facet_rows <- validate_cols(
    facet_rows,
    "facet_rows"
  )

  facet_grid_cols_missing <- is.null(
    facet_grid_cols
  )

  facet_grid_cols <- validate_cols(
    facet_grid_cols,
    "facet_grid_cols"
  )

  if (identical(facet_type, "grid")) {
    grid_cols <- facet_grid_cols

    if (isTRUE(facet_grid_cols_missing) &&
        !length(facet_rows) &&
        length(facet_cols) >= 2L) {
      facet_rows <- facet_cols[1]
      grid_cols <- facet_cols[-1]
    } else if (!length(grid_cols)) {
      grid_cols <- facet_cols
    }

    return(
      list(
        type = "grid",
        wrap_cols = character(),
        rows = facet_rows,
        cols = grid_cols,
        all_cols = unique(
          c(facet_rows, grid_cols)
        )
      )
    )
  }

  list(
    type = "wrap",
    wrap_cols = facet_cols,
    rows = character(),
    cols = character(),
    all_cols = facet_cols
  )
}
