#' @title Write Pipeline Results to Excel
#'
#' @description
#' Writes key tables from a DrugSigNet pipeline result object to a multi-sheet
#' Excel workbook.
#'
#' @details
#' `write_pipeline_results()` exports tables from `drugNetworkPipeline()`,
#' `drugSignaturePipeline()`, or `drugRepurposingPipeline()`. Single-mode
#' repurposing exports match the corresponding standalone pipeline layout;
#' combined exports include signature, network, and integrated worksheets.
#'
#' The workbook may include raw drug-searching results, harmonized drug
#' rankings, drug annotations, top-ranked drugs with annotations, and functional
#' enrichment results when these sections are available in `result_obj`.
#'
#' Sheet names are sanitized to satisfy Excel naming rules and are made unique
#' automatically.
#'
#' @param result_obj Result object returned by a DrugSigNet pipeline. Both the
#'   S4 pipeline objects returned by current pipeline functions and legacy list
#'   results are supported.
#' @param file_path Output `.xlsx` file path. If a directory is supplied, the
#'   workbook is written as `drugsignet_pipeline_results.xlsx` inside that
#'   directory.
#' @param top_n Positive integer threshold used to create the top-ranked drug
#'   sheet. Default is `100`.
#'
#' @return
#' Invisibly returns the output file path.
#'
#' @examples
#' \dontrun{
#' write_pipeline_results(
#'   result_obj = result,
#'   file_path = "DrugSigNet_pipeline_results.xlsx",
#'   top_n = 100
#' )
#'
#' write_pipeline_results(
#'   result_obj = result,
#'   file_path = "results/",
#'   top_n = 250
#' )
#' }
#'
#' @importFrom dplyr filter if_any all_of select distinct left_join
#' @export
write_pipeline_results <- function(result_obj, file_path, top_n = 100) {
  if (.is_drug_searching_pipeline(result_obj)) {
    pipeline_object <- result_obj
    result_obj <- list(
      DrugSearching = methods::slot(pipeline_object, "DrugSearching"),
      RankAggregation = methods::slot(pipeline_object, "RankAggregation"),
      type = methods::slot(pipeline_object, "type")
    )
    object_slots <- methods::slotNames(pipeline_object)
    if ("DrugAnnotation" %in% object_slots) {
      result_obj$DrugAnnotation <- methods::slot(pipeline_object, "DrugAnnotation")
    }
    if ("Visualization" %in% object_slots) {
      result_obj$Visualization <- methods::slot(pipeline_object, "Visualization")
    }
  }

  if (!is.list(result_obj)) {
    stop("`result_obj` must be a DrugSigNet pipeline result object or list.", call. = FALSE)
  }

  if (!is.character(file_path) || length(file_path) != 1 || !nzchar(file_path)) {
    stop("`file_path` must be a non-empty file path.", call. = FALSE)
  }

  if (dir.exists(file_path)) {
    file_path <- file.path(file_path, "drugsignet_pipeline_results.xlsx")
  }
  if (!grepl("\\.xlsx$", file_path, ignore.case = TRUE)) {
    file_path <- paste0(file_path, ".xlsx")
  }

  if (!is.numeric(top_n) || length(top_n) != 1 || is.na(top_n) || top_n < 1) {
    stop("`top_n` must be a positive number.", call. = FALSE)
  }
  top_n <- as.integer(top_n)

  repurposing_sections <- intersect(
    c("signature", "network", "integrated"),
    names(result_obj$RankAggregation)
  )

  if (length(repurposing_sections) == 1L &&
      identical(result_obj$type, repurposing_sections)) {
    section <- repurposing_sections[[1]]
    result_obj$RankAggregation <- result_obj$RankAggregation[[section]]
    if (!is.null(result_obj$DrugAnnotation[[section]])) {
      result_obj$DrugAnnotation <- result_obj$DrugAnnotation[[section]]
    }
    if (is.list(result_obj$DrugSearching)) {
      for (name in intersect(c("Raw", "Processed"), names(result_obj$DrugSearching))) {
        if (!is.null(result_obj$DrugSearching[[name]][[section]])) {
          result_obj$DrugSearching[[name]] <- result_obj$DrugSearching[[name]][[section]]
        }
      }
    }
    repurposing_sections <- character()
  }

  extract_result_df <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.data.frame(x)) return(x)
    if (isS4(x) && "result" %in% methods::slotNames(x)) {
      val <- x@result
      if (is.data.frame(val)) return(val)
    }
    NULL
  }

  sanitize_sheet_name <- function(name, used) {
    nm <- gsub("[\\[\\]\\*\\?/\\\\:]", "_", as.character(name))
    nm <- trimws(nm)
    if (!nzchar(nm)) nm <- "Sheet"
    nm <- substr(nm, 1, 31)
    base <- nm
    idx <- 1L
    while (nm %in% used) {
      suffix <- paste0("_", idx)
      nm <- paste0(substr(base, 1, max(1, 31 - nchar(suffix))), suffix)
      idx <- idx + 1L
    }
    nm
  }

  flatten_result_tables <- function(x, parent = NULL) {
    out <- list()
    df <- extract_result_df(x)
    if (!is.null(df)) {
      nm <- if (is.null(parent)) "result" else parent
      out[[nm]] <- df
      return(out)
    }

    if (is.list(x)) {
      nms <- names(x)
      if (is.null(nms)) nms <- paste0("item_", seq_along(x))
      for (i in seq_along(x)) {
        key <- if (is.null(parent)) nms[[i]] else paste(parent, nms[[i]], sep = "_")
        out <- c(out, flatten_result_tables(x[[i]], parent = key))
      }
      return(out)
    }
    out
  }

  sheets <- list()

  # 1) Drug searching raw results per method
  raw_root <- NULL
  if (!is.null(result_obj$DrugSearching)) {
    if (is.list(result_obj$DrugSearching) && "Raw" %in% names(result_obj$DrugSearching)) {
      raw_root <- result_obj$DrugSearching$Raw
    } else {
      raw_root <- result_obj$DrugSearching
    }
  }
  raw_tables <- flatten_result_tables(raw_root)
  if (length(raw_tables) > 0) {
    sheets <- c(sheets, raw_tables)
  }

  collect_summary_sheets <- function(payload, prefix = "") {
    section_sheets <- list()
    sheet_name <- function(name) paste0(prefix, name)

    # Drug rankings
    ranking_df <- NULL
    if (!is.null(payload$RankAggregation$Network_Harmonized)) {
      ranking_df <- payload$RankAggregation$Network_Harmonized
    } else if (!is.null(payload$RankAggregation$Signature_Harmonized)) {
      ranking_df <- payload$RankAggregation$Signature_Harmonized
    } else if (!is.null(payload$RankAggregation$Signature_Network_Harmonized)) {
      ranking_df <- payload$RankAggregation$Signature_Network_Harmonized
    }
    if (is.data.frame(ranking_df)) {
      section_sheets[[sheet_name("Drug_Rankings")]] <- ranking_df
    }

    # Drug annotations
    features_df <- NULL
    if (!is.null(payload$DrugAnnotation$Features) &&
        is.data.frame(payload$DrugAnnotation$Features)) {
      features_df <- payload$DrugAnnotation$Features
      section_sheets[[sheet_name("Drug_Annotations")]] <- features_df
    }

    # Top N drugs across CRank / Dowdall / RRA + annotations
    if (is.data.frame(ranking_df) && "Drug" %in% names(ranking_df)) {
      rank_cols <- c(
        grep("CRank$", names(ranking_df), value = TRUE),
        grep("Dowdall$", names(ranking_df), value = TRUE),
        grep("Dowdall_rank$", names(ranking_df), value = TRUE),
        grep("RRA$", names(ranking_df), value = TRUE),
        grep("RRA_rank$", names(ranking_df), value = TRUE)
      )
      rank_cols <- unique(rank_cols)
      rank_cols <- rank_cols[vapply(ranking_df[rank_cols], is.numeric, logical(1))]

      if (length(rank_cols) > 0) {
        top_df <- ranking_df %>%
          dplyr::filter(
            dplyr::if_any(
              dplyr::all_of(rank_cols),
              ~ !is.na(.x) & .x <= top_n
            )
          ) %>%
          dplyr::select(Drug, dplyr::all_of(rank_cols)) %>%
          dplyr::distinct()

        if (is.data.frame(features_df) && nrow(top_df) > 0) {
          top_df <- top_df %>%
            dplyr::left_join(features_df, by = "Drug")
        }

        section_sheets[[sheet_name(paste0("Top_", top_n, "_Drugs"))]] <- top_df
      }
    }

    # Enriched terms
    enrich_df <- extract_result_df(payload$DrugAnnotation$Functional_Enrichment)
    if (is.data.frame(enrich_df)) {
      section_sheets[[sheet_name(paste0("Enriched_Terms_Top_", top_n))]] <- enrich_df
    }

    section_sheets
  }

  if (length(repurposing_sections) > 0) {
    for (section in repurposing_sections) {
      section_payload <- list(
        RankAggregation = result_obj$RankAggregation[[section]],
        DrugAnnotation = result_obj$DrugAnnotation[[section]]
      )
      sheets <- c(
        sheets,
        collect_summary_sheets(section_payload, prefix = paste0(section, "_"))
      )
    }
  } else {
    sheets <- c(sheets, collect_summary_sheets(result_obj))
  }

  # drop empty / invalid
  sheets <- sheets[vapply(sheets, is.data.frame, logical(1))]
  sheets <- sheets[vapply(sheets, nrow, integer(1)) >= 0]
  if (length(sheets) == 0) {
    stop("No writable tables found in `result_obj`.", call. = FALSE)
  }

  # normalize sheet names
  used <- character()
  out_sheets <- list()
  for (nm in names(sheets)) {
    sn <- sanitize_sheet_name(nm, used)
    used <- c(used, sn)
    out_sheets[[sn]] <- sheets[[nm]]
  }

  openxlsx::write.xlsx(out_sheets, file = file_path, overwrite = TRUE)
  invisible(file_path)
}
