#' @title Write Pipeline Results to Excel
#'
#' @description
#' Writes key tables from a DrugSigNet pipeline result object to a multi-sheet
#' Excel workbook.
#'
#' @details
#' `write_pipeline_results()` exports tables from `drugNetworkPipeline()`,
#' `drugSignaturePipeline()`, or `drugRepurposingPipeline()`. Unified
#' repurposing results retain separate signature, network, and integrated
#' worksheets according to the mode that was run.
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
    if (is.list(x)) {
      nms <- names(x)
      if (is.null(nms)) nms <- paste0("item_", seq_along(x))
      for (i in seq_along(x)) {
        key <- if (is.null(parent)) nms[[i]] else paste(parent, nms[[i]], sep = "_")
        out <- c(out, flatten_result_tables(x[[i]], parent = key))
      }
      return(out)
    }

    df <- extract_result_df(x)
    if (!is.null(df)) {
      nm <- if (is.null(parent)) "result" else parent
      out[[nm]] <- df
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

  table_scope <- function(name, suffix) {
    sub(paste0("_?", suffix, "$"), "", name, perl = TRUE)
  }

  scoped_sheet_name <- function(scope, label) {
    if (!nzchar(scope)) label else paste(scope, label, sep = "_")
  }

  # 2) Drug rankings. Repurposing-pipeline results nest these tables under
  # signature, network, and integrated mode names.
  rank_tables <- flatten_result_tables(result_obj$RankAggregation)
  rank_tables <- rank_tables[grepl("Harmonized$", names(rank_tables))]

  # 3) Drug annotations, also potentially nested by repurposing mode.
  annotation_tables <- flatten_result_tables(result_obj$DrugAnnotation)
  feature_tables <- annotation_tables[grepl("Features$", names(annotation_tables))]
  enrichment_tables <- annotation_tables[grepl("Functional_Enrichment$", names(annotation_tables))]

  for (feature_name in names(feature_tables)) {
    scope <- table_scope(feature_name, "Features")
    sheets[[scoped_sheet_name(scope, "Drug_Annotations")]] <- feature_tables[[feature_name]]
  }

  # 4) Rankings and Top N drugs across CRank / Dowdall / RRA, with annotations
  # from the corresponding repurposing mode when available.
  for (ranking_name in names(rank_tables)) {
    ranking_df <- rank_tables[[ranking_name]]
    if (!is.data.frame(ranking_df)) next

    ranking_scope <- table_scope(
      ranking_name,
      "(?:Signature_Network_|Network_|Signature_)?Harmonized"
    )
    sheets[[scoped_sheet_name(ranking_scope, "Drug_Rankings")]] <- ranking_df

    if (!"Drug" %in% names(ranking_df)) next

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

      features_df <- NULL
      feature_scopes <- vapply(
        names(feature_tables),
        table_scope,
        character(1),
        suffix = "Features"
      )
      matching_feature <- which(feature_scopes == ranking_scope)
      if (length(matching_feature) > 0) {
        features_df <- feature_tables[[matching_feature[[1]]]]
      }

      if (is.data.frame(features_df) && nrow(top_df) > 0) {
        top_df <- top_df %>%
          dplyr::left_join(features_df, by = "Drug")
      }

      sheets[[scoped_sheet_name(ranking_scope, paste0("Top_", top_n, "_Drugs"))]] <- top_df
    }
  }

  # 5) Enriched terms for every available repurposing mode.
  for (enrichment_name in names(enrichment_tables)) {
    scope <- table_scope(enrichment_name, "Functional_Enrichment")
    sheets[[scoped_sheet_name(scope, paste0("Enriched_Terms_Top_", top_n))]] <-
      enrichment_tables[[enrichment_name]]
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
