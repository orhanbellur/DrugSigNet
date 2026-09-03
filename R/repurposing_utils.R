#' Validate Network Inputs Before Running Pipelines
#'
#' @description
#' Validates network analysis inputs (`ppi`, `dti`, `disease_genes`) before
#' expensive method execution. This helper checks required schemas, missing
#' values, duplicated edges, self-loops, and disease-gene overlap with the PPI
#' network.
#'
#' @param ppi A data frame with columns `gene1` and `gene2`.
#' @param dti A data frame with columns `Drug` and `Target`.
#' @param disease_genes A data frame with column `gene`.
#' @param strict Logical; if `TRUE`, overlap failures stop with an error.
#'   If `FALSE`, overlap failures emit a warning.
#'
#' @return Invisibly returns `TRUE` when checks pass.
#' @export
validate_network_inputs <- function(ppi, dti, disease_genes, strict = TRUE) {
  validate_network_columns(ppi, c("gene1", "gene2"), "ppi")
  validate_network_columns(dti, c("Drug", "Target"), "dti")
  validate_network_columns(disease_genes, c("gene"), "disease_genes")

  if (!is.logical(strict) || length(strict) != 1 || is.na(strict)) {
    stop("`strict` must be a single logical value.", call. = FALSE)
  }

  ppi_key <- ppi %>%
    dplyr::transmute(gene1 = as.character(gene1), gene2 = as.character(gene2))

  if (any(is.na(ppi_key$gene1) | is.na(ppi_key$gene2) |
          !nzchar(trimws(ppi_key$gene1)) | !nzchar(trimws(ppi_key$gene2)))) {
    stop("`ppi` contains NA/empty values in `gene1` or `gene2`.", call. = FALSE)
  }

  edge_sig <- paste(ppi_key$gene1, ppi_key$gene2, sep = "::")
  if (anyDuplicated(edge_sig) > 0) {
    stop("`ppi` contains duplicated edges.", call. = FALSE)
  }

  if (any(ppi_key$gene1 == ppi_key$gene2)) {
    stop("`ppi` contains self-loops where `gene1 == gene2`.", call. = FALSE)
  }

  dti_key <- dti %>%
    dplyr::transmute(Drug = as.character(Drug), Target = as.character(Target))

  if (any(is.na(dti_key$Drug) | is.na(dti_key$Target) |
          !nzchar(trimws(dti_key$Drug)) | !nzchar(trimws(dti_key$Target)))) {
    stop("`dti` contains NA/empty values in `Drug` or `Target`.", call. = FALSE)
  }

  disease_tbl <- disease_genes %>% dplyr::transmute(gene = as.character(gene))
  if (any(is.na(disease_tbl$gene) | !nzchar(trimws(disease_tbl$gene)))) {
    stop("`disease_genes` contains NA/empty values in `gene`.", call. = FALSE)
  }

  nodes <- unique(c(ppi_key$gene1, ppi_key$gene2))
  overlap_n <- sum(unique(disease_tbl$gene) %in% nodes)

  if (overlap_n == 0) {
    msg <- "No overlap between `disease_genes$gene` and PPI network nodes."
    if (strict) {
      stop(msg, call. = FALSE)
    } else {
      warning(msg, call. = FALSE)
    }
  }

  invisible(TRUE)
}

.list_or_empty <- function(x) {
  if (is.null(x)) list() else x
}

build_pipeline_object <- function(raw = list(),
                                  processed = list(),
                                  rank_aggregation = list(),
                                  drug_annotation = NULL,
                                  visualization = NULL,
                                  type = "signature") {
  DrugSearchingPipeline(
    DrugSearching = list(
      Raw = .list_or_empty(raw),
      Processed = .list_or_empty(processed)
    ),
    RankAggregation = .list_or_empty(rank_aggregation),
    DrugAnnotation = drug_annotation,
    Visualization = visualization,
    type = type
  )
}

.pipeline_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be TRUE or FALSE.", name), call. = FALSE)
  }
  isTRUE(x)
}

.pipeline_message <- function(pipeline, text, step = NULL, total = NULL) {
  prefix <- paste0("[DrugSigNet] ", pipeline)
  if (!is.null(step) && !is.null(total)) {
    prefix <- sprintf("%s [%d/%d]", prefix, as.integer(step), as.integer(total))
  }
  message(sprintf("%s: %s", prefix, text))
}

.repurposing_mode_sections <- function(mode) {
  switch(
    mode,
    signature = "signature",
    network = "network",
    both = c("signature", "network", "integrated"),
    stop("Unknown repurposing mode: ", mode, call. = FALSE)
  )
}

.validate_signature_result_for_integration <- function(signature_result) {
  if (!.is_drug_searching_pipeline(signature_result)) {
    stop(
      "The signature stage did not return a DrugSearchingPipeline object.",
      call. = FALSE
    )
  }

  pairwise <- signature_result@RankAggregation$Pairwise
  if (!is.list(pairwise) || length(pairwise) == 0L) {
    stop(
      "The signature stage completed without pairwise rank aggregation results; ",
      "integration cannot continue. Run drugSignaturePipeline() with the same ",
      "arguments to inspect the signature-stage warning.",
      call. = FALSE
    )
  }

  invisible(signature_result)
}

.safe_rename_drug <- function(df) {
  if ("Name" %in% names(df) && !"Drug" %in% names(df)) {
    df <- dplyr::rename(df, Drug = Name)
  }
  df
}

.join_by_drug <- function(x, y) {
  dplyr::full_join(x, y, by = "Drug") %>%
    dplyr::distinct()
}

.rank_values <- function(values, ties_method, decreasing = TRUE, na_fill = NULL) {
  values <- suppressWarnings(as.numeric(values))
  if (ties_method == "dense") {
    ranked <- if (isTRUE(decreasing)) {
      dplyr::dense_rank(-values)
    } else {
      dplyr::dense_rank(values)
    }
  } else {
    ranked <- if (isTRUE(decreasing)) {
      rank(-values, ties.method = ties_method, na.last = "keep")
    } else {
      rank(values, ties.method = ties_method, na.last = "keep")
    }
  }
  if (!is.null(na_fill)) {
    ranked[is.na(ranked)] <- na_fill
  }
  ranked
}

ensure_pipeline_sections <- function(result) {
  if (is.null(result$DrugSearching)) {
    result$DrugSearching <- list()
  }
  if (is.null(result$DrugSearching$Raw)) {
    result$DrugSearching$Raw <- list()
  }
  if (is.null(result$DrugSearching$Processed)) {
    result$DrugSearching$Processed <- list()
  }
  if (is.null(result$RankAggregation)) {
    result$RankAggregation <- list()
  }
  if (!is.null(result$Visualization) && !is.list(result$Visualization)) {
    result$Visualization <- list(result$Visualization)
  }
  result
}
