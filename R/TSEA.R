#' @title Target Set Enrichment Analysis
#'
#' @description
#' Performs target set enrichment analysis against Gene Ontology libraries
#' using Enrichr.
#'
#' @details
#' `TSEA()` tests whether a set of drug targets is enriched for biological
#' pathways or functional terms. It is mainly used to summarize the biological
#' functions represented by targets of top-ranked drugs.
#'
#' For Gene Ontology enrichment, `source = "GO"` and `ont` selects Biological
#' Process (`"BP"`), Molecular Function (`"MF"`), Cellular Component (`"CC"`),
#' or all three ontologies (`"ALL"`). The current backend selects GO libraries
#' using `ont`; it does not implement KEGG dispatch through `source`.
#'
#' `targetList` should contain gene symbols by default. If another identifier
#' type is supplied, use `target_id_from` to specify the source identifier type.
#' Supported aliases include `"ENSEMBL"` or `"ensembl_gene_id"` for Ensembl gene
#' IDs, `"ENTREZID"` or `"entrezgene_id"` for Entrez IDs, and `"SYMBOL"` or
#' `"hgnc_symbol"` for HGNC symbols. Non-symbol identifiers are converted to
#' HGNC symbols before enrichment. If `target_id_from = NULL`, Ensembl gene IDs
#' are detected automatically when possible; otherwise targets are treated as
#' HGNC symbols.
#'
#' A character vector or one-column data frame can be supplied directly or passed
#' through `object` for pipe-friendly use.
#'
#' @param object Optional `DrugAnnotation` object. For pipe-friendly use, a
#'   character vector, factor, or data frame can also be supplied as `object`
#'   when `targetList` is omitted.
#' @param targetList Character vector of gene symbols or other gene identifiers.
#'   Required unless supplied through `object`.
#' @param source Compatibility source label stored in the result parameters.
#'   Use `"GO"`. The current backend does not select libraries from this value;
#'   `"KEGG"` does not request a KEGG analysis.
#' @param ont Gene Ontology namespace selected by the backend. One of
#'   `"BP"`, `"MF"`, `"CC"`, or `"ALL"`. Default is `"BP"`.
#' @param Adjusted.P.value Adjusted p-value cutoff used to filter enrichment
#'   results. Default is `0.05`.
#' @param target_id_from Optional AnnotationDbi keytype or supported alias
#'   describing the identifiers in `targetList`. If `NULL`, targets are treated
#'   as HGNC symbols unless Ensembl gene IDs are detected automatically.
#'
#' @section Background and interpretation:
#' The current libraries are `GO_Biological_Process_2023`,
#' `GO_Molecular_Function_2023` and `GO_Cellular_Component_2023`. Targets are
#' mapped to symbols and deduplicated before submission. There is no custom
#' background/universe argument; adjusted p-values refer to the Enrichr library
#' background, not a restricted set of experimentally detectable genes. Record
#' library names, retrieval dates, mapping losses and the candidate-selection
#' rule. Shared targets and annotation coverage can affect enrichment; an enriched
#' term does not establish a drug's mechanism or therapeutic effect.
#'
#' @seealso [annotate_drugs()], [plot_enriched_terms()]
#'
#' @return
#' A `DrugAnnotation` object containing filtered enrichment results in
#' `object@result`. Gene Ontology results include an `ont` column identifying
#' the BP, MF, or CC namespace.
#'
#' @examples
#' \dontrun{
#' target_genes <- c("TP53", "BRCA1", "EGFR")
#'
#' res_go <- TSEA(
#'   targetList = target_genes,
#'   source = "GO",
#'   ont = "BP",
#'   Adjusted.P.value = 0.01
#' )
#'
#' ensembl_targets <- c("ENSG00000141510", "ENSG00000012048")
#'
#' res_ensembl <- TSEA(
#'   targetList = ensembl_targets,
#'   source = "GO",
#'   ont = "ALL",
#'   target_id_from = "ENSEMBL"
#' )
#'
#' # Pipe-friendly use
#' res_pipe <- TSEA(ensembl_targets, ont = "ALL")
#' }
#'
#' @importFrom dplyr mutate filter bind_rows
#' @export
setGeneric(
  "TSEA",
  function(object = NULL,
           targetList,
           source = "GO",
           ont = c("BP", "MF", "CC", "ALL"),
           Adjusted.P.value = 0.05,
           target_id_from = NULL) {

    if (!is.null(object) && !methods::is(object, "DrugAnnotation") && missing(targetList)) {
      if (is.character(object) || is.factor(object)) {
        targetList <- as.character(object)
        object <- NULL
      } else if (is.data.frame(object)) {
        if ("targetList" %in% names(object)) {
          targetList <- as.character(object$targetList)
        } else if ("gene" %in% names(object)) {
          targetList <- as.character(object$gene)
        } else if (ncol(object) == 1) {
          targetList <- as.character(object[[1]])
        } else {
          stop("Piped data frames must contain `targetList` or `gene`, or have exactly one column.", call. = FALSE)
        }
        object <- NULL
      }
    }

    if (is.null(object)) {
      if (missing(targetList) || !is.character(targetList)) {
        stop(
          "`targetList` must be a character vector of gene identifiers.",
          call. = FALSE
        )
      }

      object <- DrugAnnotation(
        result = data.frame(),
        input_data = targetList,
        source = source,
        ont = match.arg(ont),
        Adjusted.P.value = Adjusted.P.value,
        target_id_from = target_id_from
      )
    }

    standardGeneric("TSEA")
  }
)


############################################################
# S4 Method: TSEA for DrugAnnotation
############################################################

#' @rdname TSEA
setMethod(
  "TSEA",
  signature = "DrugAnnotation",
  function(object) {

    params  <- object@parameters
    ont     <- params$ont
    cutoff  <- params$Adjusted.P.value

    # ---- Convert identifiers to gene symbols when requested ----
    target_symbols <- unique(trimws(as.character(params$input_data)))
    target_symbols <- target_symbols[!is.na(target_symbols) & nzchar(target_symbols)]

    target_id_from <- params$target_id_from
    if (!is.null(target_id_from)) {
      if (!is.character(target_id_from) || length(target_id_from) != 1 || is.na(target_id_from) || !nzchar(trimws(target_id_from))) {
        stop("`target_id_from` must be NULL or a single non-empty AnnotationDbi keytype or supported alias.", call. = FALSE)
      }
      target_id_from <- trimws(target_id_from)
    } else if (.looks_like_ensembl_gene_ids(target_symbols)) {
      target_id_from <- "ENSEMBL"
      message("[DrugSigNet] Detected Ensembl gene IDs in `targetList`; using target_id_from = 'ENSEMBL'.")
    }

    if (!is.null(target_id_from) && !identical(.normalize_orgdb_keytype(target_id_from), "SYMBOL")) {
      mapped_ids <- .map_gene_ids_orgdb(
        ids = target_symbols,
        from = target_id_from,
        to = "SYMBOL"
      )

      symbol_col <- names(mapped_ids)[toupper(names(mapped_ids)) == "SYMBOL"][1]
      if (is.na(symbol_col)) {
        stop("AnnotationDbi conversion did not return a `SYMBOL` column.", call. = FALSE)
      }

      target_symbols <- unique(trimws(as.character(mapped_ids[[symbol_col]])))
      target_symbols <- target_symbols[!is.na(target_symbols) & nzchar(target_symbols)]

      if (length(target_symbols) == 0) {
        stop("No valid HGNC symbols were returned by AnnotationDbi conversion.", call. = FALSE)
      }
    }

    # ---- Run enrichment ----
    enriched_terms <- .runEnrichR(target_symbols, ont)

    # ---- Process results ----
    if (ont == "ALL") {

      object@result <- .tsea_bind_results(list(
        .tsea_prepare_result(
          enriched_terms[["GO_Biological_Process_2023"]], "BP", cutoff
        ),
        .tsea_prepare_result(
          enriched_terms[["GO_Molecular_Function_2023"]], "MF", cutoff
        ),
        .tsea_prepare_result(
          enriched_terms[["GO_Cellular_Component_2023"]], "CC", cutoff
        )
      ))

    } else {

      object@result <- .tsea_prepare_result(enriched_terms[[1L]], ont, cutoff)
    }

    object
  }
)

# EnrichR returns ordinary data frames. Keep the small schema annotation and
# cutoff operation in base R so TSEA does not need to enter dplyr's native
# data-mask/vector-chop path during long-running package test sessions.
.tsea_prepare_result <- function(result, ont, cutoff) {
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  if (!"Adjusted.P.value" %in% names(result)) {
    stop(
      "Enrichment results must contain an `Adjusted.P.value` column.",
      call. = FALSE
    )
  }

  result$ont <- ont
  adjusted_p <- result$Adjusted.P.value
  keep <- !is.na(adjusted_p) & adjusted_p <= cutoff
  result <- result[keep, , drop = FALSE]
  rownames(result) <- NULL
  result
}

# Base equivalent of bind_rows() for the three GO result tables. EnrichR
# normally returns the same schema for each ontology, while the fill behavior
# keeps this robust to an ontology-specific extra column.
.tsea_bind_results <- function(results) {
  column_names <- unique(unlist(lapply(results, names), use.names = FALSE))
  results <- lapply(results, function(result) {
    missing_columns <- setdiff(column_names, names(result))
    for (column in missing_columns) {
      result[[column]] <- rep(NA, nrow(result))
    }
    result[column_names]
  })

  combined <- do.call(rbind, results)
  rownames(combined) <- NULL
  combined
}

############################################################
# Internal: EnrichR wrapper
############################################################

.runEnrichR <- function(targetList, ont) {
  if (!requireNamespace("enrichR", quietly = TRUE)) {
    stop("Package 'enrichR' is required for enrichment analysis.", call. = FALSE)
  }

  # enrichR initializes the service URL/handle in its attach hook rather than
  # its namespace-load hook. Attach the already-loaded namespace only for this
  # request and always restore the caller's search path afterwards. This uses
  # R's namespace lifecycle instead of calling enrichR's private hook or
  # attempting to reproduce its version-specific internal state.
  enrichr_attached_here <- !"package:enrichR" %in% search()
  if (enrichr_attached_here) {
    suppressPackageStartupMessages(attachNamespace(asNamespace("enrichR")))
    on.exit(
      detach("package:enrichR", unload = FALSE, character.only = TRUE),
      add = TRUE
    )
  }

  dbs <- switch(
    ont,
    BP  = "GO_Biological_Process_2023",
    MF  = "GO_Molecular_Function_2023",
    CC  = "GO_Cellular_Component_2023",
    ALL = c("GO_Biological_Process_2023",
            "GO_Molecular_Function_2023",
            "GO_Cellular_Component_2023"),
    stop("Invalid ontology specified.", call. = FALSE)
  )

  enriched <- tryCatch(
    enrichR::enrichr(targetList, dbs),
    error = function(e)
      stop("Enrichment analysis failed: ", e$message, call. = FALSE)
  )

  if (!is.list(enriched) || length(enriched) == 0) {
    stop(
      "Enrichment analysis returned no results. Check target list.",
      call. = FALSE
    )
  }

  enriched
}
