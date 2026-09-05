#' @title LINCS Method for Signature-Based Drug Searching
#'
#' @description
#' Performs gene expression signature-based drug repurposing using the LINCS
#' algorithm.
#'
#' @details
#' The query may contain up-regulated genes (`upset`), down-regulated genes
#' (`downset`), or both. At least one of `upset` or `downset` must be provided;
#' they cannot both be `NULL`.
#'
#' `ref_db` can be `"cmap"` or `"lincs2"`. A local HDF5 path returned by
#' `load_signature_refdb()` can also be supplied to use a frozen Synapse
#' reference database. Frozen databases can also be fetched directly by setting
#' `signature_refdb_mode = "frozen"` or `"frozen_force"` and providing
#' `auth_token`.
#'
#' @param object Optional `SignatureBased` object. If `NULL`, a new object is
#'   created from `upset`, `downset`, and `ref_db`.
#' @param upset Character vector of up-regulated Entrez gene IDs. May be `NULL`
#'   if `downset` is provided.
#' @param downset Character vector of down-regulated Entrez gene IDs. May be
#'   `NULL` if `upset` is provided.
#' @param ref_db Reference database. Use `"cmap"` or `"lincs2"`, or provide a
#'   local HDF5 path returned by `load_signature_refdb()` to use a frozen Synapse
#'   reference database.
#' @param tau Logical; whether to apply tau ranking. Default is `FALSE`.
#' @param sortby Character string specifying the score used to sort LINCS
#'   results. Default is `"WTCS"`.
#' @param chunk_size Integer; chunk size used by the LINCS search. Default is
#'   `5000`.
#' @param GeneType Character string specifying the gene type used by the
#'   reference database search. Default is `"reference"`.
#' @inheritParams get_drug_signature
#'
#' @return
#' A `SignatureBased` object containing the LINCS search results and query
#' metadata.
#'
#' @examples
#' \dontrun{
#' upset <- c("7157", "1956", "5290")
#' downset <- c("7422", "4318", "348")
#'
#' # Search using the default LINCS reference database
#' res <- lincs_method(
#'   upset = upset,
#'   downset = downset,
#'   ref_db = "lincs2"
#' )
#'
#' # Run a one-sided query
#' res_up <- lincs_method(
#'   upset = upset,
#'   downset = NULL,
#'   ref_db = "lincs2"
#' )
#'
#' # Automatically fetch and use a frozen Synapse reference database
#' res_frozen <- lincs_method(
#'   upset = upset,
#'   downset = downset,
#'   ref_db = "lincs2",
#'   signature_refdb_mode = "frozen",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#'
#' # Reuse a local frozen HDF5 reference database
#' lincs_ref <- load_signature_refdb(
#'   ref_db = "lincs2",
#'   signature_refdb_mode = "frozen",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#'
#' res_local <- lincs_method(
#'   upset = upset,
#'   downset = downset,
#'   ref_db = lincs_ref
#' )
#' }
#'
#' @importFrom signatureSearch qSig gess_lincs result
#' @importFrom magrittr %>%
#' @export
setGeneric(
  "lincs_method",
  function(object = NULL, upset, downset, ref_db, tau = FALSE, sortby = "WTCS",
           chunk_size = 5000, GeneType = "reference",
           signature_refdb_mode = c("default", "frozen", "frozen_force"),
           auth_token = NULL, validate_signature_refdb = TRUE) {
    # Automatically handle the case where object is NULL
    if (is.null(object)) {
      # Validate `upset` and `downset`
      validate_query_vector <- function(vec, name) {
        if (!is.character(vec)) stop(sprintf("`%s` must be a character vector.", name))
        if (length(vec) == 0) stop(sprintf("`%s` cannot be empty.", name))
      }
      if (!is.null(upset)) validate_query_vector(upset, "upset")
      if (!is.null(downset)) validate_query_vector(downset, "downset")
      if (is.null(upset) && is.null(downset)) {
        stop("At least one of `upset` or `downset` must be provided.")
      }

      # Create a SignatureBased object
      ref_db <- .resolve_signature_refdb(
        ref_db = ref_db,
        signature_refdb_mode = signature_refdb_mode,
        auth_token = auth_token,
        validate_hdf5 = validate_signature_refdb
      )
      query <- list(upset = upset, downset = downset)
      object <- SignatureBased(
        result = data.frame(),
        query = query,
        signature_method = "LINCS",
        refdb = ref_db,
        tau = tau,
        sortby = sortby,
        chunk_size = chunk_size,
        GeneType = GeneType
      )
    }
    standardGeneric("lincs_method")
  }
)
#' @describeIn lincs_method
#' Implements the LINCS method for Signature-Based Drug Searching.
#'
#' @export


# Method for lincs_method when object is provided (SignatureBasedDrugSearching)
setMethod(
  "lincs_method",
  signature = "SignatureBased",
  function(object) {
    # Extract query parameters
    params <- object@parameters
    query <- params$query
    upset <- query$upset
    downset <- query$downset

    # Validate query inputs
    validate_query_vector <- function(vec, name) {
      if (!is.character(vec)) stop(sprintf("`%s` must be a character vector.", name))
      if (length(vec) == 0) stop(sprintf("`%s` cannot be empty.", name))
    }
    if (!is.null(upset)) validate_query_vector(upset, "upset")
    if (!is.null(downset)) validate_query_vector(downset, "downset")
    if (is.null(upset) && is.null(downset)) {
      stop("At least one of `upset` or `downset` must be provided in the query.")
    }

    # Validate reference database
    ref_db <- params$refdb
    if (!is.character(ref_db) || length(ref_db) != 1) {
      stop("`refdb` must be a single character string specifying the reference database.")
    }

    # Extract additional parameters
    sortby <- params$sortby
    tau <- params$tau
    chunk_size <- params$chunk_size
    GeneType <- params$GeneType

    # Log the process
    cat("Running LINCS method for SignatureBased object...\n")
    cat("Reference database:", ref_db, "\n")
    cat("Query parameters:\n")
    cat("  Upset:", paste(upset, collapse = ", "), "\n")
    cat("  Downset:", paste(downset, collapse = ", "), "\n")
    cat("Sort by:", sortby, "\n")
    cat("Tau:", tau, "\n")
    cat("Chunk size:", chunk_size, "\n")
    cat("Gene type:", GeneType, "\n")

    # Perform the LINCS method search
    tryCatch({
      method_res <- .with_signature_search_attached(
        signatureSearch::qSig(query = query, gess_method = "LINCS", refdb = ref_db) %>%
          signatureSearch::gess_lincs(sortby = sortby, tau = tau, chunk_size = chunk_size, GeneType = GeneType) %>%
          signatureSearch::result()
      )

      # Update the result slot
      object@result <- method_res
    }, error = function(e) {
      stop("LINCS analysis failed with error: ", e$message)
    })

    # Return the updated object
    object@parameters <- filterSignatureParameters(object)

    return(object)
  }
)


#' Rank LINCS Signature Results
#'
#' Converts raw results from [lincs_method()] into the standardized ranking
#' format used by the signature pipeline.
#'
#' @param lincs_signature A data frame of raw LINCS signature results.
#' @param ties_method Character string passed to [base::rank()], or `"dense"`.
#' @return A data frame of standardized LINCS rank scores.
#' @export
lincs_rank_score <- function(lincs_signature, ties_method = "max") {
  if ("name" %in% names(lincs_signature)) {
    lincs_signature <- lincs_signature %>%
      dplyr::mutate(pert = ifelse(is.na(name), pert, name))
  }
  lincs_rank <- lincs_signature %>%
    dplyr::group_by(pert) %>%
    dplyr::slice_max(order_by = abs(WTCS), n = 1, with_ties = TRUE) %>%
    dplyr::mutate(cell = paste0(unique(cell), collapse = ";")) %>%
    unique() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(padj = stats::p.adjust(WTCS_Pval, method = "BY")) %>%
    dplyr::mutate(rank_score = ifelse(
      padj <= 0.05,
      1 / if (ties_method == "dense") dplyr::dense_rank(-abs(WTCS)) else base::rank(-abs(WTCS), ties.method = ties_method),
      0
    )) %>%
    dplyr::mutate(scaled_score = (abs(WTCS) - min(abs(WTCS))) / (max(abs(WTCS)) - min(abs(WTCS)))) %>%
    .data_format(score_col = "WTCS")
  return(lincs_rank)
}
