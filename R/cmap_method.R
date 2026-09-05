#' @title CMAP Method for Signature-Based Drug Searching
#'
#' @description
#' Performs gene expression signature-based drug repurposing using the CMAP
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
#' @param chunk_size Integer; number of reference signatures processed per
#'   chunk. Default is `5000`.
#' @inheritParams get_drug_signature
#'
#' @return
#' A `SignatureBased` object containing the CMAP search results and query
#' metadata.
#'
#' @examples
#' \dontrun{
#' upset <- c("7157", "1956", "5290")
#' downset <- c("7422", "4318", "348")
#'
#' # Search using the default CMAP reference database
#' res <- cmap_method(
#'   upset = upset,
#'   downset = downset,
#'   ref_db = "cmap"
#' )
#'
#' # Run a one-sided query
#' res_up <- cmap_method(
#'   upset = upset,
#'   downset = NULL,
#'   ref_db = "cmap"
#' )
#'
#' # Fetch and use the frozen Synapse reference database directly
#' res_frozen <- cmap_method(
#'   upset = upset,
#'   downset = downset,
#'   ref_db = "cmap",
#'   signature_refdb_mode = "frozen",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#'
#' # Reuse a local frozen HDF5 reference database
#' cmap_ref <- load_signature_refdb(
#'   ref_db = "cmap",
#'   signature_refdb_mode = "frozen",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#'
#' res_local <- cmap_method(
#'   upset = upset,
#'   downset = downset,
#'   ref_db = cmap_ref
#' )
#' }
#'
#' @importFrom signatureSearch qSig gess_cmap result
#' @importFrom magrittr %>%
#' @export

# Define the cmap_method generic function
setGeneric(
  "cmap_method",
  function(object = NULL, upset, downset, ref_db, chunk_size = 5000,
           signature_refdb_mode = c("default", "frozen", "frozen_force"),
           auth_token = NULL, validate_signature_refdb = TRUE) {
    # Automatically handle the case where object is NULL
    if (is.null(object)) {
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
        signature_method = "CMAP",
        refdb = ref_db,
        chunk_size = chunk_size
      )
    }
    standardGeneric("cmap_method")
  }
)

# Define the cmap_method for the SignatureBased class
#' @rdname cmap_method
setMethod(
  "cmap_method",
  signature = "SignatureBased",
  function(object) {
    # Extract parameters from the object
    params <- object@parameters
    query <- params$query
    refdb <- params$refdb
    chunk_size <- params$chunk_size

    # Extract `upset` and `downset`
    upset <- query$upset
    downset <- query$downset

    # Helper function to validate input vectors
    validate_query_vector <- function(vec, name) {
      if (!is.character(vec)) stop(sprintf("`%s` must be a character vector.", name))
      if (length(vec) == 0) stop(sprintf("`%s` cannot be empty.", name))
    }

    # Validate `upset` and `downset`
    if (!is.null(upset)) validate_query_vector(upset, "upset")
    if (!is.null(downset)) validate_query_vector(downset, "downset")
    if (is.null(upset) && is.null(downset)) {
      stop("At least one of `upset` or `downset` must be provided in the query.")
    }

    # Validate reference database
    if (!is.character(refdb) || length(refdb) != 1) {
      stop("`refdb` must be a single character string specifying the reference database.")
    }

    # Log the process
    cat("Running cmap_method for SignatureBased object...\n")
    cat("Reference database:", refdb, "\n")
    cat("Query parameters:\n")
    cat("  Upset:", paste(upset, collapse = ", "), "\n")
    cat("  Downset:", paste(downset, collapse = ", "), "\n")
    cat("Chunk size:", chunk_size, "\n")

    # Perform the CMAP analysis
    tryCatch(
      {
        method_res <- .with_signature_search_attached(
          signatureSearch::qSig(query = query, gess_method = "CMAP", refdb = refdb) %>%
            signatureSearch::gess_cmap(chunk_size = chunk_size) %>%
            signatureSearch::result()
        )

        # Update the result slot
        object@result <- method_res
      },
      error = function(e) {
        stop("CMAP analysis failed with error: ", e$message)
      }
    )

    # Return the updated object
    object@parameters <- filterSignatureParameters(object)

    return(object)
  }
)


#' Rank CMAP Signature Results
#'
#' Converts raw results from [cmap_method()] into the standardized ranking
#' format used by the signature pipeline.
#'
#' @param cmap_signature A data frame of raw CMAP signature results.
#' @param confidence_level Numeric confidence level for the Kolmogorov-Smirnov
#'   critical-value cutoff.
#' @param ties_method Character string passed to [base::rank()], or `"dense"`.
#' @return A data frame of standardized CMAP rank scores.
#' @export
cmap_rank_score <- function(cmap_signature, confidence_level = 0.95, ties_method = "max") {
  cmap.sig <- BoutrosLab.plotting.general::critical.value.ks.test(
    sum(cmap_signature$N_upset[1], cmap_signature$N_downset[1]), confidence_level
  )
  if ("name" %in% names(cmap_signature)) {
    cmap_signature <- cmap_signature %>%
      dplyr::mutate(pert = ifelse(is.na(name), pert, name))
  }
  cmap_rank <- cmap_signature %>%
    dplyr::group_by(pert) %>%
    dplyr::slice_max(order_by = abs(raw_score), n = 1, with_ties = TRUE) %>%
    dplyr::mutate(cell = paste0(unique(cell), collapse = ";")) %>%
    unique() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(rank_score = ifelse(
      abs(raw_score) >= cmap.sig,
      1 / if (ties_method == "dense") dplyr::dense_rank(-abs(raw_score)) else base::rank(-abs(raw_score), ties.method = ties_method),
      0
    )) %>%
    .data_format(score_col = "raw_score")
  return(cmap_rank)
}
