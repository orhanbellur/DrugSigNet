#' @title gCMAP Method for Signature-Based Drug Searching
#'
#' @description
#' Performs gene expression signature-based drug repurposing using the gCMAP
#' algorithm.
#'
#' @details
#' The method compares a ranked gene expression signature with signatures in a
#' reference database using the gCMAP algorithm. The input `signature_matrix`
#' must be a numeric matrix containing log2 fold changes, with one column and
#' gene identifiers stored as row names.
#'
#' `higher` and `lower` define the thresholds used to select up- and
#' down-regulated genes from the input signature. `padj` can be used to filter
#' the returned results by adjusted p-value when supported by the reference
#' database.
#'
#' `ref_db` can be `"cmap"` or `"lincs2"`. A local HDF5 path returned by
#' `load_signature_refdb()` can also be supplied to use a frozen Synapse
#' reference database. Frozen databases can also be fetched directly by setting
#' `signature_refdb_mode = "frozen"` or `"frozen_force"` and providing
#' `auth_token`.
#'
#' @param object Optional `SignatureBased` object. If `NULL`, a new object is
#'   created from `signature_matrix`, `ref_db`, `higher`, `lower`, and `padj`.
#' @param signature_matrix Numeric matrix containing log2 fold changes. The
#'   matrix must have one column, with gene identifiers stored as row names.
#' @param ref_db Reference database. Use `"cmap"` or `"lincs2"`, or provide a
#'   local HDF5 path returned by `load_signature_refdb()` to use a frozen Synapse
#'   reference database.
#' @param higher Numeric threshold for selecting higher-expressed genes. If
#'   `NULL`, no upper threshold is applied by this wrapper.
#' @param lower Numeric threshold for selecting lower-expressed genes. If
#'   `NULL`, no lower threshold is applied by this wrapper.
#' @param padj Optional adjusted p-value threshold used to filter results when
#'   supported by the selected reference database.
#' @param chunk_size Integer; number of reference signatures processed per
#'   chunk. Default is `5000`.
#' @inheritParams get_drug_signature
#'
#' @return
#' A `SignatureBased` object containing the gCMAP search results and query
#' metadata.
#'
#' @examples
#' \dontrun{
#' # Differential gene expression signature (log2 fold changes)
#' signature_matrix <- matrix(
#'   c(1.35, -0.82, 2.11, -1.47),
#'   ncol = 1,
#'   dimnames = list(
#'     c("7157", "1956", "5290", "7422"),
#'     "log2FC"
#'   )
#' )
#'
#' # Search using the default CMAP reference database
#' res <- gcmap_method(
#'   signature_matrix = signature_matrix,
#'   ref_db = "cmap",
#'   higher = 1,
#'   lower = -1,
#'   padj = 0.05
#' )
#'
#' # Automatically fetch and use a frozen Synapse reference database
#' res_frozen <- gcmap_method(
#'   signature_matrix = signature_matrix,
#'   ref_db = "cmap",
#'   higher = 1,
#'   lower = -1,
#'   padj = 0.05,
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
#' res_local <- gcmap_method(
#'   signature_matrix = signature_matrix,
#'   ref_db = cmap_ref,
#'   higher = 1,
#'   lower = -1,
#'   padj = 0.05
#' )
#' }
#'
#' @importFrom signatureSearch qSig gess_gcmap result
#' @importFrom magrittr %>%
#' @export
setGeneric(
  "gcmap_method",
  function(object = NULL, signature_matrix, ref_db, higher = NULL, lower = NULL, padj = NULL,
           chunk_size = 5000,
           signature_refdb_mode = c("default", "frozen", "frozen_force"),
           auth_token = NULL, validate_signature_refdb = TRUE) {
    # Automatically handle the case where object is NULL
    if (is.null(object)) {
      # Validate `signature_matrix`
      if (!is.matrix(signature_matrix) || !is.numeric(signature_matrix)) {
        stop("`signature_matrix` must be a numeric matrix.")
      }
      if (ncol(signature_matrix) != 1) {
        stop("`signature_matrix` must have exactly one column.")
      }
      if (is.null(rownames(signature_matrix))) {
        stop("Gene labels must be stored as row names in `signature_matrix`.")
      }

      # Create the SignatureBased object
      ref_db <- .resolve_signature_refdb(
        ref_db = ref_db,
        signature_refdb_mode = signature_refdb_mode,
        auth_token = auth_token,
        validate_hdf5 = validate_signature_refdb
      )
      object <- SignatureBased(
        result = data.frame(),
        query = signature_matrix,
        signature_method = "gCMAP",
        padj = padj,
        higher = higher,
        lower = lower,
        refdb = ref_db,
        chunk_size = chunk_size
      )
    }
    standardGeneric("gcmap_method")
  }
)

#' @describeIn gcmap_method
#' Implements the gCMAP method for `SignatureBased`.
#'
#' @export
setMethod(
  "gcmap_method",
  signature = "SignatureBased",
  function(object) {
    # Extract query parameters
    query <- object@parameters$query
    ref_db <- object@parameters$refdb
    higher <- object@parameters$higher
    lower <- object@parameters$lower
    padj <- object@parameters$padj
    chunk_size <- object@parameters$chunk_size

    # Validate `query`
    if (!is.matrix(query) || !is.numeric(query)) {
      stop("The `query` slot in the `SignatureBased` object must be a numeric matrix.")
    }
    if (ncol(query) != 1) {
      stop("The `query` matrix must have exactly one column.")
    }
    if (is.null(rownames(query))) {
      stop("Gene labels must be stored as row names in the `query` matrix.")
    }

    # Validate `ref_db`
    if (!is.character(ref_db) || length(ref_db) != 1) {
      stop("`refdb` must be a single character string specifying the reference database.")
    }

    # Log the process
    cat("Running gcmap_method for SignatureBased object...\n")
    cat("Reference database:", ref_db, "\n")
    cat("Query matrix dimensions:", dim(query), "\n")
    cat("Chunk size:", chunk_size, "\n")

    # Perform the gCMAP analysis
    tryCatch(
      {
        method_res <- signatureSearch::qSig(query = query, gess_method = "gCMAP", refdb = ref_db) %>%
          signatureSearch::gess_gcmap(higher = higher, lower = lower, padj = padj, chunk_size = chunk_size) %>%
          signatureSearch::result()

        # Update the result slot
        object@result <- method_res
      },
      error = function(e) {
        stop("gCMAP analysis failed with error: ", e$message)
      }
    )

    # Return the updated object
    object@parameters <- filterSignatureParameters(object)

    return(object)
  }
)


#' Rank gCMAP Signature Results
#'
#' Converts raw results from [gcmap_method()] into the standardized ranking
#' format used by the signature pipeline.
#'
#' @param gcmap_signature A data frame of raw gCMAP signature results.
#' @param cmap_signature Corresponding raw CMAP results used to derive the cutoff.
#' @param trend_name Optional trend used to derive the CMAP cutoff.
#' @param ties_method Character string passed to [base::rank()], or `"dense"`.
#' @return A data frame of standardized gCMAP rank scores.
#' @export
gcmap_rank_score <- function(gcmap_signature, cmap_signature, trend_name = NULL, ties_method = "max") {
  cmap.sig <- BoutrosLab.plotting.general::critical.value.ks.test(
    sum(cmap_signature$N_upset[1], cmap_signature$N_downset[1]), 0.95
  )
  if (!is.null(trend_name)) {
    cmap_signature <- cmap_signature %>% dplyr::filter(trend == trend_name)
  }
  gcmap.sig <- min(abs(cmap_signature$scaled_score)[which(abs(cmap_signature$raw_score) >= cmap.sig)])
  if ("name" %in% names(gcmap_signature)) {
    gcmap_signature <- gcmap_signature %>%
      dplyr::mutate(pert = ifelse(is.na(name), pert, name))
  }
  gcmap_rank <- gcmap_signature %>%
    dplyr::group_by(pert) %>%
    dplyr::slice_max(order_by = abs(effect), n = 1, with_ties = TRUE) %>%
    dplyr::mutate(cell = paste0(unique(cell), collapse = ";")) %>%
    unique() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      rank_score = ifelse(
        abs(effect) >= gcmap.sig,
        1 / if (ties_method == "dense") dplyr::dense_rank(-abs(effect)) else base::rank(-abs(effect), ties.method = ties_method),
        0
      ),
      scaled_score = effect
    ) %>%
    .data_format(score_col = "effect")
  return(gcmap_rank)
}
