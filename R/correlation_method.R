#' @title Correlation Method for Signature-Based Drug Searching
#'
#' @description
#' Performs gene expression signature-based drug repurposing using a
#' correlation-based search.
#'
#' @details
#' The method compares the input gene expression signature with signatures in a
#' reference database using the selected correlation method. The input
#' `signature_matrix` must be a numeric matrix containing log2 fold changes,
#' with one column and gene identifiers stored as row names.
#'
#' `ref_db` can be `"cmap"` or `"lincs2"`. A local HDF5 path returned by
#' `load_signature_refdb()` can also be supplied to use a frozen Synapse
#' reference database. Frozen databases can also be fetched directly by setting
#' `signature_refdb_mode = "frozen"` or `"frozen_force"` and providing
#' `auth_token`.
#'
#' @param object Optional `SignatureBased` object. If `NULL`, a new object is
#'   created from `signature_matrix`, `ref_db`, and `method`.
#' @param signature_matrix Numeric matrix containing log2 fold changes. The
#'   matrix must have one column, with gene identifiers stored as row names.
#' @param ref_db Reference database. Use `"cmap"` or `"lincs2"`, or provide a
#'   local HDF5 path returned by `load_signature_refdb()` to use a frozen Synapse
#'   reference database.
#' @param method Correlation method. One of `"spearman"`, `"pearson"`, or
#'   `"kendall"`. Default is `"spearman"`.
#' @param chunk_size Integer; number of reference signatures processed per
#'   chunk. Default is `5000`.
#' @inheritParams get_drug_signature
#'
#' @return
#' A `SignatureBased` object containing the correlation search results and query
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
#' res <- correlation_method(
#'   signature_matrix = signature_matrix,
#'   ref_db = "cmap"
#' )
#'
#' # Use Pearson correlation instead of the default Spearman correlation
#' res_pearson <- correlation_method(
#'   signature_matrix = signature_matrix,
#'   ref_db = "cmap",
#'   method = "pearson"
#' )
#'
#' # Automatically fetch and use a frozen Synapse reference database
#' res_frozen <- correlation_method(
#'   signature_matrix = signature_matrix,
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
#' res_local <- correlation_method(
#'   signature_matrix = signature_matrix,
#'   ref_db = cmap_ref
#' )
#' }
#'
#' @importFrom signatureSearch qSig gess_cor result
#' @importFrom magrittr %>%
#' @export
setGeneric(
  "correlation_method",
  function(object = NULL, signature_matrix, ref_db, method = "spearman",
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

      # Validate `method`
      method <- match.arg(method, c("pearson", "kendall", "spearman"))

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
        signature_method = "Correlation",
        method = method,
        refdb = ref_db,
        chunk_size = chunk_size
      )
    }
    standardGeneric("correlation_method")
  }
)


#' @describeIn correlation_method
#' Implements the Correlation method for Signature-Based Drug Searching.
#'
#' @export
setMethod(
  "correlation_method",
  signature = "SignatureBased",
  function(object) {
    # Extract parameters from the object
    params <- object@parameters
    query <- params$query
    ref_db <- params$refdb
    method <- params$method
    chunk_size <- params$chunk_size

    # Validate the query
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

    # Validate `method`
    method <- match.arg(method, c("pearson", "kendall", "spearman"))

    # Log the process
    cat("Running correlation_method for SignatureBased object...\n")
    cat("Reference database:", ref_db, "\n")
    cat("Query matrix dimensions:", dim(query), "\n")
    cat("Correlation method:", method, "\n")
    cat("Chunk size:", chunk_size, "\n")

    # Perform correlation-based analysis
    tryCatch(
      {
        method_res <- signatureSearch::qSig(query = query, gess_method = "Cor", refdb = ref_db) %>%
          signatureSearch::gess_cor(method = method, chunk_size = chunk_size) %>%
          signatureSearch::result()

        # Update the result slot
        object@result <- method_res
      },
      error = function(e) {
        stop("Correlation analysis failed with error: ", e$message)
      }
    )

    # Filter and update parameters for the object
    object@parameters <- filterSignatureParameters(object)
    return(object)
  }
)


#' Rank Correlation Signature Results
#'
#' Converts raw results from [correlation_method()] into the standardized
#' ranking format used by the signature pipeline.
#'
#' @param correlation_signature A data frame of raw correlation results.
#' @param n Number of observations used to calculate correlation significance.
#' @param ties_method Character string passed to [base::rank()], or `"dense"`.
#' @return A data frame of standardized correlation rank scores.
#' @export
correlation_rank_score <- function(correlation_signature, n, ties_method = "max") {
  if ("name" %in% names(correlation_signature)) {
    correlation_signature <- correlation_signature %>%
      dplyr::mutate(pert = ifelse(is.na(name), pert, name))
  }
  correlation_rank <- correlation_signature %>%
    dplyr::group_by(pert) %>%
    dplyr::slice_max(order_by = abs(cor_score), n = 1, with_ties = TRUE) %>%
    dplyr::mutate(cell = paste0(unique(cell), collapse = ";")) %>%
    unique() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      pval = 2 * stats::pt(-abs(abs(cor_score) * sqrt(n - 2) / sqrt(1 - abs(cor_score)^2)), df = n - 2),
      rank_score = ifelse(
        pval <= 0.05,
        1 / if (ties_method == "dense") dplyr::dense_rank(-abs(cor_score)) else base::rank(-abs(cor_score), ties.method = ties_method),
        0
      ),
      scaled_score = (abs(cor_score) - min(abs(cor_score))) / (max(abs(cor_score)) - min(abs(cor_score)))
    ) %>%
    .data_format(score_col = "cor_score")
  return(correlation_rank)
}
