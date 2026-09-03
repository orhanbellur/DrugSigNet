#' @title Calculate Drug-Drug Structural Similarity
#'
#' @description
#' Computes pairwise structural similarity between drugs using canonical SMILES
#' strings and molecular fingerprints.
#'
#' @details
#' `get_drug_drug_similarity()` first resolves drug names to canonical SMILES
#' strings using `get_drug_smiles()`. It then parses the SMILES strings with
#' `rcdk`, generates molecular fingerprints, and calculates pairwise structural
#' similarity.
#'
#' The currently supported similarity metric is Tanimoto similarity. Values
#' range from `0` to `1`, where `0` indicates no shared fingerprint features and
#' `1` indicates identical fingerprints.
#'
#' If a drug has multiple pipe-delimited SMILES annotations, the first non-empty
#' SMILES string is used so that the output keeps one row and one column per
#' input drug. Drugs without valid or parsable SMILES are excluded from the
#' similarity calculation.
#'
#' Annotation resources are loaded from the local cache or Synapse using `force`
#' and `auth_token`.
#'
#' @inheritParams annotate_drugs
#' @param source Annotation source passed to `get_drug_smiles()`. One of
#'   `"All"`, `"OpenTargets"`, `"CHEMBL"`, `"TTD"`, or
#'   `"B3DB_classification"`. Default is `"All"`.
#' @param fingerprint Molecular fingerprint type passed to `rcdk`.
#'   Common options include `"standard"`, `"extended"`, and `"pubchem"`.
#'   Default is `"standard"`.
#' @param method Similarity metric. Currently only `"tanimoto"` is supported.
#'
#' @return
#' A `DrugAnnotation` object. The `result` slot contains the pairwise structural
#' similarity matrix as a data frame. The input drug-to-SMILES lookup table is
#' stored in `object@parameters$drug_smiles_table`, and the numeric matrix is
#' also stored as `attr(object@result, "similarity_matrix")`.
#'
#' @examples
#' \dontrun{
#' res <- get_drug_drug_similarity(
#'   drugs = c("Imatinib", "Gefitinib", "Erlotinib"),
#'   source = "CHEMBL",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#'
#' drug_df <- data.frame(
#'   Drug = c("Midostaurin", "Pemigatinib")
#' )
#'
#' res_ttd <- get_drug_drug_similarity(
#'   drugs = drug_df,
#'   source = "TTD",
#'   fingerprint = "standard",
#'   method = "tanimoto",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#'
#' # Pipe-friendly use
#' res_pipe <- get_drug_drug_similarity(
#'   object = drug_df,
#'   source = "OpenTargets",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#' }
#'
#' @importFrom rcdk parse.smiles get.fingerprint
#' @importFrom dplyr select distinct mutate filter
#' @importFrom tibble tibble
#' @export
setGeneric(
  "get_drug_drug_similarity",
  function(object = NULL,
           drugs = NULL,
           source = c("All", "OpenTargets", "CHEMBL", "TTD", "B3DB_classification"),
           fingerprint = "standard",
           method = "tanimoto",
           force = FALSE,
           auth_token = NULL) {

    source <- match.arg(source)

    if (is.null(drugs) && !is.null(object) && !methods::is(object, "DrugAnnotation")) {
      drugs <- object
      object <- NULL
    }

    if (is.null(drugs) && !is.null(object) && methods::is(object, "DrugAnnotation")) {
      input_vec <- object@parameters$input_data
    } else if (is.character(drugs) || is.factor(drugs)) {
      input_vec <- as.character(drugs)
    } else if (is.data.frame(drugs)) {
      if ("Drug" %in% names(drugs)) {
        input_vec <- trimws(as.character(drugs$Drug))
      } else if (ncol(drugs) == 1) {
        input_vec <- trimws(as.character(drugs[[1]]))
      } else {
        stop("`drugs` data frame must contain a 'Drug' column or have exactly one column.")
      }
    } else {
      stop("`drugs` must be a character vector, factor, or data frame containing drug names.")
    }

    input_vec <- trimws(unique(stats::na.omit(input_vec)))
    input_vec <- input_vec[nzchar(input_vec)]
    if (length(input_vec) == 0) {
      stop("No valid drug names supplied.")
    }

    if (!is.character(fingerprint) || length(fingerprint) != 1 || is.na(fingerprint)) {
      stop("`fingerprint` must be a single character string.")
    }

    if (!is.character(method) || length(method) != 1 || is.na(method)) {
      stop("`method` must be a single character string.")
    }
    if (tolower(method) != "tanimoto") {
      stop("Currently only method = 'tanimoto' is supported.")
    }

    if (is.null(object)) {
      object <- new(
        "DrugAnnotation",
        result = data.frame(),
        parameters = list()
      )
    }

    object@parameters$input_data <- input_vec
    object@parameters$source <- source
    object@parameters$fingerprint <- fingerprint
    object@parameters$method <- method
    object@parameters$force <- force
    object@parameters$auth_token <- auth_token

    standardGeneric("get_drug_drug_similarity")
  }
)

# ==========================================================
# Method: DrugAnnotation
# ==========================================================
#' @rdname get_drug_drug_similarity
setMethod(
  "get_drug_drug_similarity",
  signature = "DrugAnnotation",
  function(object) {

    params <- object@parameters
    source <- params$source
    fingerprint <- params$fingerprint
    method <- params$method
    force <- isTRUE(params$force)
    auth_token <- params$auth_token

    input_tbl <- tibble::tibble(
      Drug = params$input_data
    )

    result_tbl <- get_drug_smiles(
      drugs = input_tbl,
      source = source,
      force = force,
      auth_token = auth_token
    )@result %>%
      dplyr::select(Drug, canonical_smiles) %>%
      dplyr::mutate(
        canonical_smiles = .drug_similarity_first_smiles(canonical_smiles)
      ) %>%
      dplyr::distinct()

    # -------------------------------------
    # Structural similarity calculation
    # -------------------------------------
    valid_tbl <- result_tbl %>%
      dplyr::filter(!is.na(canonical_smiles) & nzchar(canonical_smiles))

    if (nrow(valid_tbl) < 2) {
      empty_mat <- matrix(numeric(0), nrow = 0, ncol = 0)

      if (nrow(valid_tbl) == 1) {
        empty_mat <- matrix(1, nrow = 1, ncol = 1)
        dim_names <- make.unique(valid_tbl$Drug)
        rownames(empty_mat) <- dim_names
        colnames(empty_mat) <- dim_names
      }

      warning(
        "Fewer than two drugs with valid canonical SMILES; returning available similarity matrix.",
        call. = FALSE
      )

      object@result <- as.data.frame(empty_mat, stringsAsFactors = FALSE)
      attr(object@result, "similarity_matrix") <- empty_mat
      object@parameters$drug_smiles_table <- result_tbl
      object@parameters$fingerprint <- fingerprint
      object@parameters$method <- method

      return(object)
    }

    # Parse SMILES -> molecules
    mols <- rcdk::parse.smiles(valid_tbl$canonical_smiles)
    ok <- !vapply(mols, is.null, logical(1))

    if (sum(ok) < 2) {
      warning(
        "Fewer than two drugs have parsable canonical SMILES; returning available similarity matrix.",
        call. = FALSE
      )

      parsed_tbl <- valid_tbl[ok, , drop = FALSE]
      fallback_mat <- matrix(numeric(0), nrow = 0, ncol = 0)

      if (nrow(parsed_tbl) == 1) {
        fallback_mat <- matrix(1, nrow = 1, ncol = 1)
        dim_names <- make.unique(parsed_tbl$Drug)
        rownames(fallback_mat) <- dim_names
        colnames(fallback_mat) <- dim_names
      }

      object@result <- as.data.frame(fallback_mat, stringsAsFactors = FALSE)
      attr(object@result, "similarity_matrix") <- fallback_mat
      object@parameters$drug_smiles_table <- result_tbl
      object@parameters$fingerprint <- fingerprint
      object@parameters$method <- method

      return(object)
    }

    valid_tbl <- valid_tbl[ok, , drop = FALSE]
    mols <- mols[ok]

    # Fingerprints
    fps <- lapply(
      mols,
      rcdk::get.fingerprint,
      type = fingerprint
    )

    # Tanimoto for rcdk fingerprints (bit sets)
    tanimoto <- function(fp1, fp2) {
      b1 <- fp1@bits
      b2 <- fp2@bits
      inter <- length(intersect(b1, b2))
      uni <- length(unique(c(b1, b2)))
      if (uni == 0) return(0)
      inter / uni
    }

    n <- length(fps)
    sim_matrix <- matrix(0, nrow = n, ncol = n)

    for (i in seq_len(n)) {
      sim_matrix[i, i] <- 1
      if (i < n) {
        for (j in (i + 1):n) {
          sim <- tanimoto(fps[[i]], fps[[j]])
          sim_matrix[i, j] <- sim
          sim_matrix[j, i] <- sim
        }
      }
    }

    dim_names <- make.unique(valid_tbl$Drug)
    rownames(sim_matrix) <- dim_names
    colnames(sim_matrix) <- dim_names

    # -------------------------------------
    # Store results:
    #   - result slot: similarity matrix
    #   - parameters: lookup table + settings
    # -------------------------------------
    object@result <- as.data.frame(sim_matrix, stringsAsFactors = FALSE)
    attr(object@result, "similarity_matrix") <- sim_matrix
    object@parameters$drug_smiles_table <- result_tbl
    object@parameters$fingerprint <- fingerprint
    object@parameters$method <- method

    object
  }
)


.drug_similarity_first_smiles <- function(x) {
  vapply(
    as.character(x),
    function(value) {
      if (is.na(value) || !nzchar(trimws(value))) {
        return(NA_character_)
      }
      candidates <- trimws(strsplit(value, "\\|", fixed = FALSE)[[1]])
      candidates <- candidates[nzchar(candidates)]
      if (length(candidates)) candidates[1] else NA_character_
    },
    character(1),
    USE.NAMES = FALSE
  )
}
