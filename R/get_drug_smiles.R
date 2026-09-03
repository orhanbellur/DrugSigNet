#' @title Extract Drug SMILES Strings
#'
#' @description
#' Retrieves canonical SMILES annotations for one or more drugs from supported
#' DrugSigNet annotation sources.
#'
#' @details
#' `get_drug_smiles()` extracts SMILES annotations for the supplied drugs. The
#' function accepts a character vector or a one-column data frame and returns a
#' `DrugAnnotation` object with SMILES information stored in `object@result`.
#'
#' Drug matching is performed by drug name and is case-insensitive through the
#' shared annotation-matching helpers. Annotation resources are loaded from the
#' local cache or Synapse using `force` and `auth_token`.
#'
#' Supported sources are `"All"`, `"OpenTargets"`, `"CHEMBL"`, `"TTD"`, and
#' `"B3DB_classification"`. When `source = "All"`, SMILES annotations are
#' combined across available sources.
#'
#' Multiple SMILES values may be associated with a single drug across sources;
#' these are collapsed with `|` by the shared annotation helpers.
#'
#' @inheritParams annotate_drugs
#' @param source Annotation source. One of `"All"`, `"OpenTargets"`,
#'   `"CHEMBL"`, `"TTD"`, or `"B3DB_classification"`. Default is `"All"`.
#'
#' @return
#' A `DrugAnnotation` object containing a data frame of drug SMILES annotations
#' in `object@result`.
#'
#' @examples
#' \dontrun{
#' res <- get_drug_smiles(
#'   drugs = c("Imatinib", "Gefitinib"),
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#'
#' res_chembl <- get_drug_smiles(
#'   drugs = c("Imatinib", "Gefitinib"),
#'   source = "CHEMBL",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#'
#' drug_df <- data.frame(
#'   drug = c("Cetirizine", "Pentazocine")
#' )
#'
#' res_df <- get_drug_smiles(
#'   drugs = drug_df,
#'   source = "All",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#' }
#'
#' @importFrom dplyr select distinct left_join all_of all_of
#' @importFrom tibble tibble
#' @export
setGeneric(
  "get_drug_smiles",
  function(object = NULL,
           drugs = NULL,
           source = c("All", "OpenTargets", "CHEMBL", "TTD", "B3DB_classification"),
           force = FALSE,
           auth_token = NULL) {

    source <- match.arg(source)

    if (is.null(drugs)) {
      if (is.null(object)) {
        stop("`drugs` is missing. Supply a character vector or one-column data frame.")
      }
      if (methods::is(object, "DrugAnnotation")) {
        stop("`drugs` is missing. When supplying a DrugAnnotation object, also provide `drugs`.")
      }
      drugs <- object
      object <- NULL
    }

    if (is.character(drugs) || is.factor(drugs)) {
      input_vec <- as.character(drugs)
    } else if (is.data.frame(drugs)) {
      if (ncol(drugs) != 1) {
        stop("`drugs` data frame must contain exactly one column.")
      }
      input_vec <- as.character(drugs[[1]])
    } else {
      stop("`drugs` must be a character vector or a one-column data frame.")
    }

    input_vec <- unique(trimws(stats::na.omit(input_vec)))
    input_vec <- input_vec[nzchar(input_vec)]
    if (length(input_vec) == 0) {
      stop("No valid drug names supplied.")
    }

    if (is.null(object)) {
      object <- new(
        "DrugAnnotation",
        result = data.frame(),
        parameters = list(
          input_data = input_vec,
          source = source,
          force = force,
          auth_token = auth_token
        )
      )
    }

    standardGeneric("get_drug_smiles")
  }
)

#' @rdname get_drug_smiles
setMethod(
  "get_drug_smiles",
  signature = "DrugAnnotation",
  function(object) {

    params <- object@parameters
    source <- params$source
    force <- isTRUE(params$force)
    auth_token <- params$auth_token

    input_tbl <- tibble::tibble(Drug = params$input_data)

    smiles_tbl <- load_annotation_section(
      "smiles",
      aliases = c("Integrated_smiles"),
      force = force,
      auth_token = auth_token
    )

    if (is.null(smiles_tbl)) {
      smiles_tbl <- load_drugsignet_data_compat("Integrated_smiles", envir = environment(), force = force, auth_token = auth_token)
    }

    smiles_tbl <- filter_annotation_source(smiles_tbl, source)

    drug_col <- resolve_annotation_col(smiles_tbl, c("drug_name", "name", "Drug", "drug"), "drug")
    smiles_col <- resolve_annotation_col(
      smiles_tbl,
      c("canonical_smiles", "smiles", "SMILES", "canonicalSmiles"),
      "canonical_smiles"
    )

    ref_tbl <- smiles_tbl %>%
      dplyr::select(
        Drug = dplyr::all_of(drug_col),
        canonical_smiles = dplyr::all_of(smiles_col)
      ) %>%
      dplyr::distinct() %>%
      collapse_annotations_by_drug()

    object@result <- left_join_annotations_by_drug(
      input_tbl,
      ref_tbl,
      empty_cols = "canonical_smiles"
    )

    object
  }
)
