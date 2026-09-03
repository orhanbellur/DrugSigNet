#' @title Extract Drug Synonyms
#'
#' @description
#' Retrieves alternative names and synonyms for one or more drugs from supported
#' DrugSigNet annotation sources.
#'
#' @details
#' `get_drug_synonyms()` extracts synonym annotations for the supplied drugs.
#' The function accepts a character vector or a one-column data frame and returns
#' a `DrugAnnotation` object with synonym information stored in `object@result`.
#'
#' Drug matching is performed by drug name and is case-insensitive through the
#' shared annotation-matching helpers. Depending on the selected source, returned
#' synonyms may include generic names, brand names, chemical names, research
#' names, or other alternative identifiers.
#'
#' Supported sources are `"All"`, `"OpenTargets"`, `"CHEMBL"`, and `"TTD"`.
#' When `source = "All"`, synonym annotations are combined across available
#' sources. Annotation resources are loaded from the local cache or Synapse using
#' `force` and `auth_token`.
#'
#' @inheritParams annotate_drugs
#' @param source Annotation source. One of `"All"`, `"OpenTargets"`,
#'   `"CHEMBL"`, or `"TTD"`. Default is `"All"`.
#'
#' @return
#' A `DrugAnnotation` object containing a data frame of drug synonym annotations
#' in `object@result`.
#'
#' @examples
#' \dontrun{
#' res <- get_drug_synonyms(
#'   drugs = c("Imatinib", "Gefitinib"),
#'   source = "CHEMBL",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#'
#' drug_df <- data.frame(
#'   drug = c("Cetirizine", "Pentazocine")
#' )
#'
#' res_opentargets <- get_drug_synonyms(
#'   drugs = drug_df,
#'   source = "OpenTargets",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#'
#' # Pipe-friendly use
#' res_pipe <- get_drug_synonyms(
#'   object = drug_df,
#'   source = "TTD",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#' }
#'
#' @importFrom dplyr select distinct left_join all_of all_of
#' @importFrom tibble tibble
#' @export
setGeneric(
  "get_drug_synonyms",
  function(object = NULL,
           drugs = NULL,
           source = c("All", "OpenTargets", "CHEMBL", "TTD"),
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

    if (is.character(drugs)) {
      input_vec <- drugs
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

    standardGeneric("get_drug_synonyms")
  }
)

#' @rdname get_drug_synonyms
setMethod(
  "get_drug_synonyms",
  signature = "DrugAnnotation",
  function(object) {

    params <- object@parameters
    source <- params$source
    force <- isTRUE(params$force)
    auth_token <- params$auth_token

    input_tbl <- tibble::tibble(Drug = params$input_data)

    synonym_tbl <- load_annotation_section(
      "synonyms",
      aliases = c("Integrated_synonyms"),
      force = force,
      auth_token = auth_token
    )

    if (is.null(synonym_tbl)) {
      synonym_tbl <- load_drugsignet_data_compat("Integrated_synonyms", envir = environment(), force = force, auth_token = auth_token)
    }

    synonym_tbl <- filter_annotation_source(synonym_tbl, source)

    drug_col <- resolve_annotation_col(synonym_tbl, c("drug_name", "name", "Drug", "drug"), "drug")
    synonym_col <- resolve_annotation_col(synonym_tbl, c("synonyms", "Synonyms", "synonym", "alias"), "synonyms")

    ref_tbl <- synonym_tbl %>%
      dplyr::select(
        Drug = dplyr::all_of(drug_col),
        synonyms = dplyr::all_of(synonym_col)
      ) %>%
      dplyr::distinct() %>%
      collapse_annotations_by_drug()

    object@result <- left_join_annotations_by_drug(
      input_tbl,
      ref_tbl,
      empty_cols = "synonyms"
    )

    object
  }
)
