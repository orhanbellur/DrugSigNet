#' @title Extract Drug Indications
#'
#' @description
#' Retrieves therapeutic indication annotations for one or more drugs from
#' supported DrugSigNet annotation sources.
#'
#' @details
#' `get_drug_indications()` extracts disease or therapeutic indication
#' annotations for the supplied drugs. The function accepts a character vector
#' or a one-column data frame and returns a `DrugAnnotation` object with
#' indication information stored in `object@result`.
#'
#' Drug matching is performed by drug name and is case-insensitive through the
#' shared annotation-matching helpers. Annotation resources are loaded from the
#' local cache or Synapse using `force` and `auth_token`.
#'
#' Supported sources are `"All"`, `"OpenTargets"`, `"CHEMBL"`, and `"TTD"`.
#' When `source = "All"`, indication annotations are combined across available
#' sources.
#'
#' Multiple indications may be associated with a single drug across sources;
#' these are collapsed with `|` by the shared annotation helpers.
#'
#' @inheritParams annotate_drugs
#' @param source Annotation source. One of `"All"`, `"OpenTargets"`,
#'   `"CHEMBL"`, or `"TTD"`. Default is `"All"`.
#'
#' @return
#' A `DrugAnnotation` object containing a data frame of drug indication
#' annotations in `object@result`.
#'
#' @examples
#' \dontrun{
#' res <- get_drug_indications(
#'   drugs = c("Imatinib", "Gefitinib"),
#'   source = "CHEMBL",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#'
#' drug_df <- data.frame(
#'   drug = c("Cetirizine", "Pentazocine")
#' )
#'
#' res_df <- get_drug_indications(
#'   drugs = drug_df,
#'   source = "All",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#'
#' res_ttd <- get_drug_indications(
#'   drugs = c("Midostaurin", "Pemigatinib"),
#'   source = "TTD",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#' }
#'
#' @importFrom dplyr select distinct mutate left_join rename all_of
#' @importFrom tibble tibble
#' @export
setGeneric(
  "get_drug_indications",
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

    standardGeneric("get_drug_indications")
  }
)

#' @rdname get_drug_indications
setMethod(
  "get_drug_indications",
  signature = "DrugAnnotation",
  function(object) {

    params <- object@parameters
    source <- params$source
    force <- isTRUE(params$force)
    auth_token <- params$auth_token

    input_tbl <- tibble::tibble(Drug = params$input_data)

    indication_tbl <- load_annotation_section(
      "indications",
      aliases = c("Integrated_indications"),
      force = force,
      auth_token = auth_token
    )

    if (is.null(indication_tbl)) {
      indication_tbl <- load_drugsignet_data_compat("Integrated_indications", envir = environment(), force = force, auth_token = auth_token)
    }

    indication_tbl <- filter_annotation_source(indication_tbl, source)

    drug_col <- resolve_annotation_col(indication_tbl, c("drug_name", "name", "Drug", "drug"), "drug")
    indication_col <- resolve_annotation_col(
      indication_tbl,
      c("indications", "indication", "drug_indication", "efoName"),
      "indications"
    )

    ref_tbl <- indication_tbl %>%
      dplyr::select(
        Drug = dplyr::all_of(drug_col),
        indication = dplyr::all_of(indication_col)
      ) %>%
      dplyr::distinct() %>%
      collapse_annotations_by_drug()

    object@result <- left_join_annotations_by_drug(
      input_tbl,
      ref_tbl,
      empty_cols = "indication"
    )

    object
  }
)
