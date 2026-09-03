#' @title Extract Drug Mechanisms of Action
#'
#' @description
#' Retrieves mechanism of action annotations for one or more drugs from
#' supported DrugSigNet annotation sources.
#'
#' @details
#' `get_drug_moa()` extracts mechanism of action annotations for the supplied
#' drugs. The function accepts a character vector or a one-column data frame and
#' returns a `DrugAnnotation` object with MoA information stored in
#' `object@result`.
#'
#' Drug matching is performed by drug name and is case-insensitive through the
#' shared annotation-matching helpers. Annotation resources are loaded from the
#' local cache or Synapse using `force` and `auth_token`.
#'
#' Supported sources are `"All"`, `"OpenTargets"`, and `"CHEMBL"`. When
#' `source = "All"`, mechanism of action annotations are combined across
#' available sources.
#'
#' Multiple mechanisms of action may be associated with a single drug across
#' sources; these are collapsed with `|` by the shared annotation helpers.
#'
#' @inheritParams annotate_drugs
#' @param source Annotation source. One of `"All"`, `"OpenTargets"`, or
#'   `"CHEMBL"`. Default is `"All"`.
#'
#' @return
#' A `DrugAnnotation` object containing a data frame of mechanism of action
#' annotations in `object@result`.
#'
#' @examples
#' \dontrun{
#' res <- get_drug_moa(
#'   drugs = c("Imatinib", "Gefitinib"),
#'   source = "CHEMBL",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#'
#' drug_df <- data.frame(
#'   drug = c("Cetirizine", "Pentazocine")
#' )
#'
#' res_df <- get_drug_moa(
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
  "get_drug_moa",
  function(object = NULL,
           drugs,
           source = c("All", "OpenTargets", "CHEMBL"),
           force = FALSE,
           auth_token = NULL) {

    source <- match.arg(source)

    if (missing(drugs)) {
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

    standardGeneric("get_drug_moa")
  }
)

#' @rdname get_drug_moa
setMethod(
  "get_drug_moa",
  signature = "DrugAnnotation",
  function(object) {

    params <- object@parameters
    source <- params$source
    force <- isTRUE(params$force)
    auth_token <- params$auth_token

    input_tbl <- tibble::tibble(Drug = params$input_data)

    moa_tbl <- load_annotation_section(
      "moa",
      aliases = c("moA", "Integrated_moA"),
      force = force,
      auth_token = auth_token
    )

    if (is.null(moa_tbl)) {
      moa_tbl <- load_drugsignet_data_compat("Integrated_moA", envir = environment(), force = force, auth_token = auth_token)
    }

    moa_tbl <- filter_annotation_source(moa_tbl, source)

    drug_col <- resolve_annotation_col(moa_tbl, c("drug_name", "name", "Drug", "drug"), "drug")
    moa_col <- resolve_annotation_col(
      moa_tbl,
      c("mechanism_of_action", "mechanismOfAction", "moa", "target_function"),
      "mechanism of action"
    )

    ref_tbl <- moa_tbl %>%
      dplyr::select(
        Drug = dplyr::all_of(drug_col),
        mechanismOfAction = dplyr::all_of(moa_col)
      ) %>%
      dplyr::distinct() %>%
      collapse_annotations_by_drug()

    object@result <- left_join_annotations_by_drug(
      input_tbl,
      ref_tbl,
      empty_cols = "mechanismOfAction"
    )

    object
  }
)
