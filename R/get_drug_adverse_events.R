#' @title Extract Drug Adverse Events and Toxicity Information
#'
#' @description
#' Retrieves adverse event, warning, and toxicity annotations for one or more
#' drugs from supported DrugSigNet annotation sources.
#'
#' @details
#' `get_drug_adverse_events()` extracts safety-related annotations for the
#' supplied drugs. The function accepts a character vector or a one-column data
#' frame and returns a `DrugAnnotation` object with adverse event and toxicity
#' information stored in `object@result`.
#'
#' Drug matching is performed by drug name and is case-insensitive through the
#' shared annotation-matching helpers. Annotation resources are loaded from the
#' local cache or Synapse using `force` and `auth_token`.
#'
#' Supported sources are `"All"`, `"OpenTargets"`, and `"CHEMBL"`. Returned
#' annotations may include regulatory warning categories and toxicity classes,
#' depending on the selected source.
#'
#' Not all drugs have available safety annotations; missing values are retained
#' to reflect source data completeness.
#'
#' @inheritParams annotate_drugs
#' @param source Annotation source. One of `"All"`, `"OpenTargets"`, or
#'   `"CHEMBL"`. Default is `"All"`.
#'
#' @return
#' A `DrugAnnotation` object containing a data frame of adverse event, warning,
#' and toxicity annotations in `object@result`.
#'
#' @examples
#' \dontrun{
#' res_opentargets <- get_drug_adverse_events(
#'   drugs = c("Cetirizine", "Pentazocine"),
#'   source = "OpenTargets",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#'
#' drug_df <- data.frame(
#'   drug = c("Phenylbutazone", "Diclofenac sodium")
#' )
#'
#' res_chembl <- get_drug_adverse_events(
#'   drugs = drug_df,
#'   source = "CHEMBL",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#'
#' res_all <- get_drug_adverse_events(
#'   drugs = drug_df,
#'   source = "All",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#' }
#'
#' @importFrom dplyr select distinct
#' @importFrom tibble tibble
#' @export
setGeneric(
  "get_drug_adverse_events",
  function(object = NULL,
           drugs = NULL,
           source = c("All", "OpenTargets", "CHEMBL"),
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

    standardGeneric("get_drug_adverse_events")
  }
)

#' @rdname get_drug_adverse_events
setMethod(
  "get_drug_adverse_events",
  signature = "DrugAnnotation",
  function(object) {

    params <- object@parameters
    source <- params$source
    force <- isTRUE(params$force)
    auth_token <- params$auth_token

    input_tbl <- tibble::tibble(Drug = params$input_data)

    adverse_tbl <- load_annotation_section(
      "adverse_events",
      aliases = c("adverse", "Integrated_adverse"),
      force = force,
      auth_token = auth_token
    )

    if (is.null(adverse_tbl)) {
      adverse_tbl <- load_drugsignet_data_compat("Integrated_adverse", envir = environment(), force = force, auth_token = auth_token)
    }

    adverse_tbl <- filter_annotation_source(adverse_tbl, source)

    drug_col <- resolve_annotation_col(adverse_tbl, c("drug_name", "name", "Drug", "drug"), "drug")
    warning_col <- resolve_annotation_col(
      adverse_tbl,
      c("warning_type", "warning_type.x", "warning_type.y", "warningType"),
      "warning_type",
      required = FALSE
    )
    tox_col <- resolve_annotation_col(
      adverse_tbl,
      c("toxicity_class", "toxicity_class.x", "toxicity_class.y", "toxicityClass", "warning_class"),
      "toxicity_class",
      required = FALSE
    )

    selected <- c(Drug = drug_col, warning_type = warning_col, toxicity_class = tox_col)
    selected <- selected[!is.na(selected)]

    ref_tbl <- adverse_tbl[, unname(selected), drop = FALSE]
    names(ref_tbl) <- names(selected)

    if (!"warning_type" %in% names(ref_tbl)) ref_tbl$warning_type <- NA_character_
    if (!"toxicity_class" %in% names(ref_tbl)) ref_tbl$toxicity_class <- NA_character_

    ref_tbl <- ref_tbl %>%
      dplyr::select(Drug, warning_type, toxicity_class) %>%
      dplyr::distinct() %>%
      collapse_annotations_by_drug()

    object@result <- left_join_annotations_by_drug(
      input_tbl,
      ref_tbl,
      empty_cols = c("warning_type", "toxicity_class")
    )

    object
  }
)
