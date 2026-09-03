#' @title Retrieve Drug Regulatory or Development Status
#'
#' @description
#' Retrieves regulatory approval or development status information for one or
#' more drugs from supported DrugSigNet annotation sources.
#'
#' @details
#' `get_drug_status()` extracts drug status annotations for the supplied drugs.
#' The function accepts a character vector or a one-column data frame and returns
#' a `DrugAnnotation` object with status information stored in `object@result`.
#'
#' Drug matching is performed by drug name and is case-insensitive through the
#' shared annotation-matching helpers. Annotation resources are loaded from the
#' local cache or Synapse using `force` and `auth_token`.
#'
#' Supported sources are `"All"`, `"OpenTargets"`, `"CHEMBL"`, and `"TTD"`.
#' Depending on the selected source, `Highest_status` may represent the highest
#' development phase, approval status, or regulatory status available for each
#' drug.
#'
#' @inheritParams annotate_drugs
#' @param source Annotation source. One of `"All"`, `"OpenTargets"`,
#'   `"CHEMBL"`, or `"TTD"`. Default is `"All"`.
#'
#' @return
#' A `DrugAnnotation` object containing a data frame of drug status annotations
#' in `object@result`.
#'
#' @examples
#' \dontrun{
#' res <- get_drug_status(
#'   drugs = c("Imatinib", "Gefitinib"),
#'   source = "OpenTargets",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#'
#' drug_df <- data.frame(
#'   drug = c("Cetirizine", "Pentazocine")
#' )
#'
#' res_df <- get_drug_status(
#'   drugs = drug_df,
#'   source = "All",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#'
#' res_ttd <- get_drug_status(
#'   drugs = c("Midostaurin", "Pemigatinib"),
#'   source = "TTD",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#' }
#'
#' @importFrom dplyr select distinct left_join all_of all_of
#' @importFrom tibble tibble
#' @export
setGeneric(
  "get_drug_status",
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

    # -------------------------------
    # Validate & normalize drug input
    # -------------------------------
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

    # -------------------------------
    # Create object if missing
    # -------------------------------
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

    standardGeneric("get_drug_status")
  }
)

# --------------------------------------------------
# Method: DrugAnnotation
# --------------------------------------------------
#' @rdname get_drug_status
setMethod(
  "get_drug_status",
  signature = "DrugAnnotation",
  function(object) {

    params <- object@parameters
    source <- params$source
    force <- isTRUE(params$force)
    auth_token <- params$auth_token

    input_tbl <- tibble::tibble(Drug = params$input_data)

    status_tbl <- load_annotation_section(
      "status",
      aliases = c("Highest_status", "Integrated_status"),
      force = force,
      auth_token = auth_token
    )

    if (is.null(status_tbl)) {
      status_tbl <- load_drugsignet_data_compat("Integrated_status", envir = environment(), force = force, auth_token = auth_token)
    }

    status_tbl <- filter_annotation_source(status_tbl, source)

    drug_col <- resolve_annotation_col(status_tbl, c("drug_name", "name", "Drug", "drug"), "drug")
    status_col <- resolve_annotation_col(
      status_tbl,
      c("Highest_status", "highest_status", "max_phase", "status"),
      "Highest_status"
    )

    ref_tbl <- status_tbl %>%
      dplyr::select(
        Drug = dplyr::all_of(drug_col),
        Highest_status = dplyr::all_of(status_col)
      ) %>%
      dplyr::distinct() %>%
      collapse_annotations_by_drug()

    object@result <- left_join_annotations_by_drug(
      input_tbl,
      ref_tbl,
      empty_cols = "Highest_status"
    )

    object
  }
)
