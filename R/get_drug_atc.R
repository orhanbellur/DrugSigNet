#' @title Extract Drug ATC Classifications
#'
#' @description
#' Retrieves Anatomical Therapeutic Chemical (ATC) classification annotations
#' for one or more drugs from supported DrugSigNet annotation sources.
#'
#' @details
#' `get_drug_atc()` extracts ATC classification information for the supplied
#' drugs. The function accepts a character vector or a one-column data frame and
#' returns a `DrugAnnotation` object with ATC information stored in
#' `object@result`.
#'
#' Drug matching is performed by drug name and is case-insensitive through the
#' shared annotation-matching helpers. Annotation resources are loaded from the
#' local cache or Synapse using `force` and `auth_token`.
#'
#' Supported sources are `"All"`, `"WHO"`, and `"CHEMBL"`. Returned annotations
#' may include ATC codes and descriptions across hierarchy levels 1 to 5,
#' depending on the selected source.
#'
#' Multiple ATC classifications may be associated with a single drug across
#' sources; these are collapsed with `|` by the shared annotation helpers.
#'
#' @inheritParams annotate_drugs
#' @param source Annotation source. One of `"All"`, `"WHO"`, or `"CHEMBL"`.
#'   Default is `"All"`.
#'
#' @return
#' A `DrugAnnotation` object containing a data frame of ATC classification
#' annotations in `object@result`.
#'
#' @examples
#' \dontrun{
#' res_chembl <- get_drug_atc(
#'   drugs = c("Imatinib", "Gefitinib"),
#'   source = "CHEMBL",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#'
#' drug_df <- data.frame(
#'   drug = c("Cetirizine", "Pentazocine")
#' )
#'
#' res_who <- get_drug_atc(
#'   drugs = drug_df,
#'   source = "WHO",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#'
#' res_all <- get_drug_atc(
#'   drugs = c("Imatinib", "Gefitinib"),
#'   source = "All",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#' }
#'
#' @importFrom dplyr distinct select all_of
#' @importFrom tibble tibble
#' @export
setGeneric(
  "get_drug_atc",
  function(object = NULL,
           drugs,
           source = c("All", "WHO", "CHEMBL"),
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

    standardGeneric("get_drug_atc")
  }
)

#' @rdname get_drug_atc
setMethod(
  "get_drug_atc",
  signature = "DrugAnnotation",
  function(object) {

    params <- object@parameters
    source <- params$source
    force <- isTRUE(params$force)
    auth_token <- params$auth_token

    input_tbl <- tibble::tibble(Drug = params$input_data)

    atc_tbl <- load_annotation_section(
      "atc",
      aliases = c("ATC", "Integrated_ATC"),
      force = force,
      auth_token = auth_token
    )

    if (is.null(atc_tbl)) {
      atc_tbl <- load_drugsignet_data_compat("Integrated_ATC", envir = environment(), force = force, auth_token = auth_token)
    }

    atc_tbl <- filter_annotation_source(atc_tbl, source)

    drug_col <- resolve_annotation_col(atc_tbl, c("drug_name", "atc_level5_name", "name", "Drug", "drug"), "drug")

    level1_col <- resolve_annotation_col(atc_tbl, c("atc_level1", "level1"), "ATC level1", required = FALSE)
    level2_col <- resolve_annotation_col(atc_tbl, c("atc_level2", "level2"), "ATC level2", required = FALSE)
    level3_col <- resolve_annotation_col(atc_tbl, c("atc_level3", "level3"), "ATC level3", required = FALSE)
    level4_col <- resolve_annotation_col(atc_tbl, c("atc_level4", "level4"), "ATC level4", required = FALSE)
    level5_col <- resolve_annotation_col(atc_tbl, c("atc_level5", "level5"), "ATC level5", required = FALSE)
    level1_desc_col <- resolve_annotation_col(atc_tbl, c("atc_level1_description", "atc_level1_name", "level1_description"), "ATC level1 description", required = FALSE)
    level2_desc_col <- resolve_annotation_col(atc_tbl, c("atc_level2_description", "atc_level2_name", "level2_description"), "ATC level2 description", required = FALSE)
    level3_desc_col <- resolve_annotation_col(atc_tbl, c("atc_level3_description", "atc_level3_name", "level3_description"), "ATC level3 description", required = FALSE)
    level4_desc_col <- resolve_annotation_col(atc_tbl, c("atc_level4_description", "atc_level4_name", "level4_description"), "ATC level4 description", required = FALSE)

    selected <- c(
      Drug = drug_col,
      level1 = level1_col,
      level2 = level2_col,
      level3 = level3_col,
      level4 = level4_col,
      level5 = level5_col,
      level1_description = level1_desc_col,
      level2_description = level2_desc_col,
      level3_description = level3_desc_col,
      level4_description = level4_desc_col
    )
    selected <- selected[!is.na(selected)]

    ref_tbl <- atc_tbl[, unname(selected), drop = FALSE]
    names(ref_tbl) <- names(selected)
    ref_tbl <- ref_tbl %>% dplyr::distinct()

    atc_cols <- c(
      "level1", "level2", "level3", "level4", "level5",
      "level1_description", "level2_description",
      "level3_description", "level4_description"
    )
    for (col in atc_cols) {
      if (!col %in% names(ref_tbl)) ref_tbl[[col]] <- NA_character_
    }

    ref_tbl <- ref_tbl %>%
      dplyr::select(Drug, dplyr::all_of(atc_cols)) %>%
      collapse_annotations_by_drug()

    object@result <- left_join_annotations_by_drug(
      input_tbl,
      ref_tbl,
      empty_cols = atc_cols
    )

    object
  }
)
