#' @title Retrieve Clinical Trials for Drugs
#'
#' @description
#' Retrieves clinical trial records involving one or more specified drugs, with
#' optional filtering by disease or condition.
#'
#' @details
#' `get_drug_trials()` searches the DrugSigNet clinical trial annotation table
#' for records whose intervention field contains the supplied drug names.
#' Clinical trial data are loaded from the local cache or Synapse using `force`
#' and `auth_token`.
#'
#' Drug matching is performed with fixed-string matching against the
#' `Interventions` field. When `ignore_case = TRUE`, both input drug names and
#' intervention text are converted to lowercase before matching. Leading
#' `"Drug:"` prefixes in intervention strings are removed during normalization.
#'
#' If `condition` is provided, records are first filtered by the `Conditions`
#' field before drug matching. A `matched_drug` column is added to indicate which
#' input drug or drugs matched each clinical trial record. Multiple matched drugs
#' are separated with semicolons.
#'
#' @inheritParams annotate_drugs
#' @param ignore_case Logical; whether drug and condition matching should be
#'   case-insensitive. Default is `TRUE`.
#'
#' @return
#' A `DrugAnnotation` object containing matching clinical trial records in
#' `object@result`.
#'
#' @examples
#' \dontrun{
#' res <- get_drug_trials(
#'   drugs = c("Imatinib", "Gefitinib"),
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#'
#' drug_df <- data.frame(
#'   drug = c("Cetirizine", "Pentazocine")
#' )
#'
#' res_pain <- get_drug_trials(
#'   drugs = drug_df,
#'   condition = "pain",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#'
#' # Pipe-friendly use
#' res_pipe <- get_drug_trials(
#'   object = drug_df,
#'   condition = "cancer",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#' }
#'
#' @seealso
#' `annotate_drugs()`
#'
#' @importFrom dplyr filter mutate select distinct
#' @export
setGeneric(
  "get_drug_trials",
  function(object = NULL,
           drugs = NULL,
           condition = NULL,
           ignore_case = TRUE,
           force = FALSE,
           auth_token = NULL) {

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

    input_vec <- trimws(unique(stats::na.omit(input_vec)))
    input_vec <- input_vec[nzchar(input_vec)]
    if (length(input_vec) == 0) {
      stop("No valid drug names supplied.")
    }

    # -------------------------------
    # Validate condition (if provided)
    # -------------------------------
    if (!is.null(condition)) {
      if (!is.character(condition) || length(condition) != 1) {
        stop("`condition` must be a single character string.")
      }
      if (is.na(condition) || !nzchar(trimws(condition))) {
        stop("`condition` must be a non-empty character string.")
      }
      condition <- trimws(condition)
    }

    # -------------------------------
    # Validate ignore_case
    # -------------------------------
    if (!is.logical(ignore_case) || length(ignore_case) != 1 || is.na(ignore_case)) {
      stop("`ignore_case` must be a single TRUE/FALSE value.")
    }

    # -------------------------------
    # Create object if missing
    # -------------------------------
    if (is.null(object)) {
      object <- new(
        "DrugAnnotation",
        result = data.frame(),
        parameters = list(
          input_data  = input_vec,
          condition   = condition,
          ignore_case = ignore_case,
          force = force,
          auth_token = auth_token
        )
      )
    }

    standardGeneric("get_drug_trials")
  }
)

# --------------------------------------------------
# Method: DrugAnnotation
# --------------------------------------------------
#' @rdname get_drug_trials
setMethod(
  "get_drug_trials",
  signature = "DrugAnnotation",
  function(object) {

    params <- object@parameters
    drug_vec    <- params$input_data
    condition   <- params$condition
    ignore_case <- params$ignore_case
    force       <- isTRUE(params$force)
    auth_token  <- params$auth_token

    # -------------------------------
    # Load ClinicalTrials.gov dataset
    #   (cache-first via Synapse; fallback to bundled data)
    # -------------------------------
    ref_tbl <- load_annotation_section(
      "clinical_trials",
      aliases = c("ClinicalTrialsGov", "ctg_studies"),
      force = force,
      auth_token = auth_token
    )

    if (is.null(ref_tbl)) {
      ref_tbl <- load_drugsignet_data_compat("ctg_studies", envir = environment(), force = force, auth_token = auth_token)
    }

    conditions_col <- resolve_annotation_col(ref_tbl, c("Conditions", "conditions", "condition"), "Conditions")
    interventions_col <- resolve_annotation_col(ref_tbl, c("Interventions", "interventions", "intervention"), "Interventions")

    # -------------------------------
    # Optional condition filter
    # -------------------------------
    if (!is.null(condition)) {
      ref_tbl <- ref_tbl %>%
        dplyr::filter(
          grepl(
            pattern = condition,
            x = .data[[conditions_col]],
            ignore.case = ignore_case
          )
        )
    }

    # -------------------------------
    # Prepare normalized interventions and drugs
    # -------------------------------
    if (ignore_case) {
      drug_vec_norm <- tolower(trimws(as.character(drug_vec)))
      ref_tbl <- ref_tbl %>%
        dplyr::mutate(
          .intervention_norm = .normalize_interventions(.data[[interventions_col]])
        )
    } else {
      drug_vec_norm <- trimws(as.character(drug_vec))
      ref_tbl <- ref_tbl %>%
        dplyr::mutate(
          .intervention_norm = gsub("^\\s*drug:\\s*", "", .data[[interventions_col]])
        )
    }

    # -------------------------------
    # Filter by drug (fixed-string search)
    # -------------------------------
    keep <- vapply(
      ref_tbl$.intervention_norm,
      .contains_any_drug,
      logical(1),
      drugs_norm = drug_vec_norm
    )

    ref_tbl <- ref_tbl[keep, , drop = FALSE]

    # -------------------------------
    # Annotate matched drug(s)
    # -------------------------------
    ref_tbl <- ref_tbl %>%
      dplyr::mutate(
        matched_drug = vapply(
          .intervention_norm,
          .extract_matched_drugs,
          character(1),
          drugs_raw = drug_vec,
          drugs_norm = drug_vec_norm
        )
      ) %>%
      dplyr::select(-.intervention_norm) %>%
      dplyr::distinct()

    # -------------------------------
    # Store result
    # -------------------------------
    object@result <- ref_tbl
    object
  }
)


# --------------------------------------------------
# Helper: normalize intervention text
# --------------------------------------------------
.normalize_interventions <- function(x) {
  x <- tolower(x)
  x <- gsub("^\\s*drug:\\s*", "", x)
  x
}

# --------------------------------------------------
# Helper: fixed-string drug matching without giant regex
# --------------------------------------------------
.contains_any_drug <- function(text, drugs_norm) {
  if (is.na(text) || !nzchar(text) || length(drugs_norm) == 0) {
    return(FALSE)
  }
  any(vapply(drugs_norm, function(pattern) grepl(pattern, text, fixed = TRUE), logical(1)))
}

.extract_matched_drugs <- function(text, drugs_raw, drugs_norm) {
  if (is.na(text) || !nzchar(text) || length(drugs_norm) == 0) {
    return(NA_character_)
  }

  hits <- drugs_raw[vapply(drugs_norm, function(pattern) grepl(pattern, text, fixed = TRUE), logical(1))]
  hits <- unique(stats::na.omit(hits))
  if (length(hits) == 0) {
    return(NA_character_)
  }
  paste(hits, collapse = ";")
}
