#' Retrieve Drug Annotations
#'
#' @description
#' Retrieves and combines drug annotations from one or more supported
#' data sources, including drug synonyms, mechanisms of action, SMILES,
#' blood-brain barrier permeability, ATC classification, indications,
#' approval status, adverse events, and optionally clinical trial
#' information.
#'
#' @param object Optional input for pipe-friendly workflows. Used only when
#'   `drugs` is not supplied. Accepts a character vector, a one-column data
#'   frame, or a data frame containing a `Drug` column.
#' @param drugs Drugs to annotate. Accepts a character vector, a one-column
#'   data frame, or a data frame containing a `Drug` column.
#' @param source Annotation source. One of `"All"`, `"CHEMBL"`,
#'   `"OpenTargets"`, or `"TTD"`.
#' @param condition Optional disease or indication used to retrieve matching
#'   clinical trial information.
#' @param force Logical; force refresh of cached annotation resources when
#'   downloading data from Synapse.
#' @param auth_token Optional Synapse authentication token. If `NULL`,
#'   the `SYNAPSE_AUTH_TOKEN` environment variable is used.
#'
#' @details
#' Annotation layers unavailable for the selected source are skipped
#' automatically. Available layers include drug synonyms, approval status,
#' indications, mechanism of action, blood-brain barrier permeability, ATC
#' classification, adverse events, SMILES structures, and clinical trials when
#' `condition` is provided.
#'
#' @return
#' A data frame containing one row per drug with all available annotation
#' fields merged across the selected data source(s).
#'
#' @examples
#' \dontrun{
#' annotate_drugs(
#'   drugs = c("metformin", "donepezil", "nifedipine"),
#'   source = "All"
#' )
#'
#' drug_df <- data.frame(
#'   Drug = c("metformin", "donepezil", "nifedipine")
#' )
#'
#' annotate_drugs(
#'   object = drug_df,
#'   source = "CHEMBL"
#' )
#'
#' annotate_drugs(
#'   drugs = c("metformin", "donepezil"),
#'   source = "All",
#'   condition = "Alzheimer disease"
#' )
#' }
#'
#' @export
annotate_drugs <- function(object = NULL,
                           drugs = NULL,
                           source = c("All", "CHEMBL", "OpenTargets", "TTD"),
                           condition = NULL,
                           force = FALSE,
                           auth_token = NULL) {

  source <- match.arg(source)

  # ------------------------------------------------------------
  # Clean input
  # ------------------------------------------------------------
  if (is.null(drugs) && !is.null(object)) {
    drugs <- object
  }

  if (is.null(drugs)) {
    stop("`drugs` is missing. Supply a character vector or data frame.")
  }

  if (is.character(drugs) || is.factor(drugs)) {
    drugs <- as.character(drugs)
  } else if (is.data.frame(drugs)) {
    if ("Drug" %in% names(drugs)) {
      drugs <- as.character(drugs$Drug)
    } else if (ncol(drugs) == 1) {
      drugs <- as.character(drugs[[1]])
    } else {
      stop("`drugs` data frame must contain a 'Drug' column or have exactly one column.")
    }
  } else {
    stop("`drugs` must be a character vector, factor, or data frame.")
  }

  drugs <- unique(trimws(as.character(drugs)))
  drugs <- drugs[!is.na(drugs) & nzchar(drugs)]

  if (length(drugs) == 0) {
    return(data.frame(Drug = character(), stringsAsFactors = FALSE))
  }

  # ------------------------------------------------------------
  # Safe extractor
  # ------------------------------------------------------------
  .safe_extract <- function(expr) {
    tryCatch(
      expr@result,
      error = function(e) {
        msg <- conditionMessage(e)
        if (!grepl("^Dataset `.*` could not be loaded\\.$", msg)) {
          warning(msg, call. = FALSE)
        }
        data.frame(Drug = character())
      }
    )
  }

  resolve_layer_source <- function(requested_source, supported_sources, fallback_source = NULL) {
    if (requested_source %in% supported_sources) {
      return(requested_source)
    }
    if (requested_source == "All" && "All" %in% supported_sources) {
      return("All")
    }
    if (!is.null(fallback_source) && fallback_source %in% supported_sources) {
      return(fallback_source)
    }
    if ("All" %in% supported_sources) {
      return("All")
    }
    supported_sources[[1]]
  }

  layer_sources <- list(
    synonyms = resolve_layer_source(source, c("OpenTargets", "CHEMBL", "TTD", "All")),
    status = resolve_layer_source(source, c("OpenTargets", "CHEMBL", "TTD", "All")),
    indications = resolve_layer_source(source, c("OpenTargets", "CHEMBL", "TTD", "All")),
    moa = resolve_layer_source(source, c("OpenTargets", "CHEMBL", "All")),
    atc = resolve_layer_source(source, c("WHO", "CHEMBL", "All"), fallback_source = "WHO"),
    adverse_events = resolve_layer_source(source, c("OpenTargets", "CHEMBL", "All")),
    smiles = resolve_layer_source(source, c("OpenTargets", "CHEMBL", "TTD", "B3DB_classification", "All"))
  )

  # ------------------------------------------------------------
  # Retrieve annotation layers
  # ------------------------------------------------------------
  annotation_list <- list(
    synonyms       = .safe_extract(get_drug_synonyms(drugs = drugs, source = layer_sources$synonyms, force = force, auth_token = auth_token)),
    status         = .safe_extract(get_drug_status(drugs = drugs, source = layer_sources$status, force = force, auth_token = auth_token)),
    indications    = .safe_extract(get_drug_indications(drugs = drugs, source = layer_sources$indications, force = force, auth_token = auth_token)),
    moa            = .safe_extract(get_drug_moa(drugs = drugs, source = layer_sources$moa, force = force, auth_token = auth_token)),
    bbb            = .safe_extract(get_drug_bbb(drugs = drugs, force = force, auth_token = auth_token)),
    atc            = .safe_extract(get_drug_atc(drugs = drugs, source = layer_sources$atc, force = force, auth_token = auth_token)),
    adverse_events = .safe_extract(get_drug_adverse_events(drugs = drugs, source = layer_sources$adverse_events, force = force, auth_token = auth_token)),
    smiles         = .safe_extract(get_drug_smiles(drugs = drugs, source = layer_sources$smiles, force = force, auth_token = auth_token))
  )

  # Keep only non-empty data frames
  annotation_list <- annotation_list[
    vapply(annotation_list, nrow, numeric(1)) > 0
  ]

  # Ensure Drug column exists
  annotation_list <- lapply(annotation_list, function(df) {
    if (!"Drug" %in% names(df) && ncol(df) > 0) {
      df <- dplyr::rename(df, Drug = 1)
    }
    df
  })

  # ------------------------------------------------------------
  # Merge all base annotations
  # ------------------------------------------------------------
  Annotation_res <- if (length(annotation_list) > 0) {
    purrr::reduce(annotation_list, dplyr::full_join, by = "Drug")
  } else {
    data.frame(Drug = drugs, stringsAsFactors = FALSE)
  }

  # ------------------------------------------------------------
  # Optional: Clinical Trials
  # ------------------------------------------------------------
  if (!is.null(condition)) {

    trials <- tryCatch(
      get_drug_trials(drugs = drugs, condition = condition, force = force, auth_token = auth_token)@result,
      error = function(e) {
        warning(conditionMessage(e), call. = FALSE)
        NULL
      }
    )

    if (!is.null(trials) && nrow(trials) > 0) {

      trials <- trials %>%
        dplyr::select(
          matched_drug,
          Conditions,
          Phases,
          `NCT Number`
        ) %>%
        dplyr::rename(
          Drug = matched_drug,
          Clinical_trial_conditions = Conditions,
          Clinical_trial_phase = Phases,
          NCT_Number = `NCT Number`
        ) %>%
        dplyr::group_by(Drug) %>%
        dplyr::summarise(
          Clinical_trial_conditions = paste(unique(na.omit(Clinical_trial_conditions)), collapse = "|"),
          Clinical_trial_phase      = paste(unique(na.omit(Clinical_trial_phase)), collapse = "|"),
          NCT_Number                = paste(unique(na.omit(NCT_Number)), collapse = "|"),
          .groups = "drop"
        )

      Annotation_res <- dplyr::left_join(Annotation_res, trials, by = "Drug")
    }
  }

  Annotation_res %>%
    dplyr::arrange(Drug) %>%
    dplyr::distinct()
}
