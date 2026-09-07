#' Collapse annotation values
#'
#' Internal helper to collapse repeated annotation values per drug.
#'
#' @param x Vector of values.
#' @return A pipe-delimited character scalar or `NA_character_`.
#' @keywords internal
collapse_annotation_values <- function(x) {
  values <- unique(trimws(as.character(stats::na.omit(x))))
  values <- values[nzchar(values)]
  if (length(values) == 0) {
    return(NA_character_)
  }
  paste(values, collapse = "|")
}

#' Aggregate annotation rows per drug
#'
#' Internal helper that converts one-to-many annotation tables into one row per
#' drug with pipe-delimited values in each annotation column.
#'
#' @param ref_tbl A data frame with a `Drug` column.
#' @return A data frame with one row per `Drug`.
#' @keywords internal
collapse_annotations_by_drug <- function(ref_tbl) {
  if (!is.data.frame(ref_tbl) || !"Drug" %in% names(ref_tbl)) {
    stop("`ref_tbl` must be a data.frame with a `Drug` column.", call. = FALSE)
  }

  annotation_cols <- setdiff(names(ref_tbl), "Drug")

  if (length(annotation_cols) == 0) {
    return(
      ref_tbl %>%
        dplyr::mutate(Drug = as.character(Drug)) %>%
        dplyr::distinct(Drug)
    )
  }

  ref_tbl %>%
    dplyr::mutate(Drug = as.character(Drug)) %>%
    dplyr::group_by(Drug) %>%
    dplyr::summarise(
      dplyr::across(
        .cols = dplyr::all_of(annotation_cols),
        .fns = collapse_annotation_values
      ),
      .groups = "drop"
    )
}


#' Load DrugSigNet data with backward-compatible fallback
#'
#' Internal helper that uses `load_drugsignet_data()` when available and falls
#' back to `utils::data()` for older installations/workflows.
#'
#' @param dataset Dataset object name.
#' @param envir Environment where the object should be loaded.
#' @return The loaded dataset object.
#' @keywords internal
load_drugsignet_data_compat <- function(dataset, envir = parent.frame(), force = FALSE, auth_token = NULL) {
  if (exists("load_drugsignet_data", mode = "function", inherits = TRUE)) {
    return(load_drugsignet_data(dataset, envir = envir, force = force, auth_token = auth_token))
  }

  suppressWarnings(utils::data(list = dataset, package = "DrugSigNet", envir = envir))
  if (!exists(dataset, envir = envir, inherits = FALSE)) {
    stop(sprintf("Dataset `%s` could not be loaded.", dataset), call. = FALSE)
  }
  get(dataset, envir = envir, inherits = FALSE)
}


#' Load DrugSigNet dataset from Synapse cache (fallback to bundled data)
#'
#' @param dataset Dataset object name.
#' @param envir Environment where the object should be assigned.
#' @param force Logical; force re-download in \code{load_synapse_annotations()}.
#' @param auth_token Optional Synapse token.
#' @return The loaded dataset object.
#' @keywords internal
load_drugsignet_data <- function(dataset,
                                 envir = parent.frame(),
                                 force = FALSE,
                                 auth_token = NULL) {
  synapse_obj <- tryCatch(
    load_synapse_annotations(force = force, auth_token = auth_token),
    error = function(e) {
      if (!is.null(auth_token) && nzchar(auth_token)) {
        warning("Synapse annotation load failed: ", conditionMessage(e), call. = FALSE)
      }
      NULL
    }
  )

  if (is.null(synapse_obj) && file.exists("drug_annotation.rds")) {
    synapse_obj <- tryCatch(readRDS("drug_annotation.rds"), error = function(e) NULL)
  }

  if (!is.null(synapse_obj)) {
    get_section <- function(path) {
      out <- synapse_obj
      for (nm in path) {
        out <- out[[nm]]
        if (is.null(out)) return(NULL)
      }
      out
    }

    combine_sources <- function(source_name) {
      safe_full_join <- function(x, y) {
        join_keys <- intersect(
          names(x),
          names(y)
        )
        join_keys <- intersect(join_keys, c("drug_name", "drug_id", "chembl_id"))

        if (length(join_keys) == 0) {
          return(dplyr::bind_rows(x, y))
        }

        dplyr::full_join(x, y, by = join_keys)
      }

      subset_integrated_by_source <- function(tbl) {
        if (!is.data.frame(tbl) || !"source" %in% names(tbl)) return(NULL)
        keep <- grepl(source_name, as.character(tbl$source), fixed = TRUE)
        out <- tbl[keep, , drop = FALSE]
        if (nrow(out) == 0) return(NULL)
        out
      }

      candidates <- list(
        # If Synapse object includes full source table, use it first.
        get_section(c(source_name)),
        # Primary sections from drug_annotation.rds
        get_section(c("moA", source_name)),
        get_section(c("indications", source_name)),
        get_section(c("synonyms", source_name)),
        get_section(c("ATC", source_name)),
        # Optional status/adverse sections (support common aliases)
        get_section(c("status", source_name)),
        get_section(c("Highest_status", source_name)),
        get_section(c("adverse_events", source_name)),
        get_section(c("adverse", source_name)),
        # Integrated slices filtered by `source` label
        subset_integrated_by_source(get_section(c("status", "Integrated"))),
        subset_integrated_by_source(get_section(c("adverse_events", "Integrated"))),
        subset_integrated_by_source(get_section(c("adverse", "Integrated")))
      )
      candidates <- Filter(function(x) is.data.frame(x), candidates)
      if (length(candidates) == 0) return(NULL)
      Reduce(safe_full_join, candidates)
    }

    get_annotation <- function(name, aliases = character()) {
      candidates <- unique(c(name, aliases))
      for (candidate in candidates) {
        obj <- get_section(c("annotations", candidate))
        if (is.data.frame(obj)) return(obj)
      }
      for (candidate in candidates) {
        obj <- get_section(c(candidate))
        if (is.data.frame(obj)) return(obj)
      }
      NULL
    }

    obj <- switch(
      dataset,
      OpenTargets = combine_sources("OpenTargets"),
      CHEMBL = combine_sources("CHEMBL"),
      TTD = combine_sources("TTD"),
      WHO = get_annotation("atc", c("ATC", "WHO")),
      Integrated_moA = get_annotation("moa", c("moA", "Integrated_moA")),
      Integrated_indications = get_annotation("indications", c("Integrated_indications")),
      Integrated_status = get_annotation("status", c("Highest_status", "Integrated_status")),
      Integrated_synonyms = get_annotation("synonyms", c("Integrated_synonyms")),
      Integrated_ATC = get_annotation("atc", c("ATC", "Integrated_ATC")),
      B3DB_classification = get_annotation("bbb", c("BBB", "B3DB", "B3DB_classification")),
      ctg_studies = get_annotation("clinical_trials", c("ClinicalTrialsGov", "ctg_studies")),
      Integrated_adverse = get_annotation("adverse_events", c("adverse", "Integrated_adverse")),
      Integrated_smiles = get_annotation("smiles", c("Integrated_smiles")),
      NULL
    )

    if (!is.null(obj)) {
      assign(dataset, obj, envir = envir)
      return(obj)
    }
  }

  suppressWarnings(utils::data(list = dataset, package = "DrugSigNet", envir = envir))
  if (!exists(dataset, envir = envir, inherits = FALSE)) {
    stop(sprintf("Dataset `%s` could not be loaded.", dataset), call. = FALSE)
  }
  get(dataset, envir = envir, inherits = FALSE)
}

#' Resolve a column from candidate names
#'
#' Internal helper for annotation getters.
#'
#' @param tbl Data frame to inspect.
#' @param candidates Candidate column names in preference order.
#' @param field Field name used in errors.
#' @param required Logical; whether missing column should error.
#' @return Matching column name or `NA_character_`.
#' @keywords internal
resolve_annotation_col <- function(tbl, candidates, field, required = TRUE) {
  hit <- candidates[candidates %in% names(tbl)][1]
  if (is.na(hit)) {
    if (isTRUE(required)) {
      stop(sprintf(
        "No supported `%s` column found. Checked: %s",
        field,
        paste(candidates, collapse = ", ")
      ), call. = FALSE)
    }
    return(NA_character_)
  }
  hit
}

#' Filter integrated annotation rows by source label
#'
#' Internal helper that implements source matching for the integrated
#' `drug_annotation$annotations` tables. `All` returns the table unchanged;
#' otherwise a case-insensitive grep is applied to the `source` column when it
#' is available.
#'
#' @param tbl Annotation data frame.
#' @param source Source requested by the user.
#' @return Filtered annotation data frame.
#' @keywords internal
filter_annotation_source <- function(tbl, source) {
  if (!is.data.frame(tbl) || identical(source, "All") || !"source" %in% names(tbl)) {
    return(tbl)
  }

  source_pattern <- switch(
    tolower(trimws(source)),
    "opentragets" = "OpenTargets",
    "open targets" = "OpenTargets",
    "b3db_classification" = "B3DB",
    source
  )

  keep <- grepl(
    pattern = source_pattern,
    x = as.character(tbl$source),
    ignore.case = TRUE
  )
  tbl[keep, , drop = FALSE]
}

#' Load a DrugSigNet integrated annotation section
#'
#' Internal helper that reads the current Synapse/cache `drug_annotation.rds`
#' schema (`annotations = list(moa = ..., indications = ..., ...)`) and falls
#' back to legacy top-level names when necessary.
#'
#' @param section Primary annotation section name.
#' @param aliases Additional legacy section names to try.
#' @param force Logical; force Synapse cache refresh.
#' @param auth_token Optional Synapse auth token.
#' @return A data frame or `NULL`.
#' @keywords internal
load_annotation_section <- function(section, aliases = character(), force = FALSE, auth_token = NULL) {
  synapse_obj <- tryCatch(
    load_synapse_annotations(force = force, auth_token = auth_token),
    error = function(e) {
      if (!is.null(auth_token) && nzchar(auth_token)) {
        warning("Synapse annotation load failed: ", conditionMessage(e), call. = FALSE)
      }
      NULL
    }
  )

  if (is.null(synapse_obj) && file.exists("drug_annotation.rds")) {
    synapse_obj <- tryCatch(readRDS("drug_annotation.rds"), error = function(e) NULL)
  }

  candidate_names <- unique(c(section, aliases))

  if (!is.null(synapse_obj)) {
    for (nm in candidate_names) {
      tbl <- synapse_obj[["annotations"]][[nm]]
      if (is.data.frame(tbl)) return(tbl)
    }

    for (nm in candidate_names) {
      tbl <- synapse_obj[[nm]]
      if (is.data.frame(tbl)) return(tbl)
    }
  }

  NULL
}

#' Join annotations to an input drug table
#'
#' @param input_tbl Input table with `Drug` column.
#' @param ref_tbl Reference table with `Drug` column.
#' @param empty_cols Annotation columns to include when `ref_tbl` is empty.
#' @return Joined data frame.
#' @keywords internal
left_join_annotations_by_drug <- function(input_tbl, ref_tbl, empty_cols = character()) {
  if (is.null(ref_tbl) || !is.data.frame(ref_tbl) || nrow(ref_tbl) == 0) {
    out <- input_tbl
    for (col in empty_cols) {
      if (!col %in% names(out)) out[[col]] <- NA_character_
    }
    return(dplyr::distinct(out))
  }

  input_tbl %>%
    dplyr::mutate(.drug_key = tolower(trimws(Drug))) %>%
    dplyr::left_join(
      ref_tbl %>%
        dplyr::mutate(.drug_key = tolower(trimws(Drug))) %>%
        dplyr::select(-Drug),
      by = ".drug_key"
    ) %>%
    dplyr::select(-.drug_key) %>%
    dplyr::distinct()
}
