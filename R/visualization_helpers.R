.safe_exec <- function(expr) {
  tryCatch(expr, error = function(e) {
    warning(conditionMessage(e), call. = FALSE)
    NULL
  })
}

.normalize_condition_key <- function(x) {
  x <- as.character(x)
  x <- dplyr::coalesce(x, "")
  x %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("\\([^\\)]*\\)", "") %>%
    stringr::str_replace_all("['\\u2019]", "") %>%
    stringr::str_replace_all("[[:punct:]]", " ") %>%
    stringr::str_squish()
}

.choose_condition_label <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & trimws(x) != ""]
  if (length(x) == 0) return(NA_character_)

  clean <- stringr::str_remove_all(x, "\\([^\\)]*\\)")
  clean <- stringr::str_squish(clean)

  key <- clean %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("['\\u2019]", "") %>%
    stringr::str_replace_all("[[:punct:]]", " ") %>%
    stringr::str_squish()

  df <- data.frame(raw = x, clean = clean, key = key, stringsAsFactors = FALSE) %>%
    dplyr::distinct(clean, key, .keep_all = TRUE)

  score_label <- function(s) {
    has_apostrophe <- stringr::str_detect(s, "['\\u2019]")
    has_upper <- stringr::str_detect(s, "[A-Z]")
    all_lower <- identical(s, stringr::str_to_lower(s))
    nchar(s) + ifelse(has_apostrophe, 50, 0) + ifelse(has_upper, 20, 0) - ifelse(all_lower, 10, 0)
  }

  best <- df$clean[which.max(vapply(df$clean, score_label, numeric(1)))][1]
  if (identical(best, stringr::str_to_lower(best))) best <- tools::toTitleCase(best)
  best
}

.phase_to_num <- function(x) {
  x <- tolower(dplyr::coalesce(as.character(x), ""))
  vapply(x, function(s) {
    if (!nzchar(s)) return(NA_integer_)
    if (stringr::str_detect(s, "approved")) return(5L)
    nums <- stringr::str_extract_all(s, "[1-4]")[[1]]
    nums <- suppressWarnings(as.integer(nums))
    nums <- nums[!is.na(nums)]
    if (length(nums) == 0) NA_integer_ else max(nums)
  }, integer(1))
}

.num_to_phase <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x == 5L ~ "Approved",
    x == 4L ~ "Phase 4",
    x == 3L ~ "Phase 3",
    x == 2L ~ "Phase 2",
    x == 1L ~ "Phase 1",
    TRUE ~ NA_character_
  )
}

.extract_enrichment_df <- function(functional_enrichment) {
  if (is.null(functional_enrichment)) {
    return(NULL)
  }
  if (is.data.frame(functional_enrichment)) {
    return(functional_enrichment)
  }
  if (isS4(functional_enrichment) && "result" %in% methods::slotNames(functional_enrichment)) {
    return(functional_enrichment@result)
  }
  NULL
}

.build_plot_inputs <- function(rank_df,
                               topk_union_drugs = NULL,
                               top_k_union_drugs = NULL,
                               features_df = NULL,
                               functional_enrichment = NULL,
                               top_k,
                               trial_condition = NULL) {
  if (!is.data.frame(rank_df) || !"Drug" %in% names(rank_df)) {
    stop("`rank_df` must be a data frame containing a `Drug` column.", call. = FALSE)
  }

  if (!is.numeric(top_k) || length(top_k) != 1 || is.na(top_k) || top_k < 1) {
    stop("`top_k` must be a single positive integer.", call. = FALSE)
  }

  top_k <- as.integer(top_k)

  if (is.null(topk_union_drugs) && !is.null(top_k_union_drugs)) {
    topk_union_drugs <- top_k_union_drugs
  }

  # keep full rank table
  rank_df <- rank_df %>%
    dplyr::distinct(Drug, .keep_all = TRUE)

  # use all numeric columns from rank_df as rank columns
  rank_cols <- names(rank_df)[vapply(rank_df, is.numeric, logical(1))]
  rank_cols <- setdiff(rank_cols, "highest_phase_num")

  if (length(rank_cols) == 0) {
    return(list())
  }

  plot_inputs <- list()

  # ------------------------------------------------------------
  # Auxiliary plots can still use top_k union drugs
  # ------------------------------------------------------------
  plot_drugs <- character(0)
  if (!is.null(topk_union_drugs) &&
      is.data.frame(topk_union_drugs) &&
      "Drug" %in% names(topk_union_drugs)) {
    plot_drugs <- unique(stats::na.omit(topk_union_drugs$Drug))
  }

  feature_topk <- NULL
  if (!is.null(features_df) &&
      is.data.frame(features_df) &&
      "Drug" %in% names(features_df) &&
      length(plot_drugs) > 0) {
    feature_topk <- features_df %>%
      dplyr::filter(Drug %in% plot_drugs)
  }

  # ------------------------------------------------------------
  # Approved indications
  # ------------------------------------------------------------
  if (!is.null(feature_topk) &&
      nrow(feature_topk) > 0 &&
      "indication" %in% names(feature_topk)) {

    approved_indications <- feature_topk %>%
      dplyr::select(Drug, indication) %>%
      tidyr::separate_rows(indication, sep = "\\|") %>%
      dplyr::mutate(indication = stringr::str_squish(indication)) %>%
      dplyr::filter(!is.na(indication), indication != "") %>%
      dplyr::filter(stringr::str_detect(stringr::str_to_lower(indication), "approved")) %>%
      dplyr::mutate(indication = stringr::str_remove(indication, "\\s*\\([^\\)]*\\)\\s*$")) %>%
      dplyr::distinct()

    if (nrow(approved_indications) > 0) {
      plot_inputs$drug_indications <- approved_indications
    }
  }

  # ------------------------------------------------------------
  # Full rank table for ranking plots
  # ------------------------------------------------------------
  top_hits_input <- rank_df
  overlap_input  <- rank_df

  # ------------------------------------------------------------
  # Add trial-condition Status using FULL features table
  # Do not filter rows away; keep full rank table
  # ------------------------------------------------------------
  if (!is.null(trial_condition) &&
      !is.null(features_df) &&
      is.data.frame(features_df) &&
      "Drug" %in% names(features_df) &&
      all(c("indication", "Clinical_trial_conditions", "Clinical_trial_phase") %in% names(features_df))) {

    condition_hits <- rank_df %>%
      dplyr::select(Drug) %>%
      dplyr::left_join(
        features_df %>%
          dplyr::select(Drug, indication, Clinical_trial_conditions, Clinical_trial_phase),
        by = "Drug"
      ) %>%
      tidyr::pivot_longer(
        cols = c(indication, Clinical_trial_conditions),
        names_to = "match_source",
        values_to = "match_text"
      ) %>%
      dplyr::filter(!is.na(match_text), trimws(match_text) != "") %>%
      tidyr::separate_rows(match_text, sep = "\\|") %>%
      tidyr::separate_rows(Clinical_trial_phase, sep = "\\|") %>%
      dplyr::mutate(
        match_text = stringr::str_squish(match_text),
        Clinical_trial_phase = dplyr::coalesce(as.character(Clinical_trial_phase), ""),
        Clinical_trial_phase = stringr::str_squish(Clinical_trial_phase),
        condition_key = .normalize_condition_key(match_text),
        match_text_with_phase = dplyr::if_else(
          match_source == "Clinical_trial_conditions" & Clinical_trial_phase != "",
          paste0(match_text, " (", Clinical_trial_phase, ")"),
          match_text
        ),
        phase_num = .phase_to_num(match_text_with_phase)
      ) %>%
      dplyr::filter(
        stringr::str_detect(
          condition_key,
          stringr::regex(.normalize_condition_key(trial_condition), ignore_case = TRUE)
        )
      ) %>%
      dplyr::group_by(Drug) %>%
      dplyr::summarise(
        highest_phase_num = {
          x <- phase_num[!is.na(phase_num)]
          if (length(x) == 0) NA_integer_ else max(x)
        },
        condition_label = .choose_condition_label(match_text),
        .groups = "drop"
      ) %>%
      dplyr::mutate(Status = .num_to_phase(highest_phase_num)) %>%
      dplyr::select(Drug, Status)

    top_hits_input <- top_hits_input %>%
      dplyr::left_join(condition_hits, by = "Drug")

    overlap_input <- overlap_input %>%
      dplyr::left_join(condition_hits, by = "Drug")
  } else if (!is.null(trial_condition)) {
    # Keep trial-condition plots available when the annotation lookup returns
    # no matching clinical-trial rows.
    top_hits_input <- top_hits_input %>%
      dplyr::mutate(Status = NA_character_)
    overlap_input <- overlap_input %>%
      dplyr::mutate(Status = NA_character_)
  }

  # ------------------------------------------------------------
  # Rank-based plots: full row set + all rank columns
  # ------------------------------------------------------------
  if (!is.null(trial_condition) && "Status" %in% names(top_hits_input)) {
    plot_inputs$top_k_hits <- top_hits_input %>%
      dplyr::select(Drug, Status, dplyr::all_of(rank_cols))

    plot_inputs$top_k_overlap <- overlap_input %>%
      dplyr::select(Drug, Status, dplyr::all_of(rank_cols))

    if (length(rank_cols) >= 2) {
      plot_inputs$rank_agreement <- overlap_input %>%
        dplyr::select(Drug, Status, dplyr::all_of(rank_cols))

      plot_inputs$rank_distribution_scatter <- overlap_input %>%
        dplyr::select(Drug, Status, dplyr::all_of(rank_cols))
    }
  } else {
    plot_inputs$top_k_hits <- top_hits_input %>%
      dplyr::select(Drug, dplyr::all_of(rank_cols))

    plot_inputs$top_k_overlap <- overlap_input %>%
      dplyr::select(Drug, dplyr::all_of(rank_cols))

    if (length(rank_cols) >= 2) {
      plot_inputs$rank_agreement <- overlap_input %>%
        dplyr::select(Drug, dplyr::all_of(rank_cols))

      plot_inputs$rank_distribution_scatter <- overlap_input %>%
        dplyr::select(Drug, dplyr::all_of(rank_cols))
    }
  }

  # ------------------------------------------------------------
  # Similarity
  # ------------------------------------------------------------
  similarity_tbl <- .safe_exec(
    tryCatch(
      get_drug_drug_similarity(drugs = plot_drugs, source = "All")@result,
      error = function(e) NULL
    )
  )

  if (!is.null(similarity_tbl) &&
      is.data.frame(similarity_tbl) &&
      nrow(similarity_tbl) >= 2 &&
      ncol(similarity_tbl) >= 2) {
    plot_inputs$drug_similarity <- as.matrix(similarity_tbl)
  }

  # ------------------------------------------------------------
  # Enrichment
  # ------------------------------------------------------------
  enrichment_df <- .extract_enrichment_df(functional_enrichment)
  if (!is.null(enrichment_df) &&
      is.data.frame(enrichment_df) &&
      nrow(enrichment_df) > 0) {
    plot_inputs$enriched_terms <- enrichment_df
  }

  # ------------------------------------------------------------
  # Drug hierarchy
  # ------------------------------------------------------------
  if (!is.null(feature_topk) && nrow(feature_topk) > 0) {
    hierarchy_cols <- c("level1_description", "mechanismOfAction", "Drug")
    if (all(hierarchy_cols %in% names(feature_topk))) {
      plot_inputs$drug_hierarchy <- list(
        data_df = feature_topk,
        hierarchy_cols = hierarchy_cols,
        plot_type = "sunburst"
      )
    }
  }

  plot_inputs
}

.build_visualization <- function(plot_inputs) {
  .safe_exec({
    if (length(plot_inputs) == 0) {
      list(plots = list(), errors = list(no_input = "No valid plotting inputs available."))
    } else {
      plot_all(plot_inputs = plot_inputs, continue_on_error = TRUE, verbose = FALSE)
    }
  })
}
