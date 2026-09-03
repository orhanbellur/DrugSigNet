# Internal shared formatter used by the exported signature rank-score functions.

# =====================
# Ranking Functions
# =====================

# Standardize method-specific signature scores into the shared processed
# perturbation schema used by downstream rank aggregation.
.data_format <- function(signature_data, score_col) {
  required_cols <- c("pert", "rank_score", "scaled_score", "trend", score_col)

  missing_cols <- setdiff(required_cols, names(signature_data))
  if (length(missing_cols) > 0L) {
    stop(
      "Missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  output_cols <- c(
    "pert", "rank_score", score_col, "trend", "scaled_score",
    "cell", "t_gn_sym", "MOAss", "PCIDss", "ID",
    "name_synonym_list", "group"
  )
  rename_cols <- c(
    perturbation = "pert", ssTargets = "t_gn_sym",
    Name_synonyms = "name_synonym_list", Drug_status = "group"
  )

  pert_struct <- signature_data %>%
    dplyr::select(dplyr::any_of(output_cols)) %>%
    dplyr::rename(dplyr::any_of(rename_cols)) %>%
    dplyr::group_by(perturbation) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c("cell", "MOAss", "ssTargets", "PCIDss")),
        ~ paste0(na.omit(unique(.x)), collapse = ";")
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(rank_score)) %>%
    unique()

  return(pert_struct)
}

# Combine only annotation fields actually supplied by the signature results.
# In particular, a NULL drug synonym map must not manufacture ID, synonym, or
# status columns.
.harmonize_signature_metadata <- function(signature_data) {
  for (column in intersect(c("cell", "ssTargets"), names(signature_data))) {
    signature_data <- tidyr::separate_rows(
      signature_data,
      dplyr::all_of(column),
      sep = ";"
    )
  }

  collapsed_cols <- intersect(
    c("cell", "MOAss", "ssTargets", "PCIDss"),
    names(signature_data)
  )
  mapped_cols <- intersect(
    c("ID", "Name_synonyms", "Drug_status"),
    names(signature_data)
  )

  signature_data %>%
    dplyr::group_by(perturbation) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(collapsed_cols),
        ~ paste0(stats::na.omit(unique(.x)), collapse = ";")
      ),
      dplyr::across(dplyr::all_of(mapped_cols), unique),
      .groups = "drop"
    ) %>%
    unique()
}
