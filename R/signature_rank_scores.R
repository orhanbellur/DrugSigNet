# Internal shared formatter used by the exported signature rank-score functions.

# =====================
# Ranking Functions
# =====================

# Resolve the Kolmogorov-Smirnov critical-value helper across supported
# BoutrosLab.plotting.general releases. Version 7 renamed the original
# `ks.test.critical.value()` function to `critical.value.ks.test()`, and some
# binary builds do not export the renamed symbol even though it is present in
# the namespace. Looking it up explicitly keeps the calculation owned by the
# dependency while avoiding a brittle `pkg::symbol` export requirement.
.drugsignet_ks_critical_value <- function(n, confidence,
                                          alternative = "two.sided",
                                          namespace = NULL) {
  if (is.null(namespace)) {
    namespace <- asNamespace("BoutrosLab.plotting.general")
  }
  if (identical(namespace, asNamespace("BoutrosLab.plotting.general")) &&
      "critical.value.ks.test" %in%
        getNamespaceExports("BoutrosLab.plotting.general")) {
    return(BoutrosLab.plotting.general::critical.value.ks.test(
      n,
      confidence,
      alternative = alternative
    ))
  }
  candidates <- c("critical.value.ks.test", "ks.test.critical.value")
  available <- candidates[vapply(
    candidates,
    exists,
    logical(1),
    envir = namespace,
    mode = "function",
    inherits = FALSE
  )]
  if (!length(available)) {
    stop(
      "The installed BoutrosLab.plotting.general package does not provide ",
      "a Kolmogorov-Smirnov critical-value function. Please reinstall or ",
      "update BoutrosLab.plotting.general.",
      call. = FALSE
    )
  }

  get(available[[1L]], envir = namespace, inherits = FALSE)(
    n,
    confidence,
    alternative = alternative
  )
}

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
