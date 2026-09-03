

#' Reformat Drug-Target Network by Target Set
#'
#' Groups drugs that share identical target sets and returns a normalized
#' drug-target table compatible with network-based methods.
#'
#' @param df A data frame containing at least `ID`, `Drug`, `Target`, and `Group`.
#'
#' @return A tibble with columns `ID`, `Drug`, `Target`, `Group`, `Target_Group_ID`.
#' @export
Drug_target_reformat <- function(df) {
  validate_network_columns(df, c("ID", "Drug", "Target", "Group"), "df")

  df <- df %>%
    dplyr::select(-tidyselect::any_of(c("Target_Set", "Target_Group_ID")))

  df_new <- df %>%
    dplyr::filter(!is.na(Target)) %>%
    dplyr::group_by(Drug) %>%
    dplyr::summarise(
      Target_Set = paste(sort(unique(Target)), collapse = "|"),
      .groups = "drop"
    ) %>%
    dplyr::group_by(Target_Set) %>%
    dplyr::mutate(Target_Group_ID = dplyr::cur_group_id()) %>%
    dplyr::ungroup()

  df %>%
    dplyr::filter(!is.na(Target)) %>%
    dplyr::left_join(df_new, by = "Drug") %>%
    dplyr::group_by(Target_Group_ID) %>%
    dplyr::reframe(
      Drug = paste(unique(Drug), collapse = "|"),
      ID = paste(unique(ID), collapse = "|"),
      Target = unique(Target),
      Group = dplyr::first(Group)
    ) %>%
    dplyr::select(ID, Drug, Target, Group, Target_Group_ID)
}
#' Resolve Network Inputs
#'
#' Internal helper to resolve default network datasets from Synapse/cache when
#' network inputs are omitted and to standardize/validate network schemas.
#'
#' @param ppi_network A data frame with `gene1`, `gene2` columns, or `NULL`.
#' @param drug_target_network A data frame with `ID`, `Drug`, `Target`, `Group`
#'   columns, or `NULL`.
#' @param group_drug_targets Logical; if `TRUE`, drugs sharing identical target
#'   sets are collapsed into grouped drug nodes via `Drug_target_reformat()`.
#' @param force Logical; if defaults are loaded from Synapse, force cache refresh.
#' @param auth_token Optional Synapse token used when default network inputs need
#'   to be downloaded and no valid cache is available. If `NULL`,
#'   `SYNAPSE_AUTH_TOKEN` is used.
#'
#' @return A named list containing `ppi_network` and `drug_target_network`.
#' @export
resolve_network_inputs <- function(ppi_network = NULL,
                                   drug_target_network = NULL,
                                   group_drug_targets = TRUE,
                                   force = FALSE,
                                   auth_token = NULL) {
  standardize_ppi <- function(x) {
    x <- tibble::as_tibble(x)
    if ("confidence" %in% names(x)) {
      x <- x %>% dplyr::filter(confidence == "High")
    }
    x %>%
      dplyr::select(gene1, gene2) %>%
      dplyr::distinct()
  }

  standardize_dti <- function(x) {
    x <- tibble::as_tibble(x)
    if (all(c("Drug_confidence", "Target_confidence") %in% names(x))) {
      x <- x %>%
        dplyr::filter(
          Drug_confidence == "High",
          Target_confidence %in% c("High", "Medium")
        )
    }
    x %>%
      dplyr::select(ID, Drug, Target, Group) %>%
      dplyr::distinct()
  }

  if (is.null(ppi_network)) {
    ppi_network <- load_drugsignet_network("gene_gene", force = force, auth_token = auth_token) %>%
      standardize_ppi()
  }

  if (is.null(drug_target_network)) {
    drug_target_network <- load_drugsignet_network("drug_target", force = force, auth_token = auth_token) %>%
      standardize_dti()
  }

  validate_network_columns(ppi_network, c("gene1", "gene2"), "ppi_network")
  validate_network_columns(drug_target_network, c("ID", "Drug", "Target", "Group"), "drug_target_network")

  if (!is.logical(group_drug_targets) || length(group_drug_targets) != 1 || is.na(group_drug_targets)) {
    stop("`group_drug_targets` must be TRUE or FALSE.", call. = FALSE)
  }

  if (isTRUE(group_drug_targets)) {
    drug_target_network <- Drug_target_reformat(drug_target_network)
  }

  list(ppi_network = tibble::as_tibble(ppi_network), drug_target_network = tibble::as_tibble(drug_target_network))
}

validate_network_columns <- function(x, required_columns, object_name) {
  if (!is.data.frame(x)) {
    stop(sprintf("`%s` must be a data.frame/tibble.", object_name), call. = FALSE)
  }

  missing_columns <- setdiff(required_columns, names(x))
  if (length(missing_columns) > 0) {
    stop(
      sprintf("`%s` is missing required columns: %s.", object_name, paste(missing_columns, collapse = ", ")),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


create_temp_work_dir <- function(prefix = "drugsignet_") {
  work_dir <- tempfile(prefix)
  ok <- dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  if (!ok || !dir.exists(work_dir)) {
    stop(sprintf("Failed to create temporary working directory: %s", work_dir), call. = FALSE)
  }
  work_dir
}


.filter_drug_target_network_by_flags <- function(drug_target_network,
                                                 include_indirect_drugs = TRUE,
                                                 include_non_approved_drugs = TRUE,
                                                 disease_genes = NULL,
                                                 ...) {
  validate_network_columns(drug_target_network, c("ID", "Drug", "Target", "Group"), "drug_target_network")

  out <- tibble::as_tibble(drug_target_network)

  if (!isTRUE(include_non_approved_drugs)) {
    out <- out %>%
      dplyr::filter(!is.na(Group)) %>%
      dplyr::filter(tolower(Group) %in% c("approved", "fda approved", "fda_approved"))
  }

  # NOTE: indirect drug filtering depends on network topology and disease seeds
  # and is handled downstream during graph construction.
  out
}

.clean_python_result <- function(x) {

  if (is.data.frame(x)) {
    attr(x, "pandas.index") <- NULL
  }

  x
}
