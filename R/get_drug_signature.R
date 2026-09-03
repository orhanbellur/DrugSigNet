#' @title Retrieve Drug-Induced Gene Expression Signatures
#'
#' @description
#' Retrieves drug-induced gene expression signatures from CMAP or LINCS2
#' reference databases.
#'
#' @details
#' `get_drug_signature()` extracts perturbation profiles for a specified drug
#' from CMAP or LINCS2 reference databases. By default, all matching treatment
#' profiles are used.
#'
#' Profile selection can be restricted with `cell` or `profile_ids`. For CMAP,
#' `cell` may contain one or more cell lines; the requested cell order is
#' preserved so the resulting profile collapse matches cell-restricted legacy
#' analyses. For LINCS2, `profile_ids` can be used to select exact HDF5 profile
#' columns such as `pert_id__cell__type`. When `profile_ids` is supplied, it
#' takes precedence over automatic drug and cell matching.
#'
#' By default, reference data are loaded from ExperimentHub. Frozen Synapse
#' reference databases can be used by setting `signature_refdb_mode = "frozen"`
#' or `"frozen_force"`. In this case, the corresponding HDF5 reference database
#' is downloaded through `load_signature_refdb()` and cached locally.
#'
#' For LINCS2, drug names are resolved to perturbation identifiers using the
#' `lincs_pert_info2` metadata table from the `signatureSearch` package.
#'
#' Gene identifiers are returned as Entrez IDs and, when available, mapped to
#' Ensembl IDs and HGNC gene symbols. A user-supplied `gene_map` is used first;
#' missing gene symbols are then resolved through the package fallback mapping.
#'
#' If `n_up` or `n_down` is supplied, treatment profiles are collapsed per gene
#' by selecting the signed value with the largest absolute magnitude, and the top
#' up- or down-regulated genes are returned separately.
#'
#' @param drug Character string specifying the drug name.
#' @param cell Optional character vector specifying one or more cell lines to
#'   retain. If `NULL`, all matching cell lines are used.
#' @param profile_ids Optional character vector of exact reference-database HDF5
#'   column names to retain. When supplied, this takes precedence over automatic
#'   drug matching and `cell` filtering.
#' @param refdb Reference database. One of `"cmap"` or `"lincs2"`.
#' @param signature_refdb_mode Signature reference database mode. Use
#'   `"default"` for the default ExperimentHub reference, `"frozen"` for the
#'   cached Synapse HDF5 reference database, or `"frozen_force"` to redownload
#'   the frozen database.
#' @param auth_token Optional Synapse authentication token used when
#'   `signature_refdb_mode` requests a frozen database. If `NULL`,
#'   `SYNAPSE_AUTH_TOKEN` is used.
#' @param validate_signature_refdb Logical; whether to validate frozen HDF5
#'   reference databases before use. Default is `TRUE`.
#' @param gene_map Optional data frame mapping gene identifiers. Supported
#'   columns include Entrez, Ensembl, and gene symbol columns such as `Entrez`,
#'   `ENTREZID`, `Ensembl`, `ENSEMBL`, `Gene_symbol`, `Symbol`, or `SYMBOL`.
#' @param n_up Optional positive integer specifying the number of top
#'   up-regulated genes to return. If `NULL`, no separate up-regulated gene table
#'   is returned.
#' @param n_down Optional positive integer specifying the number of top
#'   down-regulated genes to return. If `NULL`, no separate down-regulated gene
#'   table is returned.
#'
#' @return
#' A list containing the drug name, reference database, selected treatment
#' profiles, extracted signature table, and optionally `up_genes` and
#' `down_genes`.
#'
#' @examples
#' \dontrun{
#' sig <- get_drug_signature(
#'   drug = "miglitol",
#'   refdb = "lincs2"
#' )
#'
#' sig_cmap <- get_drug_signature(
#'   drug = "bepridil",
#'   cell = c("MCF7", "HL60", "PC3"),
#'   refdb = "cmap",
#'   n_up = 100,
#'   n_down = 100
#' )
#'
#' sig_frozen <- get_drug_signature(
#'   drug = "miglitol",
#'   refdb = "lincs2",
#'   signature_refdb_mode = "frozen",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#' }
#'
#' @importFrom utils data head
#' @export
get_drug_signature <- function(drug,
                               cell = NULL,
                               profile_ids = NULL,
                               refdb = c("cmap", "lincs2"),
                               signature_refdb_mode = c("default", "frozen", "frozen_force"),
                               auth_token = NULL,
                               validate_signature_refdb = TRUE,
                               gene_map = NULL,
                               n_up = NULL,
                               n_down = NULL) {

  refdb <- match.arg(refdb)
  signature_refdb_mode <- match.arg(signature_refdb_mode)
  validate_signature_refdb <- .pipeline_flag(validate_signature_refdb, "validate_signature_refdb")

  if (identical(signature_refdb_mode, "default") && !requireNamespace("ExperimentHub", quietly = TRUE))
    stop("Package 'ExperimentHub' is required", call. = FALSE)

  if (!requireNamespace("HDF5Array", quietly = TRUE))
    stop("Package 'HDF5Array' is required", call. = FALSE)

  if (!requireNamespace("rhdf5", quietly = TRUE))
    stop("Package 'rhdf5' is required", call. = FALSE)

  if (!is.character(drug) || length(drug) != 1L || is.na(drug) || !nzchar(trimws(drug)))
    stop("`drug` must be a single non-empty character string.", call. = FALSE)

  drug <- trimws(drug)

  if (!is.null(cell)) {
    if (!is.character(cell) || anyNA(cell))
      stop("`cell` must be NULL or a character vector without NA values.", call. = FALSE)

    cell <- unique(trimws(cell))
    cell <- cell[nzchar(cell)]
    if (!length(cell)) cell <- NULL
  }

  if (!is.null(profile_ids)) {
    if (!is.character(profile_ids) || anyNA(profile_ids))
      stop("`profile_ids` must be NULL or a character vector without NA values.", call. = FALSE)

    profile_ids <- unique(trimws(profile_ids))
    profile_ids <- profile_ids[nzchar(profile_ids)]
    if (!length(profile_ids)) profile_ids <- NULL
  }

  n_up <- .validate_signature_gene_count(n_up, "n_up")
  n_down <- .validate_signature_gene_count(n_down, "n_down")


  ############################################################
  # Load reference database
  ############################################################

  if (identical(signature_refdb_mode, "default")) {
    eh <- ExperimentHub::ExperimentHub()

    expr_path <- switch(
      refdb,
      lincs2 = eh[["EH7297"]],
      cmap   = eh[["EH3223"]]
    )
  } else {
    expr_path <- .resolve_signature_refdb(
      ref_db = refdb,
      signature_refdb_mode = signature_refdb_mode,
      auth_token = auth_token,
      validate_hdf5 = validate_signature_refdb
    )
  }


  ############################################################
  # Load metadata
  ############################################################

  rn <- as.character(rhdf5::h5read(expr_path, "rownames"))
  cn <- as.character(rhdf5::h5read(expr_path, "colnames"))


  ############################################################
  # Identify treatment columns
  ############################################################

  if (!is.null(profile_ids)) {

    missing_profiles <- setdiff(profile_ids, cn)
    if (length(missing_profiles)) {
      warning(
        length(missing_profiles),
        " requested profile(s) were not found in ",
        refdb,
        ".",
        call. = FALSE
      )
    }

    # Preserve requested ordering because signed abs-max tie handling is
    # order-sensitive.
    trt_cols <- profile_ids[profile_ids %in% cn]

  } else if (refdb == "cmap") {

    if (is.null(cell)) {

      # Standard DrugSigNet behavior: use all profiles for the drug.
      trt_cols <- cn[startsWith(cn, paste0(drug, "__"))]

      # Backward-compatible fallback for non-standard CMAP column names.
      if (!length(trt_cols)) {
        trt_cols <- cn[grepl(drug, cn, fixed = TRUE)]
      }

    } else {

      # Legacy-compatible behavior: resolve each requested CMAP cell
      # independently and preserve the supplied cell order.
      trt_cols <- unlist(
        lapply(cell, function(cell_i) {
          exact_profile <- paste0(drug, "__", cell_i, "__trt_cp")

          if (exact_profile %in% cn) {
            return(exact_profile)
          }

          drug_match <- startsWith(cn, paste0(drug, "__"))
          cell_match <- grepl(paste0("__", cell_i, "__"), cn, fixed = TRUE)
          cn[drug_match & cell_match]
        }),
        use.names = FALSE
      )
    }

  } else {

    lincs_data_env <- new.env(parent = emptyenv())
    utils::data("lincs_pert_info2", package = "signatureSearch", envir = lincs_data_env)
    lincs_pert_info2 <- get("lincs_pert_info2", envir = lincs_data_env)

    drug_ids <- unique(
      lincs_pert_info2$pert_id[
        lincs_pert_info2$pert_iname == drug
      ]
    )

    if (!length(drug_ids))
      stop("Drug not found in LINCS perturbation metadata.", call. = FALSE)

    idx <- Reduce(
      "|",
      lapply(drug_ids, grepl, x = cn, fixed = TRUE)
    )

    if (!is.null(cell)) {
      cell_idx <- Reduce(
        "|",
        lapply(cell, function(cell_i) {
          grepl(paste0("__", cell_i, "__"), cn, fixed = TRUE)
        })
      )
      idx <- idx & cell_idx
    }

    trt_cols <- cn[idx]
  }

  trt_cols <- unique(as.character(trt_cols))

  if (!length(trt_cols))
    stop("No treatment profiles found for this drug and profile selection.", call. = FALSE)


  ############################################################
  # Load expression matrix
  ############################################################

  mat <- HDF5Array::HDF5Array(expr_path, "assay")

  rownames(mat) <- rn
  colnames(mat) <- cn

  trt_cols <- trt_cols[trt_cols %in% cn]
  mat <- mat[, trt_cols, drop = FALSE]
  mat <- as.matrix(mat)


  ############################################################
  # Build signature table
  ############################################################

  signature_df <- data.frame(
    Entrez = rn,
    mat,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )


  ############################################################
  # Map gene symbols and Ensembl IDs
  ############################################################

  gene_mapping <- .prepare_signature_gene_map(gene_map)
  gene_symbols <- .map_signature_gene_symbols(signature_df$Entrez, gene_mapping)
  ensembl_ids <- .map_signature_gene_ids(signature_df$Entrez, gene_mapping, target = "Ensembl")

  if (length(gene_symbols) != nrow(signature_df))
    stop("Gene mapping length mismatch.", call. = FALSE)

  signature_df$Gene_symbol <- gene_symbols
  signature_df$Ensembl <- ensembl_ids

  signature_df <- signature_df %>%
    dplyr::select(Entrez, Ensembl, Gene_symbol, dplyr::everything())


  ############################################################
  # Return result
  ############################################################

  result <- list(
    drug = drug,
    refdb = refdb,
    cells = cell,
    selected_profiles = trt_cols,
    signature = signature_df
  )

  if (!is.null(n_up)) {
    result$up_genes <- .extract_top_signature_genes(
      signature_df = signature_df,
      n = n_up,
      direction = "up"
    )
  }

  if (!is.null(n_down)) {
    result$down_genes <- .extract_top_signature_genes(
      signature_df = signature_df,
      n = n_down,
      direction = "down"
    )
  }

  result
}


############################################################
# Internal helper: validate requested top gene counts
############################################################

.validate_signature_gene_count <- function(n, name) {

  if (is.null(n)) {
    return(NULL)
  }

  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 1 || n != as.integer(n)) {
    stop("`", name, "` must be NULL or a positive integer.", call. = FALSE)
  }

  as.integer(n)
}


############################################################
# Internal helper: extract top up/down-regulated genes
############################################################

.extract_top_signature_genes <- function(signature_df, n, direction = c("up", "down")) {

  direction <- match.arg(direction)

  profile_cols <- setdiff(colnames(signature_df), c("Entrez", "Ensembl", "Gene_symbol"))

  if (!length(profile_cols)) {
    stop("No expression profile columns available to rank signature genes.", call. = FALSE)
  }

  profile_mat <- as.matrix(signature_df[, profile_cols, drop = FALSE])
  storage.mode(profile_mat) <- "numeric"

  signature_score <- .signed_absmax_by_row(profile_mat)

  keep <- !is.na(signature_score)
  ranked <- signature_df[keep, , drop = FALSE]
  ranked$signature_score <- signature_score[keep]

  id_cols <- intersect(c("Entrez", "Ensembl", "Gene_symbol"), colnames(ranked))
  ranked <- ranked[, c(id_cols, "signature_score", profile_cols), drop = FALSE]

  ranked <- if (direction == "up") {
    ranked[ranked$signature_score > 0, , drop = FALSE]
  } else {
    ranked[ranked$signature_score < 0, , drop = FALSE]
  }

  ord <- order(ranked$signature_score, decreasing = identical(direction, "up"))
  ranked <- ranked[ord, , drop = FALSE]
  ranked <- utils::head(ranked, n)
  rownames(ranked) <- NULL

  ranked
}


############################################################
# Internal helper: signed absolute maximum across profiles
############################################################

.signed_absmax_by_row <- function(mat) {
  apply(
    mat,
    1,
    function(x) {
      if (all(is.na(x))) {
        NA_real_
      } else {
        x[which.max(abs(x))]
      }
    }
  )
}


############################################################
# Internal helpers: optional gene map support
############################################################

.prepare_signature_gene_map <- function(gene_map = NULL) {
  if (is.null(gene_map)) {
    return(NULL)
  }
  if (!is.data.frame(gene_map)) {
    stop("`gene_map` must be a data frame or NULL.", call. = FALSE)
  }

  resolve_col <- function(candidates) {
    hit <- candidates[candidates %in% colnames(gene_map)]
    if (length(hit)) hit[1] else NULL
  }

  entrez_col <- resolve_col(c("Entrez", "ENTREZID", "entrez", "NCBI.gene..formerly.Entrezgene..ID"))
  ensembl_col <- resolve_col(c("Ensembl", "ENSEMBL", "Gene.stable.ID", "ensembl"))
  symbol_col <- resolve_col(c("Gene_symbol", "Symbol", "SYMBOL", "symbol", "hgnc_symbol", "HGNC.symbol"))

  if (is.null(entrez_col) && is.null(ensembl_col) && is.null(symbol_col)) {
    stop("`gene_map` must contain at least one supported Entrez, Ensembl, or symbol column.", call. = FALSE)
  }

  data.frame(
    Entrez = if (!is.null(entrez_col)) as.character(gene_map[[entrez_col]]) else NA_character_,
    Ensembl = if (!is.null(ensembl_col)) sub("\\..*$", "", as.character(gene_map[[ensembl_col]])) else NA_character_,
    Gene_symbol = if (!is.null(symbol_col)) as.character(gene_map[[symbol_col]]) else NA_character_,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ trimws(as.character(.x)))) %>%
    dplyr::filter(
      (!is.na(Entrez) & nzchar(Entrez)) |
        (!is.na(Ensembl) & nzchar(Ensembl)) |
        (!is.na(Gene_symbol) & nzchar(Gene_symbol))
    ) %>%
    dplyr::distinct()
}

.map_signature_gene_ids <- function(ids, gene_mapping, target = c("Ensembl", "Gene_symbol", "Entrez")) {
  target <- match.arg(target)
  ids <- sub("\\..*$", "", as.character(ids))
  out <- rep(NA_character_, length(ids))

  if (is.null(gene_mapping) || !nrow(gene_mapping)) {
    return(out)
  }

  for (source in c("Entrez", "Ensembl", "Gene_symbol")) {
    source_values <- gene_mapping[[source]]
    hit <- match(ids[is.na(out)], source_values)
    values <- gene_mapping[[target]][hit]
    use <- !is.na(hit) & !is.na(values) & nzchar(values)
    idx <- which(is.na(out))
    out[idx[use]] <- values[use]
  }

  out
}

.map_signature_gene_symbols <- function(ids, gene_mapping = NULL) {
  mapped_symbols <- .map_signature_gene_ids(ids, gene_mapping, target = "Gene_symbol")
  missing <- is.na(mapped_symbols) | !nzchar(mapped_symbols)

  if (any(missing)) {
    fallback <- .map_entrez_to_gene_symbols(ids[missing])
    mapped_symbols[missing] <- fallback
  }

  mapped_symbols
}
