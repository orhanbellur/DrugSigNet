#' @title Extract a Drug-Centered PPI Subgraph
#'
#' @description
#' Extracts a drug-centered protein-protein interaction subgraph from
#' drug-target interactions, PPI edges, disease genes, and optionally
#' drug-induced signature genes.
#'
#' @details
#' `extract_drug_subgraph()` builds a local network around a selected drug. The
#' first hop connects the drug to its direct targets in the drug-target
#' interaction table. Additional hops expand through the supplied PPI network.
#'
#' If `ppi` or `dti` is `NULL`, the corresponding default DrugSigNet network is
#' loaded from the local cache or Synapse using `load_drugsignet_network()`.
#' Default networks are filtered to high-confidence PPI edges and high-confidence
#' drug-target interactions with high- or medium-confidence targets when these
#' confidence columns are available.
#'
#' Expansion after the first hop can be restricted with `k2_filter_by`.
#' Use `"none"` to traverse all PPI nodes, `"disease"` to restrict traversal to
#' disease genes, or `"drug_signature"` to restrict traversal to genes in the
#' supplied or computed drug signature.
#'
#' When `compute_drug_signature = TRUE`, drug-induced signature genes are
#' computed with `get_drug_signature()` using the selected `refdb`,
#' `signature_refdb_mode`, `n_up`, and `n_down` settings. Optional
#' `signature_cells` or `signature_profile_ids` can restrict the reference
#' profiles so that a predefined or legacy analysis can be reproduced.
#'
#' @param ppi Optional PPI network data frame with `gene1` and `gene2` columns.
#'   If `NULL`, the default DrugSigNet gene-gene network is loaded.
#' @param dti Optional drug-target interaction data frame with `Drug` and
#'   `Target` columns. If `NULL`, the default DrugSigNet drug-target network is
#'   loaded.
#' @param disease_genes_df Data frame with `Ensembl` and `source` columns.
#' @param drug_signature_df Optional data frame with an `Ensembl` column.
#' @param drug Character string specifying the drug name or identifier.
#' @param k_hops Positive integer number of hops to extract. Default is `2`.
#' @param drug_network Drug label used in the drug-target/network table.
#'   Defaults to `drug`.
#' @param drug_signature_id Drug label used when computing a drug signature.
#'   Defaults to `drug`.
#' @param k2_filter_by Node set used for second and later PPI hops. One of
#'   `"none"`, `"disease"`, or `"drug_signature"`. Default is `"none"`.
#' @param compute_drug_signature Logical; if `TRUE`, compute
#'   `drug_signature_df` with `get_drug_signature()`. Default is `FALSE`.
#' @param Gene_map Optional gene mapping table passed to `get_drug_signature()`
#'   when `compute_drug_signature = TRUE`.
#' @param signature_cells Optional character vector of cell lines passed to
#'   `get_drug_signature()` when computing the drug signature. For CMAP, this
#'   can reproduce a cell-restricted legacy analysis.
#' @param signature_profile_ids Optional character vector of exact reference
#'   profile column names passed to `get_drug_signature()`. This is especially
#'   useful for reproducing exact LINCS2 `pert_id__cell__type` selections.
#' @inheritParams get_drug_signature
#' @param force Logical; if `TRUE`, force refresh of default network cache when
#'   `ppi` or `dti` is `NULL`. Default is `FALSE`.
#' @param auth_token Optional Synapse authentication token used when loading
#'   default networks or frozen signature reference databases.
#'
#' @return
#' A list with two elements:
#' \describe{
#'   \item{subgraph}{Edge data frame for the extracted subgraph.}
#'   \item{metadata}{List containing drug labels, optional drug signature data,
#'   hop settings, reference database settings, and whether default networks were
#'   loaded.}
#' }
#'
#' Returns `NULL` if no valid graph can be built or if the selected drug is not
#' present in the resolved network.
#'
#' @examples
#' \dontrun{
#' subg <- extract_drug_subgraph(
#'   disease_genes_df = disease_genes,
#'   drug = "miglitol",
#'   k_hops = 2
#' )
#'
#' subg_sig <- extract_drug_subgraph(
#'   disease_genes_df = disease_genes,
#'   drug = "miglitol",
#'   compute_drug_signature = TRUE,
#'   refdb = "lincs2",
#'   n_up = 100,
#'   n_down = 100,
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#' }
#'
#' @importFrom dplyr mutate distinct filter transmute bind_rows pull
#' @importFrom tibble as_tibble
#' @export
extract_drug_subgraph <- function(ppi = NULL,
                                  dti = NULL,
                                  disease_genes_df = NULL,
                                  drug_signature_df = NULL,
                                  drug = NULL,
                                  k_hops = 2,
                                  drug_network = drug,
                                  drug_signature_id = drug,
                                  k2_filter_by = c("none", "disease", "drug_signature"),
                                  compute_drug_signature = FALSE,
                                  Gene_map = NULL,
                                  refdb = c("cmap", "lincs2"),
                                  signature_refdb_mode = c("default", "frozen", "frozen_force"),
                                  signature_cells = NULL,
                                  signature_profile_ids = NULL,
                                  n_up = 100,
                                  n_down = 100,
                                  force = FALSE,
                                  auth_token = NULL) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required", call. = FALSE)
  }

  refdb <- match.arg(refdb)
  signature_refdb_mode <- match.arg(signature_refdb_mode)
  k2_filter_by <- match.arg(k2_filter_by)

  network_inputs <- .resolve_drug_subgraph_networks(ppi, dti, force = force, auth_token = auth_token)
  ppi <- network_inputs$ppi
  dti <- network_inputs$dti

  if (!is.data.frame(disease_genes_df)) {
    stop("`disease_genes_df` must be a data frame.", call. = FALSE)
  }
  if (!is.character(drug) || length(drug) != 1L || is.na(drug) || !nzchar(trimws(drug))) {
    stop("`drug` must be a single non-empty character string.", call. = FALSE)
  }
  if (!is.numeric(k_hops) || length(k_hops) != 1L || is.na(k_hops) || k_hops < 1 || k_hops != as.integer(k_hops)) {
    stop("`k_hops` must be a positive integer.", call. = FALSE)
  }
  if (!is.logical(compute_drug_signature) || length(compute_drug_signature) != 1L || is.na(compute_drug_signature)) {
    stop("`compute_drug_signature` must be TRUE or FALSE.", call. = FALSE)
  }

  .require_columns(ppi, c("gene1", "gene2"), "ppi")
  .require_columns(dti, c("Drug", "Target"), "dti")
  .require_columns(disease_genes_df, c("Ensembl", "source"), "disease_genes_df")

  drug <- trimws(drug)
  drug_network <- trimws(as.character(drug_network))
  drug_signature_id <- trimws(as.character(drug_signature_id))
  k_hops <- as.integer(k_hops)

  clean_id <- function(x) sub("\\..*$", "", as.character(x))

  if (compute_drug_signature) {
    if (!exists("get_drug_signature", mode = "function")) {
      stop("`compute_drug_signature = TRUE` requires `get_drug_signature()`.", call. = FALSE)
    }

    sig <- get("get_drug_signature", mode = "function")(
      drug = drug_signature_id,
      cell = signature_cells,
      profile_ids = signature_profile_ids,
      refdb = refdb,
      signature_refdb_mode = signature_refdb_mode,
      auth_token = auth_token,
      gene_map = Gene_map,
      n_up = n_up,
      n_down = n_down
    )

    drug_signature_df <- .drug_subgraph_signature_from_get_drug_signature(sig)
    if (!nrow(drug_signature_df)) {
      stop("Drug signature mapping failed for drug = ", drug_signature_id, call. = FALSE)
    }
    drug_signature_df <- drug_signature_df %>%
      dplyr::mutate(Ensembl = clean_id(Ensembl)) %>%
      dplyr::distinct()
  }

  ppi <- ppi %>%
    dplyr::mutate(
      gene1 = clean_id(gene1),
      gene2 = clean_id(gene2),
      type = if ("type" %in% names(.)) as.character(type) else "PPI"
    )

  dti <- dti %>%
    dplyr::mutate(
      Drug = as.character(Drug),
      Target = clean_id(Target)
    )

  disease_genes_df <- disease_genes_df %>%
    dplyr::mutate(
      Ensembl = clean_id(Ensembl),
      source = as.character(source)
    )

  if (!is.null(drug_signature_df)) {
    .require_columns(drug_signature_df, "Ensembl", "drug_signature_df")
    drug_signature_df <- drug_signature_df %>%
      dplyr::mutate(Ensembl = clean_id(Ensembl)) %>%
      dplyr::distinct()
  }

  ppi_edges <- ppi %>%
    dplyr::transmute(
      from = gene1,
      to = gene2,
      type = type,
      edge_class = "PPI"
    )

  dt_edges <- dti %>%
    dplyr::filter(Drug == drug_network) %>%
    dplyr::transmute(
      from = Drug,
      to = Target,
      type = "DT",
      edge_class = "DT"
    )

  edges <- dplyr::bind_rows(ppi_edges, dt_edges) %>%
    dplyr::filter(
      !is.na(from),
      !is.na(to),
      from != "",
      to != "",
      from != to
    ) %>%
    dplyr::distinct()

  if (!nrow(edges)) {
    return(NULL)
  }

  g <- igraph::graph_from_data_frame(d = edges, directed = FALSE)

  if (!(drug_network %in% igraph::V(g)$name)) {
    return(NULL)
  }

  disease_set <- disease_genes_df %>%
    dplyr::pull(Ensembl) %>%
    unique()

  sig_set <- if (!is.null(drug_signature_df)) {
    drug_signature_df %>%
      dplyr::pull(Ensembl) %>%
      unique()
  } else {
    character(0)
  }

  visited <- drug_network
  frontier <- drug_network

  for (k in seq_len(k_hops)) {
    if (k == 1) {
      allowed_edges <- igraph::E(g)[type == "DT"]
    } else {
      edge_ends <- igraph::ends(g, igraph::E(g))
      allowed_nodes <- switch(
        k2_filter_by,
        disease = disease_set,
        drug_signature = sig_set,
        none = igraph::V(g)$name
      )
      allowed_edges <- igraph::E(g)[
        edge_class == "PPI" &
          edge_ends[, 1] %in% allowed_nodes &
          edge_ends[, 2] %in% allowed_nodes
      ]
    }

    gk <- igraph::subgraph.edges(
      graph = g,
      eids = allowed_edges,
      delete.vertices = FALSE
    )

    nbrs <- setdiff(
      igraph::V(g)$name[
        unlist(
          igraph::neighborhood(
            graph = gk,
            order = 1,
            nodes = frontier
          )
        )
      ],
      visited
    )

    if (k >= 2 && k2_filter_by != "none") {
      nbrs <- intersect(nbrs, allowed_nodes)
    }

    if (!length(nbrs)) {
      break
    }

    visited <- c(visited, nbrs)
    frontier <- nbrs
  }

  sg <- igraph::induced_subgraph(
    graph = g,
    vids = unique(visited)
  )

  list(
    subgraph = igraph::as_data_frame(sg, what = "edges"),
    metadata = list(
      drug_network = drug_network,
      drug_signature = drug_signature_df,
      k_hops = k_hops,
      k2_filter_by = k2_filter_by,
      refdb = refdb,
      signature_refdb_mode = signature_refdb_mode,
      signature_cells = signature_cells,
      signature_profile_ids = signature_profile_ids,
      selected_signature_profiles = if (compute_drug_signature) sig$selected_profiles else NULL,
      default_ppi_loaded = isTRUE(network_inputs$default_ppi_loaded),
      default_dti_loaded = isTRUE(network_inputs$default_dti_loaded)
    )
  )
}

.resolve_drug_subgraph_networks <- function(ppi = NULL,
                                            dti = NULL,
                                            force = FALSE,
                                            auth_token = NULL) {
  default_ppi_loaded <- is.null(ppi)
  default_dti_loaded <- is.null(dti)

  if (is.null(ppi)) {
    ppi <- load_drugsignet_network("gene_gene", force = force, auth_token = auth_token)
  }
  if (is.null(dti)) {
    dti <- load_drugsignet_network("drug_target", force = force, auth_token = auth_token)
  }

  if (!is.data.frame(ppi)) {
    stop("`ppi` must be a data frame or NULL.", call. = FALSE)
  }
  if (!is.data.frame(dti)) {
    stop("`dti` must be a data frame or NULL.", call. = FALSE)
  }

  ppi <- tibble::as_tibble(ppi)
  dti <- tibble::as_tibble(dti)

  if ("confidence" %in% names(ppi)) {
    ppi <- ppi %>% dplyr::filter(confidence == "High")
  }
  if (all(c("Drug_confidence", "Target_confidence") %in% names(dti))) {
    dti <- dti %>%
      dplyr::filter(
        Drug_confidence == "High",
        Target_confidence %in% c("High", "Medium")
      )
  }

  .require_columns(ppi, c("gene1", "gene2"), "ppi")
  .require_columns(dti, c("Drug", "Target"), "dti")

  if (!("type" %in% names(ppi))) {
    ppi$type <- "PPI"
  }

  list(
    ppi = ppi,
    dti = dti,
    default_ppi_loaded = default_ppi_loaded,
    default_dti_loaded = default_dti_loaded
  )
}

.drug_subgraph_signature_from_get_drug_signature <- function(sig) {
  signature_parts <- list()
  if (!is.null(sig$up_genes) && is.data.frame(sig$up_genes) && nrow(sig$up_genes)) {
    up_genes <- sig$up_genes
    up_genes$direction <- "up"
    signature_parts[[length(signature_parts) + 1L]] <- up_genes
  }
  if (!is.null(sig$down_genes) && is.data.frame(sig$down_genes) && nrow(sig$down_genes)) {
    down_genes <- sig$down_genes
    down_genes$direction <- "down"
    signature_parts[[length(signature_parts) + 1L]] <- down_genes
  }
  if (!length(signature_parts)) {
    return(data.frame(Ensembl = character(), direction = character(), score = numeric()))
  }

  signature_df <- dplyr::bind_rows(signature_parts)
  id_col <- intersect(c("Ensembl", "Gene.stable.ID", "Gene_symbol", "Gene", "Entrez"), names(signature_df))
  if (!length(id_col)) {
    return(data.frame(Ensembl = character(), direction = character(), score = numeric()))
  }

  dplyr::distinct(data.frame(
    Ensembl = as.character(signature_df[[id_col[1]]]),
    direction = if ("direction" %in% names(signature_df)) signature_df$direction else NA_character_,
    score = if ("score" %in% names(signature_df)) {
      signature_df$score
    } else if ("signature_score" %in% names(signature_df)) {
      signature_df$signature_score
    } else {
      NA_real_
    },
    stringsAsFactors = FALSE
  ))
}

.require_columns <- function(x, cols, name) {
  missing_cols <- setdiff(cols, colnames(x))
  if (length(missing_cols)) {
    stop(
      "`", name, "` must contain column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}
