#' @title TrustRank for Network-Based Drug Searching
#'
#' @description
#' Prioritizes candidate drugs or target nodes using TrustRank propagation on an
#' integrated protein-protein interaction and drug-target network.
#'
#' @details
#' TrustRank is a seed-based propagation method derived from PageRank. Disease
#' genes are used as trusted seed nodes, and their signal is propagated through
#' the integrated network to prioritize drugs or targets topologically close to
#' the disease module.
#'
#' The integrated network is built from a protein-protein interaction network
#' (`ppi_network`) and a drug-target interaction network
#' (`drug_target_network`). If either network is `NULL`, the corresponding
#' high-confidence default DrugSigNet network is loaded with
#' `load_drugsignet_network()` from the local cache or Synapse.
#'
#' This implementation follows the network-based drug search setting used in
#' CADDIE, where disease seed genes are analyzed on a combined interactome of
#' protein-protein and drug-target interactions.
#'
#' @inheritParams Degree_centrality
#' @param hub_penalty Numeric hub penalty used to reduce the influence of highly
#'   connected nodes. Must be between `0` and `1`. Default is `0.01`.
#' @param damping_factor Numeric damping factor used during TrustRank
#'   propagation. Must be between `0` and `1`. Default is `0.95`.
#'
#' @return
#' A `NetworkBased` object containing TrustRank results in `object@result` and
#' method parameters in `object@parameters`.
#'
#' @examples
#' \dontrun{
#' disease_genes <- data.frame(
#'   gene = c("ENSG00000130203", "ENSG00000142192", "ENSG00000171867")
#' )
#'
#' res <- TrustRank(
#'   disease_genes = disease_genes,
#'   hub_penalty = 0.01,
#'   damping_factor = 0.95,
#'   target = "drug"
#' )
#' }
#'
#' @references
#' Gyongyi Z, Garcia-Molina H, Pedersen JO. Combating web spam with TrustRank.
#' In: \emph{Proceedings of the Thirtieth International Conference on Very Large
#' Data Bases}. 2004:576-587.
#'
#' Hartung M, Anastasi E, Mamdouh ZM, Nogales C, Schmidt HHHW, Baumbach J,
#' Zolotareva O, List M. Cancer driver drug interaction explorer.
#' \emph{Nucleic Acids Research}. 2022;50(W1):W138-W144.
#' \doi{10.1093/nar/gkac384}
#'
#' @importFrom reticulate py_run_file r_to_py
#' @importFrom dplyr filter mutate left_join select group_by ungroup distinct rename if_all pull
#' @importFrom tidyr drop_na
#' @export
setGeneric(
  "TrustRank",
  function(object = NULL,
           ppi_network = NULL,
           drug_target_network = NULL,
           disease_genes,
           hub_penalty = 0.01,
           damping_factor = 0.95,
           max_deg = NULL,
           result_size = NULL,
           target = "drug",
           include_indirect_drugs = TRUE,
           include_non_approved_drugs = TRUE,
           filter_paths = TRUE,
           force = FALSE,
           auth_token = NULL) {

    if (is.null(object)) {
      network_inputs <- resolve_network_inputs(ppi_network, drug_target_network, force = force, auth_token = auth_token)
      ppi_network <- network_inputs$ppi_network
      drug_target_network <- network_inputs$drug_target_network

      # Ensure max_deg default
      if (is.null(max_deg)) max_deg <- .Machine$integer.max

      if (is.null(result_size)) {
        result_size <- dplyr::n_distinct(drug_target_network$Drug)
      }

      if (!is.numeric(result_size) || length(result_size) != 1 || is.na(result_size) || !is.finite(result_size) || result_size < 1) {
        stop("`result_size` must be a single positive finite numeric value or NULL.", call. = FALSE)
      }

      result_size <- as.integer(result_size)

      object <- NetworkBased(
        result = data.frame(),
        ppi_network = ppi_network,
        drug_target_network = drug_target_network,
        disease_genes = disease_genes,
        target = target,
        include_indirect_drugs = include_indirect_drugs,
        include_non_approved_drugs = include_non_approved_drugs,
        filter_paths = filter_paths,
        hub_penalty = hub_penalty,
        damping_factor = damping_factor,
        max_deg = max_deg,
        result_size = result_size,
        method = "TrustRank"
      )
    }

    standardGeneric("TrustRank")
  }
)

#' @title Validate Inputs
#' @description Validates the required inputs for the TrustRank analysis.
#' @inheritParams TrustRank
#' @return Throws an error if inputs are invalid, otherwise invisible.
validateInputs <- function(object) {
  required <- c("ppi_network", "drug_target_network", "disease_genes")

  missing <- required[!vapply(required, function(x) !is.null(object@parameters[[x]]), logical(1))]
  if (length(missing) > 0) {
    stop("Missing required inputs: ", paste(missing, collapse = ", "))
  }

  # Basic schema checks (helpful error messages)
  validate_network_columns(object@parameters$ppi_network, c("gene1", "gene2"), "ppi_network")
  validate_network_columns(object@parameters$drug_target_network, c("ID", "Drug", "Target", "Group"), "drug_target_network")

  dg <- object@parameters$disease_genes
  if (!is.data.frame(dg) || !"gene" %in% names(dg)) {
    stop("`disease_genes` must be a data frame with a column named `gene`.")
  }

  if (is.null(object@parameters$max_deg)) {
    object@parameters$max_deg <- .Machine$integer.max
  }

  invisible(TRUE)
}

#' @title Generate Graph Files
#' @description Generates the necessary files for running the TrustRank algorithm.
#' @param temp_dir Temporary directory for storing intermediate files.
#' @inheritParams TrustRank
#' @return A list of file paths for the graph components.
generateGraphFiles <- function(temp_dir, ppi_network, drug_target_network, disease_genes) {
  graph_file_path <- file.path(temp_dir, "graph_network")

  cat("Generating PPI and Drug graphs...\n")
  graph_files <- createNodeToEdge(ppi_network, drug_target_network)

  cat("Filtering seed genes...\n")
  filtered_seed_genes <- disease_genes %>%
    dplyr::mutate(gene = as.character(gene)) %>%
    dplyr::filter(gene %in% as.character(graph_files$ppi_graph$gene_nodes$gene))

  cat("Generating complete graph network...\n")
  createGraphNetwork(
    drug_node_edge = graph_files$drug_graph$drug_edges,
    gene_node_edge = graph_files$ppi_graph$gene_edges,
    drug_node = graph_files$drug_graph$drug_nodes,
    gene_node = graph_files$ppi_graph$gene_nodes,
    file_name = graph_file_path
  )

  list(
    ppi_graph = graph_files$ppi_graph,
    drug_graph = graph_files$drug_graph,
    filtered_seed_genes = filtered_seed_genes,
    graph_file_path = graph_file_path
  )
}

#' @title Create Node-to-Edge Graphs
#' @description Creates node-to-edge mappings for PPI and drug-target networks.
#' @inheritParams TrustRank
#' @return A list containing PPI and drug graph components.
createNodeToEdge <- function(ppi_network, drug_target_network) {

  ppi_network <- ppi_network %>%
    tidyr::drop_na() %>%
    dplyr::filter(dplyr::if_all(dplyr::everything(), ~ . != ""))

  Drug_genes <- drug_target_network %>%
    dplyr::select(ID, Drug, Target, Group) %>%
    dplyr::filter(dplyr::if_all(dplyr::everything(), ~ . != "")) %>%
    tidyr::drop_na() %>%
    dplyr::pull(Target)

  gene_nodes <- data.frame(gene = unique(c(ppi_network$gene1, ppi_network$gene2, Drug_genes))) %>%
    dplyr::mutate(gene_graph_id = paste0("g", dplyr::row_number()))

  gene_edges <- ppi_network %>%
    dplyr::filter(gene1 %in% gene_nodes$gene & gene2 %in% gene_nodes$gene) %>%
    unique() %>%
    dplyr::left_join(gene_nodes, by = c("gene1" = "gene")) %>%
    dplyr::rename(g1_graph_id = gene_graph_id) %>%
    dplyr::left_join(gene_nodes, by = c("gene2" = "gene")) %>%
    dplyr::rename(g2_graph_id = gene_graph_id) %>%
    dplyr::mutate(graphId = paste(g1_graph_id, g2_graph_id, sep = "-")) %>%
    dplyr::select(g1 = gene1, g2 = gene2, g1_graph_id, g2_graph_id, graphId) %>%
    unique()

  drug_nodes <- drug_target_network %>%
    dplyr::select(ID, Drug, Group) %>%
    unique() %>%
    dplyr::filter(dplyr::if_all(dplyr::everything(), ~ . != "")) %>%
    tidyr::drop_na() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(drug_graph_id = paste0("d", dplyr::dense_rank(Drug))) %>%
    dplyr::ungroup()

  drug_edges <- drug_target_network %>%
    dplyr::select(ID, Drug, Target, Group) %>%
    dplyr::filter(dplyr::if_all(dplyr::everything(), ~ . != "")) %>%
    tidyr::drop_na() %>%
    dplyr::left_join(drug_nodes, by = c("ID", "Drug", "Group")) %>%
    dplyr::left_join(gene_nodes, by = c("Target" = "gene")) %>%
    dplyr::mutate(graphId = paste(drug_graph_id, gene_graph_id, sep = "-")) %>%
    dplyr::ungroup() %>%
    unique()

  list(
    ppi_graph = list(gene_nodes = gene_nodes, gene_edges = gene_edges),
    drug_graph = list(drug_nodes = drug_nodes, drug_edges = drug_edges)
  )
}

#' @title Create Complete Graph Network
#' @description Integrates PPI and drug-target graphs into a complete network.
#' @param drug_node_edge Drug node-to-edge mappings.
#' @param gene_node_edge Gene node-to-edge mappings.
#' @param drug_node Drug node data.
#' @param gene_node Gene node data.
#' @param file_name Output file name for the graph network.
createGraphNetwork <- function(drug_node_edge, gene_node_edge, drug_node, gene_node, file_name) {
  python_graph_script <- system.file("Python", "make_graphs.py", package = "DrugSigNet")
  if (!file.exists(python_graph_script)) {
    stop("Unable to find Python script: make_graphs.py in the package directory.")
  }
  py_module <- reticulate::py_run_file(python_graph_script)
  py_module$create_gt(drug_node_edge, gene_node_edge, drug_node, gene_node, file_name)
}

# --------------------------------------------------
# Method: TrustRank (NetworkBased)
#   - uses a dedicated writable work directory (avoids openxlsx tempdir failures)
#   - does NOT unlink(tempdir())
# --------------------------------------------------
#' @rdname TrustRank
setMethod(
  "TrustRank",
  signature = "NetworkBased",
  function(object) {

    validateInputs(object)
    cat("Running TrustRank network-based drug search method...\n")

    params <- object@parameters
    ppi_network <- params$ppi_network
    drug_target_network <- params$drug_target_network
    disease_genes <- params$disease_genes

    # Dedicated writable directory for this run
    work_dir <- tempfile("DrugSigNet_TrustRank_")
    dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(work_dir) || file.access(work_dir, 2) != 0) {
      stop("Could not create a writable temporary directory: ", work_dir)
    }
    on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

    files_path <- generateGraphFiles(
      temp_dir = work_dir,
      ppi_network = ppi_network,
      drug_target_network = drug_target_network,
      disease_genes = disease_genes
    )

    seed_genes <- disease_genes %>%
      dplyr::pull(gene) %>%
      as.character() %>%
      unique() %>%
      intersect(as.character(files_path$ppi_graph$gene_nodes$gene)) %>%
      as.list()
    seed_genes <- reticulate::r_to_py(seed_genes)

    trust_params <- list(
      include_indirect_drugs = reticulate::r_to_py(params$include_indirect_drugs),
      include_non_approved_drugs = reticulate::r_to_py(params$include_non_approved_drugs),
      filter_paths = reticulate::r_to_py(params$filter_paths),
      hub_penalty = reticulate::r_to_py(params$hub_penalty),
      damping_factor = reticulate::r_to_py(params$damping_factor),
      max_deg = reticulate::r_to_py(ifelse(is.null(params$max_deg), .Machine$integer.max, params$max_deg)),
      result_size = reticulate::r_to_py(params$result_size),
      target = params$target,
      ignored_edge_types = list()
    )

    TrustRank_script <- system.file("Python", "TrustRank.py", package = "DrugSigNet")
    if (!file.exists(TrustRank_script)) {
      stop("Unable to find Python script: TrustRank.py in the package directory.")
    }

    py <- reticulate::py_run_file(TrustRank_script)
    trust_rank_result <- py$trust_rank(
      file_path = paste0(files_path$graph_file_path, ".gt"),
      file_name = NULL,
      seeds = seed_genes,
      include_indirect_drugs = trust_params$include_indirect_drugs,
      include_non_approved_drugs = trust_params$include_non_approved_drugs,
      filter_paths = trust_params$filter_paths,
      max_deg = trust_params$max_deg,
      result_size = trust_params$result_size,
      hub_penalty = trust_params$hub_penalty,
      damping_factor = trust_params$damping_factor,
      target = trust_params$target,
      ignored_edge_types = trust_params$ignored_edge_types
    )

    #object@result <- trust_rank_result[[2]]
    object@result <- .clean_python_result(
      trust_rank_result[[2]]
    )
    object@parameters <- filterNetworkParameters(object)

    object
  }
)
