#' @title Harmonic Centrality for Network-Based Drug Searching
#'
#' @description
#' Prioritizes candidate drugs or target nodes using harmonic centrality on an
#' integrated protein-protein interaction and drug-target network.
#'
#' @details
#' Harmonic centrality scores nodes by summing inverse shortest-path distances
#' to reachable nodes. Unlike classical closeness centrality, it remains defined
#' in disconnected graphs, making it suitable for fragmented biological networks.
#'
#' The integrated network is built from a protein-protein interaction network
#' (`ppi_network`) and a drug-target interaction network
#' (`drug_target_network`). Disease genes are used as seed nodes to identify
#' drugs or targets close to the disease module. If either network is `NULL`,
#' the corresponding high-confidence default DrugSigNet network is loaded with
#' `load_drugsignet_network()` from the local cache or Synapse.
#'
#' This implementation follows the network-based drug search setting used in
#' CADDIE, where disease seed genes are analyzed on a combined interactome of
#' protein-protein and drug-target interactions.
#'
#' @inheritParams Degree_centrality
#' @param hub_penalty Numeric hub penalty used to reduce the influence of highly
#'   connected nodes. Default is `0.01`.
#'
#' @return
#' A `NetworkBased` object containing harmonic centrality results in
#' `object@result` and method parameters in `object@parameters`.
#'
#' @examples
#' \dontrun{
#' disease_genes <- data.frame(
#'   gene = c("ENSG00000130203", "ENSG00000142192", "ENSG00000171867")
#' )
#'
#' res <- Harmonic_centrality(
#'   disease_genes = disease_genes,
#'   hub_penalty = 0.01,
#'   target = "drug"
#' )
#' }
#'
#' @references
#' Hartung M, Anastasi E, Mamdouh ZM, Nogales C, Schmidt HHHW, Baumbach J,
#' Zolotareva O, List M. Cancer driver drug interaction explorer.
#' \emph{Nucleic Acids Research}. 2022;50(W1):W138-W144.
#' \doi{10.1093/nar/gkac384}
#'
#' @importFrom reticulate r_to_py py_run_file
#' @importFrom dplyr pull
#' @export
setGeneric("Harmonic_centrality", function(object = NULL, ppi_network = NULL, drug_target_network = NULL, disease_genes,
                                           hub_penalty = 0.01,
                                           max_deg = NULL, result_size = NULL,
                                           target = "drug", include_indirect_drugs = TRUE,
                                           include_non_approved_drugs = TRUE, filter_paths = TRUE,
                                           force = FALSE, auth_token = NULL) {
  if (is.null(object)) {
    network_inputs <- resolve_network_inputs(ppi_network, drug_target_network, force = force, auth_token = auth_token)
    ppi_network <- network_inputs$ppi_network
    drug_target_network <- network_inputs$drug_target_network

    if (is.null(max_deg)) max_deg <- .Machine$integer.max

    if (is.null(result_size)) {
      result_size <- dplyr::n_distinct(drug_target_network$Drug)
    }

    if (!is.numeric(result_size) || length(result_size) != 1 || is.na(result_size) || !is.finite(result_size) || result_size < 1) {
      stop("`result_size` must be a single positive finite numeric value or NULL.", call. = FALSE)
    }

    result_size <- as.integer(result_size)

    # Create a NetworkBased object if not provided
    object <- NetworkBased(
      ppi_network = ppi_network,
      drug_target_network = drug_target_network,
      disease_genes = disease_genes,
      method = "Harmonic_centrality",
      hub_penalty = hub_penalty,
      max_deg = max_deg,
      result_size = result_size,
      target = target,
      include_indirect_drugs = include_indirect_drugs,
      include_non_approved_drugs = include_non_approved_drugs,
      filter_paths = filter_paths
    )
  }
  standardGeneric("Harmonic_centrality")
})

#' @describeIn Harmonic_centrality Implements the Harmonic Centrality calculation for the NetworkCentrality object.
setMethod("Harmonic_centrality", signature = "NetworkBased", function(object) {
  # Validate inputs
  validateInputs(object)
  work_dir <- create_temp_work_dir()
  on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)
  cat("Running Harmonic centrality network-based drug search method...\n")

  # Extract parameters
  params <- object@parameters
  ppi_network <- params$ppi_network
  drug_target_network <- params$drug_target_network
  disease_genes <- params$disease_genes

  # Generate graph files
  files_path <- generateGraphFiles(
    temp_dir = work_dir,
    ppi_network = ppi_network,
    drug_target_network = drug_target_network,
    disease_genes = disease_genes
  )


  # Prepare seed genes for Python processing directly from R input
  seed_genes <- disease_genes %>%
    dplyr::pull(gene) %>%
    as.character() %>%
    unique() %>%
    intersect(as.character(files_path$ppi_graph$gene_nodes$gene)) %>%
    as.list()
  seed_genes <- reticulate::r_to_py(seed_genes)

  # Prepare parameters for Python
  params <- list(
    include_indirect_drugs = reticulate::r_to_py(params$include_indirect_drugs),
    include_non_approved_drugs = reticulate::r_to_py(params$include_non_approved_drugs),
    filter_paths = reticulate::r_to_py(params$filter_paths),
    hub_penalty = reticulate::r_to_py(params$hub_penalty),
    max_deg = reticulate::r_to_py(ifelse(is.null(params$max_deg), .Machine$integer.max, params$max_deg)),
    result_size = reticulate::r_to_py(params$result_size),
    target = params$target,
    ignored_edge_types = list()
  )

  # Locate the Python script dynamically
  HarmonicCentrality_script <- system.file("Python", "Harmonic_centrality.py", package = "DrugSigNet")
  if (!file.exists(HarmonicCentrality_script)) {
    stop("Unable to find Python script: Harmonic_centrality.py in the package directory.")
  }


  # Run the Python script for harmonic centrality
  py <- reticulate::py_run_file(HarmonicCentrality_script)

  harmonic_centrality_result <- py$harmonic_centrality(
    file_path = paste0(files_path$graph_file_path, ".gt"),
    file_name = NULL,
    seeds = seed_genes,
    include_indirect_drugs = params$include_indirect_drugs,
    include_non_approved_drugs = params$include_non_approved_drugs,
    filter_paths = params$filter_paths,
    max_deg = params$max_deg,
    result_size = params$result_size,
    hub_penalty = params$hub_penalty,
    target = params$target,
    ignored_edge_types = params$ignored_edge_types
  )

  #object@result <- harmonic_centrality_result[[2]]
  object@result <- .clean_python_result(
    harmonic_centrality_result[[2]]
  )
  object@parameters <- filterNetworkParameters(object)

  return(object)
})
