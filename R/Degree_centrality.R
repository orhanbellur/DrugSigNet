#' @title Degree Centrality for Network-Based Drug Searching
#'
#' @description
#' Prioritizes candidate drugs or target nodes using degree centrality on an
#' integrated protein-protein interaction and drug-target network.
#'
#' @details
#' Degree centrality is a local network measure that ranks nodes by their number
#' of direct connections. In network-based drug repurposing, it can prioritize
#' drugs or targets connected to the disease module in the integrated
#' interactome.
#'
#' The integrated network is built from a protein-protein interaction network
#' (`ppi_network`) and a drug-target interaction network
#' (`drug_target_network`). Disease genes are used as seed nodes. If either
#' network is `NULL`, the corresponding high-confidence default DrugSigNet
#' network is loaded with `load_drugsignet_network()` from the local cache or
#' Synapse.
#'
#' This implementation follows the network-based drug search setting used in
#' CADDIE, where disease seed genes are analyzed on a combined interactome of
#' protein-protein and drug-target interactions.
#'
#' @param object Optional `NetworkBased` object. If `NULL`, a new object is
#'   created from the supplied network inputs and method parameters.
#' @param ppi_network Protein-protein interaction network. Expected to contain
#'   columns `gene1` and `gene2`. If `NULL`, the default `"gene_gene"` network
#'   is loaded.
#' @param drug_target_network Drug-target interaction network. Expected to
#'   contain columns `ID`, `Drug`, `Target`, and `Group`. If `NULL`, the default
#'   `"drug_target"` network is loaded.
#' @param disease_genes Data frame containing disease seed genes in a column
#'   named `gene`.
#' @param max_deg Maximum allowed node degree. Nodes with degree greater than
#'   this value are excluded. If `NULL`, defaults to `.Machine$integer.max`.
#' @param result_size Number of top-ranked results to return. If `NULL`, it is
#'   inferred from the number of unique drugs in `drug_target_network`.
#' @param target Node type to prioritize. Default is `"drug"`.
#' @param include_indirect_drugs Logical; whether to include drugs indirectly
#'   connected to disease genes. Default is `TRUE`.
#' @param include_non_approved_drugs Logical; whether to include non-approved or
#'   investigational drugs. Default is `TRUE`.
#' @param filter_paths Logical; whether to restrict graph paths according to the
#'   backend implementation. Default is `TRUE`.
#' @param force Logical; force redownload of default networks from Synapse
#'   instead of using the local cache.
#' @param auth_token Optional Synapse authentication token. If `NULL`, the
#'   `SYNAPSE_AUTH_TOKEN` environment variable is used.
#'
#' @return
#' A `NetworkBased` object containing degree centrality results in
#' `object@result` and method parameters in `object@parameters`.
#'
#' @examples
#' \dontrun{
#' disease_genes <- data.frame(
#'   gene = c("ENSG00000130203", "ENSG00000142192", "ENSG00000171867")
#' )
#'
#' # Run with default DrugSigNet networks
#' res <- Degree_centrality(
#'   disease_genes = disease_genes,
#'   target = "drug"
#' )
#'
#' # Run with user-provided networks
#' ppi_network <- data.frame(
#'   gene1 = c("ENSG00000130203", "ENSG00000142192"),
#'   gene2 = c("ENSG00000171867", "ENSG00000198786")
#' )
#'
#' drug_target_network <- data.frame(
#'   ID = c("DB0001", "DB0002"),
#'   Drug = c("drug_a", "drug_b"),
#'   Target = c("ENSG00000130203", "ENSG00000171867"),
#'   Group = c("approved", "approved")
#' )
#'
#' res_custom <- Degree_centrality(
#'   ppi_network = ppi_network,
#'   drug_target_network = drug_target_network,
#'   disease_genes = disease_genes,
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
setGeneric("Degree_centrality", function(object = NULL, ppi_network = NULL, drug_target_network = NULL, disease_genes,
                                         max_deg = NULL, result_size = NULL, target = "drug",
                                         include_indirect_drugs = TRUE, include_non_approved_drugs = TRUE,
                                         filter_paths = TRUE, force = FALSE, auth_token = NULL) {
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
      result = data.frame(),
      ppi_network = ppi_network,
      drug_target_network = drug_target_network,
      disease_genes = disease_genes,
      max_deg = max_deg,
      result_size = result_size,
      target = target,
      include_indirect_drugs = include_indirect_drugs,
      include_non_approved_drugs = include_non_approved_drugs,
      filter_paths = filter_paths,
      method = "Degree_centrality"
    )
  }
  standardGeneric("Degree_centrality")
})

#' @describeIn Degree_centrality Implements the Degree Centrality calculation for the NetworkCentrality object.
setMethod("Degree_centrality", signature = "NetworkBased", function(object) {
  # Validate inputs
  validateInputs(object)
  work_dir <- create_temp_work_dir()
  on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)
  cat("Running Degree Centrality network-based drug search method...\n")

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
  degree_params <- list(
    include_indirect_drugs = reticulate::r_to_py(params$include_indirect_drugs),
    include_non_approved_drugs = reticulate::r_to_py(params$include_non_approved_drugs),
    filter_paths = reticulate::r_to_py(params$filter_paths),
    max_deg = reticulate::r_to_py(ifelse(is.null(params$max_deg), .Machine$integer.max, params$max_deg)),
    result_size = reticulate::r_to_py(params$result_size),
    target = params$target,
    ignored_edge_types = list()
  )

  # Locate the Python script dynamically
  DegreeCentrality_script <- system.file("Python", "Degree_centrality.py", package = "DrugSigNet")
  if (!file.exists(DegreeCentrality_script)) {
    stop("Unable to find Python script: Degree_centrality.py in the package directory.")
  }


  # Execute the Degree Centrality algorithm via Python
  py <- reticulate::py_run_file(DegreeCentrality_script)
  degree_centrality_result <- py$degree_centrality(
    file_path = paste0(files_path$graph_file_path, ".gt"),
    file_name = NULL,
    seeds = seed_genes,
    include_indirect_drugs = degree_params$include_indirect_drugs,
    include_non_approved_drugs = degree_params$include_non_approved_drugs,
    filter_paths = degree_params$filter_paths,
    max_deg = degree_params$max_deg,
    result_size = degree_params$result_size,
    target = degree_params$target,
    ignored_edge_types = degree_params$ignored_edge_types
  )

  # Update the result and parameters in the object
  #object@result <- degree_centrality_result[[2]]
  object@result <- .clean_python_result(
    degree_centrality_result[[2]]
  )
  object@parameters <- filterNetworkParameters(object)

  return(object)
})
