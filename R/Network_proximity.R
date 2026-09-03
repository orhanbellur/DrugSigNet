#' @title Network Proximity for Network-Based Drug Searching
#'
#' @description
#' Prioritizes candidate drugs using network proximity between drug targets and
#' disease genes in an integrated interactome.
#'
#' @details
#' Network proximity measures how close drug targets are to disease-associated
#' genes in the interactome. Drugs whose targets are closer to the disease module
#' are prioritized as candidates for repurposing or follow-up analysis.
#'
#' The integrated network is built from a protein-protein interaction network
#' (`ppi_network`) and a drug-target interaction network
#' (`drug_target_network`). Disease genes define the disease module. If either
#' network is `NULL`, the corresponding high-confidence default DrugSigNet
#' network is loaded with `load_drugsignet_network()` from the local cache or
#' Synapse.
#'
#' The method estimates a background distribution using random simulations and
#' ranks drugs by their proximity to the supplied disease genes.
#'
#' This implementation follows the network medicine framework used to identify
#' drug-repurposing opportunities for COVID-19.
#'
#' @inheritParams Degree_centrality
#' @param n_simulations Number of random simulations used to estimate the
#'   proximity background distribution. Default is `1000`.
#' @param n_workers Number of CPU workers used for the simulation procedure.
#'   Default is `1`.
#' @param random_seed Integer random seed used for reproducible simulations.
#'   Default is `42`.
#'
#' @return
#' A `NetworkBased` object containing network proximity results in
#' `object@result` and method parameters in `object@parameters`.
#'
#' @examples
#' \dontrun{
#' disease_genes <- data.frame(
#'   gene = c("ENSG00000130203", "ENSG00000142192", "ENSG00000171867")
#' )
#'
#' # Run with default DrugSigNet networks
#' res <- Network_proximity(
#'   disease_genes = disease_genes,
#'   n_simulations = 1000,
#'   n_workers = 1,
#'   random_seed = 42
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
#' res_custom <- Network_proximity(
#'   ppi_network = ppi_network,
#'   drug_target_network = drug_target_network,
#'   disease_genes = disease_genes,
#'   n_simulations = 1000,
#'   n_workers = 1,
#'   random_seed = 42
#' )
#' }
#'
#' @references
#' Morselli Gysi D, do Valle Í, Zitnik M, Ameli A, Gan X, Varol O,
#' Ghiassian SD, Patten JJ, Davey RA, Loscalzo J, Barabási AL. Network
#' medicine framework for identifying drug-repurposing opportunities for
#' COVID-19. \emph{Proceedings of the National Academy of Sciences of the
#' United States of America}. 2021;118(19):e2025581118.
#' \doi{10.1073/pnas.2025581118}
#'
#' @importFrom reticulate r_to_py py_run_file
#' @importFrom dplyr pull rename left_join select everything
#' @importFrom stats na.omit
#' @export
setGeneric("Network_proximity", function(object = NULL, ppi_network = NULL, drug_target_network = NULL, disease_genes,
                                         include_indirect_drugs = TRUE, include_non_approved_drugs = TRUE, n_simulations = 1000L, n_workers = 1L,
                                         result_size = NULL, random_seed = 42L, force = FALSE, auth_token = NULL) {
  network_inputs <- resolve_network_inputs(ppi_network, drug_target_network, force = force, auth_token = auth_token)
  ppi_network <- network_inputs$ppi_network
  drug_target_network <- network_inputs$drug_target_network
  drug_target_network <- .filter_drug_target_network_by_flags(
    drug_target_network = drug_target_network,
    disease_genes = disease_genes,
    include_indirect_drugs = include_indirect_drugs,
    include_non_approved_drugs = include_non_approved_drugs
  )
  if (is.null(result_size)) {
    drug_col <- if ("Drug" %in% names(drug_target_network)) "Drug" else if ("ID" %in% names(drug_target_network)) "ID" else NULL
    result_size <- if (!is.null(drug_col)) {
      length(unique(stats::na.omit(drug_target_network[[drug_col]])))
    } else {
      nrow(drug_target_network)
    }
  }
  if (!is.numeric(result_size) || length(result_size) != 1 || is.na(result_size) || result_size < 1) {
    stop("`result_size` must be a single positive numeric value or NULL.", call. = FALSE)
  }
  result_size <- as.integer(result_size)
  n_workers <- as.integer(n_workers)

  if (is.null(object)) {
    # Create a NetworkBased object if not provided
    object <- NetworkBased(
      ppi_network = ppi_network,
      drug_target_network = drug_target_network,
      disease_genes = disease_genes,
      method = "Network_proximity",
      include_indirect_drugs = include_indirect_drugs,
      include_non_approved_drugs = include_non_approved_drugs,
      n_simulations = n_simulations,
      ncpus = n_workers,
      start = 0L,
      end = result_size,
      random_seed = random_seed
    )
  }
  standardGeneric("Network_proximity")
})

#' @title Network Proximity Method for Network-Based Drug Search
#' @description
#' Method for the Network Proximity drug search using a network-based proximity algorithm.
#' The method performs simulations and ranks drugs based on their proximity to disease genes.
#'
#' @param object A `NetworkBased` object containing the networks, disease genes,
#'   and network-proximity parameters.
#' @return The same \code{NetworkBased} object with the results of the Network Proximity analysis.
#'
#' @export
setMethod("Network_proximity", signature = "NetworkBased", function(object) {
  # Validate inputs
  validateInputs(object)
  work_dir <- create_temp_work_dir()
  on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)
  cat("Running Network proximity network-based drug search method...\n")

  # Extract parameters
  params <- object@parameters
  ppi_network <- params$ppi_network
  drug_target_network <- params$drug_target_network
  disease_genes <- params$disease_genes

  # Generate graph files
  files_path <- generateGraphFiles(
    temp_dir = work_dir,
    ppi_network = params$ppi_network,
    drug_target_network = params$drug_target_network,
    disease_genes = params$disease_genes
  )

  seed_genes <- disease_genes %>%
    dplyr::pull(gene) %>%
    as.character() %>%
    unique() %>%
    intersect(as.character(files_path$ppi_graph$gene_nodes$gene)) %>%
    as.list()

  # Prepare parameters for Python
  params <- list(
    include_indirect_drugs = reticulate::r_to_py(params$include_indirect_drugs),
    include_non_approved_drugs = reticulate::r_to_py(params$include_non_approved_drugs),
    n_simulations = reticulate::r_to_py(as.integer(params$n_simulations)),
    ncpus = reticulate::r_to_py(as.integer(params$ncpus)),
    start = reticulate::r_to_py(as.integer(params$start)),
    end = reticulate::r_to_py(as.integer(params$end)),
    random_seed = reticulate::r_to_py(as.integer(params$random_seed))
  )

  # Locate the Python script dynamically
  NetworkProximity_script <- system.file("Python", "Network_proximity.py", package = "DrugSigNet")
  if (!file.exists(NetworkProximity_script)) {
    stop("Unable to find Python script: Network_proximity.py in the package directory.")
  }

  # Load and run the Network Proximity Python function
  py <- reticulate::py_run_file(NetworkProximity_script)
  Network_Proximity_result <- py$Network_Proximity(
    disease_genes = seed_genes,
    drug_edges_df = files_path$drug_graph$drug_edges,
    gene_edges_df = files_path$ppi_graph$gene_edges,
    nsims = params$n_simulations,
    ncpus = params$ncpus,
    start = params$start,
    end = params$end,
    random_seed = params$random_seed
  ) %>% dplyr::rename("ID" = "Drug")

  Network_Proximity_result <- dplyr::left_join(
    Network_Proximity_result,
    unique(drug_target_network[, c("ID", "Drug")]),
    by = "ID"
  ) %>%
    dplyr::select(-ID) %>%
    dplyr::select(Drug,everything()) %>%
    unique() %>%
    na.omit()

  object@result <- Network_Proximity_result
  object@parameters <- filterNetworkParameters(object)

  return(object)
})
