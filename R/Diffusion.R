#' @title Diffusion for Network-Based Drug Searching
#'
#' @description
#' Prioritizes candidate drugs using diffusion-based scoring on an integrated
#' protein-protein interaction and drug-target network.
#'
#' @details
#' Diffusion methods propagate signal from disease seed genes across a network so
#' that nearby nodes receive higher scores. In network-based drug repurposing,
#' this prioritizes drugs whose targets are close to disease-associated genes in
#' the integrated interactome.
#'
#' The integrated network is built from a protein-protein interaction network
#' (`ppi_network`) and a drug-target interaction network
#' (`drug_target_network`). Disease genes are used as seed nodes. If either
#' network is `NULL`, the corresponding high-confidence default DrugSigNet
#' network is loaded with `load_drugsignet_network()` from the local cache or
#' Synapse.
#'
#' Diffusion intermediate files are stored in `output_dir`. If the required
#' similarity matrices are missing, they are generated automatically and reused
#' in later analyses that use the same directory.
#'
#' This implementation follows the network medicine framework used to identify
#' drug-repurposing opportunities for COVID-19.
#'
#' @inheritParams Degree_centrality
#' @param ties_method Method used to resolve ties in diffusion-based rankings.
#'   Default is `"max"`.
#' @param output_dir Directory used to store diffusion similarity matrices and
#'   related intermediate files. Defaults to `tempdir()`.
#'
#' @return
#' A `NetworkBased` object containing diffusion results in `object@result` and
#' method parameters in `object@parameters`. The resolved output directory is
#' stored in `object@parameters$output_dir`.
#'
#' @examples
#' \dontrun{
#' disease_genes <- data.frame(
#'   gene = c("ENSG00000130203", "ENSG00000142192", "ENSG00000171867")
#' )
#'
#' # Run with default DrugSigNet networks
#' res <- Diffusion(
#'   disease_genes = disease_genes,
#'   output_dir = tempdir()
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
#' res_custom <- Diffusion(
#'   ppi_network = ppi_network,
#'   drug_target_network = drug_target_network,
#'   disease_genes = disease_genes,
#'   ties_method = "max",
#'   output_dir = tempdir()
#' )
#' }
#'
#' @references
#' Gysi DM, do Valle Í, Zitnik M, Ameli A, Gan X, Varol O, Ghiassian SD,
#' Patten JJ, Davey RA, Loscalzo J, Barabási AL. Network medicine framework for
#' identifying drug-repurposing opportunities for COVID-19. \emph{Proceedings of
#' the National Academy of Sciences of the United States of America}.
#' 2021;118(19):e2025581118. \doi{10.1073/pnas.2025581118}
#'
#' @importFrom reticulate r_to_py py_run_file
#' @importFrom dplyr pull select rename left_join everything
#' @importFrom openxlsx write.xlsx
#' @importFrom utils write.csv
#' @importFrom stats na.omit
#' @export
setGeneric("Diffusion", function(object = NULL, ppi_network = NULL, drug_target_network = NULL, disease_genes, include_indirect_drugs = TRUE, include_non_approved_drugs = TRUE, ties_method = "max", output_dir = tempdir(),
                                 force = FALSE, auth_token = NULL) {
  network_inputs <- resolve_network_inputs(ppi_network, drug_target_network, force = force, auth_token = auth_token)
  ppi_network <- network_inputs$ppi_network
  drug_target_network <- network_inputs$drug_target_network
  drug_target_network <- .filter_drug_target_network_by_flags(
    drug_target_network = drug_target_network,
    disease_genes = disease_genes,
    include_indirect_drugs = include_indirect_drugs,
    include_non_approved_drugs = include_non_approved_drugs
  )

  if (is.null(object)) {
    # Create a NetworkBased object if not provided
    object <- NetworkBased(
      ppi_network = ppi_network,
      drug_target_network = drug_target_network,
      disease_genes = disease_genes,
      method = "Diffusion",
      include_indirect_drugs = include_indirect_drugs,
      include_non_approved_drugs = include_non_approved_drugs,
      ties_method = ties_method,
      output_dir = output_dir
    )
  }
  standardGeneric("Diffusion")
})


#' @rdname Diffusion
setMethod("Diffusion", signature = "NetworkBased", function(object) {
  validateInputs(object)
  work_dir <- create_temp_work_dir()
  on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)
  cat("Running Diffusion network-based drug search method...\n")

  ensure_diffusion_files <- function(files_path, work_dir) {
    ppi_graph_file <- files_path$ppi_graph_file
    drug_graph_file <- files_path$drug_graph_file
    seed_genes_file <- files_path$seed_genes_file

    if (is.null(ppi_graph_file) || !is.character(ppi_graph_file) || !nzchar(ppi_graph_file) || !file.exists(ppi_graph_file) ||
        is.null(drug_graph_file) || !is.character(drug_graph_file) || !nzchar(drug_graph_file) || !file.exists(drug_graph_file)) {
      graph_excel_path <- file.path(work_dir, "graph_network.xlsx")
      openxlsx::write.xlsx(
        x = list(
          gene_edges = files_path$ppi_graph$gene_edges,
          drug_edges = files_path$drug_graph$drug_edges
        ),
        file = graph_excel_path,
        overwrite = TRUE
      )
      ppi_graph_file <- graph_excel_path
      drug_graph_file <- graph_excel_path
    }

    if (is.null(seed_genes_file) || !is.character(seed_genes_file) || !nzchar(seed_genes_file) || !file.exists(seed_genes_file)) {
      seed_genes_file <- file.path(work_dir, "seed_genes.csv")
      utils::write.csv(
        x = files_path$filtered_seed_genes %>% dplyr::select(gene),
        file = seed_genes_file,
        row.names = FALSE,
        quote = TRUE
      )
    }

    list(
      ppi_graph_file = ppi_graph_file,
      drug_graph_file = drug_graph_file,
      seed_genes_file = seed_genes_file
    )
  }

  # Extract parameters
  params <- object@parameters
  ppi_network <- params$ppi_network
  drug_target_network <- params$drug_target_network
  disease_genes <- params$disease_genes

  out_path <- params$output_dir
  if (is.null(out_path) || !is.character(out_path) || length(out_path) != 1 || !nzchar(out_path)) {
    out_path <- work_dir
  }
  if (!dir.exists(out_path)) {
    dir.create(out_path, recursive = TRUE, showWarnings = FALSE)
  }

  # Generate graph files
  files_path <- generateGraphFiles(
    temp_dir = work_dir,
    ppi_network = params$ppi_network,
    drug_target_network = params$drug_target_network,
    disease_genes = params$disease_genes
  )

  diffusion_files <- ensure_diffusion_files(files_path, work_dir)

  seed_genes <- files_path$filtered_seed_genes %>%
    dplyr::pull(gene) %>%
    as.character() %>%
    unique() %>%
    as.list()

  # Prepare parameters for Python
  diffusion_params <- list(
    ties_method = reticulate::r_to_py(params$ties_method)
  )

  # Define the required pickle files
  required_pickle_files <- file.path(out_path, c(
    "ppi_id_2_node.p",
    "ppi_node_2_id.p",
    "PPI_DSD_100.p",
    "PPI_KL_100.p",
    "PPI_JS_100.p"
  ))

  # Check if all required files exist
  if (!all(file.exists(required_pickle_files))) {
    paste(
      " The following required pickle files are missing:",
      paste(required_pickle_files[!file.exists(required_pickle_files)], collapse = ", ")
    )

    # # Locate the Python script dynamically
    python_script_path <- system.file("Python", "Diffusion_matrix_generator.py", package = "DrugSigNet")
    if (!file.exists(python_script_path)) {
      stop("Unable to find Python script: Diffusion_matrix_generator.py in the package directory.")
    }

    cat("Generating similarity matrices for the PPI network...\n")
    process_ppi <- reticulate::py_run_file(python_script_path)

    num_random_walks <- reticulate::r_to_py(as.integer(100))

    generated_files <- process_ppi$process_ppi_network(ppi_file = diffusion_files$ppi_graph_file, num_random_walks = num_random_walks)

    file.copy(generated_files$id2node, file.path(out_path, "ppi_id_2_node.p"), overwrite = TRUE)
    file.copy(generated_files$node2id, file.path(out_path, "ppi_node_2_id.p"), overwrite = TRUE)
    file.copy(generated_files$dsd_matrix, file.path(out_path, "PPI_DSD_100.p"), overwrite = TRUE)
    file.copy(generated_files$kl_matrix, file.path(out_path, "PPI_KL_100.p"), overwrite = TRUE)
    file.copy(generated_files$js_matrix, file.path(out_path, "PPI_JS_100.p"), overwrite = TRUE)
  }

  # Locate the Python script dynamically
  python_script2_path <- system.file("Python", "Diffusion_run_pipelines.py", package = "DrugSigNet")
  if (!file.exists(python_script2_path)) {
    stop("Unable to find Python script: Diffusion_run_pipelines.py in the package directory.")
  }

  # Load the Python function for Diffusion-Based method
  py <- reticulate::py_run_file(python_script2_path)

  # Run the diffusion-based pipelines using Python
  Diffusion_methods_results <- py$Diff_run_pipelines(
    drug_edges_df = files_path$drug_graph$drug_edges,
    seed_genes = seed_genes,
    gene_edges_df = files_path$ppi_graph$gene_edges,
    ties_method = diffusion_params$ties_method,
    path = out_path
  ) %>%
    dplyr::rename("ID" = "Drug") %>%
    left_join(., unique(params$drug_target_network[, c("ID","Drug")])) %>%
    dplyr::select(-ID) %>%
    dplyr::select(Drug, everything()) %>%
    na.omit() %>%
    unique()

  # Check for successful results generation
  if (is.null(Diffusion_methods_results)) {
    stop("Error: Diffusion-based method did not return results.")
  }

  cat("Diffusion-Based search completed successfully.\n")
  object@result <- Diffusion_methods_results
  object@parameters$output_dir <- out_path
  object@parameters <- filterNetworkParameters(object)

  return(object)
})
