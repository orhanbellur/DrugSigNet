#' @title Optimize Parameters for DrugSigNet Ranking Methods
#'
#' @description
#' Performs grid-based parameter optimization for selected DrugSigNet ranking
#' and rank aggregation methods.
#'
#' @details
#' `optimize_parameters()` evaluates parameter combinations for one selected
#' method and returns the corresponding ranked results. It supports network
#' methods (`TrustRank` and `Harmonic_centrality`) and the CRank rank
#' aggregation method.
#'
#' For `TrustRank`, the grid is defined by `hub_penalty` and `damping_factor`.
#' For `Harmonic_centrality`, the grid is defined by `hub_penalty`. For
#' `CRank`, the grid is defined by `prior` and `num_bin`.
#'
#' Network methods use the resolved PPI and drug-target networks and require
#' `disease_genes`. If either network is `NULL`, default DrugSigNet networks can
#' be loaded from the local cache or Synapse. Because network methods rely on
#' Python/reticulate graph backends, they are run sequentially for stability.
#' CRank optimization can use multiple workers.
#'
#' @param method Ranking method to optimize. One of `"TrustRank"`,
#'   `"Harmonic_centrality"`, or `"CRank"`.
#' @param input_data Data frame used for CRank rank aggregation. Required when
#'   `method = "CRank"`.
#' @inheritParams TrustRank
#' @inheritParams CRank
#' @param hub_penalty Numeric vector of hub penalty values to test for network
#'   methods. Default is `seq(0, 1, 0.1)`.
#' @param damping_factor Numeric vector of damping factors to test for
#'   `TrustRank`. Default is `seq(0, 1, 0.1)`.
#' @param prior Numeric vector of CRank prior values to test. Default is
#'   `seq(0.001, 0.1, 0.001)`.
#' @param num_bin Integer vector of CRank bin counts to test. Default is
#'   `seq(100, 1000, 100)`.
#' @param cancer_drugs Optional vector of disease-relevant drugs reserved for
#'   benchmarking workflows.
#' @param positive_drugs Optional vector of positive-control drugs reserved for
#'   benchmarking workflows.
#' @param negative_drugs Optional vector of negative-control drugs reserved for
#'   benchmarking workflows.
#' @param n_workers Number of parallel workers. Network methods are run
#'   sequentially for Python/reticulate backend stability; CRank optimization can
#'   use multiple workers. Default is `8`.
#'
#' @return
#' A named list of optimization results. Each element contains the parameter
#' label, tested parameter values, and the corresponding method result.
#'
#' @examples
#' \dontrun{
#' rank_df <- data.frame(
#'   Drug = c("drug_a", "drug_b", "drug_c"),
#'   Method1 = c(1, 2, 3),
#'   Method2 = c(2, 1, 3)
#' )
#'
#' crank_grid <- optimize_parameters(
#'   method = "CRank",
#'   input_data = rank_df,
#'   prior = c(0.01, 0.05, 0.093),
#'   num_bin = c(100, 200),
#'   reverse = TRUE,
#'   n_workers = 2
#' )
#'
#' disease_genes <- data.frame(
#'   gene = c("ENSG00000130203", "ENSG00000142192")
#' )
#'
#' trust_grid <- optimize_parameters(
#'   method = "TrustRank",
#'   disease_genes = disease_genes,
#'   hub_penalty = c(0, 0.01, 0.1),
#'   damping_factor = c(0.85, 0.95),
#'   result_size = 100
#' )
#' }
#'
#' @export
optimize_parameters <- function(method = c("TrustRank", "Harmonic_centrality", "CRank"),
                                input_data = NULL,
                                ppi_network = NULL,
                                drug_target_network = NULL,
                                disease_genes = NULL,
                                hub_penalty = seq(0, 1, 0.1),
                                damping_factor = seq(0, 1, 0.1),
                                prior = seq(0.001, 0.1, 0.001),
                                num_bin = seq(100, 1000, 100),
                                num_iter = 1000,
                                ties_method = "max",
                                reverse = FALSE,
                                cancer_drugs = NULL,
                                positive_drugs = NULL,
                                negative_drugs = NULL,
                                result_size = NULL,
                                n_workers = 8,
                                force = FALSE,
                                auth_token = NULL) {

  method <- match.arg(method)

  if (!is.numeric(n_workers) || length(n_workers) != 1 || is.na(n_workers) || n_workers < 1) {
    stop("`n_workers` must be a positive integer.", call. = FALSE)
  }
  n_workers <- as.integer(n_workers)

  if (method == "CRank") {
    if (is.null(input_data) || !is.data.frame(input_data)) {
      stop("`input_data` must be a data frame when `method = \"CRank\"`.", call. = FALSE)
    }
    if (!is.numeric(prior) || length(prior) == 0 || any(is.na(prior))) {
      stop("`prior` must be a non-empty numeric vector for CRank optimization.", call. = FALSE)
    }
    if (!is.numeric(num_bin) || length(num_bin) == 0 || any(is.na(num_bin))) {
      stop("`num_bin` must be a non-empty numeric vector for CRank optimization.", call. = FALSE)
    }
    if (!is.numeric(num_iter) || length(num_iter) != 1 || is.na(num_iter) || num_iter < 1) {
      stop("`num_iter` must be a positive integer.", call. = FALSE)
    }

    param_grid <- expand.grid(
      prior = prior,
      num_bin = as.integer(num_bin),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
  } else {
    network_inputs <- resolve_network_inputs(ppi_network, drug_target_network, force = force, auth_token = auth_token)
    ppi_network <- network_inputs$ppi_network
    drug_target_network <- network_inputs$drug_target_network

    if (is.null(disease_genes)) {
      stop("`disease_genes` must be provided for network method optimization.", call. = FALSE)
    }
    if (!is.numeric(hub_penalty) || length(hub_penalty) == 0 || any(is.na(hub_penalty))) {
      stop("`hub_penalty` must be a non-empty numeric vector.", call. = FALSE)
    }
    if (method == "TrustRank" && (!is.numeric(damping_factor) || length(damping_factor) == 0 || any(is.na(damping_factor)))) {
      stop("`damping_factor` must be a non-empty numeric vector for TrustRank optimization.", call. = FALSE)
    }

    if (is.null(result_size)) result_size <- length(unique(drug_target_network$Drug))

    if (method == "TrustRank") {
      param_grid <- expand.grid(
        hub = hub_penalty,
        damping = damping_factor,
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
      )
    } else {
      param_grid <- data.frame(hub = hub_penalty)
    }
  }

  run_benchmark <- function(params) {
    tryCatch({
      if (method == "TrustRank") {
        res <- TrustRank(
          ppi_network = ppi_network,
          drug_target_network = drug_target_network,
          disease_genes = disease_genes,
          hub_penalty = params$hub,
          damping_factor = params$damping,
          max_deg = .Machine$integer.max,
          result_size = result_size,
          target = "drug",
          include_indirect_drugs = TRUE,
          include_non_approved_drugs = TRUE,
          filter_paths = TRUE
        )
        param_name <- paste0("TrustRank_damp_", params$damping, "_hub_", params$hub)
      } else if (method == "Harmonic_centrality") {
        res <- Harmonic_centrality(
          ppi_network = ppi_network,
          drug_target_network = drug_target_network,
          disease_genes = disease_genes,
          hub_penalty = params$hub,
          max_deg = .Machine$integer.max,
          result_size = result_size,
          target = "drug",
          include_indirect_drugs = TRUE,
          include_non_approved_drugs = TRUE,
          filter_paths = TRUE
        )
        param_name <- paste0("Harmonic_centrality_hub_", params$hub)
      } else {
        res <- CRank(
          input_data = input_data,
          ties_method = ties_method,
          prior = params$prior,
          num_bin = params$num_bin,
          num_iter = num_iter,
          reverse = reverse
        )
        param_name <- paste0("CRank_prior_", params$prior, "_num_bin_", params$num_bin)
      }

      list(
        param_name = param_name,
        params = as.list(params),
        result = res
      )
    }, error = function(e) {
      warning("Error running parameter set: ", e$message, call. = FALSE)
      NULL
    })
  }

  tasks <- lapply(seq_len(nrow(param_grid)), function(i) param_grid[i, , drop = FALSE])
  names(tasks) <- sprintf("parameter_set_%d", seq_along(tasks))

  parameter_label <- function(params) {
    if (method == "TrustRank") {
      return(paste0("TrustRank_damp_", params$damping, "_hub_", params$hub))
    }
    if (method == "Harmonic_centrality") {
      return(paste0("Harmonic_centrality_hub_", params$hub))
    }
    paste0("CRank_prior_", params$prior, "_num_bin_", params$num_bin)
  }

  effective_n_workers <- n_workers
  if (method %in% c("TrustRank", "Harmonic_centrality") && n_workers > 1L) {
    message(
      "[DrugSigNet] Parameter optimization: ",
      method,
      " uses Python/reticulate graph backends; running sequentially to avoid ",
      "parallel n_workers failures, duplicate retries, and leaked semaphores."
    )
    effective_n_workers <- 1L
  }

  results <- run_pipeline_tasks(
    tasks = tasks,
    FUN = run_benchmark,
    n_workers = effective_n_workers,
    label = "Parameter optimization",
    task_label = parameter_label,
    psock_packages = "DrugSigNet",
    fallback = TRUE,
    progress = TRUE
  )

  results <- Filter(Negate(is.null), results)

  names(results) <- vapply(
    results,
    function(x) x$param_name,
    character(1)
  )

  results
}
