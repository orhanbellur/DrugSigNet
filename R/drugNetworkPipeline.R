#' @title Drug Network-Based Pipeline
#'
#' @description
#' Runs a complete network-based drug prioritization workflow using centrality,
#' diffusion, network proximity, rank aggregation, and optional drug annotation.
#'
#' @details
#' `drugNetworkPipeline()` prioritizes candidate drugs from disease-associated
#' genes using network-based methods. The workflow first resolves the
#' protein-protein interaction network (`ppi_network`) and drug-target
#' interaction network (`drug_target_network`). If either network is `NULL`,
#' the corresponding default DrugSigNet network can be loaded from the local
#' cache or Synapse.
#'
#' The pipeline always runs centrality-based methods, including TrustRank,
#' harmonic centrality, and degree centrality. If `run_all_network_methods =
#' TRUE`, it additionally runs network proximity and diffusion-based methods.
#'
#' Method-specific rankings are integrated using CRank, Dowdall, and Robust Rank
#' Aggregation (RRA). The final harmonized network ranking is stored in the
#' `Network_Harmonized` section of the returned object.
#'
#' If `run_drug_annotation = TRUE`, ranked drugs are annotated and target set
#' enrichment analysis is performed on targets of top-ranked drugs. If
#' `run_visualization = TRUE`, visualization-ready outputs are included in the
#' returned pipeline object.
#'
#' @param ppi_network Optional protein-protein interaction network. Expected to
#'   contain columns `gene1` and `gene2`. If `NULL`, the default DrugSigNet
#'   gene-gene network is loaded.
#' @param drug_target_network Optional drug-target interaction network. Expected
#'   to contain columns such as `ID`, `Drug`, `Target`, and `Group`. If `NULL`,
#'   the default DrugSigNet drug-target network is loaded.
#' @param disease_genes Disease-associated genes used as seed genes. Can be a
#'   character vector or a data frame containing a `gene` column.
#' @param run_all_network_methods Logical; if `TRUE`, run TrustRank, harmonic
#'   centrality, degree centrality, network proximity, and diffusion. If
#'   `FALSE`, run only TrustRank, harmonic centrality, and degree centrality.
#'   Default is `TRUE`.
#' @param include_indirect_drugs Logical; whether to include drugs indirectly
#'   connected to disease genes. Default is `TRUE`.
#' @param include_non_approved_drugs Logical; whether to include non-approved or
#'   investigational drugs. Default is `TRUE`.
#' @param hub_penalty Numeric hub penalty used by centrality-based methods. If a
#'   single value is provided, it is used for both TrustRank and harmonic
#'   centrality. If two values are provided, the first is used for TrustRank and
#'   the second for harmonic centrality. Default is `0.01`.
#' @param damping_factor Numeric damping factor used by TrustRank. Must be
#'   between `0` and `1`. Default is `0.95`.
#' @param result_size Optional number of ranked results returned per method. If
#'   `NULL`, it is inferred from the number of unique drugs in the resolved
#'   drug-target network.
#' @param n_simulations Number of random simulations used by
#'   `Network_proximity()`. Default is `1000`.
#' @param n_workers Number of workers used by methods that support parallel
#'   execution, currently network proximity. Centrality methods are run
#'   sequentially for reticulate/Python backend stability. Default is `5`.
#' @param prior Numeric prior used by CRank aggregation. Default is `0.093`.
#' @param num_bin Number of bins used by CRank aggregation. Default is `200`.
#' @param ties_method Method used to resolve ranking ties. One of `"max"`,
#'   `"min"`, `"average"`, or `"dense"`. Default is `"max"`.
#' @param output_dir Optional directory used by diffusion methods to store
#'   intermediate files.
#' @param top_k Number of top-ranked drugs used for downstream annotation and
#'   target enrichment. Default is `100`.
#' @param trial_condition Optional condition used to retrieve matching clinical
#'   trial annotations.
#' @param drug_annotation_source Drug annotation source. One of `"All"`,
#'   `"OpenTargets"`, `"CHEMBL"`, or `"TTD"`.
#' @param target_id_from Optional AnnotationDbi keytype or supported alias
#'   describing target identifiers used for target set enrichment. If `NULL`,
#'   targets are treated as HGNC symbols unless Ensembl gene IDs are detected.
#' @param force Logical; force refresh of Synapse-backed annotation or network
#'   resources where supported. Default is `FALSE`.
#' @param auth_token Optional Synapse authentication token used by annotation
#'   and network-loading helpers.
#' @param run_drug_annotation Logical; if `TRUE`, annotate ranked drugs and run
#'   target set enrichment. Default is `TRUE`.
#' @param run_visualization Logical; if `TRUE`, build visualization metadata and
#'   plots. Default is `TRUE`.
#'
#' @return
#' A `DrugSearchingPipeline` S4 object containing raw network-search results,
#' processed rankings, rank aggregation results, and optionally annotation and
#' visualization sections.
#'
#' @examples
#' \dontrun{
#' disease_genes <- data.frame(
#'   gene = c("ENSG00000130203", "ENSG00000142192", "ENSG00000171867")
#' )
#'
#' # Run network workflow using default DrugSigNet networks
#' res <- drugNetworkPipeline(
#'   disease_genes = disease_genes,
#'   top_k = 100
#' )
#'
#' # Run only centrality-based network methods
#' res_centrality <- drugNetworkPipeline(
#'   disease_genes = disease_genes,
#'   run_all_network_methods = FALSE,
#'   top_k = 100
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
#' res_custom <- drugNetworkPipeline(
#'   ppi_network = ppi_network,
#'   drug_target_network = drug_target_network,
#'   disease_genes = disease_genes,
#'   run_all_network_methods = FALSE,
#'   top_k = 100
#' )
#' }
#'
#' @seealso
#' `TrustRank()`, `Harmonic_centrality()`, `Degree_centrality()`,
#' `Network_proximity()`, `Diffusion()`, `CRank()`, `Dowdall()`, `RRA()`,
#' `annotate_drugs()`, `TSEA()`
#'
#' @importFrom purrr reduce
#' @importFrom dplyr distinct rename full_join dense_rank mutate last_col select pull filter across all_of everything
#' @importFrom tibble tibble
#' @export
drugNetworkPipeline <- function(
    ppi_network = NULL,
    drug_target_network = NULL,
    disease_genes,
    run_all_network_methods = TRUE,
    include_indirect_drugs = TRUE,
    include_non_approved_drugs = TRUE,
    hub_penalty = 0.01,
    damping_factor = 0.95,
    result_size = NULL,
    n_simulations = 1000,
    n_workers = 5,
    prior = 0.093,
    num_bin = 200,
    ties_method = "max",
    output_dir = NULL,
    top_k = 100,
    trial_condition = NULL,
    drug_annotation_source = c("All", "OpenTargets", "CHEMBL", "TTD"),
    target_id_from = NULL,
    force = FALSE,
    auth_token = NULL,
    run_drug_annotation = TRUE,
    run_visualization = TRUE
) {
  pipeline_builder <- build_pipeline_object
  .pipeline_message("Network", "Validating inputs and options.", 1, 9)
  run_all_network_methods <- .pipeline_flag(run_all_network_methods, "run_all_network_methods")
  run_drug_annotation <- .pipeline_flag(run_drug_annotation, "run_drug_annotation")
  run_visualization <- .pipeline_flag(run_visualization, "run_visualization")

  # -------------------------------------------------------------------------
  # Validation
  # -------------------------------------------------------------------------

  if (missing(disease_genes) || length(disease_genes) == 0)
    stop("`disease_genes` must be provided and non-empty.", call. = FALSE)

  if (is.data.frame(disease_genes)) {
    if (!"gene" %in% names(disease_genes)) {
      disease_genes <- tibble::tibble(gene = disease_genes[[1]])
    }
  } else {
    disease_genes <- tibble::tibble(gene = disease_genes)
  }

  disease_genes <- disease_genes %>%
    dplyr::mutate(gene = as.character(gene)) %>%
    dplyr::filter(!is.na(gene), gene != "") %>%
    dplyr::distinct(gene)

  if (!ties_method %in% c("max", "min", "average", "dense"))
    stop("`ties_method` must be 'max', 'min', 'average', or 'dense'.",
         call. = FALSE)

  if (!is.numeric(top_k) || length(top_k) != 1 || top_k < 1)
    stop("`top_k` must be a positive integer.", call. = FALSE)
  top_k <- as.integer(top_k)
  drug_annotation_source <- match.arg(drug_annotation_source)

  if (!is.numeric(hub_penalty) || length(hub_penalty) < 1 || any(is.na(hub_penalty))) {
    stop("`hub_penalty` must be numeric and non-missing.", call. = FALSE)
  }

  if (length(hub_penalty) > 2) {
    stop("`hub_penalty` must contain one or two numeric values in [0, 1].", call. = FALSE)
  }

  if (any(hub_penalty < 0 | hub_penalty > 1)) {
    stop("`hub_penalty` values must be within [0, 1].", call. = FALSE)
  }

  trust_hub_penalty <- hub_penalty[[1]]
  harmonic_hub_penalty <- if (length(hub_penalty) == 1) {
    hub_penalty[[1]]
  } else {
    hub_penalty[[2]]
  }

  if (!is.numeric(damping_factor) || length(damping_factor) != 1 || is.na(damping_factor)) {
    stop("`damping_factor` must be a single numeric value.", call. = FALSE)
  }

  if (damping_factor < 0 || damping_factor > 1) {
    stop("`damping_factor` must be within [0, 1].", call. = FALSE)
  }

  if (!is.numeric(n_workers) || length(n_workers) != 1 || is.na(n_workers) || n_workers < 1) {
    stop("`n_workers` must be a positive integer.", call. = FALSE)
  }

  n_workers <- as.integer(n_workers)
  requested_result_size <- result_size

  res <- list()

  # -------------------------------------------------------------------------
  # Network Resolution
  # -------------------------------------------------------------------------

  .pipeline_message("Network", "Resolving network inputs and result size.", 2, 9)
  network_inputs <- resolve_network_inputs(
    ppi_network,
    drug_target_network,
    force = force,
    auth_token = auth_token
  )



  ppi_network <- network_inputs$ppi_network
  drug_target_network <- network_inputs$drug_target_network

  if (is.null(requested_result_size)) {
    requested_result_size <- length(unique(drug_target_network$Drug))
  }

  if (!is.numeric(requested_result_size) || length(requested_result_size) != 1 ||
      is.na(requested_result_size) || !is.finite(requested_result_size) || requested_result_size < 1) {
    stop("`result_size` must be a single positive finite numeric value or NULL.",
         call. = FALSE)
  }

  result_size <- as.integer(requested_result_size)



  # -------------------------------------------------------------------------
  # Helper Functions
  # -------------------------------------------------------------------------

  rank_vector <- function(x) {
    if (!is.numeric(x)) return(x)

    if (ties_method == "dense") {
      ifelse(x > 0, dplyr::dense_rank(-x), 0)
    } else {
      ifelse(
        x > 0,
        rank(-x, ties.method = ties_method, na.last = "keep"),
        0
      )
    }
  }

  rank_numeric_cols <- function(df) {
    df %>%
      dplyr::mutate(
        dplyr::across(where(is.numeric), rank_vector)
      )
  }

  filter_drug_target_network_by_flags <- .filter_drug_target_network_by_flags

  run_centrality <- function(method_name, hub_penalty_value = NULL, damping_factor_value = NULL) {

    centrality_drug_target_network <- filter_drug_target_network_by_flags(
      drug_target_network = drug_target_network,
      include_indirect_drugs = include_indirect_drugs,
      include_non_approved_drugs = include_non_approved_drugs
    )

    method_object <- NetworkBased(
      result = data.frame(),
      ppi_network = ppi_network,
      drug_target_network = centrality_drug_target_network,
      disease_genes = disease_genes,
      target = "drug",
      include_indirect_drugs = include_indirect_drugs,
      include_non_approved_drugs = include_non_approved_drugs,
      filter_paths = TRUE,
      hub_penalty = if (is.null(hub_penalty_value)) trust_hub_penalty else hub_penalty_value,
      damping_factor = if (is.null(damping_factor_value)) damping_factor else damping_factor_value,
      result_size = result_size,
      method = method_name
    )

    method_fun <- get(method_name, mode = "function")
    method_args <- list(
      object = method_object,
      ppi_network = ppi_network,
      drug_target_network = centrality_drug_target_network,
      disease_genes = disease_genes,
      hub_penalty = if (is.null(hub_penalty_value)) trust_hub_penalty else hub_penalty_value,
      damping_factor = if (is.null(damping_factor_value)) damping_factor else damping_factor_value,
      result_size = result_size,
      target = "drug",
      include_indirect_drugs = include_indirect_drugs,
      include_non_approved_drugs = include_non_approved_drugs,
      filter_paths = TRUE
    )
    method_args <- method_args[intersect(names(method_args), names(formals(method_fun)))]
    do.call(method_fun, method_args)
  }

  run_agg <- function(fun, df, label, reverse_flag = FALSE) {

    if (identical(fun, CRank)) {

      agg <- CRank(
        input_data = dplyr::distinct(df),
        ties_method = ties_method,
        prior = prior,
        num_bin = num_bin,
        num_iter = 1000,
        reverse = reverse_flag
      )

    } else if (identical(fun, Dowdall)) {

      agg <- Dowdall(
        input_data = dplyr::distinct(df),
        ties_method = ties_method,
        reverse = reverse_flag
      )

    } else if (identical(fun, RRA)) {

      agg <- RRA(
        input_data = dplyr::distinct(df),
        ties_method = ties_method,
        reverse = reverse_flag,
        full = TRUE,
        exact = FALSE
      )

    } else {
      stop("Unsupported aggregation method.", call. = FALSE)
    }

    #agg@result <- .safe_rename_drug(agg@result)
    agg@result <- .clean_python_result(
      agg@result
    )

    agg@result <- .safe_rename_drug(
      agg@result
    )

    agg@result <- agg@result %>%
      dplyr::rename(!!label := dplyr::last_col()) %>%
      dplyr::select(Drug, !!label)

    return(agg)
  }

  # -------------------------------------------------------------------------
  # 1. Drug Searching (Centrality)
  # -------------------------------------------------------------------------

  centrality_jobs <- .network_centrality_registry(
    trust_hub_penalty = trust_hub_penalty,
    harmonic_hub_penalty = harmonic_hub_penalty,
    damping_factor = damping_factor
  )

  run_centrality_job <- function(job) {
    run_centrality(
      method_name = job$method_name,
      hub_penalty_value = job$hub_penalty_value,
      damping_factor_value = job$damping_factor_value
    )
  }

  .pipeline_message("Network", "Running centrality methods sequentially.", 3, 9)
  if (n_workers > 1L) {
    .pipeline_message(
      "Network",
      "Centrality methods use Python/reticulate backends; running sequentially for stability.",
      3,
      9
    )
  }

  message(sprintf(
    "[DrugSigNet] Network centrality method: running %d task(s) sequentially.",
    length(centrality_jobs)
  ))

  centrality_pb <- utils::txtProgressBar(min = 0, max = length(centrality_jobs), style = 3)
  on.exit(close(centrality_pb), add = TRUE)

  res$DrugSearching <- vector("list", length(centrality_jobs))
  names(res$DrugSearching) <- names(centrality_jobs)

  for (idx in seq_along(centrality_jobs)) {
    job <- centrality_jobs[[idx]]
    message(sprintf("[DrugSigNet] Network centrality method start: %s", job$method_name))
    elapsed <- system.time({
      res$DrugSearching[[idx]] <- run_centrality_job(job)
    })
    message(sprintf(
      "[DrugSigNet] Network centrality method completed: %s (%.1f sec)",
      job$method_name,
      unname(elapsed[["elapsed"]])
    ))
    utils::setTxtProgressBar(centrality_pb, idx)
  }

  message("[DrugSigNet] Network centrality method: task execution complete.")

  .pipeline_message("Network", "Combining centrality outputs.", 4, 9)

  centrality_res <- lapply(
    res$DrugSearching,
    function(x) .safe_rename_drug(x@result)
  )

  caddie_res <- purrr::reduce(
    lapply(names(centrality_res), function(nm) {
      dplyr::rename(centrality_res[[nm]], !!nm := Score)
    }),
    dplyr::full_join,
    by = "Drug"
  )

  caddie_res <- rank_numeric_cols(caddie_res)

  # -------------------------------------------------------------------------
  # 2. Diffusion & Network Proximity
  # -------------------------------------------------------------------------
  diffusion_df <- NULL
  netprox <- NULL

  if (run_all_network_methods) {
    .pipeline_message("Network", "Running Network_proximity and Diffusion.", 5, 9)
    message("[DrugSigNet] Running Network_proximity...")

    res$DrugSearching$Network_proximity <- Network_proximity(
      ppi_network = ppi_network,
      drug_target_network = drug_target_network,
      disease_genes = disease_genes,
      include_indirect_drugs = include_indirect_drugs,
      include_non_approved_drugs = include_non_approved_drugs,
      n_simulations = n_simulations,
      n_workers = as.integer(n_workers),
      result_size = result_size,
      random_seed = 42
    )
    message("[DrugSigNet] Network_proximity completed.")

    message("[DrugSigNet] Running Diffusion...")
    res$DrugSearching$Diffusion <- Diffusion(
      ppi_network = ppi_network,
      drug_target_network = drug_target_network,
      disease_genes = disease_genes,
      include_indirect_drugs = include_indirect_drugs,
      include_non_approved_drugs = include_non_approved_drugs,
      ties_method = ties_method,
      output_dir = output_dir
    )
    message("[DrugSigNet] Diffusion completed.")

    diffusion_res <- .safe_rename_drug(
      res$DrugSearching$Diffusion@result
    )

    diffusion_rank_cols <- .network_diffusion_rank_cols()
    available_diffusion_cols <- intersect(diffusion_rank_cols, names(diffusion_res))
    if (length(available_diffusion_cols) == 0) {
      stop("Diffusion result did not include any recognized rank columns.", call. = FALSE)
    }

    diffusion_df <- diffusion_res %>%
      dplyr::select(dplyr::all_of(c("Drug", available_diffusion_cols))) %>%
      dplyr::distinct() %>%
      stats::na.omit()

    netprox <- .safe_rename_drug(
      res$DrugSearching$Network_proximity@result
    ) %>%
      dplyr::mutate(
        Network_proximity = .rank_values(Z, ties_method, decreasing = FALSE)
      ) %>%
      dplyr::select(Drug, Network_proximity)
  } else {
    .pipeline_message("Network", "Skipping Network_proximity and Diffusion; run_all_network_methods=FALSE.", 5, 9)
  }

  # -------------------------------------------------------------------------
  # 3. Diffusion Aggregation
  # -------------------------------------------------------------------------

  aggregation_methods <- .network_aggregation_registry()

  res$RankAggregation$Diffusion <- NULL

  if (run_all_network_methods) {
    .pipeline_message("Network", "Running diffusion rank aggregation.", 6, 9)

    res$RankAggregation$Diffusion <- lapply(
      names(aggregation_methods),
      function(mname)
        run_agg(
          aggregation_methods[[mname]]$fun,
          diffusion_df,
          paste0("Diffusion_", mname),
          aggregation_methods[[mname]]$reverse
        )
    )

    names(res$RankAggregation$Diffusion) <- names(aggregation_methods)
  } else {
    .pipeline_message("Network", "Skipping diffusion rank aggregation; run_all_network_methods=FALSE.", 6, 9)
  }
  # -------------------------------------------------------------------------
  # 4. Harmonized Aggregation
  # -------------------------------------------------------------------------

  res$RankAggregation$Harmonized <- list()
  .pipeline_message("Network", "Running harmonized rank aggregation.", 7, 9)

  for (mname in names(aggregation_methods)) {

    harm_input <- caddie_res

    if (run_all_network_methods && !is.null(res$RankAggregation$Diffusion)) {
      harm_input <- .join_by_drug(
        harm_input,
        res$RankAggregation$Diffusion[[mname]]@result
      )
    }

    if (!is.null(netprox))
      harm_input <- .join_by_drug(harm_input, netprox)

    res$RankAggregation$Harmonized[[mname]] <-
      run_agg(
        aggregation_methods[[mname]]$fun,
        harm_input,
        paste0("Network_", mname),
        aggregation_methods[[mname]]$reverse
      )
  }

  # -------------------------------------------------------------------------
  # 5. Final Merge
  # -------------------------------------------------------------------------

  res$RankAggregation$Network_Harmonized <-
    purrr::reduce(
      list(
        res$RankAggregation$Harmonized$CRank@result,
        res$RankAggregation$Harmonized$Dowdall@result,
        res$RankAggregation$Harmonized$RRA@result
      ),
      .join_by_drug
    )

  if (run_all_network_methods && !is.null(diffusion_df) && !is.null(netprox)) {

    diffusion_agg_df <- purrr::reduce(
      list(
        res$RankAggregation$Diffusion$CRank@result,
        res$RankAggregation$Diffusion$Dowdall@result,
        res$RankAggregation$Diffusion$RRA@result
      ),
      .join_by_drug
    )

    res$RankAggregation$Network_Harmonized <-
      caddie_res %>%
      dplyr::full_join(netprox, by = "Drug") %>%
      dplyr::full_join(diffusion_df, by = "Drug") %>%
      dplyr::full_join(diffusion_agg_df, by = "Drug") %>%
      dplyr::full_join(
        res$RankAggregation$Network_Harmonized,
        by = "Drug"
      )

  } else {

    res$RankAggregation$Network_Harmonized <-
      dplyr::full_join(
        caddie_res,
        res$RankAggregation$Network_Harmonized,
        by = "Drug"
      )
  }

  # -------------------------------------------------------------------------
  # 6. Annotation
  # -------------------------------------------------------------------------

  top_k_union_drugs <- extract_top_ranked_drugs(
    df = res$RankAggregation$Network_Harmonized,
    rank_cols = c(
      "Network_CRank",
      "Network_Dowdall",
      "Network_RRA"
    ),
    top_n = top_k
  )

  cache_file <- file.path(tools::R_user_dir("DrugSigNet", which = "cache"), "drug_annotation.rds")
  can_annotate <- !(is.null(auth_token) && !file.exists(cache_file))

  ranked_drugs <- unique(top_k_union_drugs$Drug)

  if (run_drug_annotation && isTRUE(can_annotate)) {
    if ((isTRUE(force) || !file.exists(cache_file)) && !is.null(auth_token) && nzchar(auth_token)) {
      .drugsignet_require_synapser("download DrugSigNet annotation data")
    }
    .pipeline_message("Network", "Annotating network-ranked drugs.", 8, 9)
    res$DrugAnnotation$top_k_union_drugs <- top_k_union_drugs
    annotate_args <- list(
      drugs = res$RankAggregation$Network_Harmonized$Drug,
      source = drug_annotation_source,
      condition = trial_condition,
      force = force,
      auth_token = auth_token
    )
    ann_formals <- tryCatch(names(formals(annotate_drugs)), error = function(e) character())
    if (length(ann_formals) > 0) {
      annotate_args <- annotate_args[intersect(names(annotate_args), ann_formals)]
    }
    res$DrugAnnotation$Features <- do.call(annotate_drugs, annotate_args)
  } else {
    .pipeline_message("Network", if (run_drug_annotation) "Skipping drug annotation; annotation cache/token unavailable." else "Skipping drug annotation; run_drug_annotation=FALSE.", 8, 9)
    res$DrugAnnotation <- NULL
  }

  if (run_drug_annotation && isTRUE(can_annotate) && !is.null(drug_target_network)) {

    targets <- drug_target_network %>%
      dplyr::filter(Drug %in% ranked_drugs) %>%
      dplyr::pull(Target) %>%
      unique() %>% as.character()

    if (length(targets) > 0) {
      message("[DrugSigNet] Running network functional enrichment.")
      res$DrugAnnotation$Functional_Enrichment <-
        TSEA(
          targetList = targets,
          source = "GO",
          ont = "ALL",
          target_id_from = target_id_from
        )
    }
  }


  # -------------------------------------------------------------------------
  # 7. Visualization
  # -------------------------------------------------------------------------

  if (run_visualization) {
    .pipeline_message("Network", "Building visualization payload.", 9, 9)
    plot_inputs <- .build_plot_inputs(
      rank_df = res$RankAggregation$Network_Harmonized,
      top_k_union_drugs = top_k_union_drugs,
      features_df = res$DrugAnnotation$Features,
      functional_enrichment = res$DrugAnnotation$Functional_Enrichment,
      top_k = top_k,
      trial_condition = trial_condition
    )

    res$Visualization <- .build_visualization(plot_inputs)
  } else {
    .pipeline_message("Network", "Skipping visualization; run_visualization=FALSE.", 9, 9)
    res$Visualization <- NULL
  }

  raw_payload <- res$DrugSearching
  processed_payload <- list()

  for (method_name in names(raw_payload)) {
    method_result <- raw_payload[[method_name]]

    if (!methods::is(method_result, "NetworkBased")) {
      next
    }

    method_df <- .safe_rename_drug(method_result@result)

    if ("Score" %in% names(method_df)) {
      method_df <- method_df %>%
        dplyr::mutate(Rank = rank_vector(Score))
    } else if ("Z" %in% names(method_df)) {
      method_df <- method_df %>%
        dplyr::mutate(
          Rank = .rank_values(Z, ties_method, decreasing = FALSE)
        )
    }

    processed_payload[[method_name]] <- method_df
  }

  res$DrugSearching <- list(
    Raw = raw_payload,
    Processed = processed_payload
  )

  res <- ensure_pipeline_sections(res)
  res$PipelineObject <- pipeline_builder(
    raw = res$DrugSearching$Raw,
    processed = res$DrugSearching$Processed,
    rank_aggregation = res$RankAggregation,
    drug_annotation = res$DrugAnnotation,
    visualization = res$Visualization,
    type = "network"
  )

  return(res$PipelineObject)
}
