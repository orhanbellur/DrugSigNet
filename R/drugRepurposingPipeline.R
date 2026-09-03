#' @title Run Unified Drug Repurposing Workflow
#'
#' @description
#' Runs signature-based, network-based, or integrated drug repurposing workflows
#' in a single call.
#'
#' @details
#' `drugRepurposingPipeline()` provides a unified interface to the main
#' DrugSigNet workflows. Depending on `mode`, it runs
#' `drugSignaturePipeline()`, `drugNetworkPipeline()`, or both followed by
#' `integrateSignatureNetwork()`.
#'
#' In `mode = "signature"`, only signature-based drug searching is performed.
#' In `mode = "network"`, only network-based drug searching is performed. In
#' `mode = "both"`, the signature and network workflows are run separately and
#' then integrated into a combined result.
#'
#' Signature mode uses `padj = 0.05` by default and forwards it directly to
#' `drugSignaturePipeline()`. To reproduce the same results with a standalone
#' call, explicitly pass `padj = 0.05` there as well. Use the exact argument name
#' `n_workers` in both interfaces.
#'
#' If `run_drug_annotation = TRUE`, downstream annotation and target set
#' enrichment are performed where supported. If `run_visualization = TRUE`,
#' visualization-ready outputs are included in the returned object.
#'
#' @inheritParams drugSignaturePipeline
#' @inheritParams drugNetworkPipeline
#'
#' @param mode Workflow mode. One of `"signature"`, `"network"`, or `"both"`.
#' @param padj Optional adjusted p-value threshold passed to supported
#'   signature-search methods. Default is `0.05`.
#' @param n_workers Number of parallel workers used by supported pipeline
#'   components.
#' @param chunk_size Integer; number of reference signatures processed per
#'   chunk by signature-search methods. Default is `5000`.
#' @param signature_refdb_auth_token Optional Synapse authentication token used
#'   for frozen signature reference databases. If `NULL`, `auth_token` or the
#'   `SYNAPSE_AUTH_TOKEN` environment variable is used.
#' @param drug_annotation_source Drug annotation source used by network and
#'   integrated workflows. One of `"All"`, `"OpenTargets"`, `"CHEMBL"`, or
#'   `"TTD"`.
#'
#' @return
#' A `DrugSearchingPipeline` S4 object containing workflow-specific raw results,
#' processed rankings, rank aggregation results, and optionally annotation and
#' visualization sections.
#'
#' @examples
#' \dontrun{
#' signature_input <- data.frame(
#'   Entrez = c("7157", "1956", "5290", "7422"),
#'   FC = c(1.35, -0.82, 2.11, -1.47)
#' )
#'
#' disease_genes <- data.frame(
#'   gene = c("ENSG00000130203", "ENSG00000142192", "ENSG00000171867")
#' )
#'
#' res_signature <- drugRepurposingPipeline(
#'   signature_input = signature_input,
#'   mode = "signature",
#'   top_k = 100
#' )
#'
#' res_network <- drugRepurposingPipeline(
#'   disease_genes = disease_genes,
#'   mode = "network",
#'   top_k = 100
#' )
#'
#' res_both <- drugRepurposingPipeline(
#'   signature_input = signature_input,
#'   disease_genes = disease_genes,
#'   mode = "both",
#'   top_k = 100,
#'   n_workers = 1
#' )
#' }
#'
#' @export
drugRepurposingPipeline <- function(signature_input = NULL,
                                    ppi_network = NULL,
                                    drug_target_network = NULL,
                                    disease_genes = NULL,
                                    mode = c("signature", "network", "both"),
                                    padj = 0.05,
                                    trend = NULL,
                                    drug_name_synonym = NULL,
                                    ties_method = "max",
                                    prior = 0.093,
                                    num_bin = 200,
                                    n_workers = 1,
                                    chunk_size = 5000,
                                    signature_refdb_mode = c("default", "frozen", "frozen_force"),
                                    signature_refdb_auth_token = NULL,
                                    validate_signature_refdb = TRUE,
                                    top_k = 100,
                                    trial_condition = NULL,
                                    include_indirect_drugs = TRUE,
                                    include_non_approved_drugs = TRUE,
                                    run_all_network_methods = TRUE,
                                    hub_penalty = 0.01,
                                    damping_factor = 0.95,
                                    result_size = NULL,
                                    n_simulations = 1000,
                                    output_dir = NULL,
                                    drug_annotation_source = c("All", "OpenTargets", "CHEMBL", "TTD"),
                                    target_id_from = NULL,
                                    force = FALSE,
                                    auth_token = NULL,
                                    run_drug_annotation = TRUE,
                                    run_visualization = TRUE) {
  pipeline_builder <- build_pipeline_object
  run_all_network_methods <- .pipeline_flag(run_all_network_methods, "run_all_network_methods")
  run_drug_annotation <- .pipeline_flag(run_drug_annotation, "run_drug_annotation")
  run_visualization <- .pipeline_flag(run_visualization, "run_visualization")

  mode <- match.arg(mode)
  signature_refdb_mode <- match.arg(signature_refdb_mode)
  drug_annotation_source <- match.arg(drug_annotation_source)
  .pipeline_message("Repurposing", "Validating mode and required inputs.", 1, if (mode == "both") 5 else 3)

  if (mode %in% c("signature", "both") && is.null(signature_input)) {
    stop("`signature_input` is required when mode is 'signature' or 'both'.", call. = FALSE)
  }

  if (mode %in% c("network", "both") && is.null(disease_genes)) {
    stop("`disease_genes` is required when mode is 'network' or 'both'.", call. = FALSE)
  }

  out <- list(
    signature_result = NULL,
    network_result = NULL,
    integrated_result = NULL,
    DrugSearching = list(),
    RankAggregation = list(),
    DrugAnnotation = if (run_drug_annotation) list() else NULL,
    Visualization = if (run_visualization) list() else NULL,
    metadata = list(mode = mode)
  )

  call_with_supported_args <- function(fun, args) {
    supported <- names(formals(fun))
    args <- args[intersect(names(args), supported)]
    do.call(fun, args)
  }

  run_signature <- function(signature_n_workers = n_workers, disable_progressr = FALSE) {
    if (isTRUE(disable_progressr)) {
      old_options <- options(progressr.enable = FALSE)
      on.exit(options(old_options), add = TRUE)
    }
    call_with_supported_args(
      drugSignaturePipeline,
      list(
        signature_input = signature_input,
        padj = padj,
        trend = trend,
        drug_name_synonym = drug_name_synonym,
        ties_method = ties_method,
        prior = prior,
        num_bin = num_bin,
        n_workers = signature_n_workers,
        chunk_size = chunk_size,
        signature_refdb_mode = signature_refdb_mode,
        signature_refdb_auth_token = signature_refdb_auth_token,
        validate_signature_refdb = validate_signature_refdb,
        top_k = top_k,
        trial_condition = trial_condition,
        target_id_from = target_id_from,
        force = force,
        auth_token = auth_token,
        run_drug_annotation = run_drug_annotation,
        run_visualization = run_visualization
      )
    )
  }

  run_network <- function(network_n_workers = n_workers) {
    call_with_supported_args(
      drugNetworkPipeline,
      list(
        ppi_network = ppi_network,
        drug_target_network = drug_target_network,
        disease_genes = disease_genes,
        run_all_network_methods = run_all_network_methods,
        include_indirect_drugs = include_indirect_drugs,
        include_non_approved_drugs = include_non_approved_drugs,
        hub_penalty = hub_penalty,
        damping_factor = damping_factor,
        result_size = result_size,
        n_simulations = n_simulations,
        n_workers = network_n_workers,
        prior = prior,
        num_bin = num_bin,
        ties_method = ties_method,
        output_dir = output_dir,
        top_k = top_k,
        trial_condition = trial_condition,
        drug_annotation_source = drug_annotation_source,
        target_id_from = target_id_from,
        force = force,
        auth_token = auth_token,
        run_drug_annotation = run_drug_annotation,
        run_visualization = run_visualization
      )
    )
  }

  if (mode == "both") {
    .pipeline_message("Repurposing", "Running signature pipeline.", 2, 5)
    t_sig <- system.time({
      out$signature_result <- run_signature(signature_n_workers = n_workers)
    })
    .validate_signature_result_for_integration(out$signature_result)
    .pipeline_message("Repurposing", sprintf("Signature pipeline completed in %.1f sec.", unname(t_sig[["elapsed"]])), 2, 5)

    .pipeline_message("Repurposing", "Running network pipeline.", 3, 5)
    t_net <- system.time({
      out$network_result <- run_network(network_n_workers = n_workers)
    })
    .pipeline_message("Repurposing", sprintf("Network pipeline completed in %.1f sec.", unname(t_net[["elapsed"]])), 3, 5)
  } else if (mode == "signature") {
    .pipeline_message("Repurposing", "Running signature pipeline.", 2, 3)
    t_sig <- system.time({
      out$signature_result <- run_signature(signature_n_workers = n_workers)
    })
    .pipeline_message("Repurposing", sprintf("Signature pipeline completed in %.1f sec.", unname(t_sig[["elapsed"]])), 2, 3)
  } else if (mode == "network") {
    .pipeline_message("Repurposing", "Running network pipeline.", 2, 3)
    t_net <- system.time({
      out$network_result <- run_network(network_n_workers = n_workers)
    })
    .pipeline_message("Repurposing", sprintf("Network pipeline completed in %.1f sec.", unname(t_net[["elapsed"]])), 2, 3)
  }

  if (mode == "both") {
    .pipeline_message("Repurposing", "Integrating signature and network results.", 4, 5)
    t_int <- system.time({
      out$integrated_result <- integrateSignatureNetwork(
        signature_res = out$signature_result,
        network_res = out$network_result,
        ties_method = ties_method,
        prior = prior,
        num_bin = num_bin,
        top_k = top_k,
        trial_condition = trial_condition,
        drug_annotation_source = drug_annotation_source,
        target_id_from = target_id_from,
        force = force,
        auth_token = auth_token,
        run_drug_annotation = run_drug_annotation,
        run_visualization = run_visualization
      )
    })
    .pipeline_message(
      "Repurposing",
      sprintf("Integration completed in %.1f sec.", unname(t_int[["elapsed"]])),
      4,
      5
    )
  }

  .section_or_empty <- function(obj, slot) {
    if (is.null(obj)) {
      return(list())
    }
    if (.is_drug_searching_pipeline(obj)) {
      if (!slot %in% methods::slotNames(obj)) {
        return(list())
      }
      return(methods::slot(obj, slot))
    }
    if (!is.null(obj[[slot]])) {
      return(obj[[slot]])
    }
    list()
  }

  mode_sections <- .repurposing_mode_sections(mode)

  .collect_sections <- function(slot, level = NULL) {
    sources <- list(
      signature = out$signature_result,
      network = out$network_result,
      integrated = out$integrated_result
    )
    values <- lapply(mode_sections, function(section) {
      value <- .section_or_empty(sources[[section]], slot)
      if (!is.null(level)) {
        value <- value[[level]]
      }
      if (is.null(value)) list() else value
    })
    names(values) <- mode_sections
    values
  }

  .pipeline_message("Repurposing", "Collecting sections and building final pipeline object.", if (mode == "both") 5 else 3, if (mode == "both") 5 else 3)

  out$DrugSearching <- list(
    Raw = .collect_sections("DrugSearching", "Raw"),
    Processed = .collect_sections("DrugSearching", "Processed")
  )
  out$RankAggregation <- .collect_sections("RankAggregation")
  out$DrugAnnotation <- if (run_drug_annotation) .collect_sections("DrugAnnotation") else NULL
  out$Visualization <- if (run_visualization) .collect_sections("Visualization") else NULL

  out <- ensure_pipeline_sections(out)
  out$PipelineObject <- pipeline_builder(
    raw = out$DrugSearching$Raw,
    processed = out$DrugSearching$Processed,
    rank_aggregation = out$RankAggregation,
    drug_annotation = out$DrugAnnotation,
    visualization = out$Visualization,
    type = mode
  )
  out$PipelineObject
}
