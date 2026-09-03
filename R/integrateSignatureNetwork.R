#' Integrate Signature- and Network-Based Drug Rankings
#'
#' @description
#' Integrates current `DrugSearchingPipeline` outputs from signature-based and
#' network-based workflows into a single `DrugSearchingPipeline` object with
#' consensus CRank, Dowdall, and Robust Rank Aggregation (RRA) rankings.
#'
#' The function:
#' \itemize{
#'   \item Accepts either `DrugSearchingPipeline` S4 objects or legacy list results
#'   \item Expands pipe-delimited network drug entries before matching overlaps
#'   \item Detects overlapping drugs between both pipelines
#'   \item Automatically harmonizes common drug identifier columns
#'   \item Combines pairwise signature ranks with raw centrality-derived, stretched network ranks
#'   \item Includes raw Network_proximity and Diffusion ranks when present
#'   \item Performs CRank, Dowdall, and RRA aggregation
#'   \item Builds updated `RankAggregation`, `DrugAnnotation`, and
#'     `Visualization` slots
#' }
#'
#' @param signature_res A `DrugSearchingPipeline` object returned by
#'   \code{drugSignaturePipeline()} or a legacy result list containing
#'   signature rank aggregation results. The direct output from
#'   \code{harmonize_signature_results()} is also supported. Integration uses
#'   `RankAggregation$Pairwise` signature ranks for integrated aggregation and
#'   `RankAggregation$Harmonized` ranks for the signature columns when present.
#' @param network_res A `DrugSearchingPipeline` object returned by
#'   \code{drugNetworkPipeline()} or a legacy result list containing
#'   `RankAggregation$Network_Harmonized`.
#' @param ties_method Character tie-handling method ("max", "min", "dense", etc.).
#' @param prior Numeric prior used in CRank. Default is 0.093.
#' @param num_bin Integer number of bins for CRank discretization. Default is 200.
#' @param top_k Integer number of top drugs used for enrichment.
#' @param trial_condition Optional condition filter passed to \code{annotate_drugs()}.
#' @param target_id_from Optional AnnotationDbi keytype or supported alias describing
#'   target identifiers used for functional enrichment. Default is \code{NULL},
#'   which treats targets as HGNC symbols unless Ensembl gene IDs are detected.
#' @param drug_annotation_source Source used in \code{annotate_drugs()}. One of \code{"All"},
#'   \code{"OpenTargets"}, \code{"CHEMBL"}, or \code{"TTD"}. Default is
#'   \code{"All"}.
#' @param force Logical; passed to \code{annotate_drugs()} and annotation layer
#'   helpers to force Synapse cache refresh where supported.
#' @param auth_token Optional Synapse authentication token passed to
#'   \code{annotate_drugs()} and annotation layer helpers.
#' @param run_drug_annotation Logical; if \code{TRUE}, annotate integrated drugs
#'   and run target-set enrichment. If \code{FALSE}, the \code{DrugAnnotation}
#'   section is omitted (`NULL`). Default is \code{TRUE}.
#' @param run_visualization Logical; if \code{TRUE}, build visualization
#'   metadata and plots. If \code{FALSE}, the \code{Visualization} section is
#'   omitted (`NULL`). Default is \code{TRUE}.
#'
#' @return A `DrugSearchingPipeline` S4 object with `type = "integration"`.
#'   The `RankAggregation` slot contains `Signature_Network_CRank`,
#'   `Signature_Network_Dowdall`, `Signature_Network_RRA`, and
#'   `Signature_Network_Harmonized`. The integration result keeps
#'   `DrugSearching` empty and returns only rank aggregation, annotation, and
#'   visualization outputs.
#'
#' @importFrom dplyr select rename filter full_join inner_join distinct if_any
#' @importFrom tidyr separate_rows
#' @importFrom stats na.omit
#' @export
integrateSignatureNetwork <- function(signature_res,
                                      network_res,
                                      ties_method = "max",
                                      prior = 0.093,
                                      num_bin = 200,
                                      top_k = 100,
                                      trial_condition = NULL,
                                      target_id_from = NULL,
                                      drug_annotation_source = c("All", "OpenTargets", "CHEMBL", "TTD"),
                                      force = FALSE,
                                      auth_token = NULL,
                                      run_drug_annotation = TRUE,
                                      run_visualization = TRUE) {
  pipeline_builder <- build_pipeline_object

  drug_annotation_source <- match.arg(drug_annotation_source)
  force <- isTRUE(force)
  run_drug_annotation <- .pipeline_flag(run_drug_annotation, "run_drug_annotation")
  run_visualization <- .pipeline_flag(run_visualization, "run_visualization")
  .pipeline_message("Integration", "Validating pipeline payloads and options.", 1, 7)

  if (!is.numeric(top_k) || length(top_k) != 1 || is.na(top_k) || top_k < 1) {
    stop("`top_k` must be a single positive integer value.", call. = FALSE)
  }
  top_k <- as.integer(top_k)

  .pipeline_message("Integration", "Extracting signature and network harmonized rank tables.", 2, 7)
  signature_payload <- .integration_payload(signature_res, "signature_res")
  network_payload <- .integration_payload(network_res, "network_res")

  signature_method_tables <- .integration_signature_method_tables(
    signature_payload = signature_payload
  )
  sig_df <- .integration_signature_harmonized_table(
    signature_payload = signature_payload,
    signature_method_tables = signature_method_tables,
    arg_name = "signature_res"
  )
  net_df <- .integration_harmonized_table(
    network_payload$RankAggregation,
    "Network_Harmonized",
    "network_res"
  )

  sig_df <- .integration_normalize_id(sig_df, "signature_res")
  net_harmonized_df <- .integration_normalize_id(net_df, "network_res")
  net_harmonized_df <- .integration_expand_network_drugs(net_harmonized_df)

  network_rank_df <- .integration_network_rank_table(
    network_payload = network_payload,
    fallback_net_df = net_harmonized_df,
    ties_method = ties_method
  )
  network_rank_df <- .integration_normalize_id(network_rank_df, "network_res_rank")
  network_rank_df <- .integration_expand_network_drugs(network_rank_df)

  signature_drugs <- unique(unlist(lapply(signature_method_tables, function(x) x$Drug), use.names = FALSE))
  common_drugs <- intersect(signature_drugs, network_rank_df$Drug)
  if (length(common_drugs) == 0) {
    stop("No overlapping drugs between signature and network results.", call. = FALSE)
  }

  .pipeline_message("Integration", "Preparing integrated rank aggregation inputs.", 3, 7)
  Signature_Network_CRank_input <- .integration_method_input(
    sig_df = signature_method_tables$CRank,
    net_df = network_rank_df,
    common_drugs = common_drugs,
    method = "CRank"
  )

  Signature_Network_Dowdall_input <- .integration_method_input(
    sig_df = signature_method_tables$Dowdall,
    net_df = network_rank_df,
    common_drugs = common_drugs,
    method = "Dowdall"
  )

  Signature_Network_RRA_input <- .integration_method_input(
    sig_df = signature_method_tables$RRA,
    net_df = network_rank_df,
    common_drugs = common_drugs,
    method = "RRA"
  )

  .pipeline_message("Integration", "Running integrated CRank, Dowdall, and RRA aggregation.", 4, 7)
  Signature_Network_CRank <- CRank(
    input_data = Signature_Network_CRank_input,
    ties_method = ties_method,
    prior = prior,
    num_bin = num_bin,
    num_iter = 1000,
    reverse = TRUE
  )

  Signature_Network_CRank@result <- Signature_Network_CRank@result %>%
    .integration_normalize_id("Signature_Network_CRank") %>%
    dplyr::rename(Signature_Network_CRank = CRank)

  Signature_Network_Dowdall <- Dowdall(
    input_data = Signature_Network_Dowdall_input,
    ties_method = ties_method,
    reverse = FALSE
  )

  Signature_Network_Dowdall@result <- Signature_Network_Dowdall@result %>%
    .integration_normalize_id("Signature_Network_Dowdall") %>%
    dplyr::rename(Signature_Network_Dowdall = Dowdall_rank)

  Signature_Network_RRA <- RRA(
    input_data = Signature_Network_RRA_input,
    ties_method = ties_method,
    reverse = FALSE,
    full = TRUE,
    exact = FALSE
  )

  Signature_Network_RRA@result <- Signature_Network_RRA@result %>%
    .integration_normalize_id("Signature_Network_RRA") %>%
    dplyr::rename(Signature_Network_RRA = RRA_rank)

  .pipeline_message("Integration", "Building integrated harmonized ranking table.", 5, 7)
  Signature_Network_Harmonized <- dplyr::inner_join(
    sig_df,
    net_harmonized_df %>% .integration_expand_network_drugs(),
    by = "Drug"
  ) %>%
    dplyr::full_join(Signature_Network_CRank@result %>% .integration_expand_network_drugs(), by = "Drug") %>%
    dplyr::full_join(
      Signature_Network_Dowdall@result %>%
        .integration_expand_network_drugs() %>%
        dplyr::select(Drug, Signature_Network_Dowdall),
      by = "Drug"
    ) %>%
    dplyr::full_join(
      Signature_Network_RRA@result %>%
        .integration_expand_network_drugs() %>%
        dplyr::select(Drug, Signature_Network_RRA),
      by = "Drug"
    ) %>%
    dplyr::distinct(Drug, .keep_all = TRUE)

  rank_cols <- names(Signature_Network_Harmonized)[
    grepl("rank|crank|dowdall|rra", names(Signature_Network_Harmonized), ignore.case = TRUE)
  ]

  top_k_union_drugs <- extract_top_ranked_drugs(
    df = Signature_Network_Harmonized,
    rank_cols = rank_cols,
    top_n = top_k
  )

  if (run_drug_annotation) {
    .pipeline_message("Integration", "Running drug annotation and target enrichment.", 6, 7)
    Functional_Enrichment <- .integration_functional_enrichment(
      drugs = top_k_union_drugs$Drug,
      target_id_from = target_id_from
    )

    Features <- tryCatch(
      annotate_drugs(
        drugs = Signature_Network_Harmonized$Drug,
        source = drug_annotation_source,
        condition = trial_condition,
        force = force,
        auth_token = auth_token
      ),
      error = function(e) {
        warning(conditionMessage(e), call. = FALSE)
        NULL
      }
    )
    DrugAnnotation <- list(
      top_k_union_drugs = top_k_union_drugs,
      Features = Features,
      Functional_Enrichment = Functional_Enrichment
    )
  } else {
    .pipeline_message("Integration", "Skipping drug annotation; run_drug_annotation=FALSE.", 6, 7)
    Features <- NULL
    Functional_Enrichment <- NULL
    DrugAnnotation <- NULL
  }

  if (run_visualization) {
    .pipeline_message("Integration", "Building visualization payload.", 7, 7)
    plot_inputs <- .build_plot_inputs(
      rank_df = Signature_Network_Harmonized,
      topk_union_drugs = top_k_union_drugs,
      features_df = Features,
      functional_enrichment = Functional_Enrichment,
      top_k = top_k,
      trial_condition = trial_condition
    )

    Visualization <- .build_visualization(plot_inputs)
  } else {
    .pipeline_message("Integration", "Skipping visualization; run_visualization=FALSE.", 7, 7)
    Visualization <- NULL
  }

  integrated_result <- list(
    RankAggregation = list(
      Signature_Network_CRank = Signature_Network_CRank,
      Signature_Network_Dowdall = Signature_Network_Dowdall,
      Signature_Network_RRA = Signature_Network_RRA,
      Signature_Network_Harmonized = Signature_Network_Harmonized
    ),
    DrugAnnotation = DrugAnnotation,
    Visualization = Visualization
  )

  pipeline_builder(
    rank_aggregation = integrated_result$RankAggregation,
    drug_annotation = integrated_result$DrugAnnotation,
    visualization = integrated_result$Visualization,
    type = "integration"
  )
}

.integration_payload <- function(x, arg_name) {
  if (.is_drug_searching_pipeline(x)) {
    payload <- list(
      DrugSearching = x@DrugSearching,
      RankAggregation = x@RankAggregation
    )
    object_slots <- methods::slotNames(x)
    if ("DrugAnnotation" %in% object_slots) {
      payload$DrugAnnotation <- x@DrugAnnotation
    }
    if ("Visualization" %in% object_slots) {
      payload$Visualization <- x@Visualization
    }
    return(payload)
  }

  if (is.list(x) && !is.null(x$PipelineObject) && .is_drug_searching_pipeline(x$PipelineObject)) {
    return(.integration_payload(x$PipelineObject, arg_name))
  }

  if (!is.list(x)) {
    stop("`", arg_name, "` must be a DrugSearchingPipeline object or a pipeline result list.", call. = FALSE)
  }

  if (is.null(x$RankAggregation) && any(c("Signature_Harmonized", "Pairwise", "Harmonized") %in% names(x))) {
    x <- list(RankAggregation = x)
  }

  x <- ensure_pipeline_sections(x)
  x
}

.integration_harmonized_table <- function(rank_aggregation, table_name, arg_name) {
  if (is.null(rank_aggregation) || !is.list(rank_aggregation)) {
    stop("`", arg_name, "` must contain a `RankAggregation` list.", call. = FALSE)
  }

  table <- rank_aggregation[[table_name]]
  if (is.null(table)) {
    stop("`", arg_name, "$RankAggregation$", table_name, "` is missing.", call. = FALSE)
  }
  if (!is.data.frame(table)) {
    stop("`", arg_name, "$RankAggregation$", table_name, "` must be a data frame.", call. = FALSE)
  }
  if (!nrow(table)) {
    stop("`", arg_name, "$RankAggregation$", table_name, "` has no rows.", call. = FALSE)
  }

  table
}

.integration_signature_harmonized_table <- function(signature_payload, signature_method_tables, arg_name) {
  rank_aggregation <- signature_payload$RankAggregation
  table <- NULL
  if (is.list(rank_aggregation)) {
    table <- rank_aggregation$Signature_Harmonized
  }

  if (is.data.frame(table) && nrow(table)) {
    return(table)
  }

  harmonized_methods <- NULL
  if (is.list(rank_aggregation)) {
    harmonized_methods <- rank_aggregation$Harmonized
  }
  if (is.list(harmonized_methods) && length(harmonized_methods)) {
    harmonized_tables <- lapply(names(harmonized_methods), function(method) {
      df <- .integration_result_df(harmonized_methods[[method]])
      if (is.null(df) || !is.data.frame(df) || !nrow(df)) {
        return(NULL)
      }
      .integration_normalize_id(
        df,
        paste0(arg_name, "$RankAggregation$Harmonized$", method)
      )
    })
    harmonized_tables <- Filter(Negate(is.null), harmonized_tables)
    if (length(harmonized_tables)) {
      return(
        Reduce(function(x, y) dplyr::full_join(x, y, by = "Drug"), harmonized_tables) %>%
          dplyr::distinct(Drug, .keep_all = TRUE)
      )
    }
  }

  if (!is.list(signature_method_tables) || !length(signature_method_tables)) {
    stop("`", arg_name, "` must contain `RankAggregation$Harmonized` or usable `RankAggregation$Pairwise` signature ranks.", call. = FALSE)
  }

  Reduce(
    function(x, y) dplyr::full_join(x, y, by = "Drug"),
    signature_method_tables
  ) %>%
    dplyr::distinct(Drug, .keep_all = TRUE)
}

.integration_detect_id_col <- function(df, arg_name) {
  candidates <- c("Drug", "Name", "perturbation", "Item", "drug", "compound", "pert_iname")
  hit <- candidates[candidates %in% names(df)][1]
  if (is.na(hit) || length(hit) == 0) {
    stop("Could not detect a drug identifier column in `", arg_name, "`.", call. = FALSE)
  }
  hit
}

.integration_normalize_id <- function(df, arg_name) {
  id_col <- .integration_detect_id_col(df, arg_name)
  df %>%
    dplyr::rename(Drug = dplyr::all_of(id_col)) %>%
    dplyr::mutate(Drug = trimws(as.character(Drug))) %>%
    dplyr::filter(!is.na(Drug), nzchar(Drug)) %>%
    dplyr::distinct(Drug, .keep_all = TRUE)
}

.integration_first_non_missing <- function(x) {
  hit <- x[!is.na(x)]
  if (length(hit)) {
    return(hit[[1]])
  }
  x[NA_integer_][1]
}

.integration_expand_network_drugs <- function(df) {
  df %>%
    tidyr::separate_rows(Drug, sep = "\\|") %>%
    dplyr::mutate(Drug = trimws(as.character(Drug))) %>%
    dplyr::filter(!is.na(Drug), nzchar(Drug)) %>%
    dplyr::group_by(Drug) %>%
    dplyr::summarise(
      dplyr::across(dplyr::everything(), .integration_first_non_missing),
      .groups = "drop"
    )
}

.integration_result_df <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (methods::is(x, "RankAggregation") || methods::is(x, "NetworkBased")) {
    return(x@result)
  }
  if (is.data.frame(x)) {
    return(x)
  }
  NULL
}

.integration_score_col <- function(df, method, dataset = NULL) {
  preferred <- switch(method,
                      CRank = c(if (!is.null(dataset)) paste0(dataset, "_CRank"), "CRank"),
                      Dowdall = c(if (!is.null(dataset)) paste0(dataset, "_Dowdall"), "Dowdall_rank", "Dowdall"),
                      RRA = c(if (!is.null(dataset)) paste0(dataset, "_RRA"), "RRA_rank", "RRA"),
                      character())
  preferred <- preferred[!is.na(preferred)]
  hit <- intersect(preferred, names(df))[1]
  if (!is.na(hit) && length(hit)) {
    return(hit)
  }

  numeric_cols <- .integration_numeric_rank_cols(df, method = method)
  if (length(numeric_cols)) {
    return(numeric_cols[1])
  }

  numeric_cols <- .integration_numeric_rank_cols(df)
  numeric_cols[1]
}

.integration_pairwise_method_table <- function(pairwise_rank_aggregation, method) {
  datasets <- intersect(c("CMAP", "LINCS", "gCMAP", "Correlation"), names(pairwise_rank_aggregation))
  if (!length(datasets)) {
    return(NULL)
  }

  method_tables <- lapply(datasets, function(dataset) {
    method_obj <- pairwise_rank_aggregation[[dataset]][[method]]
    df <- .integration_result_df(method_obj)
    if (is.null(df) || !is.data.frame(df) || !nrow(df)) {
      return(NULL)
    }

    df <- .integration_normalize_id(df, paste0("signature_res$Pairwise$", dataset, "$", method))
    score_col <- .integration_score_col(df, method = method, dataset = dataset)
    if (is.na(score_col) || !length(score_col) || !score_col %in% names(df)) {
      return(NULL)
    }

    out_col <- paste0(dataset, "_", method)
    df %>%
      dplyr::select(Drug, dplyr::all_of(score_col)) %>%
      dplyr::rename(!!out_col := dplyr::all_of(score_col))
  })

  method_tables <- Filter(Negate(is.null), method_tables)
  if (!length(method_tables)) {
    return(NULL)
  }

  Reduce(function(x, y) dplyr::full_join(x, y, by = "Drug"), method_tables) %>%
    dplyr::distinct(Drug, .keep_all = TRUE)
}

.integration_signature_method_tables <- function(signature_payload) {
  pairwise_rank_aggregation <- signature_payload$RankAggregation$Pairwise
  if (is.null(pairwise_rank_aggregation)) {
    pairwise_rank_aggregation <- signature_payload$Pairwise
  }

  if (!is.list(pairwise_rank_aggregation) || !length(pairwise_rank_aggregation)) {
    stop("`signature_res` must contain `RankAggregation$Pairwise` signature ranks for integration.", call. = FALSE)
  }

  method_tables <- list(
    CRank = .integration_pairwise_method_table(pairwise_rank_aggregation, "CRank"),
    Dowdall = .integration_pairwise_method_table(pairwise_rank_aggregation, "Dowdall"),
    RRA = .integration_pairwise_method_table(pairwise_rank_aggregation, "RRA")
  )

  missing_methods <- names(method_tables)[vapply(method_tables, is.null, logical(1))]
  if (length(missing_methods) > 0L) {
    stop(
      "`signature_res$RankAggregation$Pairwise` is missing usable pairwise signature ranks for: ",
      paste(missing_methods, collapse = ", "),
      call. = FALSE
    )
  }

  method_tables
}

.integration_network_proximity_table <- function(network_payload, ties_method) {
  df <- .integration_result_df(network_payload$DrugSearching$Raw$Network_proximity)
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) {
    return(NULL)
  }

  df <- .safe_rename_drug(df)
  df <- .integration_normalize_id(df, "network_res$DrugSearching$Raw$Network_proximity")

  if ("Z" %in% names(df)) {
    df$Network_proximity <- .rank_values(df$Z, ties_method, decreasing = FALSE)
  } else if (!"Network_proximity" %in% names(df)) {
    score_col <- .integration_numeric_rank_cols(df)[1]
    if (is.na(score_col) || !length(score_col) || !score_col %in% names(df)) {
      return(NULL)
    }
    df <- df %>% dplyr::rename(Network_proximity = dplyr::all_of(score_col))
  }

  df %>%
    dplyr::select(Drug, Network_proximity) %>%
    dplyr::distinct()
}

.integration_diffusion_table <- function(network_payload) {
  df <- .integration_result_df(network_payload$DrugSearching$Raw$Diffusion)
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) {
    return(NULL)
  }

  df <- .safe_rename_drug(df)
  df <- .integration_normalize_id(df, "network_res$DrugSearching$Raw$Diffusion")

  diffusion_rank_cols <- .network_diffusion_rank_cols()
  available_diffusion_cols <- intersect(diffusion_rank_cols, names(df))
  if (length(available_diffusion_cols) == 0) {
    stop("Diffusion result did not include any recognized rank columns.", call. = FALSE)
  }

  df %>%
    dplyr::select(dplyr::all_of(c("Drug", available_diffusion_cols))) %>%
    dplyr::distinct() %>%
    stats::na.omit()
}

.integration_network_method_table <- function(network_payload, method) {
  candidates <- list(
    network_payload$DrugSearching$Raw[[method]]
  )

  for (candidate in candidates) {
    df <- .integration_result_df(candidate)
    if (is.null(df) || !is.data.frame(df)) {
      next
    }
    if (!nrow(df)) {
      return(data.frame(Drug = character(), stringsAsFactors = FALSE))
    }

    df <- .integration_normalize_id(df, paste0("network_res$DrugSearching$", method))
    score_col <- if ("Score" %in% names(df)) {
      "Score"
    } else if (method %in% names(df)) {
      method
    } else {
      .integration_numeric_rank_cols(df)[1]
    }
    if (is.na(score_col) || !length(score_col) || !score_col %in% names(df)) {
      next
    }

    return(df %>%
             dplyr::select(Drug, dplyr::all_of(score_col)) %>%
             dplyr::rename(!!method := dplyr::all_of(score_col)))
  }

  NULL
}

.integration_network_rank_table <- function(network_payload, fallback_net_df, ties_method) {
  trust <- .integration_network_method_table(network_payload, "TrustRank")
  harmonic <- .integration_network_method_table(network_payload, "Harmonic_centrality")
  degree <- .integration_network_method_table(network_payload, "Degree_centrality")
  diffusion <- .integration_diffusion_table(network_payload)
  netprox <- .integration_network_proximity_table(network_payload, ties_method = ties_method)

  rank_tables <- Filter(
    function(x) !is.null(x) && nrow(x),
    list(trust, harmonic, degree, diffusion, netprox)
  )
  if (!length(rank_tables)) {
    return(fallback_net_df)
  }

  network_result <- Reduce(function(x, y) dplyr::full_join(x, y, by = "Drug"), rank_tables) %>%
    dplyr::distinct(Drug, .keep_all = TRUE)

  if ((length(Filter(function(x) !is.null(x) && nrow(x), list(trust, harmonic, degree))) > 0) &&
      !"Degree_centrality" %in% names(network_result)) {
    network_result$Degree_centrality <- NA_real_
  }

  rank_cols <- intersect(c("Degree_centrality", "Harmonic_centrality", "TrustRank"), names(network_result))
  for (rank_col in rank_cols) {
    network_result[[rank_col]] <- .rank_values(
      network_result[[rank_col]],
      ties_method = ties_method,
      decreasing = TRUE,
      na_fill = 0
    )
  }

  network_result
}

.integration_numeric_rank_cols <- function(df, method = NULL) {
  cols <- names(df)[vapply(df, is.numeric, logical(1))]
  cols <- setdiff(cols, "ID")

  if (!is.null(method)) {
    cols <- cols[grepl(method, cols, ignore.case = TRUE)]
  }

  cols
}

.integration_method_input <- function(sig_df,
                                      net_df,
                                      common_drugs,
                                      method) {
  sig_cols <- .integration_numeric_rank_cols(sig_df, method = method)
  if (!length(sig_cols)) {
    stop("No numeric signature `", method, "` rank columns were found.", call. = FALSE)
  }

  net_cols <- .integration_numeric_rank_cols(net_df)
  if (!length(net_cols)) {
    stop("No numeric network rank columns were found.", call. = FALSE)
  }

  network_res_rank <- net_df %>%
    dplyr::select(-dplyr::any_of("ID")) %>%
    tidyr::separate_rows(Drug, sep = "\\|") %>%
    dplyr::mutate(Drug = trimws(as.character(Drug))) %>%
    dplyr::filter(!is.na(Drug), nzchar(Drug)) %>%
    unique()

  dplyr::inner_join(
    sig_df %>%
      dplyr::filter(Drug %in% common_drugs) %>%
      dplyr::select(Drug, dplyr::all_of(sig_cols)),
    network_res_rank %>%
      dplyr::filter(Drug %in% common_drugs) %>%
      dplyr::select(Drug, dplyr::all_of(net_cols)),
    by = "Drug",
    suffix = c("_signature", "_network")
  ) %>%
    unique()
}

.integration_functional_enrichment <- function(drugs,
                                               target_id_from = NULL) {
  drugs <- unique(stats::na.omit(as.character(drugs)))
  if (!length(drugs)) {
    return(NULL)
  }

  drug_target_network <- tryCatch(
    load_drugsignet_network("drug_target"),
    error = function(e) NULL
  )
  if (is.null(drug_target_network) || !is.data.frame(drug_target_network)) {
    return(NULL)
  }

  top_k_targets <- drug_target_network %>%
    dplyr::filter(Drug %in% drugs) %>%
    dplyr::select(Target) %>%
    dplyr::distinct()

  target_list <- unique(stats::na.omit(top_k_targets$Target))
  if (!length(target_list)) {
    return(NULL)
  }

  TSEA(
    targetList = target_list,
    source = "GO",
    ont = "ALL",
    target_id_from = target_id_from
  )
}
