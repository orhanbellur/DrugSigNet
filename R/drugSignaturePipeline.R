#' @title Drug Signature-Based Pipeline
#'
#' @description
#' Runs a complete signature-based drug repurposing workflow from differential
#' expression input.
#'
#' @details
#' `drugSignaturePipeline()` performs signature-based drug searching using
#' multiple methods, including CMAP, LINCS, gCMAP, and correlation-based search.
#' The resulting method-specific drug rankings are harmonized and integrated
#' using rank aggregation methods.
#'
#' The input `signature_input` should contain two columns: `Entrez`, containing
#' Entrez gene identifiers, and `FC`, containing fold-change values, typically
#' log2 fold changes. Positive and negative fold changes are used to define
#' up- and down-regulated gene sets.
#'
#' Drug names can optionally be harmonized using `drug_name_synonym`. When
#' provided, the synonym table is standardized internally and used to map
#' perturbation names across reference databases.
#'
#' Frozen signature reference databases can be used by setting
#' `signature_refdb_mode = "frozen"` or `"frozen_force"`. In this case,
#' reference databases are downloaded through `load_signature_refdb()` and
#' cached locally using the supplied Synapse authentication token.
#'
#' If `run_drug_annotation = TRUE`, top-ranked drugs are annotated and target set
#' enrichment analysis is performed using targets of the selected `top_k` drugs.
#' If `run_visualization = TRUE`, visualization-ready outputs are added to the
#' returned pipeline object.
#'
#' @param signature_input Data frame with columns `Entrez` and `FC`.
#' @param padj Optional adjusted p-value threshold passed to supported
#'   signature-search methods. Default is `NULL`.
#' @param trend Optional character filter for perturbation direction. Use `"up"`,
#'   `"down"`, or `NULL`. Default is `NULL`.
#' @param drug_name_synonym Optional drug synonym table used to harmonize drug
#'   names across signature databases. Must contain `ID`, `Drug`,
#'   `name_synonym_list`, and `Group` columns. Legacy lowercase `name` and
#'   `group` column names are also accepted.
#' @param ties_method Method used to resolve ranking ties. Common options are
#'   `"max"`, `"min"`, and `"dense"`. Default is `"max"`.
#' @param prior Numeric prior used by CRank aggregation. Default is `0.093`.
#' @param num_bin Number of bins used by CRank aggregation. Default is `200`.
#' @param n_workers Number of parallel workers used for signature-search
#'   methods. Default is `1`.
#' @param chunk_size Integer; number of reference signatures processed per
#'   chunk by each signature-search method. Default is `5000`.
#' @inheritParams get_drug_signature
#' @param signature_refdb_auth_token Optional Synapse authentication token used
#'   when downloading frozen signature reference databases. If `NULL`,
#'   `auth_token` or the `SYNAPSE_AUTH_TOKEN` environment variable is used.
#' @param validate_signature_refdb Logical; whether to validate frozen HDF5
#'   reference databases before use. Default is `TRUE`.
#' @param top_k Number of top-ranked drugs used for downstream annotation and
#'   target enrichment. Default is `100`.
#' @param trial_condition Optional condition used to retrieve matching clinical
#'   trial annotations. Default is `NULL`.
#' @param target_id_from Optional AnnotationDbi keytype or supported alias
#'   describing target identifiers used for enrichment. If `NULL`, targets are
#'   treated as HGNC symbols unless Ensembl gene IDs are detected.
#' @param force Logical; force refresh of Synapse-backed annotation or reference
#'   resources where supported. Default is `FALSE`.
#' @param auth_token Optional Synapse authentication token used by annotation and
#'   reference database helpers.
#' @param run_drug_annotation Logical; if `TRUE`, annotate ranked drugs and run
#'   target set enrichment. Default is `TRUE`.
#' @param run_visualization Logical; if `TRUE`, build visualization metadata and
#'   plots. Default is `TRUE`.
#'
#' @return
#' A `DrugSearchingPipeline` S4 object containing raw signature-search results,
#' processed rankings, rank aggregation results, and optionally drug annotation
#' and visualization sections.
#'
#' @examples
#' \dontrun{
#' signature_input <- data.frame(
#'   Entrez = c("7157", "1956", "5290", "7422"),
#'   FC = c(1.35, -0.82, 2.11, -1.47)
#' )
#'
#' res <- drugSignaturePipeline(
#'   signature_input = signature_input,
#'   top_k = 100,
#'   n_workers = 1
#' )
#'
#' # Use frozen Synapse reference databases
#' res_frozen <- drugSignaturePipeline(
#'   signature_input = signature_input,
#'   signature_refdb_mode = "frozen",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN"),
#'   top_k = 100
#' )
#' }
#'
#' @importFrom dplyr mutate select filter group_by ungroup rename arrange slice_max full_join bind_rows summarise summarize across dense_rank any_of last_col rename_with distinct if_any all_of row_number transmute
#' @importFrom tidyr separate_rows
#' @importFrom tibble column_to_rownames as_tibble tibble
#' @export
drugSignaturePipeline <- function(signature_input, padj = NULL, trend = NULL,
                                  drug_name_synonym = NULL, ties_method = 'max', prior = 0.093, num_bin = 200, n_workers = 1,
                                  chunk_size = 5000,
                                  signature_refdb_mode = c("default", "frozen", "frozen_force"),
                                  signature_refdb_auth_token = NULL, validate_signature_refdb = TRUE,
                                  top_k = 100, trial_condition = NULL, target_id_from = NULL, force = FALSE, auth_token = NULL,
                                  run_drug_annotation = TRUE, run_visualization = TRUE) {
  pipeline_builder <- build_pipeline_object
  .pipeline_message("Signature", "Validating input signature and options.", 1, 10)
  run_drug_annotation <- .pipeline_flag(run_drug_annotation, "run_drug_annotation")
  run_visualization <- .pipeline_flag(run_visualization, "run_visualization")

  ## --------------------------
  ## Input validation & setup
  ## --------------------------
  validate_input <- function(sig_input) {
    if (!is.data.frame(sig_input)) stop("`signature_input` should be a data frame!")
    required_cols <- c("Entrez", "FC")
    if (!all(required_cols %in% colnames(sig_input)))
      stop(paste0("`signature_input` must contain columns: ", paste(required_cols, collapse = ", ")))
    if (ncol(sig_input) != 2) stop("`signature_input` must have exactly two columns: Entrez and FC!")
  }
  validate_input(signature_input)

  signature_input_edit <- .input_structure(signature_input)

  if (nrow(signature_input_edit$up) == 0 || nrow(signature_input_edit$down) == 0) {
    stop("The input signature does not contain enough data for analysis.")
  }

  if (!is.numeric(top_k) || length(top_k) != 1 || is.na(top_k) || top_k < 1) {
    stop("`top_k` must be a single positive integer value.")
  }
  top_k <- as.integer(top_k)
  n_workers <- max(1L, as.integer(n_workers))
  if (!is.null(trial_condition)) {
    if (!is.character(trial_condition) || length(trial_condition) != 1 || is.na(trial_condition) || !nzchar(trial_condition)) {
      stop("`trial_condition` must be NULL or a single non-empty character string.")
    }
  }

  last_result <- list()

  cache_file <- file.path(tools::R_user_dir("DrugSigNet", which = "cache"), "drug_annotation.rds")
  can_annotate <- !(is.null(auth_token) && !file.exists(cache_file))

  result <- tryCatch({

    ## --------------------------
    ## Methods & DB mapping
    ## --------------------------
    .pipeline_message("Signature", "Preparing method registry and drug synonym mapping.", 2, 10)
    method_registry <- .signature_method_registry()
    method_names <- .signature_method_names(method_registry)
    names(method_names) <- method_names
    signature_refdb_mode <- match.arg(signature_refdb_mode)
    validate_signature_refdb <- .pipeline_flag(validate_signature_refdb, "validate_signature_refdb")

    signature_refdb_paths <- NULL
    if (!identical(signature_refdb_mode, "default")) {
      .pipeline_message("Signature", "Loading frozen Synapse signature reference databases.", 2, 10)
      refdb_keys <- unique(method_registry$ref_db)
      signature_refdb_paths <- stats::setNames(
        lapply(refdb_keys, function(refdb_key) {
          .resolve_signature_refdb(
            ref_db = refdb_key,
            signature_refdb_mode = signature_refdb_mode,
            auth_token = if (is.null(signature_refdb_auth_token)) auth_token else signature_refdb_auth_token,
            validate_hdf5 = validate_signature_refdb
          )
        }),
        refdb_keys
      )
    }

    resolve_signature_refdb <- function(refdb_key) {
      if (!identical(signature_refdb_mode, "default")) {
        return(signature_refdb_paths[[refdb_key]])
      }
      refdb_key
    }

    ## --------------------------
    ## Drug synonym preprocessing
    ## --------------------------
    if (!is.null(drug_name_synonym)) {
      # Backward-compatible schema normalization only. Do not filter/reorder rows:
      # the old pipeline's mapping behavior is intentionally preserved.
      drug_name_synonym <- .standardize_drug_name_synonym(drug_name_synonym)
      drug_name_synonym_original <- drug_name_synonym

      drug_name_synonym_edit <- drug_name_synonym %>%
        dplyr::mutate(name = tolower(name)) %>%
        tidyr::separate_rows(name_synonym_list, sep = "\\|") %>%
        dplyr::mutate(name_synonym_list = tolower(name_synonym_list)) %>%
        unique()
    } else {
      drug_name_synonym_original <- NULL
      drug_name_synonym_edit <- NULL
    }

    ## --------------------------
    ## Process single method dispatcher
    ## --------------------------
    signature_method_config <- .signature_method_config
    process_method <- function(method, sig_input_edit) {
      method_config <- signature_method_config(method, method_registry)
      db_key <- method_config[["ref_db"]]
      db_ref <- resolve_signature_refdb(db_key)
      adjusted_padj <- if (db_key == "cmap") padj else NULL

      if (method_config[["family"]] == "CMAP") {
        cmap_method(upset = as.character(sig_input_edit$up[,1]), downset = as.character(sig_input_edit$down[,1]), ref_db = db_ref, chunk_size = chunk_size)
      } else if (method_config[["family"]] == "LINCS") {
        lincs_method(upset = as.character(sig_input_edit$up[,1]), downset = as.character(sig_input_edit$down[,1]), ref_db = db_ref, chunk_size = chunk_size)
      } else if (method_config[["family"]] == "gCMAP") {
        input_max <- min(sig_input_edit$exp[sig_input_edit$exp > 0])
        input_min <- max(sig_input_edit$exp[sig_input_edit$exp < 0])
        gcmap_method(signature_matrix = sig_input_edit$exp, ref_db = db_ref, higher = input_max, lower = input_min, padj = adjusted_padj, chunk_size = chunk_size)
      } else if (method_config[["family"]] == "Correlation") {
        correlation_method(signature_matrix = sig_input_edit$exp, ref_db = db_ref, chunk_size = chunk_size)
      } else {
        stop("Unknown processing method.")
      }
    }

    ## --------------------------
    ## Shared parallel task runner
    ## --------------------------
    run_method_once <- function(method) {
      tryCatch({
        # Keep signatureSearch attached in both sequential execution and each
        # parallel worker. Its annotation methods use unqualified data() calls,
        # while the individual method wrapper provides the same protection when
        # it is invoked outside this pipeline.
        .with_signature_search_attached(
          process_method(method, signature_input_edit)
        )
      }, error = function(e) {
        structure(
          list(method = method, message = conditionMessage(e)),
          class = "DrugSigNetSignatureMethodError"
        )
      })
    }

    .pipeline_message("Signature", sprintf("Running %d signature search methods.", length(method_names)), 3, 10)
    GESS_res <- run_pipeline_tasks(
      tasks = method_names,
      FUN = run_method_once,
      n_workers = n_workers,
      label = "Signature method",
      task_label = identity,
      psock_packages = c("DrugSigNet", "signatureSearch"),
      fallback = TRUE,
      progress = TRUE
    )

    failed_methods <- vapply(
      GESS_res,
      function(x) inherits(x, "DrugSigNetSignatureMethodError"),
      logical(1)
    )
    if (any(failed_methods)) {
      failed <- GESS_res[failed_methods]
      stop(
        paste0(
          "Signature method(s) failed: ",
          paste(vapply(failed, `[[`, character(1), "method"), collapse = ", "),
          ". First error: ", failed[[1]]$message
        ),
        call. = FALSE
      )
    }
    if (length(GESS_res) == 0L) {
      stop(
        "All signature search methods failed. If the error mentions ExperimentHub ",
        "or a forbidden EH resource, check network access/cache for signatureSearchData ",
        "reference databases and retry.",
        call. = FALSE
      )
    }

    ## --------------------------
    ## Format raw results
    ## --------------------------
    format_results <- function(GESS_res) {
      GESS_result <- list()
      GESS_result$DrugSearching$Raw <- GESS_res
      for (i in seq_along(GESS_res)) {
        GESS_result$DrugSearching$Raw[[i]] <- GESS_res[[i]]
        result_name <- names(GESS_res)[i]
        if (length(result_name) != 1L || is.na(result_name) || !nzchar(result_name)) {
          result_name <- paste0(GESS_res[[i]]@parameters[["signature_method"]], "_", toupper(GESS_res[[i]]@parameters[["refdb"]]))
        }
        names(GESS_result$DrugSearching$Raw)[i] <- result_name
      }
      return(GESS_result)
    }
    .pipeline_message("Signature", "Formatting raw method results.", 4, 10)
    GESS_result <- format_results(GESS_res)
    last_result <- GESS_result

    for (method in names(GESS_result$DrugSearching$Raw)) {
      method_config <- .signature_method_config(method, method_registry)
      if (identical(method_config[["ref_db"]], "lincs2")) {
        GESS_result$DrugSearching$Raw[[method]]@result <- .normalize_frozen_lincs2_result(
          gess_tb = GESS_result$DrugSearching$Raw[[method]]@result,
          refdb = method_config[["ref_db"]],
          signature_refdb_mode = signature_refdb_mode
        )
      }
    }

    cmap_reference_methods <- intersect(method_registry$method[method_registry$family == "CMAP"], names(GESS_result$DrugSearching$Raw))
    drug_reference_methods <- if (length(cmap_reference_methods) > 0L) cmap_reference_methods else names(GESS_result$DrugSearching$Raw)
    All_drugs <- data.frame(
      pert = unique(stats::na.omit(unlist(lapply(
        drug_reference_methods,
        function(method) tolower(GESS_result$DrugSearching$Raw[[method]]@result$pert)
      )))),
      stringsAsFactors = FALSE
    )

    if (!is.null(drug_name_synonym_edit)) {
      All_drugs_filt <- All_drugs %>%
        dplyr::filter(pert %in% drug_name_synonym_edit$name_synonym_list)
    } else {
      All_drugs_filt <- All_drugs
    }

    ## --------------------------
    ## Helper: filter & mapping
    ## --------------------------
    filter_and_map <- function(raw_obj, method, trend_name, drug_name_synonym, drug_name_synonym_edit) {
      if (!is.null(drug_name_synonym)) {
        raw_obj@result %>%
          dplyr::filter(tolower(pert) %in% All_drugs_filt$pert) %>%
          .Drug_mapping(
            trend_name = trend_name,
            drug_name_synonym = drug_name_synonym_original,
            drug_name_synonym_edit = drug_name_synonym_edit
          )
      } else {
        if (!is.null(trend_name)) {
          raw_obj@result %>% dplyr::filter(trend %in% trend_name)
        } else {
          raw_obj@result
        }
      }
    }

    .pipeline_message("Signature", "Filtering/mapping perturbations and computing method rank scores.", 5, 10)
    ## --------------------------
    ## Compute processed results per method
    ## --------------------------
    GESS_result$DrugSearching$Processed <- list()
    for (method in method_names) {
      raw_obj <- GESS_result$DrugSearching$Raw[[method]]
      method_config <- .signature_method_config(method, method_registry)

      filtered_result <- filter_and_map(
        raw_obj = raw_obj,
        method = method,
        trend_name = trend,
        drug_name_synonym = drug_name_synonym,
        drug_name_synonym_edit = drug_name_synonym_edit
      )

      if (method_config[["family"]] == "CMAP") {
        GESS_result$DrugSearching$Processed[[method]] <- filtered_result %>%
          cmap_rank_score(confidence_level = 0.95, ties_method = ties_method)
      } else if (method_config[["family"]] == "LINCS") {
        GESS_result$DrugSearching$Processed[[method]] <- filtered_result %>%
          lincs_rank_score(ties_method = ties_method)
      } else if (method_config[["family"]] == "gCMAP") {
        ref_key <- method_config[["reference_method"]]
        if (!ref_key %in% names(GESS_result$DrugSearching$Raw)) {
          warning("Skipping ", method, " because its reference method ", ref_key, " failed or was unavailable.", call. = FALSE)
          next
        }
        GESS_result$DrugSearching$Processed[[method]] <- filtered_result %>%
          gcmap_rank_score(cmap_signature =  GESS_result$DrugSearching$Raw[[ref_key]]@result, trend_name = trend, ties_method = ties_method)
      } else if (method_config[["family"]] == "Correlation") {
        ref_key <- method_config[["reference_method"]]
        if (!ref_key %in% names(GESS_result$DrugSearching$Raw)) {
          warning("Skipping ", method, " because its reference method ", ref_key, " failed or was unavailable.", call. = FALSE)
          next
        }
        n_val <- sum(GESS_result$DrugSearching$Raw[[ref_key]]@result$N_upset[1], GESS_result$DrugSearching$Raw[[ref_key]]@result$N_downset[1])
        GESS_result$DrugSearching$Processed[[method]] <- filtered_result %>%
          correlation_rank_score(n = n_val, ties_method = ties_method)
      }
    }

    .pipeline_message("Signature", "Harmonizing perturbation metadata across methods.", 6, 10)
    ## --------------------------
    ## Combine and harmonize perturbations
    ## --------------------------
    all_perturbations <- dplyr::bind_rows(lapply(GESS_result$DrugSearching$Processed, function(df) {
      df %>% dplyr::select(-c(rank_score, scaled_score)) %>% unique()
    }))

    if (!"Drug_status" %in% names(all_perturbations) && "Drug_statue" %in% names(all_perturbations)) {
      all_perturbations <- dplyr::rename(all_perturbations, Drug_status = Drug_statue)
    }

    all_perturbations <- .harmonize_signature_metadata(all_perturbations)

    for (i in seq_along(GESS_result$DrugSearching$Processed)) {
      all_perturbations_filt <- all_perturbations %>% dplyr::filter(!perturbation %in% GESS_result$DrugSearching$Processed[[i]]$perturbation)
      GESS_result$DrugSearching$Processed[[i]] <- dplyr::bind_rows(
        GESS_result$DrugSearching$Processed[[i]],
        all_perturbations_filt
      ) %>% unique() %>%
        dplyr::group_by(perturbation) %>%
        dplyr::mutate(rank_score = ifelse(is.na(rank_score), 0, rank_score))
    }
    last_result <- GESS_result

    .pipeline_message("Signature", "Running pairwise rank aggregation.", 7, 10)
    ## --------------------------
    ## Pairwise rank aggregation per dataset
    ## --------------------------
    datasets <- .signature_method_families(method_registry)
    methods_ra <- c("CRank", "Dowdall", "RRA")
    pairwise_rank_aggregation <- list()
    for (dataset in datasets) {
      method_cmap <- paste0(dataset, "_CMAP")
      method_lincs <- paste0(dataset, "_LINCS2")
      if (!all(c(method_cmap, method_lincs) %in% names(GESS_result$DrugSearching$Processed))) {
        warning(
          "Skipping pairwise rank aggregation for ", dataset,
          " because one or both reference databases failed or were unavailable.",
          call. = FALSE
        )
        next
      }
      df1 <- GESS_result$DrugSearching$Processed[[method_cmap]]
      df2 <- GESS_result$DrugSearching$Processed[[method_lincs]]
      pairwise_rank_aggregation[[dataset]] <- stats::setNames(lapply(methods_ra, function(m) {
        .calculate_rank_aggregation(m, df1, df2, prior, num_bin, ties_method)
      }), methods_ra)
    }
    GESS_result$RankAggregation$Pairwise <- pairwise_rank_aggregation
    last_result <- GESS_result

    ## rename pairwise outputs
    for (i in names(pairwise_rank_aggregation)) {
      pairwise_rank_aggregation[[i]][['CRank']]@result <- pairwise_rank_aggregation[[i]][['CRank']]@result %>%
        dplyr::rename(!!paste0(i, '_CRank') := CRank)
      pairwise_rank_aggregation[[i]][['Dowdall']]@result <- pairwise_rank_aggregation[[i]][['Dowdall']]@result %>%
        dplyr::rename(!!paste0(i, '_Dowdall') := Dowdall_rank)
      pairwise_rank_aggregation[[i]][['RRA']]@result <- pairwise_rank_aggregation[[i]][['RRA']]@result %>%
        dplyr::rename(!!paste0(i, '_RRA') := RRA_rank)
    }

    ## --------------------------
    ## Harmonize results across methods
    ## --------------------------
    if (length(pairwise_rank_aggregation) > 0L) {
      Harmonized_rank_aggregation <- stats::setNames(
        lapply(methods_ra, function(method) {
          score_col <- switch(method,
                              CRank = "CRank",
                              Dowdall = "Dowdall_rank",
                              RRA = "RRA_rank")

          combined_ranks <- pairwise_rank_aggregation %>%
            lapply(function(x) {
              .extract_drug_score_table(
                result_df = x[[method]]@result,
                score_col = score_col,
                method = method
              )
            }) %>% purrr::reduce(dplyr::full_join, by = "Drug")

          switch(method,
                 CRank = CRank(input_data = combined_ranks, ties_method = ties_method, prior = prior, num_bin = num_bin, num_iter = 1000, reverse = TRUE),
                 Dowdall = Dowdall(input_data = combined_ranks, ties_method = ties_method, reverse = FALSE),
                 RRA = RRA(input_data = combined_ranks, full = TRUE, exact = FALSE, ties_method = ties_method, reverse = FALSE)
          )
        }),
        methods_ra
      )
    } else {
      Harmonized_rank_aggregation <- list()
    }

    .pipeline_message("Signature", "Building harmonized signature ranking table.", 8, 10)
    ## --------------------------
    ## Build signature_res
    ## --------------------------
    processed_method_names <- names(GESS_result$DrugSearching$Processed)
    if (length(processed_method_names) == 0L) {
      stop("No signature methods produced processable results after filtering failed methods.", call. = FALSE)
    }
    signature_res <- processed_method_names %>%
      lapply(function(dataset) {
        GESS_result$DrugSearching$Processed[[dataset]][, c(1,2)] %>%
          dplyr::rename(!!dataset := rank_score)
      }) %>%
      purrr::reduce(dplyr::full_join, by = "perturbation") %>%
      dplyr::rename(Drug = perturbation) %>%
      unique()

    signature_res <- .transform_ranks(signature_res, ties_method = "max", reverse = TRUE)

    for (method in names(Harmonized_rank_aggregation)) {
      score_col <- switch(method,
                          CRank = "CRank",
                          Dowdall = "Dowdall_rank",
                          RRA = "RRA_rank")
      Harmonized_rank_aggregation[[method]]@result <- .extract_drug_score_table(
        result_df = Harmonized_rank_aggregation[[method]]@result,
        score_col = score_col,
        method = method
      )
    }

    if (length(Harmonized_rank_aggregation) > 0L) {
      signature_res <- dplyr::full_join(signature_res,
                                        Reduce(function(x, y) dplyr::full_join(x, y, by = "Drug"), lapply(Harmonized_rank_aggregation, function(x) x@result))
                                        , by = "Drug"
      )
    }

    # Ensure harmonized signature rank columns are present with stable names
    if ("CRank" %in% names(signature_res) && !"Signature_CRank" %in% names(signature_res)) {
      signature_res <- signature_res %>% dplyr::rename(Signature_CRank = CRank)
    }
    if ("Dowdall_rank" %in% names(signature_res) && !"Signature_Dowdall" %in% names(signature_res)) {
      signature_res <- signature_res %>% dplyr::rename(Signature_Dowdall = Dowdall_rank)
    }
    if ("RRA_rank" %in% names(signature_res) && !"Signature_RRA" %in% names(signature_res)) {
      signature_res <- signature_res %>% dplyr::rename(Signature_RRA = RRA_rank)
    }

    ## --------------------------
    ## Final assembly
    ## --------------------------
    GESS_result$RankAggregation$Pairwise <- pairwise_rank_aggregation
    GESS_result$RankAggregation$Harmonized_methods <- Harmonized_rank_aggregation
    GESS_result$RankAggregation$Signature_Harmonized <- signature_res
    last_result <- GESS_result

    # =========================================================================
    # 6. Drug Annotation
    # =========================================================================
    ranked_drugs <- unique(stats::na.omit(GESS_result$RankAggregation$Signature_Harmonized$Drug))

    preferred_rank_cols <- c("Signature_CRank", "Signature_Dowdall", "Signature_RRA")
    fallback_rank_cols <- c("CRank", "Dowdall_rank", "RRA_rank")
    available_rank_cols <- intersect(c(preferred_rank_cols, fallback_rank_cols), names(GESS_result$RankAggregation$Signature_Harmonized))

    if (length(available_rank_cols) == 0) {
      available_rank_cols <- intersect(processed_method_names, names(GESS_result$RankAggregation$Signature_Harmonized))
    }
    if (length(available_rank_cols) == 0) {
      stop("No supported rank columns found for top_k selection.")
    }

    top_k_union_drugs <- extract_top_ranked_drugs(
      df = GESS_result$RankAggregation$Signature_Harmonized,
      rank_cols = available_rank_cols,
      top_n = top_k
    )

    default_drug_target_network <- tryCatch(
      load_drugsignet_network("drug_target"),
      error = function(e) {
        warning(
          "Unable to load the default drug-target network for top_k annotation: ",
          conditionMessage(e),
          call. = FALSE
        )
        NULL
      }
    )

    top_k_drug_targets <- if (is.data.frame(default_drug_target_network)) {
      default_drug_target_network %>%
        dplyr::filter(
          Drug_confidence == "High",
          Target_confidence %in% c("High", "Medium")
        ) %>%
        dplyr::select(ID, Drug, Target) %>%
        dplyr::distinct() %>%
        dplyr::filter(Drug %in% top_k_union_drugs$Drug)
    } else {
      tibble::tibble(ID = character(), Drug = character(), Target = character())
    }

    .safe_annotation <- function(fun) {
      tryCatch(fun(), error = function(e) {
        warning(conditionMessage(e), call. = FALSE)
        NULL
      })
    }

    if (run_drug_annotation && isTRUE(can_annotate)) {
      .pipeline_message("Signature", "Running drug annotation and target enrichment.", 9, 10)
      target_list <- unique(stats::na.omit(top_k_drug_targets$Target))
      if (length(target_list) > 0) {
        GESS_result$DrugAnnotation$Functional_Enrichment <- TSEA(
          targetList = as.character(target_list),
          source = "GO",
          ont = "ALL",
          target_id_from = target_id_from
        )
      } else {
        GESS_result$DrugAnnotation$Functional_Enrichment <- NULL
        warning("No top_k drug targets available for functional enrichment.", call. = FALSE)
      }
      GESS_result$DrugAnnotation$top_k_union_drugs <- top_k_union_drugs
      GESS_result$DrugAnnotation$Features <- .safe_annotation(function() annotate_drugs(drugs = ranked_drugs, source = "All", condition = trial_condition, force = force, auth_token = auth_token))
    } else {
      .pipeline_message("Signature", if (run_drug_annotation) "Skipping drug annotation; annotation cache/token unavailable." else "Skipping drug annotation; run_drug_annotation=FALSE.", 9, 10)
      GESS_result$DrugAnnotation <- NULL
    }

    # =========================================================================
    # 7. Visualization
    # =========================================================================
    if (run_visualization) {
      .pipeline_message("Signature", "Building visualization payload.", 10, 10)
      plot_inputs <- .build_plot_inputs(
        rank_df = GESS_result$RankAggregation$Signature_Harmonized,
        top_k_union_drugs = top_k_union_drugs,
        features_df = GESS_result$DrugAnnotation$Features,
        functional_enrichment = GESS_result$DrugAnnotation$Functional_Enrichment,
        top_k = top_k,
        trial_condition = trial_condition
      )
      GESS_result$Visualization <- .build_visualization(plot_inputs)
    } else {
      .pipeline_message("Signature", "Skipping visualization; run_visualization=FALSE.", 10, 10)
      GESS_result$Visualization <- NULL
    }
    GESS_result$PipelineObject <- pipeline_builder(
      raw = GESS_result$DrugSearching$Raw,
      processed = GESS_result$DrugSearching$Processed,
      rank_aggregation = GESS_result$RankAggregation,
      drug_annotation = GESS_result$DrugAnnotation,
      visualization = GESS_result$Visualization,
      type = "signature"
    )

    last_result <- GESS_result
    GESS_result
  }, error = function(e) {
    warning(sprintf("drugSignaturePipeline encountered an error and is returning the last available result: %s", conditionMessage(e)), call. = FALSE)
    if (length(last_result) == 0) {
      partial_result <- list()
    } else {
      partial_result <- last_result
    }
    if (is.list(partial_result) &&
        !is.null(partial_result$DrugSearching) &&
        !is.null(partial_result$RankAggregation) &&
        !is.null(partial_result$DrugAnnotation) &&
        is.null(partial_result$PipelineObject)) {
      partial_result$PipelineObject <- pipeline_builder(
        raw = partial_result$DrugSearching$Raw,
        processed = partial_result$DrugSearching$Processed,
        rank_aggregation = partial_result$RankAggregation,
        drug_annotation = partial_result$DrugAnnotation,
        visualization = partial_result$Visualization,
        type = "signature"
      )
    }
    attr(partial_result, "error") <- conditionMessage(e)
    partial_result
  })

  result <- ensure_pipeline_sections(result)
  if (is.null(result$PipelineObject)) {
    result$PipelineObject <- pipeline_builder(
      raw = result$DrugSearching$Raw,
      processed = result$DrugSearching$Processed,
      rank_aggregation = result$RankAggregation,
      drug_annotation = result$DrugAnnotation,
      visualization = result$Visualization,
      type = "signature"
    )
  }
  result$PipelineObject
}

# =====================
# Helper functions
# =====================

.normalize_frozen_lincs2_result <- function(gess_tb, refdb, signature_refdb_mode) {
  if (identical(signature_refdb_mode, "default") ||
      !identical(refdb, "lincs2") ||
      !is.data.frame(gess_tb) ||
      !"pert" %in% names(gess_tb) ||
      "pert_id" %in% names(gess_tb)) {
    return(gess_tb)
  }

  utils::data("lincs_pert_info2", package = "signatureSearch", envir = environment())
  gess_tb %>%
    dplyr::left_join(
      lincs_pert_info2[, c("pert_id", "pert_iname")],
      by = c(pert = "pert_id")
    ) %>%
    dplyr::relocate(pert_iname, .after = pert) %>%
    dplyr::rename(pert_id = pert, pert = pert_iname)
}

.standardize_drug_name_synonym <- function(drug_name_synonym) {
  if (!is.data.frame(drug_name_synonym)) {
    stop("`drug_name_synonym` must be a data frame.", call. = FALSE)
  }

  # Support the new public column aliases, but otherwise preserve the old input
  # exactly (row order, duplicates, missing values, and value types).
  if (!"name" %in% names(drug_name_synonym) && "Drug" %in% names(drug_name_synonym)) {
    drug_name_synonym <- dplyr::rename(drug_name_synonym, name = Drug)
  }

  if (!"group" %in% names(drug_name_synonym) && "Group" %in% names(drug_name_synonym)) {
    drug_name_synonym <- dplyr::rename(drug_name_synonym, group = Group)
  }

  required_cols <- c("ID", "name", "name_synonym_list", "group")
  missing_cols <- setdiff(required_cols, names(drug_name_synonym))
  if (length(missing_cols) > 0L) {
    stop(
      "`drug_name_synonym` must contain columns `ID`, `Drug`, `name_synonym_list`, and `Group` ",
      "(legacy `ID`, `name`, `name_synonym_list`, and `group` are also supported). Missing: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  drug_name_synonym
}


.transform_ranks <- function(df, ties_method = "max", reverse = TRUE) {
  if (!is.data.frame(df) || ncol(df) < 2) {
    return(df)
  }

  rank_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  rank_cols <- setdiff(rank_cols, "Drug")

  if (length(rank_cols) == 0) {
    return(df)
  }

  for (col in rank_cols) {
    x <- df[[col]]
    if (all(is.na(x))) {
      df[[col]] <- NA_real_
    } else {
      df[[col]] <- .rank_values(x, ties_method, decreasing = isTRUE(reverse))
    }
  }

  df
}

.calculate_rank_aggregation <- function(method, df1, df2, prior, num_bin, ties_method) {
  df1_compact <- df1 %>%
    dplyr::select(perturbation, rank_score) %>% unique()
  # dplyr::group_by(perturbation) %>%
  # dplyr::summarise(rank_score = suppressWarnings(min(rank_score, na.rm = TRUE)), .groups = "drop")

  df2_compact <- df2 %>%
    dplyr::select(perturbation, rank_score) %>% unique()
  # dplyr::group_by(perturbation) %>%
  # dplyr::summarise(rank_score = suppressWarnings(min(rank_score, na.rm = TRUE)), .groups = "drop")

  combined_data <- dplyr::full_join(
    df1_compact %>% dplyr::rename(rank_score_1 = rank_score),
    df2_compact %>% dplyr::rename(rank_score_2 = rank_score),
    by = "perturbation"
  ) %>%
    dplyr::mutate(
      rank_score_1 = ifelse(is.na(rank_score_1), 0, rank_score_1),
      rank_score_2 = ifelse(is.na(rank_score_2), 0, rank_score_2)
    ) %>%
    dplyr::rename(Drug = perturbation)

  if (method == "CRank") {
    # Processed `rank_score` values are scores where larger is better.
    # CRank's R pre-processing then Python backend ranks negative columns, so
    # `reverse = FALSE` preserves the intended larger-is-better direction here.
    return(CRank(
      input_data = combined_data,
      ties_method = ties_method,
      prior = prior,
      num_bin = num_bin,
      num_iter = 1000,
      reverse = FALSE
    ))
  }

  if (method == "Dowdall") {
    return(Dowdall(
      input_data = combined_data,
      ties_method = ties_method,
      reverse = TRUE
    ))
  }

  if (method == "RRA") {
    return(RRA(
      input_data = combined_data,
      ties_method = ties_method,
      reverse = TRUE,
      full = TRUE,
      exact = FALSE
    ))
  }

  stop(sprintf("Unsupported rank aggregation method: %s", method), call. = FALSE)
}

.extract_drug_score_table <- function(result_df, score_col, method = NULL) {
  id_candidates <- c("Drug", "perturbation", "Name", "Item")
  id_col <- id_candidates[id_candidates %in% names(result_df)][1]

  if (is.na(id_col) || !length(id_col)) {
    stop("Could not identify a drug identifier column in rank aggregation output.", call. = FALSE)
  }

  if (!score_col %in% names(result_df)) {
    fallback_cols <- if (!is.null(method) && method == "CRank") {
      grep("(^|_)CRank$", names(result_df), value = TRUE)
    } else if (!is.null(method) && method == "Dowdall") {
      grep("(^|_)Dowdall(_rank)?$", names(result_df), value = TRUE)
    } else if (!is.null(method) && method == "RRA") {
      grep("(^|_)RRA(_rank)?$", names(result_df), value = TRUE)
    } else {
      character(0)
    }

    if (length(fallback_cols) > 0) {
      score_col <- fallback_cols[1]
    } else {
      stop(sprintf("Expected score column '%s' not found in rank aggregation output.", score_col), call. = FALSE)
    }
  }

  result_df %>%
    dplyr::transmute(
      Drug = as.character(.data[[id_col]]),
      .score_tmp = as.numeric(.data[[score_col]])
    ) %>%
    dplyr::group_by(Drug) %>%
    dplyr::summarise(
      .score_tmp = if (all(is.na(.score_tmp))) NA_real_ else suppressWarnings(min(.score_tmp, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::rename(!!score_col := .score_tmp)
}

.Drug_mapping <- function(perturbation_df, trend_name, drug_name_synonym, drug_name_synonym_edit) {
  pert_new <- perturbation_df %>%
    dplyr::select(pert) %>%
    unique() %>%
    dplyr::mutate(
      pert_lc = tolower(pert) %>%
        gsub(pattern = "^\\(.*\\)-", replacement = "", x = ., perl = TRUE)
    )

  map <- merge(
    pert_new,
    drug_name_synonym_edit,
    by.x = "pert_lc",
    by.y = "name_synonym_list"
  ) %>%
    dplyr::select(-pert_lc) %>%
    unique() %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(., drug_name_synonym)

  if (!is.null(trend_name)) {
    res <- perturbation_df %>%
      dplyr::filter(trend %in% trend_name) %>%
      dplyr::left_join(., map)
  } else {
    res <- perturbation_df %>%
      dplyr::left_join(., map)
  }

  return(res)
}

.input_structure <- function(signature_input) {
  # Convert FC column to numeric value and rename it to logfc
  signature_input <- signature_input %>%
    dplyr::mutate(FC = as.numeric(FC)) %>%
    dplyr::rename('logfc'='FC') %>%
    dplyr::mutate(Entrez = as.character(Entrez)) %>%
    unique()

  # Extract up regulated genes  and return maximum logfc for having multiple values genes
  signature_input_up <- signature_input %>%
    dplyr::filter(logfc > 0) %>%
    dplyr::group_by(Entrez) %>%
    dplyr::slice_max(logfc, n = 1)  %>%
    dplyr::arrange(dplyr::desc(logfc)) %>%
    unique()

  # Extract down regulated genes and return maximum logfc for having multiple values genes
  signature_input_down <- signature_input %>%
    dplyr::filter(logfc < 0) %>%
    dplyr::group_by(Entrez) %>%
    dplyr::slice_max(abs(logfc), n = 1) %>%
    #dplyr::select(-c(Ensembl,direction,tissue)) %>%
    dplyr::arrange(logfc) %>%
    unique()

  # Check the overlap btw up and down DEGs
  overlap <- intersect(signature_input_up$Entrez, signature_input_down$Entrez)

  # Exclude the overlapped genes
  if (length(overlap) > 0) {
    signature_input_up <- signature_input_up %>% dplyr::filter(!Entrez %in% overlap)
    signature_input_down <- signature_input_down %>% dplyr::filter(!Entrez %in% overlap)
  }

  signature_input_combined <- bind_rows(signature_input_up, signature_input_down) %>%
    mutate(logfc = as.numeric(logfc)) %>%
    tibble::column_to_rownames(var = "Entrez") %>%
    as.matrix()

  res <- list(up = as.matrix(signature_input_up$Entrez), down = as.matrix(signature_input_down$Entrez), exp = signature_input_combined)
  return(res)
}
