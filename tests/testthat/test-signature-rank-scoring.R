signature_score_fixture <- function(scores = c(-0.9, 0.9, -0.6, 0.01)) {
  data.frame(pert = letters[seq_along(scores)], cell = "A375",
    trend = ifelse(scores < 0, "down", "up"), raw_score = scores,
    scaled_score = scores, N_upset = 50L, N_downset = 50L,
    WTCS = scores, WTCS_Pval = c(1e-9, 1e-8, 1e-7, 0.9),
    effect = scores, cor_score = scores)
}

signature_scores_by_drug <- function(x) stats::setNames(x$rank_score, x$perturbation)

test_that("all four signature scorers preserve main's reciprocal-rank semantics", {
  testthat::local_mocked_bindings(.drugsignet_ks_critical_value = function(...) 0.2, .package = "DrugSigNet")
  raw <- signature_score_fixture()
  for (ties in c("max", "min", "average", "dense")) {
    expected <- switch(ties, max = c(1/2, 1/2, 1/3, 0),
      min = c(1, 1, 1/3, 0), average = c(2/3, 2/3, 1/3, 0), dense = c(1, 1, 1/2, 0))
    names(expected) <- letters[1:4]
    results <- list(cmap_rank_score(raw, ties_method = ties),
      lincs_rank_score(raw, ties_method = ties),
      gcmap_rank_score(raw, raw, ties_method = ties),
      correlation_rank_score(raw, n = 100, ties_method = ties))
    for (result in results) expect_equal(signature_scores_by_drug(result)[names(expected)], expected)
  }
})

test_that("LINCS BY correction and correlation significance gates are retained", {
  raw <- signature_score_fixture()
  raw$WTCS_Pval <- c(0.04, 0.9, 0.0001, 0.9)
  lincs <- signature_scores_by_drug(lincs_rank_score(raw))
  expect_equal(unname(lincs[c("a", "b", "c", "d")]), c(0, 0, 1/3, 0))
  cor <- signature_scores_by_drug(correlation_rank_score(raw, n = 100))
  expect_equal(unname(cor[c("a", "b", "c", "d")]), c(1/2, 1/2, 1/3, 0))
})

test_that("KS compatibility preserves the main branch's dependency cutoff", {
  skip_if_not_installed("BoutrosLab.plotting.general")
  ns <- asNamespace("BoutrosLab.plotting.general")
  available <- intersect(c("critical.value.ks.test", "ks.test.critical.value"), ls(ns, all.names = TRUE))
  skip_if(!length(available))
  reference <- get(available[1], ns)
  for (n in c(20, 100, 500)) expect_equal(DrugSigNet:::.drugsignet_ks_critical_value(n, 0.95), reference(n, 0.95))
})

test_that("frozen LINCS identifiers are normalized before grouping and scoring", {
  testthat::local_mocked_bindings(.drugsignet_ks_critical_value = function(...) 0.2, .package = "DrugSigNet")
  raw <- signature_score_fixture(c(-0.9, -0.85, -0.8, -0.01))
  raw$pert <- c("id_a1", "id_a2", "id_b", "id_c")
  lookup <- data.frame(pert_id = raw$pert, pert_iname = c("drug_a", "drug_a", "drug_b", "drug_c"))
  effective <- DrugSigNet:::.signature_effective_refdb_mode("default", 2L)
  fixed <- DrugSigNet:::.normalize_frozen_lincs2_result(raw, "lincs2", effective, metadata = lookup)
  reference <- raw; reference$pert <- lookup$pert_iname
  for (score in list(function(x) cmap_rank_score(x), function(x) lincs_rank_score(x),
    function(x) gcmap_rank_score(x, reference), function(x) correlation_rank_score(x, n = 100))) {
    expect_equal(signature_scores_by_drug(score(fixed)), signature_scores_by_drug(score(reference)))
  }
  expect_equal(unname(signature_scores_by_drug(cmap_rank_score(fixed))["drug_b"]), 1/2)
  skipped <- DrugSigNet:::.normalize_frozen_lincs2_result(raw, "lincs2", "default", metadata = lookup)
  expect_equal(unname(signature_scores_by_drug(cmap_rank_score(skipped))["id_b"]), 1/3)
})

test_that("unmapped frozen perturbations retain distinct original identifiers", {
  raw <- signature_score_fixture(); raw$pert <- c("known", "unknown_a", "unknown_b", "blank")
  lookup <- data.frame(pert_id = c("known", "blank"), pert_iname = c("drug_a", ""))
  fixed <- DrugSigNet:::.normalize_frozen_lincs2_result(raw, "lincs2", "frozen", metadata = lookup)
  expect_identical(fixed$pert, c("drug_a", "unknown_a", "unknown_b", "blank"))
  expect_identical(fixed$pert_id, raw$pert)
  expect_identical(fixed$raw_score, raw$raw_score)
})

test_that("pipeline normalization receives effective reference mode in both worker configurations", {
  lookup <- data.frame(pert_id = c("id_a1", "id_a2", "id_b"), pert_iname = c("drug_a", "drug_a", "drug_b"))
  modes <- character()
  prepared_references <- list()
  normalize <- DrugSigNet:::.normalize_frozen_lincs2_result
  aggregation <- function(method) function(object = NULL, input_data, ...) {
    value <- data.frame(Drug = as.character(input_data[[1]]))
    value[[switch(method, CRank = "CRank", Dowdall = "Dowdall_rank", RRA = "RRA_rank")]] <- seq_len(nrow(value))
    RankAggregation(result = value, rank_aggregation = method, input_data = input_data)
  }
  testthat::local_mocked_bindings(
    .drugsignet_require_signature_search = function() invisible(TRUE),
    .prepare_signature_hub = function(refdb) {
      prepared_references[[length(prepared_references) + 1L]] <<- refdb
      invisible(TRUE)
    },
    load_drugsignet_network = function(...) data.frame(ID = character(), Drug = character(), Target = character(),
      Drug_confidence = character(), Target_confidence = character()),
    .drugsignet_ks_critical_value = function(...) 0.2,
    .resolve_signature_refdb = function(ref_db, ...) paste0("/frozen/", ref_db, ".h5"),
    run_pipeline_tasks = function(tasks, FUN, ...) {
      expect_true(length(prepared_references) > 0L)
      lapply(tasks, FUN)
    },
    .run_signature_method_task = function(task) {
      raw <- signature_score_fixture()[1:3, ]
      raw$raw_score <- raw$scaled_score <- raw$WTCS <- raw$effect <- raw$cor_score <- c(-.9, -.85, -.8)
      raw$pert <- if (task$db_key == "lincs2" && grepl("/frozen/", task$ref_db, fixed = TRUE)) lookup$pert_id else lookup$pert_iname
      SignatureBased(result = raw, signature_method = task$family, refdb = task$ref_db)
    },
    .normalize_frozen_lincs2_result = function(gess_tb, refdb, signature_refdb_mode) {
      modes <<- c(modes, signature_refdb_mode)
      normalize(gess_tb, refdb, signature_refdb_mode, metadata = lookup)
    },
    CRank = aggregation("CRank"), Dowdall = aggregation("Dowdall"), RRA = aggregation("RRA"),
    .package = "DrugSigNet")
  input <- data.frame(Entrez = as.character(1:100), FC = rep(c(1, -1), each = 50))
  run <- function(workers) suppressMessages(drugSignaturePipeline(input, n_workers = workers,
    run_drug_annotation = FALSE, run_visualization = FALSE))
  sequential <- run(1L)
  expect_true(all(modes == "default"))
  modes <- character()
  parallel <- run(2L)
  expect_identical(prepared_references, list(c("cmap", "lincs2"), character()))
  expect_true(all(modes == "frozen"))
  for (method in names(sequential@DrugSearching$Processed)) {
    expect_equal(signature_scores_by_drug(parallel@DrugSearching$Processed[[method]]),
      signature_scores_by_drug(sequential@DrugSearching$Processed[[method]]))
  }
  expect_setequal(parallel@RankAggregation$Signature_Harmonized$Drug, c("drug_a", "drug_b"))
})

test_that("all three aggregators preserve the intended signature-score direction", {
  scores <- data.frame(perturbation = paste0("drug_", letters[1:6]), rank_score = 1/(1:6))
  for (method in c("CRank", "Dowdall", "RRA")) {
    result <- DrugSigNet:::.calculate_rank_aggregation(method, scores, scores, prior = 0.5, num_bin = 6L, ties_method = "max")@result
    col <- switch(method, CRank = "CRank", Dowdall = "Dowdall_rank", RRA = "RRA_rank")
    ranks <- stats::setNames(result[[col]], result$Drug)[scores$perturbation]
    expect_true(all(diff(ranks) >= 0), info = method)
    expect_lt(ranks[1], ranks[6])
    if (method == "Dowdall") expect_equal(unname(ranks), 1:6)
    if (method == "RRA") expect_equal(result$RRA_pval[match("drug_a", result$Drug)], 2/36)
  }
})
