test_that("trial-condition plot inputs survive missing trial annotations", {
  rank_df <- data.frame(
    Drug = c("drug_a", "drug_b"),
    Network_CRank = c(1, 2)
  )
  features_df <- data.frame(
    Drug = c("drug_a", "drug_b"),
    indication = c("", "")
  )

  plot_inputs <- .build_plot_inputs(
    rank_df = rank_df,
    features_df = features_df,
    top_k = 2,
    trial_condition = "covid"
  )

  expect_named(plot_inputs, c("top_k_hits", "top_k_overlap"))
  expect_true("Status" %in% names(plot_inputs$top_k_hits))
  expect_true(all(is.na(plot_inputs$top_k_hits$Status)))
  expect_true("Status" %in% names(plot_inputs$top_k_overlap))
  expect_true(all(is.na(plot_inputs$top_k_overlap$Status)))
})

test_that("status-based plot is omitted when no trial condition is requested", {
  rank_df <- data.frame(
    Drug = c("drug_a", "drug_b"),
    Network_CRank = c(1, 2)
  )

  plot_inputs <- .build_plot_inputs(
    rank_df = rank_df,
    top_k = 2
  )

  expect_false("top_k_hits" %in% names(plot_inputs))
  expect_true("top_k_overlap" %in% names(plot_inputs))
})

test_that("trial features can be loaded without full drug annotation", {
  testthat::local_mocked_bindings(
    get_drug_trials = function(drugs, condition, ...) {
      expect_identical(drugs, c("drug_a", "drug_b"))
      expect_identical(condition, "covid")
      methods::new(
        "DrugAnnotation",
        result = data.frame(
          matched_drug = c("drug_a", "drug_a"),
          Conditions = c("COVID-19", "COVID-19"),
          Phases = c("Phase 2", "Phase 3"),
          `NCT Number` = c("NCT1", "NCT2"),
          check.names = FALSE
        ),
        parameters = list()
      )
    },
    .package = "DrugSigNet"
  )

  features <- DrugSigNet:::.load_trial_features(
    drugs = c("drug_a", "drug_b"),
    condition = "covid"
  )

  expect_identical(features$Drug, "drug_a")
  expect_identical(features$Clinical_trial_phase, "Phase 2|Phase 3")
  expect_identical(features$NCT_Number, "NCT1|NCT2")
})

test_that("top-k hit plot supports trial conditions with no matches", {
  rank_df <- data.frame(
    Drug = c("drug_a", "drug_b"),
    Status = c(NA_character_, NA_character_),
    Network_CRank = c(1, 2)
  )

  plot <- plot_top_k_hits(
    drug_ranks_df = rank_df,
    plottype = "Barplot",
    file_name = ""
  )

  expect_s3_class(plot, "ggplot")
  expect_gt(nrow(plot$data), 0)
  expect_true(all(plot$data$count == 0))
})
