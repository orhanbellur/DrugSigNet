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
