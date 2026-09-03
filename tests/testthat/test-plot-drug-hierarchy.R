test_that("alluvial hierarchy plots resolve the ggalluvial stratum stat", {
  hierarchy_df <- data.frame(
    ATC = c("Nervous system", "Nervous system", "Antineoplastic"),
    MoA = c("Channel blocker", "Receptor antagonist", "Kinase inhibitor"),
    Drug = c("drug_a", "drug_b", "drug_c"),
    N = c(3, 2, 5)
  )

  plot <- plot_drug_hierarchy(
    data_df = hierarchy_df,
    hierarchy_cols = c("ATC", "MoA", "Drug"),
    value_col = "N",
    plot_type = "alluvial"
  )

  expect_s3_class(plot, "ggplot")
  expect_s3_class(plot$layers[[3]]$stat, "StatStratum")
})
