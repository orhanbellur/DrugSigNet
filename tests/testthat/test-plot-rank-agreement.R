test_that("plot_rank_agreement uses Kendall correlation by default", {
  rank_df <- data.frame(
    Drug = paste0("drug_", 1:4),
    Method1 = 1:4,
    Method2 = c(1, 3, 2, 4)
  )

  result <- plot_rank_agreement(rank_df, cluster = FALSE)

  expect_identical(result$method, "kendall")
})
