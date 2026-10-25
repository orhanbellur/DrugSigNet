test_that("plot_rank_agreement uses Kendall correlation by default", {
  rank_df <- data.frame(
    Drug = paste0("drug_", 1:4),
    Method1 = 1:4,
    Method2 = c(1, 3, 2, 4)
  )

  result <- plot_rank_agreement(rank_df, cluster = FALSE)

  expect_identical(result$method, "kendall")
})

test_that("plot_rank_agreement selects explicit base graphics devices", {
  expect_identical(
    DrugSigNet:::.drugsignet_rank_agreement_device("pdf"),
    grDevices::pdf
  )
  expect_identical(
    DrugSigNet:::.drugsignet_rank_agreement_device("png"),
    grDevices::png
  )
  expect_error(
    DrugSigNet:::.drugsignet_rank_agreement_device("unknown"),
    "Unsupported plot file type"
  )
})
