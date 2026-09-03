test_that("RRA results use Drug as the identifier column", {
  rank_df <- data.frame(
    Drug = c("drug_a", "drug_b", "drug_c"),
    method_1 = c(1, 2, 3),
    method_2 = c(2, 1, 3),
    stringsAsFactors = FALSE
  )

  result <- RRA(input_data = rank_df, reverse = FALSE)

  expect_identical(names(result@result), c("Drug", "RRA_pval", "RRA_rank"))
  expect_setequal(result@result$Drug, rank_df$Drug)
  expect_false("Name" %in% names(result@result))
})
