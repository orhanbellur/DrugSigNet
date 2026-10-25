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

test_that("RRA dense ranking preserves ties and missing values without dplyr", {
  expect_identical(
    DrugSigNet:::.drugsignet_dense_rank(c(3, 1, 3, NA_real_, 2)),
    c(3L, 1L, 3L, NA_integer_, 2L)
  )

  rank_df <- data.frame(
    Drug = c("drug_a", "drug_b", "drug_c", "drug_d"),
    method_1 = c(4, 2, 2, NA),
    method_2 = c(1, 3, 3, 4),
    stringsAsFactors = FALSE
  )

  result <- RRA(
    input_data = rank_df,
    ties_method = "dense",
    reverse = TRUE
  )

  expect_s4_class(result, "RankAggregation")
  expect_identical(names(result@result), c("Drug", "RRA_pval", "RRA_rank"))
  expect_type(result@result$RRA_rank, "integer")
  expect_equal(
    result@result$RRA_rank,
    DrugSigNet:::.drugsignet_dense_rank(result@result$RRA_pval)
  )
})
