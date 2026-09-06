test_that("Python data-frame metadata is removed without changing R metadata", {
  result <- data.frame(Drug = "drug_a", CRank = 1)
  attr(result, "pandas.index") <- methods::new("externalptr")
  attr(result, "scientific_metadata") <- "keep"

  cleaned <- DrugSigNet:::.clean_python_result(result)

  expect_null(attr(cleaned, "pandas.index"))
  expect_identical(attr(cleaned, "scientific_metadata"), "keep")
  expect_identical(cleaned$Drug, result$Drug)
  expect_identical(cleaned$CRank, result$CRank)
})

test_that("CRank returns strictly reproducible ordinary R data frames", {
  skip_if_not_installed("reticulate")
  required_modules <- c("numpy", "pandas", "scipy", "openpyxl")
  skip_if_not(all(vapply(
    required_modules,
    reticulate::py_module_available,
    logical(1)
  )), "CRank Python dependencies are not available")

  rank_df <- data.frame(
    Drug = paste0("drug_", letters[1:6]),
    Method1 = c(6, 5, 4, 3, 2, 1),
    Method2 = c(5, 6, 3, 4, 1, 2)
  )

  result1 <- CRank(
    input_data = rank_df,
    prior = 0.5,
    num_bin = 6,
    num_iter = 2
  )
  result2 <- CRank(
    input_data = rank_df,
    prior = 0.5,
    num_bin = 6,
    num_iter = 2
  )

  expect_s3_class(result1@result, "data.frame")
  expect_null(attr(result1@result, "pandas.index"))
  expect_identical(result1@result, result2@result)

  # drugSignaturePipeline stores every pairwise and harmonized CRank object
  # through this same RankAggregation result path. Verify that wrapping the
  # repeated results in its user-facing S4 structure preserves strict identity.
  pipeline1 <- DrugSigNet:::build_pipeline_object(
    rank_aggregation = list(
      Pairwise = list(CMAP = list(CRank = result1))
    ),
    drug_annotation = list(),
    type = "signature"
  )
  pipeline2 <- DrugSigNet:::build_pipeline_object(
    rank_aggregation = list(
      Pairwise = list(CMAP = list(CRank = result2))
    ),
    drug_annotation = list(),
    type = "signature"
  )

  pipeline_crank <- pipeline1@RankAggregation$Pairwise$CMAP$CRank@result
  expect_null(attr(pipeline_crank, "pandas.index"))
  expect_identical(pipeline1@DrugSearching, pipeline2@DrugSearching)
  expect_identical(pipeline1@RankAggregation, pipeline2@RankAggregation)
  expect_identical(pipeline1@DrugAnnotation, pipeline2@DrugAnnotation)
})
