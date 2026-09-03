test_that("integration readiness accepts a complete signature pipeline", {
  signature_result <- DrugSearchingPipeline(
    RankAggregation = list(Pairwise = list(CMAP = list(result = TRUE)))
  )

  expect_invisible(
    DrugSigNet:::.validate_signature_result_for_integration(signature_result)
  )
})

test_that("integration readiness rejects incomplete signature results", {
  signature_result <- DrugSearchingPipeline(RankAggregation = list())

  expect_error(
    DrugSigNet:::.validate_signature_result_for_integration(signature_result),
    "completed without pairwise rank aggregation results",
    fixed = TRUE
  )
})
