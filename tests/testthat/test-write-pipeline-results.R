test_that("write_pipeline_results accepts current S4 pipeline results", {
  skip_if_not_installed("openxlsx")

  pipeline_result <- methods::new(
    "DrugSearchingPipelineAnnotatedVisualized",
    DrugSearching = list(Raw = list(), Processed = list()),
    RankAggregation = list(
      Signature_Harmonized = data.frame(
        Drug = "example drug",
        RRA = 1,
        stringsAsFactors = FALSE
      )
    ),
    DrugAnnotation = list(
      Features = data.frame(Drug = "example drug", stringsAsFactors = FALSE)
    ),
    Visualization = list(),
    type = "signature"
  )
  output <- tempfile(fileext = ".xlsx")
  on.exit(unlink(output), add = TRUE)

  expect_identical(
    write_pipeline_results(pipeline_result, output, top_n = 100),
    output
  )
  expect_true(file.exists(output))
})
