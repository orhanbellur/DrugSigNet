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

test_that("write_pipeline_results exports every drug repurposing mode", {
  skip_if_not_installed("openxlsx")

  sections <- c("signature", "network", "integrated")
  harmonized_names <- c(
    signature = "Signature_Harmonized",
    network = "Network_Harmonized",
    integrated = "Signature_Network_Harmonized"
  )

  rank_aggregation <- stats::setNames(lapply(sections, function(section) {
    stats::setNames(
      list(data.frame(Drug = paste0(section, " drug"), RRA = 1)),
      harmonized_names[[section]]
    )
  }), sections)

  annotations <- stats::setNames(lapply(sections, function(section) {
    list(
      Features = data.frame(
        Drug = paste0(section, " drug"),
        indication = paste0(section, " indication")
      ),
      Functional_Enrichment = data.frame(
        Term = paste0(section, " term")
      )
    )
  }), sections)

  pipeline_result <- DrugSearchingPipeline(
    DrugSearching = list(
      Raw = stats::setNames(lapply(sections, function(section) {
        list(method = data.frame(Drug = paste0(section, " drug")))
      }), sections),
      Processed = list()
    ),
    RankAggregation = rank_aggregation,
    DrugAnnotation = annotations,
    type = "both"
  )

  output <- tempfile(fileext = ".xlsx")
  on.exit(unlink(output), add = TRUE)

  write_pipeline_results(pipeline_result, output, top_n = 100)
  sheets <- openxlsx::getSheetNames(output)

  expect_true(all(paste0(sections, "_Drug_Rankings") %in% sheets))
  expect_true(all(paste0(sections, "_Top_100_Drugs") %in% sheets))
  expect_true(all(paste0(sections, "_Drug_Annotations") %in% sheets))

  integrated_top <- openxlsx::read.xlsx(output, sheet = "integrated_Top_100_Drugs")
  expect_identical(integrated_top$indication, "integrated indication")
})
