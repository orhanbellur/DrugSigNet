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
  expect_true("Drug_Rankings" %in% openxlsx::getSheetNames(output))
})

test_that("write_pipeline_results supports all repurposing modes", {
  skip_if_not_installed("openxlsx")

  make_section <- function(name) {
    key <- switch(
      name,
      signature = "Signature_Harmonized",
      network = "Network_Harmonized",
      integrated = "Signature_Network_Harmonized"
    )
    stats::setNames(
      list(data.frame(Drug = paste(name, "drug"), RRA = 1)),
      key
    )
  }

  for (mode in c("signature", "network")) {
    ranking <- make_section(mode)
    direct <- DrugSearchingPipeline(RankAggregation = ranking, type = mode)
    wrapped <- DrugSearchingPipeline(
      RankAggregation = stats::setNames(list(ranking), mode),
      type = mode
    )
    direct_file <- tempfile(fileext = ".xlsx")
    wrapped_file <- tempfile(fileext = ".xlsx")
    on.exit(unlink(c(direct_file, wrapped_file)), add = TRUE)

    write_pipeline_results(direct, direct_file)
    write_pipeline_results(wrapped, wrapped_file)

    expect_identical(
      openxlsx::getSheetNames(wrapped_file),
      openxlsx::getSheetNames(direct_file)
    )
    expect_equal(
      openxlsx::read.xlsx(wrapped_file, sheet = "Drug_Rankings"),
      openxlsx::read.xlsx(direct_file, sheet = "Drug_Rankings")
    )
  }

  sections <- c("signature", "network", "integrated")
  combined <- DrugSearchingPipeline(
    RankAggregation = stats::setNames(lapply(sections, make_section), sections),
    type = "both"
  )
  combined_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(combined_file), add = TRUE)

  write_pipeline_results(combined, combined_file)

  expect_true(all(
    paste0(sections, "_Drug_Rankings") %in%
      openxlsx::getSheetNames(combined_file)
  ))
})
