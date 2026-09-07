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
  expect_identical(
    openxlsx::read.xlsx(output, sheet = "Drug_Rankings")$Drug,
    "example drug"
  )
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

test_that("single repurposing modes export like their standalone pipelines", {
  skip_if_not_installed("openxlsx")

  for (mode in c("signature", "network")) {
    harmonized_name <- if (mode == "signature") {
      "Signature_Harmonized"
    } else {
      "Network_Harmonized"
    }
    ranking <- stats::setNames(
      list(data.frame(Drug = "example drug", RRA = 1)),
      harmonized_name
    )
    annotation <- list(
      Features = data.frame(Drug = "example drug", indication = "example indication"),
      Functional_Enrichment = data.frame(Term = "example term")
    )
    searching <- list(
      Raw = list(method = data.frame(Drug = "example drug", Score = 1)),
      Processed = list()
    )

    standalone <- DrugSearchingPipeline(
      DrugSearching = searching,
      RankAggregation = ranking,
      DrugAnnotation = annotation,
      type = mode
    )
    repurposing <- DrugSearchingPipeline(
      DrugSearching = list(
        Raw = stats::setNames(list(searching$Raw), mode),
        Processed = stats::setNames(list(searching$Processed), mode)
      ),
      RankAggregation = stats::setNames(list(ranking), mode),
      DrugAnnotation = stats::setNames(list(annotation), mode),
      type = mode
    )

    standalone_output <- tempfile(fileext = ".xlsx")
    repurposing_output <- tempfile(fileext = ".xlsx")
    on.exit(unlink(c(standalone_output, repurposing_output)), add = TRUE)

    write_pipeline_results(standalone, standalone_output, top_n = 100)
    write_pipeline_results(repurposing, repurposing_output, top_n = 100)

    standalone_sheets <- openxlsx::getSheetNames(standalone_output)
    repurposing_sheets <- openxlsx::getSheetNames(repurposing_output)
    expect_identical(repurposing_sheets, standalone_sheets, info = mode)

    for (sheet in standalone_sheets) {
      expect_equal(
        openxlsx::read.xlsx(repurposing_output, sheet = sheet),
        openxlsx::read.xlsx(standalone_output, sheet = sheet),
        info = paste(mode, sheet)
      )
    }
  }
})
