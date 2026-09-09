test_that("report input expansion preserves standalone pipeline sections", {
  result <- DrugSearchingPipeline(
    RankAggregation = list(Signature_Harmonized = data.frame(Drug = "drug_a")),
    DrugAnnotation = list(Features = data.frame(Drug = "drug_a")),
    Visualization = list(plots = list(example = "plot")),
    type = "signature"
  )

  expanded <- DrugSigNet:::.report_expand_pipeline_result(result)

  expect_length(expanded, 1)
  expect_identical(expanded[[1]]$DrugAnnotation$Features$Drug, "drug_a")
  expect_identical(expanded[[1]]$Visualization$plots$example, "plot")
})

test_that("write_report exposes performance controls", {
  expect_identical(formals(write_report)$table_rows, 100)
  expect_identical(formals(write_report)$self_contained, TRUE)
})

test_that("PDF reports prefer a Unicode-capable LaTeX engine", {
  engines <- c(
    pdflatex = "/tex/pdflatex",
    xelatex = "/tex/xelatex",
    lualatex = "/tex/lualatex"
  )

  expect_identical(
    DrugSigNet:::.report_select_latex_engine(engines),
    "xelatex"
  )
  expect_identical(
    DrugSigNet:::.report_select_latex_engine(engines[c("pdflatex", "lualatex")]),
    "lualatex"
  )
})

test_that("report input expansion preserves every repurposing section", {
  sections <- c("signature", "network", "integrated")
  result <- DrugSearchingPipeline(
    DrugSearching = list(
      Raw = stats::setNames(lapply(sections, function(x) list()), sections),
      Processed = stats::setNames(lapply(sections, function(x) list()), sections)
    ),
    RankAggregation = stats::setNames(lapply(sections, function(section) {
      list(Harmonized = data.frame(Drug = paste0(section, "_drug")))
    }), sections),
    DrugAnnotation = stats::setNames(lapply(sections, function(section) {
      list(Features = data.frame(Drug = paste0(section, "_drug")))
    }), sections),
    Visualization = stats::setNames(lapply(sections, function(section) {
      list(plots = stats::setNames(list("plot"), section))
    }), sections),
    type = "both"
  )

  expanded <- DrugSigNet:::.report_expand_pipeline_result(result)

  expect_length(expanded, 3)
  expect_identical(vapply(expanded, `[[`, character(1), "type"), c(
    "signature", "network", "integration"
  ))
  expect_identical(
    vapply(expanded, DrugSigNet:::.report_result_title, character(1)),
    c("Signature-based results", "Network-based results", "Integrated results")
  )
  expect_identical(
    vapply(expanded, function(x) x$DrugAnnotation$Features$Drug, character(1)),
    paste0(sections, "_drug")
  )
  expect_true(all(vapply(
    expanded,
    DrugSigNet:::.report_has_visualization,
    logical(1)
  )))
})
