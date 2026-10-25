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

test_that("scientific report model summarizes partial and integrated outputs", {
  make_result <- function(type, prefix) list(
    DrugSearching = list(Raw = list(), Processed = list()),
    RankAggregation = stats::setNames(list(data.frame(
      Drug = c("shared", paste0(prefix, "_drug")),
      CRank = c(2, 1), Dowdall_rank = c(1, 2), RRA_rank = c(2, 1)
    )), switch(type, signature = "Signature_Harmonized", network = "Network_Harmonized", "Signature_Network_Harmonized")),
    type = type
  )
  objects <- list(make_result("signature", "sig"), make_result("network", "net"), make_result("integration", "int"))
  model <- DrugSigNet:::.report_pdf_model(objects, top_n = 10)
  expect_identical(unname(model$counts), c(2L, 2L, 2L))
  expect_identical(model$overlap, 1L)
  expect_named(model$top[[1]], c("Drug", "CRank", "Dowdall_rank", "RRA_rank"))
  expect_identical(model$top[[1]]$Drug[[1]], "sig_drug")
})

test_that("scientific report helpers omit unavailable components gracefully", {
  partial <- list(DrugSearching = list(), RankAggregation = list(), type = "network")
  model <- DrugSigNet:::.report_pdf_model(list(partial), top_n = 20)
  expect_identical(model$counts, c(network = 0L))
  expect_null(model$top[[1]])
  expect_false(DrugSigNet:::.report_has_visualization(partial))
  expect_false(DrugSigNet:::.report_has_annotation(partial))
})

test_that("scientific PDF template includes publication report features", {
  yaml <- paste(DrugSigNet:::.report_pdf_template("Study", "Analyst"), collapse = "\n")
  expect_match(yaml, "toc: true", fixed = TRUE)
  expect_match(yaml, "number_sections: true", fixed = TRUE)
  expect_match(yaml, "fig_caption: true", fixed = TRUE)
  expect_match(yaml, "fancyhdr", fixed = TRUE)
})

test_that("scientific PDF tables avoid LaTeX resizebox overflow", {
  implementation <- paste(deparse(body(DrugSigNet:::.report_render_scientific_pdf)), collapse = "\n")
  expect_false(grepl("scale_down", implementation, fixed = TRUE))
  expect_match(implementation, "nchar(values, type='width') > 80L", fixed = TRUE)
  expect_match(implementation, "ceiling(seq_along", fixed = TRUE)
})

test_that("scientific PDF renders when LaTeX is available", {
  skip_if_not_installed("rmarkdown")
  skip_if(!any(nzchar(Sys.which(c("pdflatex", "xelatex", "lualatex")))))
  result <- DrugSearchingPipeline(
    RankAggregation = list(Signature_Harmonized = data.frame(
      Drug = c("drug_a", "drug_b"), Signature_CRank = 1:2,
      Signature_Dowdall = 2:1, Signature_RRA = 1:2
    )), type = "signature"
  )
  target <- tempfile("drugsignet-report-")
  pdf <- write_report(result, file = target, device = "pdf", include_appendix = FALSE)
  expect_true(file.exists(pdf))
  expect_gt(file.info(pdf)$size, 0)
})
