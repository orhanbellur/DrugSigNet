test_that("core dependencies and conditional plotting dependencies are classified consistently", {
  description <- read.dcf(system.file("DESCRIPTION", package = "DrugSigNet"))
  imports <- trimws(strsplit(description[1, "Imports"], ",", fixed = TRUE)[[1]])

  expect_true(all(c(
    "data.table", "enrichR", "plotly"
  ) %in% imports))

  suggests <- trimws(strsplit(description[1, "Suggests"], ",", fixed = TRUE)[[1]])
  expect_false(any(c("ggalluvial", "ggforce", "wordcloud") %in% imports))
  expect_true(all(c("ggalluvial", "ggforce", "wordcloud") %in% suggests))
  expect_false("signatureSearch" %in% imports)
  expect_true("signatureSearch" %in% suggests)
  expect_false("BoutrosLab.plotting.general" %in% imports)
  expect_true("BoutrosLab.plotting.general" %in% suggests)
})
