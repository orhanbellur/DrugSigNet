test_that("automatic pipeline features are hard package dependencies", {
  description <- read.dcf(system.file("DESCRIPTION", package = "DrugSigNet"))
  imports <- trimws(strsplit(description[1, "Imports"], ",", fixed = TRUE)[[1]])

  expect_true(all(c(
    "data.table", "enrichR", "ggalluvial", "ggforce", "plotly", "wordcloud"
  ) %in% imports))

  suggests <- trimws(strsplit(description[1, "Suggests"], ",", fixed = TRUE)[[1]])
  expect_false("signatureSearch" %in% imports)
  expect_true("signatureSearch" %in% suggests)
})
