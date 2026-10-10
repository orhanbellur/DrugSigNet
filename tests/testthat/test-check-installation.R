test_that("installation check classifies runtime visualization packages as required", {
  result <- check_drugsignet_installation(
    check_python = FALSE,
    check_optional = TRUE
  )

  expect_true("wordcloud" %in% result$packages$package)
  expect_true(result$packages$required[result$packages$package == "wordcloud"])
  expect_true(all(c("enrichR", "ggalluvial", "ggforce", "plotly") %in%
    result$packages$package[result$packages$required]))
})

test_that("required runtime packages remain when optional checks are disabled", {
  result <- check_drugsignet_installation(
    check_python = FALSE,
    check_optional = FALSE
  )

  expect_true(all(c("enrichR", "plotly", "wordcloud") %in% result$packages$package))
  expect_false(any(c("kableExtra", "tinytex") %in% result$packages$package))
})
