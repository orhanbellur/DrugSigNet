test_that("installation check classifies declared optional packages correctly", {
  result <- check_drugsignet_installation(
    check_python = FALSE,
    check_optional = TRUE
  )

  expect_true("wordcloud" %in% result$packages$package)
  expect_false(result$packages$required[result$packages$package == "wordcloud"])
})

test_that("optional packages are omitted when not requested", {
  result <- check_drugsignet_installation(
    check_python = FALSE,
    check_optional = FALSE
  )

  expect_false("wordcloud" %in% result$packages$package)
})
