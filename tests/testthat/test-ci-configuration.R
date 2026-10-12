test_that("non-package installer entry points are excluded from source builds", {
  root <- testthat::test_path("..", "..")
  buildignore <- readLines(file.path(root, ".Rbuildignore"), warn = FALSE)

  expect_true(all(c(
    "^install\\.R$", "^install-pixi\\.R$", "^install\\.sh$"
  ) %in% buildignore))
})

test_that("CI uses a compatible fixed R and Bioconductor release", {
  root <- testthat::test_path("..", "..")
  workflow <- paste(readLines(
    file.path(root, ".github", "workflows", "rworkflows.yml"), warn = FALSE
  ), collapse = "\n")

  expect_match(workflow, "bioc: '3.22'", fixed = TRUE)
  expect_match(workflow, "r: '4.5.3'", fixed = TRUE)
  expect_match(workflow, "RELEASE_3_22", fixed = TRUE)
  expect_match(workflow, "RETICULATE_USE_MANAGED_VENV: 'no'", fixed = TRUE)
})

test_that("runtime dependency constraints contain no caret syntax", {
  manifest <- readLines(
    testthat::test_path("..", "..", "tools", "runtime-dependencies.tsv"),
    warn = FALSE
  )

  expect_false(any(grepl("^", manifest, fixed = TRUE)))
})
