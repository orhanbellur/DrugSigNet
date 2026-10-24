test_that("non-package installer entry points are excluded from source builds", {
  root <- drugsignet_repository_root()
  buildignore <- readLines(file.path(root, ".Rbuildignore"), warn = FALSE)

  expect_true(all(c(
    "^install\\.R$", "^install-pixi\\.R$", "^install\\.sh$"
  ) %in% buildignore))
})

test_that("CI uses a compatible fixed R and Bioconductor release", {
  root <- drugsignet_repository_root()
  workflow <- paste(readLines(
    file.path(root, ".github", "workflows", "rworkflows.yml"), warn = FALSE
  ), collapse = "\n")

  expect_match(workflow, "bioc: '3.22'", fixed = TRUE)
  expect_match(workflow, "r: '4.5.2'", fixed = TRUE)
  expect_match(workflow, "r: '4.5.3'", fixed = TRUE)
  expect_match(workflow, "RELEASE_3_22-R-4.5.2", fixed = TRUE)
  expect_match(workflow, "RETICULATE_USE_MANAGED_VENV: 'yes'", fixed = TRUE)
})

test_that("runtime dependency constraints contain no caret syntax", {
  manifest <- readLines(
    drugsignet_repository_file("tools", "runtime-dependencies.tsv"),
    warn = FALSE
  )

  expect_false(any(grepl("^", manifest, fixed = TRUE)))
})
