test_that("R bootstrap supports installation without a checkout", {
  source_file <- testthat::test_path(
    "..", "..", "tools", "install_drugsignet_micromamba.R"
  )
  bootstrap <- readLines(source_file, warn = FALSE)

  expect_true(any(grepl("repository_root = NULL", bootstrap, fixed = TRUE)))
  expect_true(any(grepl("raw.githubusercontent.com", bootstrap, fixed = TRUE)))
  expect_true(any(grepl("runtime-dependencies.tsv", bootstrap, fixed = TRUE)))
  expect_true(any(grepl("utils::download.file", bootstrap, fixed = TRUE)))
})
