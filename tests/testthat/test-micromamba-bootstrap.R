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

test_that("unified entry point calls the sourced bootstrap function", {
  repository_root <- tempfile("drugsignet-bootstrap-test-")
  dir.create(file.path(repository_root, "tools"), recursive = TRUE)
  on.exit(unlink(repository_root, recursive = TRUE), add = TRUE)
  writeLines(c(
    "install_drugsignet_micromamba <- function(package_ref, repository_root, prefix) {",
    "  list(package_ref = package_ref, repository_root = repository_root, prefix = prefix)",
    "}"
  ), file.path(repository_root, "tools", "install_drugsignet_micromamba.R"))

  entry_environment <- new.env(parent = globalenv())
  base::sys.source(
    testthat::test_path("..", "..", "install.R"),
    envir = entry_environment
  )
  result <- entry_environment$install_drugsignet(
    package_ref = "owner/repository@branch",
    repository_root = repository_root,
    prefix = "/tmp/drugsignet-prefix"
  )

  expect_identical(result$package_ref, "owner/repository@branch")
  expect_identical(result$repository_root, repository_root)
  expect_identical(result$prefix, "/tmp/drugsignet-prefix")
})
