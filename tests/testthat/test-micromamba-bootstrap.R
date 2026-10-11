test_that("R bootstrap supports installation without a checkout", {
  source_file <- testthat::test_path(
    "..", "..", "tools", "install_drugsignet_micromamba.R"
  )
  bootstrap <- readLines(source_file, warn = FALSE)

  expect_true(any(grepl("repository_root = NULL", bootstrap, fixed = TRUE)))
  expect_true(any(grepl("raw.githubusercontent.com", bootstrap, fixed = TRUE)))
  expect_true(any(grepl("curl --fail", bootstrap, fixed = TRUE)))
  expect_true(any(grepl("bash -s", bootstrap, fixed = TRUE)))
  expect_true(any(grepl("DRUGSIGNET_DEPENDENCY_MANIFEST_URL", bootstrap, fixed = TRUE)))
  expect_false(any(grepl("utils::download.file", bootstrap, fixed = TRUE)))
})

test_that("unified entry point runs a local shell bootstrap directly", {
  repository_root <- tempfile("drugsignet-bootstrap-test-")
  dir.create(file.path(repository_root, "tools"), recursive = TRUE)
  on.exit(unlink(repository_root, recursive = TRUE), add = TRUE)
  installer <- file.path(repository_root, "tools", "install_drugsignet_micromamba.sh")
  writeLines(c("#!/usr/bin/env bash", "exit 0"), installer)
  Sys.chmod(installer, "0755")
  file.create(file.path(repository_root, "tools", "runtime-dependencies.tsv"))

  entry_environment <- new.env(parent = globalenv())
  base::sys.source(
    testthat::test_path("..", "..", "install.R"),
    envir = entry_environment
  )
  result <- suppressMessages(entry_environment$install_drugsignet(
    package_ref = "owner/repository@branch",
    repository_root = repository_root,
    prefix = "/tmp/drugsignet-prefix"
  ))

  expect_identical(result, "/tmp/drugsignet-prefix/bin/R")
})

test_that("unified remote entry point does not download another R bootstrap", {
  entry <- readLines(testthat::test_path("..", "..", "install.R"), warn = FALSE)

  expect_true(any(grepl("curl --fail", entry, fixed = TRUE)))
  expect_true(any(grepl("bash -s", entry, fixed = TRUE)))
  expect_false(any(grepl("utils::download.file", entry, fixed = TRUE)))
  expect_false(any(grepl("sys.source", entry, fixed = TRUE)))
})
