test_that("signatureSearch data are available only for the wrapped operation", {
  skip_if_not_installed("signatureSearch")

  initially_attached <- "package:signatureSearch" %in% search()

  observed <- .with_signature_search_attached({
    expect_true("package:signatureSearch" %in% search())

    data_env <- new.env(parent = emptyenv())
    utils::data("clue_moa_list", envir = data_env)
    exists("clue_moa_list", envir = data_env, inherits = FALSE)
  })

  expect_true(observed)
  expect_identical(
    "package:signatureSearch" %in% search(),
    initially_attached
  )
})

test_that("signatureSearch remains attached when it was attached by the caller", {
  skip_if_not_installed("signatureSearch")

  initially_attached <- "package:signatureSearch" %in% search()
  if (!initially_attached) {
    suppressPackageStartupMessages(library("signatureSearch", character.only = TRUE))
    on.exit(
      detach("package:signatureSearch", unload = FALSE, character.only = TRUE),
      add = TRUE
    )
  }

  .with_signature_search_attached(NULL)

  expect_true("package:signatureSearch" %in% search())
})

test_that("nested signatureSearch operations restore attachment ownership", {
  skip_if_not_installed("signatureSearch")

  initially_attached <- "package:signatureSearch" %in% search()

  .with_signature_search_attached({
    expect_true("package:signatureSearch" %in% search())
    .with_signature_search_attached(
      expect_true("package:signatureSearch" %in% search())
    )
    expect_true("package:signatureSearch" %in% search())
  })

  expect_identical(
    "package:signatureSearch" %in% search(),
    initially_attached
  )
})
