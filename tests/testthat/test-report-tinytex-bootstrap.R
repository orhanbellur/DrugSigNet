report_tex_fixture <- function() {
  root <- tempfile("tinytex user path ")
  bin <- file.path(root, "bin", "test-platform")
  dir.create(bin, recursive = TRUE)
  name <- if (.Platform$OS.type == "windows") "pdflatex.exe" else "pdflatex"
  writeLines(c("#!/bin/sh", "exit 0"), file.path(bin, name))
  Sys.chmod(file.path(bin, name), "0755")
  list(root = root, bin = bin)
}

test_that("TinyTeX extracts xz bundles with no xz and an unusable external TAR", {
  work <- tempfile("tinytex-extract-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  withr::local_dir(work)
  dir.create("TinyTeX")
  writeLines("native R xz extraction", "TinyTeX/fixture.txt")
  utils::tar("bundle.tar.xz", "TinyTeX", compression = "xz", tar = "internal")
  dir.create("extracted")
  withr::local_envvar(c(PATH = file.path(work, "no-tools"), TAR = "/missing/tar"))
  expect_identical(unname(Sys.which("xz")), "")
  # Windows searches System32 even with an empty PATH. Control the extractor
  # explicitly instead of assuming the system's tar.exe cannot be discovered.
  expect_false(file.exists(Sys.getenv("TAR")))
  testthat::local_mocked_bindings(install_tinytex = function(add_path) {
    expect_false(add_path)
    expect_identical(Sys.getenv("TAR"), "internal")
    # Use the same public extraction API as TinyTeX's actual installer.
    utils::untar("bundle.tar.xz", exdir = "extracted")
    normalizePath("extracted/TinyTeX")
  }, .package = "tinytex")
  root <- .report_install_tinytex()
  expect_identical(readLines(file.path(root, "fixture.txt")), "native R xz extraction")
  expect_identical(Sys.getenv("TAR"), "/missing/tar")
})

test_that("TinyTeX installation errors restore an unset TAR setting", {
  withr::local_envvar(c(TAR = NA_character_))
  testthat::local_mocked_bindings(install_tinytex = function(...) stop("download failed"),
                                 .package = "tinytex")
  expect_error(.report_install_tinytex(), "download failed")
  expect_true(is.na(Sys.getenv("TAR", unset = NA_character_)))
})

test_that("an existing user TinyTeX is reused without reinstalling", {
  fixture <- report_tex_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  withr::local_envvar(c(PATH = file.path(fixture$root, "no-tools")))
  testthat::local_mocked_bindings(tinytex_root = function(error) fixture$root,
    install_tinytex = function(...) stop("existing installation must be reused"),
    .package = "tinytex")
  expect_identical(.report_latex_engine(), "pdflatex")
  expect_identical(normalizePath(unname(Sys.which("pdflatex")), winslash = "/"),
    normalizePath(file.path(fixture$bin,
      if (.Platform$OS.type == "windows") "pdflatex.exe" else "pdflatex"), winslash = "/"))
})

test_that("a freshly installed engine is usable without restarting R", {
  fixture <- report_tex_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  withr::local_envvar(c(PATH = file.path(fixture$root, "no-tools"), TAR = "custom-tar"))
  testthat::local_mocked_bindings(tinytex_root = function(error) "",
    install_tinytex = function(add_path) {
      expect_false(add_path)
      expect_identical(Sys.getenv("TAR"), "internal")
      fixture$root
    }, .package = "tinytex")
  expect_message(expect_identical(.report_latex_engine(), "pdflatex"), "installing TinyTeX")
  expect_identical(Sys.getenv("TAR"), "custom-tar")
})

test_that("system LaTeX is reused and genuine install errors remain visible", {
  fixture <- report_tex_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  withr::local_envvar(c(PATH = fixture$bin))
  testthat::local_mocked_bindings(tinytex_root = function(...) stop("system engine should be used"),
    install_tinytex = function(...) stop("system engine should be used"), .package = "tinytex")
  expect_identical(.report_latex_engine(), "pdflatex")
})

test_that("PDF setup restores PATH when LaTeX initialization fails", {
  original <- Sys.getenv("PATH")
  testthat::local_mocked_bindings(.report_latex_engine = function() {
    Sys.setenv(PATH = "temporary-tex-path")
    stop("initialization failed")
  }, .package = "DrugSigNet")
  expect_error(.write_pdf_report(list(), "report", tempfile(), NULL, 20L, 12L, FALSE),
               "initialization failed")
  expect_identical(Sys.getenv("PATH"), original)
})

test_that("PDF setup reports failed extraction instead of requiring a restart", {
  withr::local_envvar(c(PATH = tempfile("no-tools-")))
  testthat::local_mocked_bindings(tinytex_root = function(...) "",
    install_tinytex = function(...) stop("archive download failed"), .package = "tinytex")
  expect_message(expect_error(.report_latex_engine(), "TinyTeX install error: archive download failed"),
                 "installing TinyTeX")
})
