# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)

# Apple Silicon checks have exposed process-state corruption only after several
# native-backed test files have shared one R process.  The eventual segfault is
# nondeterministic (it has appeared in dplyr, vctrs, and graphics DLL loading),
# so the R function named in the final stack is not necessarily responsible.
# Run every test file in a fresh R process on macOS.  This retains the complete
# test suite and normal failure semantics, while ensuring that Python, Java,
# HDF5, and graphics finalizers from one test cannot affect another test file.
# Linux and Windows deliberately keep testthat's standard runner unchanged.
if (identical(Sys.info()[["sysname"]], "Darwin")) {
  test_files <- sort(list.files(
    "testthat",
    pattern = "^test.*\\.[rR]$",
    full.names = TRUE
  ))
  if (!length(test_files)) {
    stop("No testthat files were found for the isolated macOS test run.")
  }

  rscript <- file.path(R.home("bin"), "Rscript")
  inherited_libpaths <- .libPaths()
  run_test_file <- function(test_file) {
    message("\n==> macOS isolated test file: ", basename(test_file))
    expression <- paste0(
      ".libPaths(c(",
      paste(deparse(inherited_libpaths), collapse = ""),
      ", .libPaths())); ",
      "library(testthat); library(DrugSigNet); ",
      # Supplying package= gives each isolated file the same namespace-backed
      # test environment as test_check(), including access to internal helpers.
      "testthat::test_file(",
      paste(deparse(normalizePath(test_file, mustWork = TRUE)), collapse = ""),
      ", package = 'DrugSigNet', reporter = 'summary', ",
      "stop_on_failure = TRUE, ",
      "stop_on_warning = TRUE)"
    )
    system2(
      rscript,
      c("--vanilla", "-e", shQuote(expression)),
      stdout = "",
      stderr = "",
      env = "R_TESTS="
    )
  }

  # GitHub's Apple Silicon runners have enough capacity for two isolated R
  # processes. Running two files concurrently halves most of the isolation
  # overhead without sharing an R/native-library state between test files.
  # PSOCK is used deliberately: unlike forking, workers do not inherit loaded
  # native libraries from the test coordinator.
  worker_count <- min(2L, length(test_files))
  cluster <- parallel::makeCluster(worker_count, type = "PSOCK", outfile = "")
  parallel::clusterExport(
    cluster,
    c("rscript", "inherited_libpaths"),
    envir = environment()
  )
  statuses <- tryCatch(
    parallel::parLapply(cluster, test_files, run_test_file),
    finally = parallel::stopCluster(cluster)
  )

  failed <- !vapply(statuses, identical, logical(1), 0L)
  failures <- basename(test_files[failed])

  if (length(failures)) {
    stop(
      "Isolated macOS test process failed for: ",
      paste(failures, collapse = ", "),
      call. = FALSE
    )
  }
} else {
  library(DrugSigNet)
  test_check("DrugSigNet")
}
