test_that("non-package installer entry points are excluded from source builds", {
  root <- drugsignet_repository_root()
  buildignore <- readLines(file.path(root, ".Rbuildignore"), warn = FALSE)

  expect_true(all(c(
    "^install\\.R$", "^install-pixi\\.R$", "^install\\.sh$"
  ) %in% buildignore))
})

test_that("CI uses a compatible fixed R and Bioconductor release", {
  root <- drugsignet_repository_root()
  expect_false(file.exists(file.path(
    root, ".github", "workflows", "rworkflows.yml"
  )))
  workflow <- paste(readLines(
    file.path(root, ".github", "workflows", "drugsignet-check.yml"), warn = FALSE
  ), collapse = "\n")

  expect_match(workflow, "bioc: '3.22'", fixed = TRUE)
  expect_match(workflow, "r: '4.5.2'", fixed = TRUE)
  expect_match(workflow, "r: '4.5.3'", fixed = TRUE)
  expect_match(workflow, "RELEASE_3_22-R-4.5.2", fixed = TRUE)
  matrix_rows <- strsplit(workflow, "\n        - os: ", fixed = TRUE)[[1L]][-1L]
  older <- matrix_rows[grepl("r: '4.4.3'", matrix_rows, fixed = TRUE)]
  expect_setequal(vapply(strsplit(older, "\n", fixed = TRUE), `[[`, character(1), 1L),
                  c("ubuntu-latest", "macOS-latest", "windows-latest"))
  expect_true(all(grepl("bioc: '3.20'", older, fixed = TRUE)))
  expect_match(workflow, "R_BIOC_VERSION: ${{ matrix.config.bioc }}", fixed = TRUE)
  expect_true(all(grepl("https://packagemanager.posit.co/cran/2025-04-30", older, fixed = TRUE)))
  expect_match(workflow, "cran: ${{ matrix.config.rspm }}", fixed = TRUE)
  expect_match(workflow, "r44-cran-2025-04-30-v1", fixed = TRUE)
  expect_match(workflow, "later_1.4.7.tar.gz", fixed = TRUE)
  expect_match(workflow, "Register macOS Java with R 4.4", fixed = TRUE)
  expect_match(workflow, 'CMD javareconf JAVA_HOME="$JAVA_HOME"', fixed = TRUE)
  expect_match(workflow, 'source(".github/scripts/check-r44-plotting.R")', fixed = TRUE)
  expect_match(workflow, "RETICULATE_USE_MANAGED_VENV: 'no'", fixed = TRUE)
  expect_match(workflow, "r-lib/actions/setup-r-dependencies@v2", fixed = TRUE)
  expect_false(grepl("Rebuild macOS native tidyverse core", workflow, fixed = TRUE))
  expect_false(grepl("macos-r-4.5.3-native-v1", workflow, fixed = TRUE))
  expect_match(workflow, "Diagnose macOS R-only native state", fixed = TRUE)
  expect_match(workflow, "R-only dplyr and .data_format diagnostics passed", fixed = TRUE)
  expect_false(grepl("Reproduce macOS native test order", workflow, fixed = TRUE))
  expect_match(workflow, 'c("rcdk", "rJava")', fixed = TRUE)
  expect_match(workflow, "py_available(initialize = FALSE)", fixed = TRUE)
  expect_match(workflow, "DrugSigNet initialized Python", fixed = TRUE)
  expect_match(workflow, "they do not inherit an embedded libpython", fixed = TRUE)
  expect_match(workflow, "rcmdcheck::rcmdcheck", fixed = TRUE)
  expect_match(workflow, "actions/checkout@v5", fixed = TRUE)
  expect_match(workflow, "actions/upload-artifact@v6", fixed = TRUE)
  expect_match(
    workflow,
    'echo "RSPM=https://cloud.r-project.org" >> "$GITHUB_ENV"',
    fixed = TRUE
  )
  job_env <- strsplit(workflow, "\n    strategy:", fixed = TRUE)[[1L]][[1L]]
  expect_false(grepl("RSPM: https://cloud.r-project.org", job_env, fixed = TRUE))
  expect_match(workflow, "install --yes --no-install-recommends qpdf", fixed = TRUE)
  expect_false(grepl("catchpoint/workflow-telemetry-action", workflow, fixed = TRUE))
  expect_false(grepl("neurogenomics/rworkflows@", workflow, fixed = TRUE))
  expect_false(grepl("neurogenomics/rworkflows@master", workflow, fixed = TRUE))

  runtime_install <- regexpr("- id: runtime-install", workflow, fixed = TRUE)[[1L]]
  package_check <- regexpr("- id: rworkflows-check", workflow, fixed = TRUE)[[1L]]
  expect_gt(runtime_install, 0L)
  expect_gt(package_check, runtime_install)
  expect_false(grepl("- id: macos-runtime-install", workflow, fixed = TRUE))
  expect_match(workflow, "actions/setup-python@v6", fixed = TRUE)
  expect_match(workflow, "python-version: '3.12'", fixed = TRUE)
  expect_match(workflow, "synapseclient==4.4.2", fixed = TRUE)
  expect_match(workflow, '"RETICULATE_PYTHON=$python"', fixed = TRUE)
  expect_false(grepl("conda create --yes --name r-reticulate", workflow, fixed = TRUE))
  expect_match(workflow, "build_vignettes = TRUE", fixed = TRUE)
  expect_match(workflow, "DrugSigNet::setup_python_dependencies", fixed = TRUE)
  expect_match(
    workflow,
    "steps.rworkflows-check.outcome != 'skipped'",
    fixed = TRUE
  )
  expect_match(workflow, "- id: check-artifacts", fixed = TRUE)
  expect_match(
    workflow,
    "steps.check-artifacts.outputs.found == 'true'",
    fixed = TRUE
  )
  expect_false(grepl("if-no-files-found: warn", workflow, fixed = TRUE))

  validator <- paste(readLines(
    file.path(root, ".github", "scripts", "validate-r-cmd-check.R"),
    warn = FALSE
  ), collapse = "\n")
  expect_match(validator, "testthat\\\\.Rout", fixed = TRUE)
  expect_match(validator, "Diagnostic tail", fixed = TRUE)
  expect_match(validator, "Segmentation fault", fixed = TRUE)
  expect_match(validator, "caught segfault", fixed = TRUE)
  expect_match(validator, "^Traceback:", fixed = TRUE)
  expect_match(validator, 'sprintf("%6d | %s"', fixed = TRUE)
  expect_match(validator, 'grepl("\\\\.fail$"', fixed = TRUE)
  expect_false(grepl("failed outside R CMD check", validator, fixed = TRUE))
})

test_that("runtime dependency constraints contain no caret syntax", {
  manifest <- readLines(
    drugsignet_repository_file("tools", "runtime-dependencies.tsv"),
    warn = FALSE
  )

  expect_false(any(grepl("^", manifest, fixed = TRUE)))
})

test_that("macOS package tests isolate native-backed test files", {
  runner <- paste(readLines(
    drugsignet_repository_file("tests", "testthat.R"),
    warn = FALSE
  ), collapse = "\n")

  expect_match(runner, 'Sys.info()[["sysname"]]', fixed = TRUE)
  expect_match(runner, 'identical(Sys.info()[["sysname"]], "Darwin")', fixed = TRUE)
  expect_match(runner, 'file.path(R.home("bin"), "Rscript")', fixed = TRUE)
  expect_match(runner, 'parallel::makeCluster(worker_count, type = "PSOCK"', fixed = TRUE)
  expect_match(runner, "parallel::parLapply", fixed = TRUE)
  expect_match(runner, "worker_count <- min(2L", fixed = TRUE)
  expect_match(runner, "testthat::test_file(", fixed = TRUE)
  expect_match(runner, "package = 'DrugSigNet'", fixed = TRUE)
  expect_match(runner, "stop_on_failure = TRUE", fixed = TRUE)
  expect_match(runner, "stop_on_warning = TRUE", fixed = TRUE)
  expect_match(runner, 'env = "R_TESTS="', fixed = TRUE)
  expect_match(runner, 'test_check("DrugSigNet")', fixed = TRUE)
  expect_false(grepl("skip", runner, ignore.case = TRUE))
})
