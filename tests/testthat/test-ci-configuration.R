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
  expect_match(workflow, "RETICULATE_USE_MANAGED_VENV: 'no'", fixed = TRUE)
  expect_match(workflow, "r-lib/actions/setup-r-dependencies@v2", fixed = TRUE)
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
  expect_match(workflow, "actions/setup-python@v6", fixed = TRUE)
  expect_match(workflow, "python-version: '3.10'", fixed = TRUE)
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
