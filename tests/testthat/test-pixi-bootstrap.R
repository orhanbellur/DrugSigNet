test_that("Pixi installer creates and validates a project-local runtime", {
  root <- testthat::test_path("..", "..")
  installer <- paste(readLines(
    file.path(root, "tools", "install_drugsignet_pixi.sh"), warn = FALSE
  ), collapse = "\n")
  entry <- paste(readLines(file.path(root, "install-pixi.R"), warn = FALSE), collapse = "\n")

  expect_match(installer, "[workspace]", fixed = TRUE)
  expect_match(installer, "[dependencies]", fixed = TRUE)
  expect_match(installer, "[pypi-dependencies]", fixed = TRUE)
  expect_match(installer, "pixi.lock", fixed = TRUE)
  expect_match(installer, "test_installation.R", fixed = TRUE)
  expect_match(installer, "DrugSigNet-test.Rproj", fixed = TRUE)
  expect_match(installer, "RETICULATE_PYTHON=", fixed = TRUE)
  expect_match(installer, '"${pixi}" install', fixed = TRUE)
  expect_match(installer, '"${pixi}" run', fixed = TRUE)
  expect_match(installer, "--clean-env", fixed = TRUE)
  expect_match(installer, "env -u R_HOME -u R_LIBS", fixed = TRUE)
  expect_match(installer, '"${package_ref}"', fixed = TRUE)
  expect_match(entry, "install_drugsignet_pixi", fixed = TRUE)
  expect_match(entry, "runtime-dependencies.tsv", fixed = TRUE)
  expect_match(entry, 'Sys.which("wget")', fixed = TRUE)
  expect_match(entry, "utils::download.file", fixed = TRUE)
  expect_match(installer, "DRUGSIGNET_PIXI_BOOTSTRAP", fixed = TRUE)
  expect_match(installer, "command -v wget", fixed = TRUE)
  validation <- paste(readLines(
    file.path(root, "tools", "test_installation.R"), warn = FALSE
  ), collapse = "\n")
  expect_match(validation, "CONDA_PREFIX", fixed = TRUE)
  expect_match(validation, "lib = expected_library", fixed = TRUE)
})

test_that("rootless documentation does not claim fakeroot grants privileges", {
  readme <- paste(readLines(
    testthat::test_path("..", "..", "README.md"), warn = FALSE
  ), collapse = "\n")

  expect_match(readme, "fakeroot", fixed = TRUE)
  expect_match(readme, "does not grant permission", fixed = TRUE)
  expect_match(readme, "nothing is written to `/usr`", fixed = TRUE)
})
