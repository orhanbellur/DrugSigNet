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
  expect_match(installer, '"${pixi}" install', fixed = TRUE)
  expect_match(installer, '"${pixi}" run', fixed = TRUE)
  expect_match(entry, "install_drugsignet_pixi", fixed = TRUE)
  expect_match(entry, "runtime-dependencies.tsv", fixed = TRUE)
})
