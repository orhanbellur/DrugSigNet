test_that("configure scripts never provision external runtimes", {
  root <- testthat::test_path("..", "..")
  scripts <- vapply(
    c("configure", "configure.win"),
    function(path) paste(readLines(file.path(root, path), warn = FALSE), collapse = "\n"),
    character(1)
  )

  expect_false(any(grepl("configure_runtime[.]R", scripts)))
  expect_false(any(grepl("Rscript|install_miniconda", scripts)))
  expect_true(all(grepl("deferred until after installation", scripts, fixed = TRUE)))
})

test_that("DESCRIPTION does not claim indirect Debian build requirements", {
  description <- read.dcf(testthat::test_path("..", "..", "DESCRIPTION"))
  requirements <- description[1, "SystemRequirements"]

  expect_match(requirements, "Python")
  expect_match(requirements, "graph-tool", fixed = TRUE)
  expect_false(grepl("libglpk|libgmp|libgsl|libsqlite", requirements))
})
