test_that("package installation has no external-runtime configure hooks", {
  root <- testthat::test_path("..", "..")
  expect_false(file.exists(file.path(root, "configure")))
  expect_false(file.exists(file.path(root, "configure.win")))
})

test_that("DESCRIPTION does not claim indirect Debian build requirements", {
  description <- read.dcf(testthat::test_path("..", "..", "DESCRIPTION"))
  requirements <- description[1, "SystemRequirements"]

  expect_match(requirements, "Python")
  expect_match(requirements, "graph-tool", fixed = TRUE)
  expect_false(grepl("libglpk|libgmp|libgsl|libsqlite", requirements))
})
