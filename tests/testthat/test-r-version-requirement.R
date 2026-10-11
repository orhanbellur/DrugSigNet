test_that("DESCRIPTION records the supported R baseline", {
  description <- read.dcf(system.file("DESCRIPTION", package = "DrugSigNet"))

  expect_match(description[1, "Depends"], "R \\(>= 4\\.5\\.0\\)")
})
