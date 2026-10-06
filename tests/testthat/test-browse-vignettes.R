test_that("vignette browser validates names", {
  expect_error(
    browse_drugsignet_vignette("../getting-started", launch = FALSE),
    "basename"
  )
  expect_error(
    browse_drugsignet_vignette(NA_character_, launch = FALSE),
    "one non-missing"
  )
})

test_that("installed vignette can be copied to a session directory", {
  installed <- system.file("doc", "getting-started.html", package = "DrugSigNet")
  skip_if(!nzchar(installed) || !file.exists(installed), "built vignettes are unavailable")

  copied <- browse_drugsignet_vignette("getting-started", launch = FALSE)
  expect_true(file.exists(copied))
  expect_true(file.access(copied, 4L) == 0L)
})
