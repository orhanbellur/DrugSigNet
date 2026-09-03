test_that("plot_all is part of the public package API", {
  expect_true("plot_all" %in% getNamespaceExports("DrugSigNet"))
})
