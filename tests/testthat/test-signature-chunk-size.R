test_that("all signature methods expose the same chunk-size default", {
  methods <- list(cmap_method, lincs_method, gcmap_method, correlation_method)

  for (method in methods) {
    expect_identical(formals(method)$chunk_size, 5000)
  }
})

test_that("signature pipelines expose the chunk-size default", {
  expect_identical(formals(drugSignaturePipeline)$chunk_size, 5000)
  expect_identical(formals(drugRepurposingPipeline)$chunk_size, 5000)
})

test_that("chunk size is retained for every signature method family", {
  families <- c("CMAP", "LINCS", "gCMAP", "Correlation")

  for (family in families) {
    object <- SignatureBased(
      query = list(upset = "1", downset = "2"),
      signature_method = family,
      refdb = "cmap",
      chunk_size = 1234
    )
    expect_identical(filterSignatureParameters(object)$chunk_size, 1234)
  }
})
