test_that("network methods use isolated Python without importing it into R", {
  namespace_imports <- names(getNamespaceImports("DrugSigNet"))
  expect_false("reticulate" %in% namespace_imports)

  root <- drugsignet_repository_root()
  network_sources <- file.path(root, "R", c(
    "Degree_centrality.R", "Diffusion.R", "Harmonic_centrality.R",
    "Network_proximity.R", "TrustRank.R"
  ))
  implementations <- paste(
    unlist(lapply(network_sources, readLines, warn = FALSE)),
    collapse = "\n"
  )

  expect_match(implementations, ".drugsignet_network_python_call", fixed = TRUE)
  expect_false(grepl("reticulate::py_run_file", implementations, fixed = TRUE))
  expect_false(grepl("reticulate::r_to_py", implementations, fixed = TRUE))
  expect_false(grepl("@importFrom reticulate", implementations, fixed = TRUE))
})
