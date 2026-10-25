test_that("reticulate remains lazy until a Python-backed method is called", {
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

  expect_match(implementations, "reticulate::py_run_file", fixed = TRUE)
  expect_match(implementations, "reticulate::r_to_py", fixed = TRUE)
  expect_false(grepl("@importFrom reticulate", implementations, fixed = TRUE))
})
