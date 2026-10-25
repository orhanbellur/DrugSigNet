test_that("rcdk is not eagerly imported into the DrugSigNet process", {
  namespace_imports <- names(getNamespaceImports("DrugSigNet"))
  expect_false("rcdk" %in% namespace_imports)

  similarity_method <- methods::selectMethod(
    "get_drug_drug_similarity",
    "DrugAnnotation"
  )
  implementation <- paste(
    deparse(body(similarity_method)),
    collapse = "\n"
  )
  expect_match(implementation, "rcdk::parse.smiles", fixed = TRUE)
  expect_match(implementation, "rcdk::get.fingerprint", fixed = TRUE)
})
