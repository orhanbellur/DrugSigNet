test_that("rcdk is not eagerly imported into the DrugSigNet process", {
  description <- read.dcf(drugsignet_repository_file("DESCRIPTION"))
  imports <- trimws(strsplit(description[1L, "Imports"], ",", fixed = TRUE)[[1L]])
  suggests <- trimws(strsplit(description[1L, "Suggests"], ",", fixed = TRUE)[[1L]])
  expect_false("rcdk" %in% imports)
  expect_true("rcdk" %in% suggests)

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
  expect_match(implementation, 'requireNamespace("rcdk"', fixed = TRUE)
})
