test_that("integration enrichment uses the same GO ontology schema as pipelines", {
  calls <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    load_drugsignet_network = function(network, ...) {
      expect_identical(network, "drug_target")
      data.frame(
        Drug = c("drug_a", "drug_b"),
        Target = c("GENE1", "GENE2"),
        stringsAsFactors = FALSE
      )
    },
    TSEA = function(targetList, source, ont, target_id_from = NULL, ...) {
      calls$targetList <- targetList
      calls$source <- source
      calls$ont <- ont
      calls$target_id_from <- target_id_from
      "enrichment-result"
    },
    .package = "DrugSigNet"
  )

  result <- DrugSigNet:::.integration_functional_enrichment(
    drugs = "drug_a",
    target_id_from = "SYMBOL"
  )

  expect_identical(result, "enrichment-result")
  expect_identical(calls$targetList, "GENE1")
  expect_identical(calls$source, "GO")
  expect_identical(calls$ont, "ALL")
  expect_identical(calls$target_id_from, "SYMBOL")
})

test_that("single-ontology TSEA results retain an ontology column", {
  testthat::local_mocked_bindings(
    .runEnrichR = function(targetList, ont) {
      list(data.frame(
        Term = "example term",
        Overlap = "1/10",
        Adjusted.P.value = 0.01,
        stringsAsFactors = FALSE
      ))
    },
    .package = "DrugSigNet"
  )

  result <- TSEA(
    targetList = "TP53",
    source = "GO",
    ont = "BP"
  )

  expect_identical(result@result$ont, "BP")
})
