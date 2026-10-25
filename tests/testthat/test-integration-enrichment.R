test_that("integration enrichment uses the same GO ontology schema as pipelines", {
  calls <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    load_drugsignet_network = function(network, ...) {
      expect_identical(network, "drug_target")
      data.frame(
        Drug = c("drug_a", "drug_a", "drug_b", NA_character_),
        Target = c("GENE1", "GENE1", "GENE2", "GENE3"),
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

test_that("integration enrichment validates the drug-target schema", {
  testthat::local_mocked_bindings(
    load_drugsignet_network = function(...) {
      data.frame(Drug = "drug_a", stringsAsFactors = FALSE)
    },
    .package = "DrugSigNet"
  )

  expect_error(
    DrugSigNet:::.integration_functional_enrichment("drug_a"),
    "missing required column\\(s\\): Target"
  )
})

test_that("single-ontology TSEA results retain an ontology column", {
  testthat::local_mocked_bindings(
    .runEnrichR = function(targetList, ont) {
      list(data.frame(
        Term = c("retained term", "filtered term", "missing p-value"),
        Overlap = c("1/10", "2/10", "3/10"),
        Adjusted.P.value = c(0.01, 0.2, NA_real_),
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
  expect_identical(result@result$Term, "retained term")
  expect_identical(result@result$Adjusted.P.value, 0.01)
})

test_that("all-ontology TSEA results retain schemas without dplyr", {
  testthat::local_mocked_bindings(
    .runEnrichR = function(targetList, ont) {
      list(
        GO_Biological_Process_2023 = data.frame(
          Term = "BP term", Adjusted.P.value = 0.01
        ),
        GO_Molecular_Function_2023 = data.frame(
          Term = "MF term", Adjusted.P.value = 0.02, Extra = "MF metadata"
        ),
        GO_Cellular_Component_2023 = data.frame(
          Term = "filtered CC term", Adjusted.P.value = 0.2
        )
      )
    },
    .package = "DrugSigNet"
  )

  result <- TSEA(
    targetList = "TP53",
    source = "GO",
    ont = "ALL"
  )

  expect_identical(result@result$Term, c("BP term", "MF term"))
  expect_identical(result@result$ont, c("BP", "MF"))
  expect_identical(result@result$Extra, c(NA_character_, "MF metadata"))
})
