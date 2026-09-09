test_that("default PPI confidence filtering is case-insensitive", {
  testthat::local_mocked_bindings(
    load_drugsignet_network = function(network, ...) {
      expect_identical(network, "gene_gene")
      data.frame(
        gene1 = c("A", "A", "B", "C", "D"),
        gene2 = c("B", "B", "C", "D", "E"),
        confidence = c("High", "high confidence", "Medium", NA, "HIGH"),
        stringsAsFactors = FALSE
      )
    },
    .package = "DrugSigNet"
  )

  drug_targets <- data.frame(
    ID = "DB0001",
    Drug = "example drug",
    Target = "A",
    Group = "approved",
    stringsAsFactors = FALSE
  )

  result <- resolve_network_inputs(
    drug_target_network = drug_targets,
    group_drug_targets = FALSE
  )

  expect_s3_class(result$ppi_network, "tbl_df")
  expect_identical(result$ppi_network$gene1, c("A", "D"))
  expect_identical(result$ppi_network$gene2, c("B", "E"))
  expect_identical(names(result$ppi_network), c("gene1", "gene2"))
})
