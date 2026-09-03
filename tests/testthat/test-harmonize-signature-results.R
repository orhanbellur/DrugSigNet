test_that("signature methods are harmonized pairwise across pipeline results", {
  processed_result <- function(cmap_scores, lincs_scores) {
    list(
      DrugSearching = list(
        Processed = list(
          CMAP_CMAP = data.frame(
            perturbation = c("drug_a", "drug_b", "drug_c", "drug_d"),
            rank_score = cmap_scores
          ),
          CMAP_LINCS2 = data.frame(
            perturbation = c("drug_a", "drug_b", "drug_c", "drug_d"),
            rank_score = lincs_scores
          )
        )
      )
    )
  }

  signature_results <- list(
    result1 = processed_result(c(4, 3, 2, 1), c(1, 3, 4, 2)),
    result2 = processed_result(c(3, 4, 1, 2), c(2, 4, 3, 1))
  )
  result <- harmonize_signature_results(
    signature_results = signature_results,
    datasets = "CMAP",
    reference_suffixes = c("CMAP", "LINCS2"),
    methods = c("CRank", "Dowdall", "RRA"),
    num_bin = 4
  )

  expect_named(result$Pairwise, "CMAP")
  expect_setequal(
    names(result$PairwiseAcrossResults),
    c("CMAP_CMAP", "CMAP_LINCS2")
  )
  expect_setequal(
    names(result$PairwiseAcrossResults$CMAP_CMAP),
    c("CRank", "Dowdall", "RRA")
  )
  expect_true(all(c("Drug", "CMAP_CMAP_CRank") %in%
                    names(result$PairwiseAcrossResults$CMAP_CMAP$CRank@result)))
  expect_true(all(c("Drug", "CMAP_CMAP_Dowdall") %in%
                    names(result$PairwiseAcrossResults$CMAP_CMAP$Dowdall@result)))
  expect_true(all(c("Drug", "RRA_pval", "CMAP_CMAP_RRA") %in%
                    names(result$PairwiseAcrossResults$CMAP_CMAP$RRA@result)))
  expect_false(
    "Name" %in% names(result$PairwiseAcrossResults$CMAP_CMAP$RRA@result)
  )
  # A single signature family has nothing to aggregate at the second stage.
  # In particular, it must not be passed to CRank as a one-rank input.
  expect_identical(result$Harmonized, list())

  # The pre-existing family-level result is retained and still uses all four
  # inputs in the original result-then-reference order.
  old_inputs <- list(
    signature_results$result1$DrugSearching$Processed$CMAP_CMAP,
    signature_results$result1$DrugSearching$Processed$CMAP_LINCS2,
    signature_results$result2$DrugSearching$Processed$CMAP_CMAP,
    signature_results$result2$DrugSearching$Processed$CMAP_LINCS2
  )
  expected_dowdall <- calculate_rank_aggregation(
    "Dowdall", rank_inputs = old_inputs, ties_method = "max"
  )
  expected_dowdall@result <- expected_dowdall@result %>%
    dplyr::rename(CMAP_Dowdall = Dowdall_rank)
  expect_equal(
    result$Pairwise$CMAP$Dowdall@result,
    expected_dowdall@result
  )
})

test_that("RRA pairwise identifiers are normalized from legacy Name columns", {
  legacy <- data.frame(Name = "drug_a", RRA_pval = 0.01, RRA_rank = 1)
  normalized <- DrugSigNet:::.standardize_pairwise_signature_result(legacy, "RRA")

  expect_named(normalized, c("Drug", "RRA_pval", "RRA_rank"))
  expect_identical(normalized$Drug, "drug_a")
})
