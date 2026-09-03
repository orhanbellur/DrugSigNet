test_that("signature formatting omits metadata absent from frozen references", {
  input <- data.frame(
    pert = c("drug_a", "drug_b"),
    rank_score = c(1, 0.5),
    raw_score = c(-0.8, 0.6),
    trend = c("down", "up"),
    scaled_score = c(-0.8, 0.6),
    stringsAsFactors = FALSE
  )

  result <- DrugSigNet:::.data_format(input, score_col = "raw_score")
  mapped_cols <- c("ID", "Name_synonyms", "Drug_status")

  expect_identical(result$perturbation, input$pert)
  expect_false(any(mapped_cols %in% names(result)))
})

test_that("signature formatting preserves available frozen-reference metadata", {
  input <- data.frame(
    pert = "drug_a",
    rank_score = 1,
    raw_score = -0.8,
    trend = "down",
    scaled_score = -0.8,
    cell = "A375",
    ID = "DB0001",
    stringsAsFactors = FALSE
  )

  result <- DrugSigNet:::.data_format(input, score_col = "raw_score")

  expect_identical(result$cell, "A375")
  expect_identical(result$ID, "DB0001")
})

test_that("NULL synonym mapping remains compatible with metadata harmonization", {
  input <- data.frame(
    pert = c("(+)-chelidonine", "drug_b"),
    rank_score = c(1, 0.5),
    raw_score = c(-0.8, 0.6),
    trend = c("down", "up"),
    scaled_score = c(-0.8, 0.6),
    cell = c("A375", "MCF7"),
    stringsAsFactors = FALSE
  )

  formatted <- DrugSigNet:::.data_format(input, score_col = "raw_score")

  expect_no_error(
    harmonized <- DrugSigNet:::.harmonize_signature_metadata(formatted)
  )

  expect_true("(+)-chelidonine" %in% harmonized$perturbation)
  expect_false(any(c("ID", "Name_synonyms", "Drug_status") %in% names(harmonized)))
})

test_that("metadata harmonization retains mapped synonym columns when supplied", {
  input <- data.frame(
    perturbation = "drug_a",
    cell = "A375",
    ID = "DB0001",
    Name_synonyms = "drug-a|compound-a",
    Drug_status = "approved",
    stringsAsFactors = FALSE
  )

  result <- DrugSigNet:::.harmonize_signature_metadata(input)

  expect_identical(result$ID, "DB0001")
  expect_identical(result$Name_synonyms, "drug-a|compound-a")
  expect_identical(result$Drug_status, "approved")
})
