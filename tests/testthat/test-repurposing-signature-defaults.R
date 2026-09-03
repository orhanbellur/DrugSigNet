test_that("unified and standalone signature interfaces share key defaults", {
  unified <- formals(drugRepurposingPipeline)
  standalone <- formals(drugSignaturePipeline)
  shared_args <- c(
    "trend", "drug_name_synonym", "ties_method", "prior", "num_bin",
    "n_workers", "chunk_size", "signature_refdb_mode",
    "signature_refdb_auth_token", "validate_signature_refdb", "top_k",
    "trial_condition", "target_id_from", "force", "auth_token",
    "run_drug_annotation", "run_visualization"
  )

  for (argument in shared_args) {
    expect_identical(unified[[argument]], standalone[[argument]], info = argument)
  }

  expect_identical(unified$padj, 0.05)
  expect_null(standalone$padj)
})
