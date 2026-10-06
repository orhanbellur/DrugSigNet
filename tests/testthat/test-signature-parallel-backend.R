test_that("platform detection always selects portable base-R PSOCK workers", {
  backend <- detect_pipeline_parallel_backend()

  expect_identical(backend$backend, "psock")
})

test_that("base-R PSOCK workers execute and preserve task order", {
  physical_cores <- suppressWarnings(parallel::detectCores(logical = FALSE))
  skip_if(is.na(physical_cores) || physical_cores < 2L, "two CPU cores are unavailable")

  result <- run_pipeline_tasks(
    tasks = list(first = 2, second = 5),
    FUN = function(x) x * 3,
    n_workers = 2L,
    label = "PSOCK smoke test",
    psock_packages = character(),
    fallback = FALSE,
    progress = FALSE
  )

  expect_identical(result, list(first = 6, second = 15))
})

test_that("a worker error does not rerun completed work sequentially", {
  physical_cores <- suppressWarnings(parallel::detectCores(logical = FALSE))
  skip_if(is.na(physical_cores) || physical_cores < 2L, "two CPU cores are unavailable")

  expect_error(
    run_pipeline_tasks(
      tasks = list(1L, 2L),
      FUN = function(x) stop("worker failure"),
      n_workers = 2L,
      label = "PSOCK failure test",
      fallback = TRUE,
      progress = FALSE
    ),
    "tasks were not retried sequentially"
  )
})

test_that("signature methods are interleaved across reference databases", {
  registry <- .signature_method_registry()
  ordered <- registry[order(registry$family, registry$ref_label), , drop = FALSE]

  expect_identical(
    ordered$ref_db,
    rep(c("cmap", "lincs2"), length.out = nrow(ordered))
  )
  expect_length(unique(ordered$method), nrow(ordered))
})

test_that("signature worker count respects container memory", {
  expect_identical(
    .resolve_signature_worker_count(4L, memory_limit_bytes = 8 * 1024^3),
    2L
  )
  expect_identical(
    .resolve_signature_worker_count(2L, memory_limit_bytes = 3 * 1024^3),
    1L
  )
  expect_identical(
    .resolve_signature_worker_count(3L, memory_limit_bytes = NA_real_),
    3L
  )
})

test_that("signature worker failures retain the method name", {
  task <- list(
    method = "bad_method",
    family = "unknown",
    ref_db = "cmap",
    db_key = "cmap",
    signature = list(),
    padj = NULL,
    chunk_size = 10L
  )

  result <- .run_signature_method_task(task)
  expect_s3_class(result, "DrugSigNetSignatureMethodError")
  expect_identical(result$method, "bad_method")
  expect_match(result$message, "Unknown signature method family")
})
