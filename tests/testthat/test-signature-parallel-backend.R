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

test_that("signature methods are grouped into ordered reference phases", {
  registry <- .signature_method_registry()
  tasks <- lapply(seq_len(nrow(registry)), function(i) {
    list(method = registry$method[[i]], db_key = registry$ref_db[[i]])
  })
  names(tasks) <- registry$method
  groups <- .group_signature_tasks_by_refdb(tasks, unique(registry$ref_db))

  expect_identical(names(groups), c("cmap", "lincs2"))
  expect_true(all(vapply(groups$cmap, `[[`, character(1), "db_key") == "cmap"))
  expect_true(all(vapply(groups$lincs2, `[[`, character(1), "db_key") == "lincs2"))
  expect_identical(unname(unlist(lapply(groups, names))), registry$method)
})

test_that("signature methods are forced sequential for HDF5 safety", {
  expect_identical(.resolve_signature_worker_count(1L), 1L)
  expect_message(
    expect_identical(.resolve_signature_worker_count(4L), 1L),
    "forcing sequential execution"
  )
  expect_error(.resolve_signature_worker_count(0L), "positive integer")
  expect_error(.resolve_signature_worker_count(NA_integer_), "positive integer")
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
