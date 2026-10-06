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
