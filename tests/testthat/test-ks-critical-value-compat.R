test_that("KS critical values support current and legacy BoutrosLab APIs", {
  current <- new.env(parent = emptyenv())
  current$critical.value.ks.test <- function(n, conf, alternative) {
    list(n = n, conf = conf, alternative = alternative, api = "current")
  }
  legacy <- new.env(parent = emptyenv())
  legacy$ks.test.critical.value <- function(n, conf, alternative) {
    list(n = n, conf = conf, alternative = alternative, api = "legacy")
  }

  current_result <- DrugSigNet:::.drugsignet_ks_critical_value(
    100L, 0.95, namespace = current
  )
  legacy_result <- DrugSigNet:::.drugsignet_ks_critical_value(
    50L, 0.90, alternative = "one-sided", namespace = legacy
  )

  expect_identical(current_result$api, "current")
  expect_identical(current_result$alternative, "two.sided")
  expect_identical(legacy_result$api, "legacy")
  expect_identical(legacy_result$alternative, "one-sided")
})

test_that("KS compatibility never requires an exported BoutrosLab symbol", {
  implementation <- paste(deparse(
    body(DrugSigNet:::.drugsignet_ks_critical_value)
  ), collapse = "\n")

  expect_false(grepl(
    "BoutrosLab.plotting.general::critical.value.ks.test",
    implementation,
    fixed = TRUE
  ))
  expect_match(implementation, 'asNamespace("BoutrosLab.plotting.general")',
               fixed = TRUE)
  expect_match(implementation, "inherits = FALSE", fixed = TRUE)
})

test_that("signature rankers use the namespace-compatible KS helper", {
  rankers <- c(
    cmap = paste(deparse(body(cmap_rank_score)), collapse = "\n"),
    gcmap = paste(deparse(body(gcmap_rank_score)), collapse = "\n")
  )

  expect_true(all(grepl(
    ".drugsignet_ks_critical_value(",
    rankers,
    fixed = TRUE
  )))
  expect_false(any(grepl(
    "BoutrosLab.plotting.general::critical.value.ks.test",
    rankers,
    fixed = TRUE
  )))
})

test_that("KS critical-value compatibility reports an unusable dependency", {
  expect_error(
    DrugSigNet:::.drugsignet_ks_critical_value(
      100L, 0.95, namespace = new.env(parent = emptyenv())
    ),
    "does not provide a Kolmogorov-Smirnov critical-value function"
  )
})
