test_that("installation check classifies conditional visualization packages as optional", {
  result <- check_drugsignet_installation(
    check_python = FALSE,
    check_optional = TRUE
  )

  expect_true("wordcloud" %in% result$packages$package)
  expect_false(result$packages$required[result$packages$package == "wordcloud"])
  expect_false(any(result$packages$required[result$packages$package %in% c("ggalluvial", "ggforce")]))
  expect_true(all(c("enrichR", "plotly") %in%
    result$packages$package[result$packages$required]))
  expect_false(result$packages$required[result$packages$package == "signatureSearch"])
})

test_that("required runtime packages remain when optional checks are disabled", {
  result <- check_drugsignet_installation(
    check_python = FALSE,
    check_optional = FALSE
  )

  expect_true(all(c("enrichR", "plotly") %in% result$packages$package))
  expect_false(any(c("ggalluvial", "ggforce", "wordcloud", "kableExtra", "signatureSearch", "tinytex") %in%
    result$packages$package))
})

test_that("installation check resolves exports from the namespace", {
  result <- DrugSigNet::check_drugsignet_installation(
    check_python = FALSE,
    check_optional = FALSE
  )

  expect_true(all(result$functions$available))
})

test_that("installation check errors identify the failed component", {
  local_mocked_bindings(
    .drugsignet_installation_python_packages = function() "missing_test_module",
    .drugsignet_python_module_probe = function(...) list(
      available = FALSE,
      status = 1L,
      output = "ModuleNotFoundError: missing_test_module"
    ),
    .package = "DrugSigNet"
  )
  fake_python <- tempfile("python-")
  file.create(fake_python)
  on.exit(unlink(fake_python), add = TRUE)
  withr::local_envvar(RETICULATE_PYTHON = fake_python)

  expect_error(
    DrugSigNet::check_drugsignet_installation(
      check_python = TRUE,
      check_optional = FALSE,
      stop_on_error = TRUE
    ),
    "Python modules: missing_test_module",
    fixed = TRUE
  )
})

test_that("installation check retains Python import diagnostics", {
  local_mocked_bindings(
    .drugsignet_installation_python_packages = function() "broken_module",
    .drugsignet_python_module_probe = function(...) list(
      available = FALSE,
      status = 1L,
      output = "ImportError: DLL load failed while importing broken_module"
    ),
    .package = "DrugSigNet"
  )
  fake_python <- tempfile("python-")
  file.create(fake_python)
  on.exit(unlink(fake_python), add = TRUE)
  withr::local_envvar(RETICULATE_PYTHON = fake_python)

  result <- DrugSigNet::check_drugsignet_installation(
    check_python = TRUE,
    check_optional = FALSE
  )

  expect_false(result$ok)
  expect_match(result$python$diagnostic, "DLL load failed", fixed = TRUE)
})

test_that("missing optional plot packages do not fail a core installation check", {
  checker <- check_drugsignet_installation
  optional_plots <- c("ggalluvial", "ggforce", "wordcloud")
  environment(checker) <- list2env(list(
    requireNamespace = function(package, ...) {
      if (package %in% optional_plots) FALSE else base::requireNamespace(package, ...)
    }
  ), parent = environment(checker))
  result <- checker(check_python = FALSE, check_optional = TRUE, stop_on_error = TRUE)
  expect_true(result$ok)
  rows <- result$packages$package %in% optional_plots
  expect_equal(sum(rows), 3L)
  expect_false(any(result$packages$required[rows]))
  expect_false(any(result$packages$available[rows]))
})
