test_that("installation check classifies runtime visualization packages as required", {
  result <- check_drugsignet_installation(
    check_python = FALSE,
    check_optional = TRUE
  )

  expect_true("wordcloud" %in% result$packages$package)
  expect_true(result$packages$required[result$packages$package == "wordcloud"])
  expect_true(all(c("enrichR", "ggalluvial", "ggforce", "plotly") %in%
    result$packages$package[result$packages$required]))
  expect_false(result$packages$required[result$packages$package == "signatureSearch"])
})

test_that("required runtime packages remain when optional checks are disabled", {
  result <- check_drugsignet_installation(
    check_python = FALSE,
    check_optional = FALSE
  )

  expect_true(all(c("enrichR", "plotly", "wordcloud") %in% result$packages$package))
  expect_false(any(c("kableExtra", "signatureSearch", "tinytex") %in%
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
