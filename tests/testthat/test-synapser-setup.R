test_that("setup_synapser validates the isolated Python client", {
  local_mocked_bindings(
    .drugsignet_prepare_synapser_python = function(...) invisible(TRUE),
    .drugsignet_python_has_modules = function(...) TRUE,
    .package = "DrugSigNet"
  )
  old <- Sys.getenv("RETICULATE_PYTHON", unset = NA_character_)
  on.exit(if (is.na(old)) Sys.unsetenv("RETICULATE_PYTHON") else Sys.setenv(RETICULATE_PYTHON = old), add = TRUE)
  Sys.setenv(RETICULATE_PYTHON = "/mock/python")

  expect_message(expect_true(setup_synapser()), "isolated Synapse client is ready")
})

test_that("Synapse requirement accepts the isolated Python client", {
  local_mocked_bindings(
    .drugsignet_prepare_synapser_python = function(...) invisible(TRUE),
    .drugsignet_python_has_modules = function(...) TRUE,
    .package = "DrugSigNet"
  )
  old <- Sys.getenv("RETICULATE_PYTHON", unset = NA_character_)
  on.exit(if (is.na(old)) Sys.unsetenv("RETICULATE_PYTHON") else Sys.setenv(RETICULATE_PYTHON = old), add = TRUE)
  Sys.setenv(RETICULATE_PYTHON = "/mock/python")

  expect_true(DrugSigNet:::.drugsignet_require_synapser("download test data"))
})

test_that("Synapse login stores the token without loading the R wrapper", {
  old <- getOption("DrugSigNet.synapse_auth_token")
  on.exit(options(DrugSigNet.synapse_auth_token = old), add = TRUE)

  login <- DrugSigNet:::.drugsignet_synapser_function("synLogin")
  expect_true(login("test-token"))
  expect_identical(getOption("DrugSigNet.synapse_auth_token"), "test-token")
})

test_that("legacy synGet lookup uses the isolated compatibility adapter", {
  received <- NULL
  local_mocked_bindings(
    .drugsignet_syn_get = function(entity, ...) {
      received <<- c(list(entity = entity), list(...))
      list(properties = list(id = entity), path = NULL)
    },
    .package = "DrugSigNet"
  )

  syn_get <- DrugSigNet:::.drugsignet_synapser_function("synGet")
  result <- syn_get("syn75862040", downloadFile = FALSE)

  expect_identical(received$entity, "syn75862040")
  expect_identical(received$downloadFile, FALSE)
  expect_identical(result$properties$id, "syn75862040")
})

test_that("Synapse operations use the isolated Python client", {
  script <- system.file("Python", "synapse_client.py", package = "DrugSigNet")
  source <- readLines(script, warn = FALSE)

  expect_true(any(grepl("import synapseclient", source, fixed = TRUE)))
  expect_true(any(grepl("SYNAPSE_AUTH_TOKEN", source, fixed = TRUE)))
  expect_true(any(grepl('kwargs = {"downloadFile":', source, fixed = TRUE)))
  expect_true(any(grepl("syn.get(entity_id, **kwargs)", source, fixed = TRUE)))
})

test_that("network loaders bypass the version-specific R synGet wrapper", {
  network_loader <- paste(deparse(body(load_drugsignet_network)), collapse = "\n")
  signature_loader <- paste(deparse(body(load_signature_refdb)), collapse = "\n")
  annotation_loader <- paste(deparse(body(load_synapse_annotations)), collapse = "\n")

  expect_match(network_loader, ".drugsignet_syn_get", fixed = TRUE)
  expect_match(signature_loader, ".drugsignet_syn_get", fixed = TRUE)
  expect_match(annotation_loader, ".drugsignet_syn_get", fixed = TRUE)
  expect_false(grepl('.drugsignet_synapser_function("synGet")', network_loader, fixed = TRUE))
  expect_false(grepl('.drugsignet_synapser_function("synGet")', signature_loader, fixed = TRUE))
  expect_false(grepl('.drugsignet_synapser_function("synGet")', annotation_loader, fixed = TRUE))
})

test_that("Synapser rjson compatibility accepts only supported versions", {
  expect_true(DrugSigNet:::.drugsignet_rjson_version_compatible("0.2.21"))
  expect_true(DrugSigNet:::.drugsignet_rjson_version_compatible("0.2.20"))
  expect_false(DrugSigNet:::.drugsignet_rjson_version_compatible("0.2.23"))
})

test_that("enrichR remains optional and is not imported on package load", {
  imports <- getNamespaceImports("DrugSigNet")
  expect_false("enrichR" %in% names(imports))
})
