test_that("setup_synapser does not reinstall an available package", {
  local_mocked_bindings(
    .drugsignet_prepare_synapser_python = function(...) invisible(TRUE),
    .drugsignet_synapser_available = function() TRUE,
    .drugsignet_install_synapser = function(...) {
      stop("installer should not be called")
    },
    .package = "DrugSigNet"
  )

  expect_message(expect_true(setup_synapser()), "already installed")
})

test_that("setup_synapser installs missing optional support", {
  installed <- FALSE
  local_mocked_bindings(
    .drugsignet_prepare_synapser_python = function(...) invisible(TRUE),
    .drugsignet_synapser_available = function() FALSE,
    .drugsignet_install_synapser = function(...) {
      installed <<- TRUE
      TRUE
    },
    .package = "DrugSigNet"
  )

  expect_true(setup_synapser(quiet = TRUE))
  expect_true(installed)
})

test_that("missing synapser error points to the setup helper", {
  local_mocked_bindings(
    .drugsignet_prepare_synapser_python = function(...) invisible(TRUE),
    .drugsignet_synapser_available = function() FALSE,
    .drugsignet_auto_install_synapser_enabled = function() FALSE,
    .package = "DrugSigNet"
  )

  expect_error(
    DrugSigNet:::.drugsignet_require_synapser("download test data"),
    "setup_synapser\\(\\)"
  )
})

test_that("Synapse requirements install support on first use", {
  available <- FALSE
  local_mocked_bindings(
    .drugsignet_prepare_synapser_python = function(...) invisible(TRUE),
    .drugsignet_synapser_available = function() available,
    .drugsignet_auto_install_synapser_enabled = function() TRUE,
    setup_synapser = function(...) {
      available <<- TRUE
      invisible(TRUE)
    },
    .package = "DrugSigNet"
  )

  expect_message(
    expect_true(DrugSigNet:::.drugsignet_require_synapser("download test data")),
    "Installing Synapse support"
  )
  expect_true(available)
})

test_that("synGet compatibility wrapper filters unsupported 3.x arguments", {
  received <- NULL
  local_mocked_bindings(
    .drugsignet_synapser_function = function(name) {
      function(entity) {
        received <<- list(entity = entity)
        "entity"
      }
    },
    .package = "DrugSigNet"
  )

  expect_identical(
    DrugSigNet:::.drugsignet_syn_get(
      "syn123", downloadFile = FALSE, downloadLocation = tempdir()
    ),
    "entity"
  )
  expect_identical(received, list(entity = "syn123"))
})

test_that("synGet compatibility wrapper retains supported 2.x arguments", {
  received <- NULL
  local_mocked_bindings(
    .drugsignet_synapser_function = function(name) {
      function(entity, downloadFile = TRUE) {
        received <<- list(entity = entity, downloadFile = downloadFile)
        "entity"
      }
    },
    .package = "DrugSigNet"
  )

  expect_identical(
    DrugSigNet:::.drugsignet_syn_get("syn123", downloadFile = FALSE),
    "entity"
  )
  expect_identical(
    received,
    list(entity = "syn123", downloadFile = FALSE)
  )
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
