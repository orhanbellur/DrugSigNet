test_that("setup_synapser does not reinstall an available package", {
  local_mocked_bindings(
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
    .drugsignet_synapser_available = function() FALSE,
    .package = "DrugSigNet"
  )

  expect_error(
    DrugSigNet:::.drugsignet_require_synapser("download test data"),
    "setup_synapser\\(\\)"
  )
})
