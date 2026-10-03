test_that("complete conda graph environment is the setup default", {
  defaults <- formals(setup_python_dependencies)

  expect_identical(eval(defaults$method), c("conda", "auto", "virtualenv"))
  expect_true(eval(defaults$include_graph_tool))
})

test_that("automatic Python setup follows interactivity unless overridden", {
  local_envvar(DRUGSIGNET_AUTO_INSTALL_PYTHON = NA)
  local_options(DrugSigNet.auto_install_python = NULL)
  expect_identical(DrugSigNet:::.drugsignet_auto_install_enabled(), interactive())

  local_options(DrugSigNet.auto_install_python = FALSE)
  expect_false(DrugSigNet:::.drugsignet_auto_install_enabled())
})

test_that("module probe does not initialize reticulate", {
  python <- Sys.which("python3")
  skip_if(!nzchar(python), "python3 is unavailable")

  expect_true(DrugSigNet:::.drugsignet_python_has_modules(python, "sys"))
  expect_false(DrugSigNet:::.drugsignet_python_has_modules(
    python, "a_drugsignet_module_that_does_not_exist"
  ))
})
