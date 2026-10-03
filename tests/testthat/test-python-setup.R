test_that("complete conda graph environment is the setup default", {
  defaults <- formals(setup_python_dependencies)

  expect_identical(eval(defaults$method), c("conda", "auto", "virtualenv"))
  expect_true(eval(defaults$include_graph_tool))
})

test_that("automatic Python setup follows interactivity unless overridden", {
  with_clean_python_setup <- function(code) {
    old_env <- Sys.getenv("DRUGSIGNET_AUTO_INSTALL_PYTHON", unset = NA_character_)
    old_option <- getOption("DrugSigNet.auto_install_python")
    on.exit({
      if (is.na(old_env)) {
        Sys.unsetenv("DRUGSIGNET_AUTO_INSTALL_PYTHON")
      } else {
        Sys.setenv(DRUGSIGNET_AUTO_INSTALL_PYTHON = old_env)
      }
      options(DrugSigNet.auto_install_python = old_option)
    }, add = TRUE)

    Sys.unsetenv("DRUGSIGNET_AUTO_INSTALL_PYTHON")
    options(DrugSigNet.auto_install_python = NULL)
    force(code)
  }

  with_clean_python_setup({
    expect_identical(
      DrugSigNet:::.drugsignet_auto_install_enabled(),
      interactive()
    )

    options(DrugSigNet.auto_install_python = FALSE)
    expect_false(DrugSigNet:::.drugsignet_auto_install_enabled())
  })
})

test_that("module probe does not initialize reticulate", {
  python <- Sys.which("python3")
  skip_if(!nzchar(python), "python3 is unavailable")

  expect_true(DrugSigNet:::.drugsignet_python_has_modules(python, "sys"))
  expect_false(DrugSigNet:::.drugsignet_python_has_modules(
    python, "a_drugsignet_module_that_does_not_exist"
  ))
})
