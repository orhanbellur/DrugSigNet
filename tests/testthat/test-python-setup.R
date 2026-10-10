test_that("complete conda graph environment is the setup default", {
  defaults <- formals(setup_python_dependencies)

  expect_identical(eval(defaults$method), c("conda", "auto", "virtualenv"))
  expect_true(eval(defaults$include_graph_tool))
})

test_that("installation module checks omit graph_tool on Windows", {
  local_mocked_bindings(
    .drugsignet_is_windows = function() TRUE,
    .package = "DrugSigNet"
  )

  modules <- DrugSigNet:::.drugsignet_installation_python_packages()

  expect_true(all(c("numpy", "kaleido", "synapseclient") %in% modules))
  expect_false("graph_tool" %in% modules)
})

test_that("installation module checks require graph_tool off Windows", {
  local_mocked_bindings(
    .drugsignet_is_windows = function() FALSE,
    .package = "DrugSigNet"
  )

  expect_true(
    "graph_tool" %in% DrugSigNet:::.drugsignet_installation_python_packages()
  )
})

test_that("conda transaction excludes pip-only kaleido", {
  packages <- DrugSigNet:::.drugsignet_conda_packages(
    include_graph_tool = TRUE
  )

  expect_true("graph-tool" %in% packages)
  expect_false("kaleido" %in% packages)
  expect_true("kaleido" %in% DrugSigNet:::.drugsignet_python_packages())
})

test_that("automatic Python setup is opt-in", {
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
    expect_false(DrugSigNet:::.drugsignet_auto_install_enabled())

    options(DrugSigNet.auto_install_python = TRUE)
    expect_true(DrugSigNet:::.drugsignet_auto_install_enabled())
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

test_that("setup honors an already complete configured Python runtime", {
  python <- Sys.which("python3")
  skip_if(!nzchar(python), "python3 is unavailable")
  old_python <- Sys.getenv("RETICULATE_PYTHON", unset = NA_character_)
  on.exit(if (is.na(old_python)) Sys.unsetenv("RETICULATE_PYTHON") else
    Sys.setenv(RETICULATE_PYTHON = old_python), add = TRUE)
  Sys.setenv(RETICULATE_PYTHON = python)

  local_mocked_bindings(
    .drugsignet_python_packages = function(include_graph_tool = FALSE) "sys",
    .package = "DrugSigNet"
  )
  result <- setup_python_dependencies(include_graph_tool = FALSE, quiet = TRUE)

  expect_identical(result$method, "configured")
  expect_identical(result$selected_python, python)
  expect_length(result$missing, 0L)
})

test_that("conda prefix validation rejects partial environments", {
  prefix <- tempfile("drugsignet-conda-prefix-")
  python <- file.path(prefix, "bin", "python")
  dir.create(dirname(python), recursive = TRUE)
  file.create(python)
  on.exit(unlink(prefix, recursive = TRUE), add = TRUE)

  expect_false(DrugSigNet:::.drugsignet_conda_prefix_is_valid(python))
  dir.create(file.path(prefix, "conda-meta"))
  file.create(file.path(prefix, "conda-meta", "history"))
  expect_true(DrugSigNet:::.drugsignet_conda_prefix_is_valid(python))
})

test_that("graph construction avoids graph_tool.all drawing imports", {
  script <- system.file("Python", "make_graphs.py", package = "DrugSigNet")
  source <- readLines(script, warn = FALSE)

  expect_true(any(grepl("^import graph_tool as gt$", source)))
  expect_false(any(grepl("^import graph_tool\\.all", source)))
})
