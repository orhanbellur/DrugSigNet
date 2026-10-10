test_that("complete conda graph environment is the setup default", {
  defaults <- formals(setup_python_dependencies)

  expect_identical(eval(defaults$method), c("conda", "auto", "virtualenv"))
  expect_true(eval(defaults$include_graph_tool))
})

test_that("conda transaction excludes pip-only kaleido", {
  packages <- DrugSigNet:::.drugsignet_conda_packages(
    include_graph_tool = TRUE
  )

  expect_true("graph-tool" %in% packages)
  expect_false("kaleido" %in% packages)
  expect_true("kaleido" %in% DrugSigNet:::.drugsignet_python_packages())
})

test_that("conda environment operations use one micromamba executable", {
  source <- paste(
    deparse(body(DrugSigNet:::.drugsignet_prepare_conda_environment)),
    collapse = "\n"
  )

  expect_match(source, "conda_python\\(envname, conda = conda\\)")
  expect_match(source, "conda_create\\(")
  expect_match(source, "conda_install\\(")
  expect_gte(length(gregexpr("conda = conda", source, fixed = TRUE)[[1L]]), 4L)
  expect_match(source, "drugsignet_install_micromamba")
})

test_that("a configured micromamba executable is reused without downloading", {
  executable <- tempfile("micromamba-")
  file.create(executable)
  old <- Sys.getenv("DRUGSIGNET_MICROMAMBA", unset = NA_character_)
  old_root <- Sys.getenv("MAMBA_ROOT_PREFIX", unset = NA_character_)
  on.exit({
    unlink(executable)
    if (is.na(old)) {
      Sys.unsetenv("DRUGSIGNET_MICROMAMBA")
    } else {
      Sys.setenv(DRUGSIGNET_MICROMAMBA = old)
    }
    if (is.na(old_root)) {
      Sys.unsetenv("MAMBA_ROOT_PREFIX")
    } else {
      Sys.setenv(MAMBA_ROOT_PREFIX = old_root)
    }
  }, add = TRUE)
  Sys.setenv(DRUGSIGNET_MICROMAMBA = executable)

  expect_identical(
    DrugSigNet:::.drugsignet_install_micromamba(quiet = TRUE),
    normalizePath(executable)
  )
  expect_true(nzchar(Sys.getenv("MAMBA_ROOT_PREFIX")))
})

test_that("micromamba bootstrap uses an official platform release", {
  platform <- DrugSigNet:::.drugsignet_micromamba_platform()
  source <- paste(
    deparse(body(DrugSigNet:::.drugsignet_install_micromamba)),
    collapse = "\n"
  )

  expect_true(platform %in% c(
    "linux-64", "linux-aarch64", "osx-64", "osx-arm64", "win-64"
  ))
  expect_match(source, "mamba-org/micromamba-releases")
  expect_match(source, "download/micromamba-")
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

test_that("graph construction avoids graph_tool.all drawing imports", {
  script <- system.file("Python", "make_graphs.py", package = "DrugSigNet")
  source <- readLines(script, warn = FALSE)

  expect_true(any(grepl("^import graph_tool as gt$", source)))
  expect_false(any(grepl("^import graph_tool\\.all", source)))
})
