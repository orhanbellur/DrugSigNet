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

test_that("Pixi Python subprocesses receive their prefix runtime paths", {
  prefix <- tempfile("drugsignet-pixi-prefix-")
  python <- file.path(prefix, "bin", "python")
  dir.create(dirname(python), recursive = TRUE)
  file.create(python)
  on.exit(unlink(prefix, recursive = TRUE), add = TRUE)

  environment <- DrugSigNet:::.drugsignet_python_process_env(python)

  subprocess_path <- sub("^PATH=", "", environment[startsWith(environment, "PATH=")])
  subprocess_entries <- strsplit(subprocess_path, .Platform$path.sep, fixed = TRUE)[[1L]]
  expect_true(any(vapply(subprocess_entries, function(entry) {
    DrugSigNet:::.drugsignet_paths_equal(entry, dirname(python))
  }, logical(1))))
  expect_true("PYTHONNOUSERSITE=1" %in% environment)
  if (.Platform$OS.type != "windows") {
    expect_true(any(startsWith(
      environment, paste0("LD_LIBRARY_PATH=", file.path(prefix, "lib"))
    )))
  }
})

test_that("Pixi Python is associated with its project manifest", {
  project <- tempfile("DrugSigNet-test-")
  python <- file.path(project, ".pixi", "envs", "default", "bin", "python")
  dir.create(dirname(python), recursive = TRUE)
  file.create(python)
  file.create(file.path(project, "pixi.toml"))
  on.exit(unlink(project, recursive = TRUE), add = TRUE)

  expect_true(DrugSigNet:::.drugsignet_paths_equal(
    DrugSigNet:::.drugsignet_pixi_project_from_python(python),
    normalizePath(project, mustWork = FALSE)
  ))
  resolved_python <- file.path(dirname(python), "python3.12")
  file.create(resolved_python)
  expect_true(DrugSigNet:::.drugsignet_paths_equal(
    DrugSigNet:::.drugsignet_pixi_project_from_python(resolved_python),
    normalizePath(project, mustWork = FALSE)
  ))
  expect_identical(
    DrugSigNet:::.drugsignet_pixi_project_from_python(Sys.which("python3")),
    ""
  )
})

test_that("Windows Pixi Python paths use the environment root", {
  project <- tempfile("DrugSigNet-windows-pixi-")
  prefix <- file.path(project, ".pixi", "envs", "default")
  python <- file.path(prefix, "python.exe")
  dir.create(prefix, recursive = TRUE)
  file.create(python)
  file.create(file.path(project, "pixi.toml"))
  on.exit(unlink(project, recursive = TRUE), add = TRUE)

  local_mocked_bindings(
    .drugsignet_is_windows = function() TRUE,
    .package = "DrugSigNet"
  )
  withr::local_envvar(c(
    RETICULATE_PYTHON = NA,
    DRUGSIGNET_PIXI_PROJECT = project
  ))

  expect_true(DrugSigNet:::.drugsignet_paths_equal(
    DrugSigNet:::.drugsignet_python_prefix(python),
    normalizePath(prefix, winslash = "/", mustWork = FALSE)
  ))
  expect_true(DrugSigNet:::.drugsignet_paths_equal(
    DrugSigNet:::.drugsignet_pixi_project_from_python(python),
    normalizePath(project, mustWork = FALSE)
  ))
  expect_true(DrugSigNet:::.drugsignet_paths_equal(
    DrugSigNet:::.drugsignet_discover_pixi_python(),
    path.expand(python)
  ))
  environment <- DrugSigNet:::.drugsignet_python_process_env(python)
  expect_true(any(grepl("Scripts", environment, fixed = TRUE)))
  expect_true(any(grepl("Library", environment, fixed = TRUE)))
  if (.Platform$OS.type == "windows") {
    expect_true(DrugSigNet:::.drugsignet_paths_equal(
      normalizePath(project, winslash = "/"),
      shortPathName(project)
    ))
  }
})

test_that("Python runner recognizes an already active Pixi prefix", {
  implementation <- paste(deparse(
    body(DrugSigNet:::.drugsignet_python_system2)
  ), collapse = "\n")

  expect_match(implementation, 'Sys.getenv("CONDA_PREFIX")', fixed = TRUE)
  expect_match(implementation, "python_prefix", fixed = TRUE)
  expect_match(implementation, ".drugsignet_system2", fixed = TRUE)

  wrapper <- paste(deparse(
    body(DrugSigNet:::.drugsignet_system2)
  ), collapse = "\n")
  expect_match(wrapper, "Sys.setenv", fixed = TRUE)
  expect_false(grepl("system2(command, args, env", wrapper, fixed = TRUE))
})

test_that("validated runtime marker belongs to the Python prefix", {
  prefix <- tempfile("drugsignet-validated-prefix-")
  python <- file.path(prefix, "bin", "python")
  dir.create(dirname(python), recursive = TRUE)
  file.create(python)
  on.exit(unlink(prefix, recursive = TRUE), add = TRUE)

  expect_false(DrugSigNet:::.drugsignet_python_runtime_validated(python))
  file.create(file.path(prefix, ".drugsignet-runtime-validated"))
  expect_true(DrugSigNet:::.drugsignet_python_runtime_validated(python))

  linked_python <- file.path(prefix, "bin", "python-link")
  linked <- suppressWarnings(file.symlink(python, linked_python))
  if (!isTRUE(linked)) {
    skip("symbolic links are unavailable for this Windows test account")
  }
  expect_true(DrugSigNet:::.drugsignet_python_runtime_validated(linked_python))
})

test_that("persisted Pixi configuration identifies a validated runtime", {
  project <- tempfile("drugsignet-validated-project-")
  python <- file.path(project, ".pixi", "envs", "default", "bin", "python")
  marker <- file.path(
    project, ".pixi", "envs", "default", ".drugsignet-runtime-validated"
  )
  dir.create(dirname(python), recursive = TRUE)
  file.create(python)
  file.create(marker)
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  withr::local_envvar(c(
    DRUGSIGNET_PIXI_PROJECT = project,
    RETICULATE_PYTHON = python,
    DRUGSIGNET_PIXI_VALIDATION_MARKER = marker
  ))

  expect_true(DrugSigNet:::.drugsignet_validated_pixi_runtime())
  unlink(marker)
  expect_false(DrugSigNet:::.drugsignet_validated_pixi_runtime())
})

test_that("Synapse preparation bypasses Conda for validated Pixi", {
  project <- tempfile("drugsignet-synapse-project-")
  python <- file.path(project, ".pixi", "envs", "default", "bin", "python")
  marker <- file.path(
    project, ".pixi", "envs", "default", ".drugsignet-runtime-validated"
  )
  dir.create(dirname(python), recursive = TRUE)
  file.create(python)
  file.create(marker)
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  withr::local_envvar(c(
    DRUGSIGNET_PIXI_PROJECT = project,
    RETICULATE_PYTHON = python
  ))
  local_mocked_bindings(
    .drugsignet_discover_pixi_python = function() python,
    setup_python_dependencies = function(...) stop("Conda fallback was called"),
    .package = "DrugSigNet"
  )

  expect_true(DrugSigNet:::.drugsignet_prepare_synapser_python())
})

test_that("graph-tool preflight uses configured Python before Conda", {
  python <- tempfile("configured-python-")
  file.create(python)
  on.exit(unlink(python), add = TRUE)
  selected <- NULL

  local_mocked_bindings(
    .drugsignet_discover_pixi_python = function() python,
    .drugsignet_use_configured_graph_tool = function(path) {
      selected <<- path
      invisible(TRUE)
    },
    setup_python_dependencies = function(...) stop("Conda fallback was called"),
    .package = "DrugSigNet"
  )

  expect_true(DrugSigNet:::.drugsignet_require_graph_tool())
  expect_identical(selected, path.expand(python))
})

test_that("network pipeline initializes graph-tool before network resolution", {
  implementation <- paste(deparse(
    body(DrugSigNet::drugNetworkPipeline)
  ), collapse = "\n")
  graph_tool <- regexpr(
    ".drugsignet_require_graph_tool()", implementation, fixed = TRUE
  )[[1L]]
  network_resolution <- regexpr(
    "resolve_network_inputs(", implementation, fixed = TRUE
  )[[1L]]

  expect_gt(graph_tool, 0L)
  expect_gt(network_resolution, graph_tool)
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
    .drugsignet_select_python = function(...) invisible(TRUE),
    .package = "DrugSigNet"
  )
  result <- setup_python_dependencies(include_graph_tool = FALSE, quiet = TRUE)

  expect_identical(result$method, "configured")
  expect_identical(result$selected_python, unname(python))
  expect_length(result$missing, 0L)
})

test_that("configured Python is requested before reticulate availability probes", {
  implementation <- paste(deparse(body(setup_python_dependencies)), collapse = "\n")
  configured_branch <- regexpr(
    "Using the complete Python runtime", implementation, fixed = TRUE
  )[[1L]]
  use_python <- regexpr(
    ".drugsignet_select_python(configured_python)",
    implementation,
    fixed = TRUE
  )[[1L]]

  expect_gt(use_python, 0L)
  expect_gt(configured_branch, use_python)
  expect_false(grepl(
    "reticulate::py_available", substr(implementation, 1L, configured_branch),
    fixed = TRUE
  ))
})

test_that("runtime setup pins Synapse and persists an absolute Python path", {
  setup_implementation <- paste(
    deparse(body(setup_python_dependencies)), collapse = "\n"
  )
  writer_implementation <- paste(
    deparse(body(DrugSigNet:::.drugsignet_write_reticulate_python)),
    collapse = "\n"
  )

  expect_match(setup_implementation, "synapseclient==4.4.2", fixed = TRUE)
  expect_match(writer_implementation, 'winslash = "/"', fixed = TRUE)
  expect_match(writer_implementation, "PYTHON_ENV", fixed = TRUE)
})

test_that("runtime setup verifies reticulate's initialized interpreter", {
  setup_implementation <- paste(
    deparse(body(setup_python_dependencies)), collapse = "\n"
  )
  assertion_implementation <- paste(
    deparse(body(DrugSigNet:::.drugsignet_assert_reticulate_python)),
    collapse = "\n"
  )

  expect_match(
    setup_implementation, ".drugsignet_select_python", fixed = TRUE
  )
  expect_match(assertion_implementation, "reticulate::py_config()", fixed = TRUE)
  expect_match(assertion_implementation, "Configured:", fixed = TRUE)
  expect_match(assertion_implementation, "initialized:", fixed = TRUE)
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

test_that("default Pixi project is discovered without a launcher command", {
  project <- tempfile("DrugSigNet-test-")
  prefix <- file.path(project, ".pixi", "envs", "default")
  python <- if (.Platform$OS.type == "windows") {
    file.path(prefix, "python.exe")
  } else {
    file.path(prefix, "bin", "python")
  }
  dir.create(dirname(python), recursive = TRUE)
  file.create(python)
  on.exit(unlink(project, recursive = TRUE), add = TRUE)

  withr::local_envvar(c(RETICULATE_PYTHON = NA, DRUGSIGNET_PIXI_PROJECT = project))
  discovered <- DrugSigNet:::.drugsignet_discover_pixi_python()

  expect_identical(discovered, unname(path.expand(python)))
  expect_true(DrugSigNet:::.drugsignet_paths_equal(
    Sys.getenv("RETICULATE_PYTHON"), path.expand(python)
  ))
  expect_true(DrugSigNet:::.drugsignet_paths_equal(
    Sys.getenv("DRUGSIGNET_PIXI_PROJECT"), normalizePath(project)
  ))
})

test_that("graph construction avoids graph_tool.all drawing imports", {
  script <- system.file("Python", "make_graphs.py", package = "DrugSigNet")
  source <- readLines(script, warn = FALSE)

  expect_true(any(grepl("^import graph_tool as gt$", source)))
  expect_false(any(grepl("^import graph_tool\\.all", source)))
})

test_that("a new managed Conda environment requests Python 3.12", {
  python <- tempfile("drugsignet-python312-")
  file.create(python)
  on.exit(unlink(python), add = TRUE)
  withr::local_envvar(c(RETICULATE_PYTHON = NA))
  created <- FALSE
  requested <- NULL
  selected <- NULL
  local_mocked_bindings(
    conda_binary = function(...) "conda",
    conda_python = function(...) if (created) python else "",
    conda_create = function(envname, packages, channel) {
      requested <<- packages
      created <<- TRUE
    },
    use_python = function(python, required) selected <<- python,
    conda_install = function(...) stop("complete runtime must not be reinstalled"),
    .package = "reticulate"
  )
  local_mocked_bindings(
    .drugsignet_conda_prefix_is_valid = function(...) FALSE,
    .drugsignet_python_has_modules = function(...) TRUE,
    .package = "DrugSigNet"
  )
  result <- DrugSigNet:::.drugsignet_prepare_conda_environment(
    envname = "test-python312", include_graph_tool = TRUE, quiet = TRUE
  )
  expect_true(created)
  expect_identical(requested, c("python=3.12", "pip"))
  expect_identical(selected, python)
  expect_identical(result, python)
})

test_that("a valid existing managed runtime is reused without recreation", {
  python <- tempfile("existing-drugsignet-python-")
  file.create(python)
  on.exit(unlink(python), add = TRUE)
  withr::local_envvar(c(RETICULATE_PYTHON = NA))
  local_mocked_bindings(
    conda_binary = function(...) "conda",
    conda_python = function(...) python,
    conda_create = function(...) stop("existing runtime must not be replaced"),
    conda_install = function(...) stop("complete runtime must not be reinstalled"),
    use_python = function(...) invisible(NULL),
    .package = "reticulate"
  )
  local_mocked_bindings(
    .drugsignet_conda_prefix_is_valid = function(...) TRUE,
    .drugsignet_python_has_modules = function(...) TRUE,
    .package = "DrugSigNet"
  )
  expect_identical(DrugSigNet:::.drugsignet_prepare_conda_environment(
    envname = "existing-runtime", include_graph_tool = TRUE, quiet = TRUE
  ), python)
})
