#' Install Python dependencies used by DrugSigNet
#'
#' @description
#' Installs required Python modules for DrugSigNet using `reticulate`.
#'
#' By default, this creates a dedicated conda environment containing the core
#' Python modules, `kaleido`, and, on supported operating systems, `graph_tool`.
#' Native Windows installations skip `graph_tool` because it is unavailable.
#' A dedicated environment is
#' necessary because `graph_tool` is distributed by conda-forge rather than
#' PyPI. It also prevents reticulate's `uv` environment from being initialized
#' before the graph stack has been selected.
#'
#' If `RETICULATE_PYTHON` already names an interpreter containing every
#' requested module (for example the generated Pixi environment), that runtime
#' is selected directly and no Conda or Micromamba environment is created.
#' The default `~/DrugSigNet-test` Pixi project, `DRUGSIGNET_PIXI_PROJECT`, and
#' `options(DrugSigNet.pixi_project = ...)` are discovered automatically.
#' Incomplete dedicated Conda environments left by interrupted installations
#' are detected and safely recreated before packages are installed.
#'
#' @param envname Name of the Python environment to install into.
#' @param method Installation backend passed to [reticulate::py_install()].
#'   One of `"conda"`, `"auto"`, or `"virtualenv"`. The default is `"conda"`.
#' @param include_graph_tool Logical; whether to attempt installing `graph_tool`.
#'   The default is `TRUE`. If `TRUE`, `method` must be `"conda"`; DrugSigNet
#'   installs the `graph-tool` package from the `conda-forge` channel. This is
#'   automatically changed to `FALSE` on Windows.
#' @param force Logical; if `TRUE`, reinstall requested modules even if already available.
#' @param install_synapser Logical; if `TRUE`, install optional Synapse support
#'   after configuring Python. This mirrors `DRUGSIGNET_INSTALL_SYNAPSER=true`
#'   in `tools/install_local_drugsignet.R`.
#' @param write_renviron Logical; if `TRUE`, persist the selected Python by
#'   writing `RETICULATE_PYTHON` to `~/.Renviron`.
#' @param quiet Logical; if `FALSE`, prints progress messages.
#'
#' @return Invisibly returns a list with requested packages, missing packages prior
#'   to install, graph-tool install status, and installation method.
#' @export
setup_python_dependencies <- function(
    envname = "r-drugsignet",
    method = c("conda", "auto", "virtualenv"),
    include_graph_tool = TRUE,
    force = FALSE,
    install_synapser = FALSE,
    write_renviron = FALSE,
    quiet = FALSE) {
  method <- match.arg(method)

  is_windows <- .drugsignet_is_windows()
  if (is.null(include_graph_tool)) include_graph_tool <- TRUE
  if (is_windows && isTRUE(include_graph_tool)) {
    warning("graph_tool is not supported on Windows and was skipped.", call. = FALSE)
    include_graph_tool <- FALSE
  }

  configured_python <- .drugsignet_discover_pixi_python()
  requested_modules <- .drugsignet_python_packages(
    include_graph_tool = isTRUE(include_graph_tool)
  )
  runtime_validated <- .drugsignet_validated_pixi_runtime() ||
    .drugsignet_python_runtime_validated(configured_python)
  if (!isTRUE(force) && nzchar(configured_python) &&
      file.exists(configured_python) &&
      (runtime_validated ||
       .drugsignet_python_has_modules(configured_python, requested_modules))) {
    # Register the explicit interpreter before any availability probe. Calling
    # py_available(initialize = FALSE) first can still trigger reticulate's uv
    # discovery and permanently initialize the wrong Python in this session.
    .drugsignet_select_python(configured_python)
    if (!quiet) {
      message("Using the complete Python runtime from RETICULATE_PYTHON: ", configured_python)
    }
    return(invisible(list(
      packages = requested_modules,
      missing = character(0),
      method = "configured",
      envname = NULL,
      selected_python = configured_python,
      wrote_renviron = FALSE,
      install_synapser = isTRUE(install_synapser),
      synapser_installed = runtime_validated ||
        .drugsignet_python_has_modules(configured_python, "synapseclient"),
      graph_tool_requested = isTRUE(include_graph_tool),
      graph_tool_installed = runtime_validated ||
        .drugsignet_python_has_modules(configured_python, "graph_tool")
    )))
  }
  if (isTRUE(include_graph_tool) && !identical(method, "conda")) {
    stop(
      "graph_tool is not available from PyPI; use method = 'conda', or set ",
      "include_graph_tool = FALSE.",
      call. = FALSE
    )
  }

  if (identical(method, "conda")) {
    .drugsignet_prepare_conda_environment(
      envname = envname,
      include_graph_tool = include_graph_tool,
      force = force,
      quiet = quiet
    )
  }

  packages <- .drugsignet_python_packages(include_graph_tool = FALSE)
  missing <- if (isTRUE(force)) {
    packages
  } else if (identical(method, "conda")) {
    python <- reticulate::conda_python(envname)
    packages[!vapply(
      packages,
      function(package) .drugsignet_python_has_modules(python, package),
      logical(1)
    )]
  } else {
    packages[!vapply(packages, reticulate::py_module_available, logical(1))]
  }

  if (length(missing) > 0L) {
    if (!quiet) {
      message("Installing Python modules for DrugSigNet: ", paste(missing, collapse = ", "))
    }

    install_specs <- missing
    install_specs[install_specs == "synapseclient"] <- "synapseclient==4.4.2"
    reticulate::py_install(
      packages = install_specs,
      envname = envname,
      method = method,
      pip = TRUE
    )
  } else if (!quiet) {
    message("All core DrugSigNet Python modules are already available.")
  }

  graph_tool_requested <- isTRUE(include_graph_tool)
  graph_tool_installed <- if (graph_tool_requested) {
    reticulate::py_module_available("graph_tool")
  } else {
    FALSE
  }

  if (graph_tool_requested && !graph_tool_installed) {
    if (!quiet) {
      message("Attempting to install graph_tool with conda-forge.")
    }

    graph_tool_error <- NULL
    if (!identical(method, "conda")) {
      graph_tool_error <- paste0(
        "graph_tool is not available from PyPI. Re-run with ",
        "method = 'conda' or install graph_tool with your system package manager."
      )
    } else {
      tryCatch(
        {
          reticulate::conda_install(
            envname = envname,
            packages = "graph-tool",
            channel = "conda-forge",
            pip = FALSE
          )
        },
        error = function(e) {
          graph_tool_error <<- conditionMessage(e)
        }
      )
    }

    graph_tool_installed <- reticulate::py_module_available("graph_tool")
    if (!graph_tool_installed && !quiet) {
      warning(
        "graph_tool could not be installed automatically. ",
        "Install it with conda-forge (`conda install -c conda-forge graph-tool`) ",
        "or your system package manager.",
        if (!is.null(graph_tool_error)) paste0(" Last error: ", graph_tool_error),
        call. = FALSE
      )
    }
  }

  selected_python <- .drugsignet_selected_python(envname = envname, method = method)
  if (nzchar(selected_python)) {
    .drugsignet_assert_reticulate_python(selected_python)
  }
  if (isTRUE(write_renviron) && nzchar(selected_python)) {
    .drugsignet_write_reticulate_python(selected_python)
    if (!quiet) {
      message("Wrote RETICULATE_PYTHON to ~/.Renviron: ", selected_python)
    }
  }

  synapser_installed <- FALSE
  if (isTRUE(install_synapser)) {
    synapser_installed <- .drugsignet_install_synapser(quiet = quiet)
  }

  invisible(list(
    packages = c(packages, if (graph_tool_requested) "graph_tool" else character(0)),
    missing = missing,
    method = method,
    envname = envname,
    selected_python = selected_python,
    wrote_renviron = isTRUE(write_renviron) && nzchar(selected_python),
    install_synapser = isTRUE(install_synapser),
    synapser_installed = synapser_installed,
    graph_tool_requested = graph_tool_requested,
    graph_tool_installed = graph_tool_installed
  ))
}

.drugsignet_discover_pixi_python <- function() {
  configured <- Sys.getenv("RETICULATE_PYTHON")
  if (nzchar(configured) && file.exists(configured)) return(configured)

  projects <- unique(c(
    Sys.getenv("DRUGSIGNET_PIXI_PROJECT"),
    getOption("DrugSigNet.pixi_project", ""),
    file.path(path.expand("~"), "DrugSigNet-test")
  ))
  projects <- projects[nzchar(projects)]
  if (!length(projects)) return("")

  prefixes <- file.path(path.expand(projects), ".pixi", "envs", "default")
  candidates <- if (.drugsignet_is_windows()) {
    c(file.path(prefixes, "python.exe"),
      file.path(prefixes, "Scripts", "python.exe"))
  } else {
    file.path(prefixes, "bin", "python")
  }
  available <- candidates[file.exists(candidates)]
  if (!length(available)) return("")

  # Keep the lexical bin/python path rather than resolving Pixi's python3.x
  # symlink; reticulate treats the resolved target as requiring Conda.
  python <- path.expand(available[[1]])
  project <- .drugsignet_pixi_project_from_python(python)
  if (!nzchar(project)) return("")
  Sys.setenv(
    DRUGSIGNET_PIXI_PROJECT = project,
    RETICULATE_PYTHON = python
  )
  python
}

.drugsignet_prepare_conda_environment <- function(envname, include_graph_tool,
                                                   force = FALSE, quiet = FALSE) {
  # conda_binary() raises an error (rather than returning NULL) when conda is
  # absent in recent reticulate releases. Treat both outcomes as "not found"
  # so a clean Rocker/RStudio image reaches the Miniconda bootstrap below.
  conda <- tryCatch(
    reticulate::conda_binary(),
    error = function(e) NULL
  )
  if (is.null(conda) || !nzchar(conda)) {
    if (!quiet) message("Installing Miniconda for DrugSigNet.")
    reticulate::install_miniconda()
    conda <- tryCatch(
      reticulate::conda_binary(),
      error = function(e) NULL
    )
  }
  if (is.null(conda) || !nzchar(conda)) {
    stop(
      "Miniconda installation completed, but reticulate still cannot find its ",
      "conda binary. Restart R and run setup_python_dependencies() again.",
      call. = FALSE
    )
  }

  python <- tryCatch(reticulate::conda_python(envname), error = function(e) "")
  prefix_valid <- .drugsignet_conda_prefix_is_valid(python)
  if (!prefix_valid) {
    stale_prefix <- if (length(python) == 1L && nzchar(python)) {
      .drugsignet_python_prefix(python)
    } else {
      ""
    }
    if (nzchar(stale_prefix) && dir.exists(stale_prefix) &&
        identical(basename(stale_prefix), envname) &&
        identical(basename(dirname(stale_prefix)), "envs")) {
      if (!quiet) message("Removing incomplete DrugSigNet conda environment: ", stale_prefix)
      unlink(stale_prefix, recursive = TRUE, force = TRUE)
    }
    if (!quiet) message("Creating DrugSigNet conda environment: ", envname)
    reticulate::conda_create(
      envname = envname,
      packages = c("python=3.10", "pip"),
      channel = "conda-forge"
    )
    python <- reticulate::conda_python(envname)
  }

  # Set the prescriptive selector before any reticulate Python probe. This is
  # especially important on Windows, where otherwise reticulate 1.41+ can
  # initialize a temporary uv interpreter before the conda environment.
  Sys.setenv(RETICULATE_PYTHON = python)
  reticulate::use_python(python, required = TRUE)
  conda_packages <- .drugsignet_conda_packages(
    include_graph_tool = include_graph_tool
  )
  python_modules <- .drugsignet_python_packages(
    include_graph_tool = include_graph_tool
  )
  if (isTRUE(force) || !.drugsignet_python_has_modules(python, python_modules)) {
    if (!quiet) message("Installing DrugSigNet Python dependencies from conda-forge.")
    reticulate::conda_install(
      envname = envname,
      packages = conda_packages,
      channel = "conda-forge",
      pip = FALSE
    )
  }
  invisible(python)
}

.drugsignet_conda_prefix_is_valid <- function(python) {
  if (length(python) != 1L || is.na(python) || !nzchar(python) ||
      !file.exists(python)) {
    return(FALSE)
  }
  prefix <- .drugsignet_python_prefix(python)
  file.exists(file.path(prefix, "conda-meta", "history"))
}

.drugsignet_conda_packages <- function(include_graph_tool = TRUE) {
  # kaleido is intentionally absent: current conda-forge repositories do not
  # publish it for all supported platforms. setup_python_dependencies() detects
  # it after this conda transaction and installs it with pip into the same env.
  packages <- c(
    "numpy", "pandas", "scipy", "networkx", "joblib", "tqdm", "openpyxl",
    "jinja2", "markupsafe"
  )
  if (isTRUE(include_graph_tool)) packages <- c(packages, "graph-tool")
  packages
}

.drugsignet_python_module_probe <- function(python, modules) {
  script <- tempfile(fileext = ".py")
  on.exit(unlink(script), add = TRUE)
  module_literal <- paste(
    encodeString(modules, quote = "\""), collapse = ", "
  )
  writeLines(paste0(
    "import importlib; ",
    "[importlib.import_module(x) for x in [", module_literal, "]]"
  ), script)
  output <- suppressWarnings(.drugsignet_python_system2(
    python, shQuote(script), stdout = TRUE, stderr = TRUE
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  list(
    available = identical(as.integer(status), 0L),
    status = as.integer(status),
    output = as.character(output)
  )
}

.drugsignet_python_has_modules <- function(python, modules) {
  isTRUE(.drugsignet_python_module_probe(python, modules)$available)
}

.drugsignet_python_system2 <- function(python, args, stdout = FALSE,
                                        stderr = FALSE) {
  python_prefix <- .drugsignet_python_prefix(python)
  active_prefix <- Sys.getenv("CONDA_PREFIX")
  if (nzchar(active_prefix) &&
      .drugsignet_paths_equal(active_prefix, python_prefix)) {
    return(system2(
      python,
      args,
      stdout = stdout,
      stderr = stderr,
      env = .drugsignet_python_process_env(python)
    ))
  }

  project <- if (.drugsignet_validated_pixi_runtime()) {
    normalizePath(Sys.getenv("DRUGSIGNET_PIXI_PROJECT"), mustWork = FALSE)
  } else {
    .drugsignet_pixi_project_from_python(python)
  }
  pixi <- Sys.which("pixi")
  if (!nzchar(pixi)) {
    candidate <- file.path(path.expand("~"), ".pixi", "bin", "pixi")
    if (file.exists(candidate)) pixi <- candidate
  }

  if (nzchar(project) && nzchar(pixi)) {
    return(system2(
      pixi,
      c(
        "run", "--manifest-path", shQuote(file.path(project, "pixi.toml")),
        "python", args
      ),
      stdout = stdout,
      stderr = stderr
    ))
  }

  system2(
    python,
    args,
    stdout = stdout,
    stderr = stderr,
    env = .drugsignet_python_process_env(python)
  )
}

.drugsignet_pixi_project_from_python <- function(python) {
  if (length(python) != 1L || is.na(python) || !nzchar(python)) return("")
  normalized <- normalizePath(python, mustWork = FALSE)
  projects <- unique(c(
    Sys.getenv("DRUGSIGNET_PIXI_PROJECT"),
    getOption("DrugSigNet.pixi_project", ""),
    file.path(path.expand("~"), "DrugSigNet-test")
  ))
  projects <- projects[nzchar(projects)]
  for (project in projects) {
    project <- normalizePath(path.expand(project), mustWork = FALSE)
    prefix <- file.path(project, ".pixi", "envs", "default")
    runtime_dirs <- c(file.path(prefix, c("bin", "Scripts")), prefix)
    inside_runtime <- any(vapply(runtime_dirs, function(runtime_dir) {
      .drugsignet_path_starts_with(normalized, runtime_dir)
    }, logical(1)))
    if (inside_runtime &&
        file.exists(file.path(project, "pixi.toml"))) {
      return(project)
    }
  }

  portable <- gsub("\\\\", "/", normalized)
  position <- regexpr("/.pixi/envs/default(/(bin|Scripts))?/", portable,
                      perl = TRUE, ignore.case = .drugsignet_is_windows())[[1L]]
  if (position < 1L) return("")
  project <- substr(portable, 1L, position - 1L)
  if (file.exists(file.path(project, "pixi.toml"))) project else ""
}

.drugsignet_path_starts_with <- function(path, directory) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  directory <- normalizePath(directory, winslash = "/", mustWork = FALSE)
  if (.drugsignet_is_windows()) {
    path <- tolower(path)
    directory <- tolower(directory)
  }
  startsWith(path, paste0(directory, "/"))
}

.drugsignet_paths_equal <- function(first, second) {
  first <- normalizePath(first, winslash = "/", mustWork = FALSE)
  second <- normalizePath(second, winslash = "/", mustWork = FALSE)
  if (.drugsignet_is_windows()) {
    first <- tolower(first)
    second <- tolower(second)
  }
  identical(first, second)
}

.drugsignet_python_prefix <- function(python) {
  python <- normalizePath(python, winslash = "/", mustWork = FALSE)
  executable_dir <- dirname(python)
  if (tolower(basename(executable_dir)) %in% c("bin", "scripts")) {
    dirname(executable_dir)
  } else {
    executable_dir
  }
}

.drugsignet_python_runtime_validated <- function(python) {
  if (length(python) != 1L || is.na(python) || !nzchar(python) ||
      !file.exists(python)) {
    return(FALSE)
  }

  # Check the lexical interpreter path first. Pixi/conda Python executables are
  # often symlinks, and normalizePath() may resolve them outside the environment
  # prefix even though the selected entry point and validation marker are inside
  # that prefix.
  lexical_prefix <- .drugsignet_python_prefix(path.expand(python))
  if (file.exists(file.path(
    lexical_prefix, ".drugsignet-runtime-validated"
  ))) {
    return(TRUE)
  }

  projects <- unique(c(
    Sys.getenv("DRUGSIGNET_PIXI_PROJECT"),
    getOption("DrugSigNet.pixi_project", ""),
    file.path(path.expand("~"), "DrugSigNet-test")
  ))
  projects <- projects[nzchar(projects)]
  for (project in projects) {
    project <- normalizePath(path.expand(project), mustWork = FALSE)
    prefix <- file.path(project, ".pixi", "envs", "default")
    expected_pythons <- if (.drugsignet_is_windows()) {
      c(file.path(prefix, "python.exe"), file.path(prefix, "Scripts", "python.exe"))
    } else {
      file.path(prefix, "bin", "python")
    }
    marker <- file.path(
      project, ".pixi", "envs", "default", ".drugsignet-runtime-validated"
    )
    same_python <- any(vapply(expected_pythons, function(expected_python) {
      identical(path.expand(python), expected_python) || identical(
        normalizePath(python, winslash = "/", mustWork = FALSE),
        normalizePath(expected_python, winslash = "/", mustWork = FALSE)
      )
    }, logical(1)))
    if (same_python && file.exists(marker)) return(TRUE)
  }

  resolved_prefix <- .drugsignet_python_prefix(
    normalizePath(python, mustWork = TRUE)
  )
  file.exists(file.path(resolved_prefix, ".drugsignet-runtime-validated"))
}

.drugsignet_validated_pixi_runtime <- function() {
  project <- Sys.getenv("DRUGSIGNET_PIXI_PROJECT")
  python <- Sys.getenv("RETICULATE_PYTHON")
  persisted_marker <- Sys.getenv("DRUGSIGNET_PIXI_VALIDATION_MARKER")
  if (nzchar(persisted_marker) && file.exists(persisted_marker) &&
      nzchar(python) && file.exists(python)) {
    return(TRUE)
  }
  if (!nzchar(project) || !nzchar(python)) return(FALSE)

  project <- path.expand(project)
  python <- path.expand(python)
  expected_prefix <- file.path(project, ".pixi", "envs", "default")
  marker <- file.path(
    project, ".pixi", "envs", "default", ".drugsignet-runtime-validated"
  )
  file.exists(python) && file.exists(marker) &&
    (identical(normalizePath(dirname(python), winslash = "/", mustWork = FALSE),
               normalizePath(expected_prefix, winslash = "/", mustWork = FALSE)) ||
     .drugsignet_path_starts_with(python, expected_prefix))
}

.drugsignet_python_process_env <- function(python) {
  if (length(python) != 1L || is.na(python) || !nzchar(python)) return(character())

  prefix <- .drugsignet_python_prefix(python)
  prefix_bin <- if (.drugsignet_is_windows()) {
    c(prefix, file.path(prefix, "Scripts"), file.path(prefix, "Library", "bin"))
  } else {
    file.path(prefix, "bin")
  }
  prefix_lib <- file.path(prefix, "lib")
  path <- paste(c(prefix_bin, Sys.getenv("PATH")), collapse = .Platform$path.sep)
  environment <- c(
    paste0("PATH=", path),
    "PYTHONNOUSERSITE=1"
  )
  if (!.drugsignet_is_windows()) {
    library_path <- c(prefix_lib, Sys.getenv("LD_LIBRARY_PATH"))
    library_path <- library_path[nzchar(library_path)]
    environment <- c(
      environment,
      paste0("LD_LIBRARY_PATH=", paste(library_path, collapse = .Platform$path.sep))
    )
  }
  environment
}

.drugsignet_python_packages <- function(include_graph_tool = FALSE) {
  pkgs <- c("numpy", "pandas", "scipy", "networkx", "joblib", "tqdm", "openpyxl", "jinja2", "markupsafe", "kaleido", "synapseclient")
  if (isTRUE(include_graph_tool)) {
    pkgs <- c(pkgs, "graph_tool")
  }
  pkgs
}

.drugsignet_network_methods_available <- function(os_type = .Platform$OS.type,
                                                   warn = TRUE) {
  if (!identical(os_type, "windows")) return(TRUE)
  if (isTRUE(warn)) {
    warning(
      paste(
        "Network-based methods require the Python graph-tool library,",
        "which is not available for native Windows installations.",
        "",
        "Please use the DrugSigNet Docker image or Windows Subsystem",
        "for Linux (WSL) to access network-based analyses.",
        sep = "\n"
      ),
      call. = FALSE
    )
  }
  FALSE
}

.drugsignet_require_graph_tool <- function() {
  # A configured interpreter is authoritative.  In particular, do not require
  # the installer marker here: it is an installation-time optimisation and can
  # be absent after a project is copied, while its Python remains usable.  More
  # importantly, falling through to setup_python_dependencies() asks reticulate
  # to discover Conda and produces "Unable to find conda binary" even though a
  # complete Pixi interpreter was explicitly configured.
  python <- .drugsignet_discover_pixi_python()
  if (nzchar(python) && file.exists(python)) {
    # Preserve the lexical `bin/python` entry point. normalizePath() resolves
    # Pixi's symlink to `bin/python3.10`; reticulate then classifies that target
    # as a Conda-managed interpreter and calls conda_binary(), although Pixi
    # intentionally provides no Conda executable. The explicit `bin/python`
    # path is a standalone interpreter request and avoids that false discovery.
    requested_python <- path.expand(python)
    selection_error <- tryCatch(
      {
        # Do not call py_available() here. Even with initialize = FALSE, recent
        # reticulate releases perform interpreter discovery and may query Conda
        # before honoring the project-local Pixi interpreter. use_python()
        # records the explicit interpreter without that discovery step and also
        # rejects attempts to switch an already initialized Python session.
        reticulate::use_python(requested_python, required = TRUE)
        NULL
      },
      error = identity
    )
    if (!is.null(selection_error)) {
      stop(
        "DrugSigNet could not select its configured Python interpreter: ",
        requested_python, "\n", conditionMessage(selection_error),
        "\nRestart R, load DrugSigNet first, and retry the network pipeline.",
        call. = FALSE
      )
    }

    import_error <- tryCatch(
      {
        reticulate::import("graph_tool", delay_load = FALSE)
        NULL
      },
      error = identity
    )
    if (!is.null(import_error)) {
      stop(
        "DrugSigNet selected the configured Python interpreter, but could not ",
        "import graph_tool: ", requested_python, "\n",
        conditionMessage(import_error),
        call. = FALSE
      )
    }
    return(invisible(TRUE))
  }

  # No configured interpreter was found. An interpreter that was deliberately
  # initialized by the caller may still provide graph_tool; only then is it
  # safe to accept it without provisioning DrugSigNet's environment.
  if (reticulate::py_available(initialize = FALSE) &&
      reticulate::py_module_available("graph_tool")) {
    return(invisible(TRUE))
  }

  tryCatch(
    setup_python_dependencies(quiet = TRUE),
    error = function(e) {
      stop(
        "DrugSigNet network methods require the conda graph-tool environment.\n",
        conditionMessage(e),
        "\nIf synapser or reticulate was used in this R session, restart R, ",
        "load DrugSigNet first, and retry the network pipeline.",
        call. = FALSE
      )
    }
  )
  if (!reticulate::py_module_available("graph_tool")) {
    stop(
      "DrugSigNet provisioned Python, but graph_tool is still unavailable. ",
      "Run setup_python_dependencies(force = TRUE) and retry.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.drugsignet_selected_python <- function(envname, method) {
  if (identical(method, "conda")) {
    py <- tryCatch(reticulate::conda_python(envname), error = function(e) "")
    if (length(py) == 1L && !is.na(py) && nzchar(py)) {
      return(py)
    }
  }

  py_config <- tryCatch(reticulate::py_config(), error = function(e) NULL)
  if (!is.null(py_config) && !is.null(py_config$python) && nzchar(py_config$python)) {
    return(py_config$python)
  }

  ""
}

.drugsignet_assert_reticulate_python <- function(expected_python) {
  config <- reticulate::py_config()
  expected <- normalizePath(
    expected_python, winslash = "/", mustWork = TRUE
  )
  initialized <- normalizePath(
    config$python, winslash = "/", mustWork = TRUE
  )
  same <- if (.drugsignet_is_windows()) {
    identical(tolower(expected), tolower(initialized))
  } else {
    identical(expected, initialized)
  }
  if (!same) {
    stop(
      "reticulate initialized a different Python interpreter. Configured: ",
      expected, "; initialized: ", initialized,
      "\nRestart R and run setup_python_dependencies() before using any ",
      "reticulate-backed package.",
      call. = FALSE
    )
  }
  invisible(initialized)
}

.drugsignet_select_python <- function(python) {
  # This is the single initialization boundary used by setup. Keeping selection
  # in one helper makes ordering explicit and lets unit tests avoid binding a
  # process-global Python runtime that cannot be changed later in the session.
  Sys.setenv(RETICULATE_PYTHON = python)
  reticulate::use_python(python, required = TRUE)
  .drugsignet_assert_reticulate_python(python)
}

.drugsignet_write_reticulate_python <- function(python) {
  python <- normalizePath(python, winslash = "/", mustWork = TRUE)
  renviron <- path.expand("~/.Renviron")
  old <- if (file.exists(renviron)) readLines(renviron, warn = FALSE) else character()
  old <- old[!grepl("^RETICULATE_(PYTHON|PYTHON_ENV)=", old)]
  writeLines(c(old, paste0("RETICULATE_PYTHON=", python)), renviron)
}

.drugsignet_install_synapser <- function(quiet = FALSE, verify = TRUE,
                                         lib = NULL) {
  repos <- c(CRAN = "https://cloud.r-project.org")

  old_repos <- getOption("repos")
  old_timeout <- getOption("timeout")
  on.exit(options(repos = old_repos, timeout = old_timeout), add = TRUE)
  if (is.null(old_timeout) || is.na(old_timeout)) old_timeout <- 60
  options(repos = repos, timeout = max(1000, old_timeout))

  if (!requireNamespace("remotes", quietly = TRUE)) {
    utils::install.packages("remotes", repos = repos[["CRAN"]], quiet = quiet)
  }
  if (!quiet) {
    message(
      "Installing current 'synapser' from the official Sage Bionetworks repository."
    )
  }
  # Do not try the legacy Synapse RAN release first. synapser 2.1.5.356 asks
  # reticulate for numpy<=1.24.4 while modern reticulate may already have
  # initialized a uv environment with NumPy 2, and its bootstrap assumes pip is
  # present. The maintained 3.x package avoids that obsolete bootstrap path.
  remotes::install_github(
    "Sage-Bionetworks/synapser",
    dependencies = TRUE,
    upgrade = "never",
    build = FALSE,
    build_vignettes = FALSE,
    lib = lib,
    quiet = quiet
  )
  if (!isTRUE(verify)) return(TRUE)
  synapser_load <- tryCatch(
    {
      loadNamespace(.drugsignet_synapser_package())
      TRUE
    },
    error = function(e) e
  )
  if (!isTRUE(synapser_load)) {
    stop(
      "Package 'synapser' was installed but its namespace ",
      "could not be loaded: ", conditionMessage(synapser_load),
      "\nRetry setup_synapser() and inspect the preceding installation output.",
      call. = FALSE
    )
  }
  TRUE
}

.drugsignet_rjson_version_compatible <- function(version) {
  utils::compareVersion(as.character(version), "0.2.21") <= 0L
}

.drugsignet_is_windows <- function() {
  tolower(Sys.info()[["sysname"]]) == "windows"
}

.drugsignet_auto_install_enabled <- function() {
  # Loading an R package must not unexpectedly download or mutate an external
  # runtime. Users can explicitly opt in or call setup_python_dependencies().
  opt <- getOption("DrugSigNet.auto_install_python", FALSE)
  env <- Sys.getenv("DRUGSIGNET_AUTO_INSTALL_PYTHON", "")

  if (nzchar(env)) {
    env_flag <- tolower(env) %in% c("1", "true", "yes", "y")
    return(isTRUE(env_flag))
  }

  isTRUE(opt)
}

.drugsignet_maybe_auto_install_python <- function() {
  if (!.drugsignet_auto_install_enabled()) {
    return(invisible(FALSE))
  }

  tryCatch(
    {
      setup_python_dependencies(quiet = TRUE)
      TRUE
    },
    error = function(e) {
      packageStartupMessage(
        "DrugSigNet could not auto-install Python dependencies. ",
        "Run setup_python_dependencies() manually. Error: ",
        conditionMessage(e)
      )
      FALSE
    }
  )
}
