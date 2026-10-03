#' Install Python dependencies used by DrugSigNet
#'
#' @description
#' Installs required Python modules for DrugSigNet using `reticulate`.
#'
#' By default, this creates a dedicated conda environment containing the core
#' Python modules, `kaleido`, and `graph_tool`. A dedicated environment is
#' necessary because `graph_tool` is distributed by conda-forge rather than
#' PyPI. It also prevents reticulate's `uv` environment from being initialized
#' before the graph stack has been selected.
#'
#' @param envname Name of the Python environment to install into.
#' @param method Installation backend passed to [reticulate::py_install()].
#'   One of `"conda"`, `"auto"`, or `"virtualenv"`. The default is `"conda"`.
#' @param include_graph_tool Logical; whether to attempt installing `graph_tool`.
#'   The default is `TRUE`. If `TRUE`, `method` must be `"conda"`; DrugSigNet
#'   installs the `graph-tool` package from the `conda-forge` channel.
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
  } else {
    packages[!vapply(packages, reticulate::py_module_available, logical(1))]
  }

  if (length(missing) > 0L) {
    if (!quiet) {
      message("Installing Python modules for DrugSigNet: ", paste(missing, collapse = ", "))
    }

    reticulate::py_install(
      packages = missing,
      envname = envname,
      method = method,
      pip = TRUE
    )
  } else if (!quiet) {
    message("All core DrugSigNet Python modules are already available.")
  }

  graph_tool_requested <- isTRUE(include_graph_tool)
  graph_tool_installed <- reticulate::py_module_available("graph_tool")

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

.drugsignet_prepare_conda_environment <- function(envname, include_graph_tool,
                                                   force = FALSE, quiet = FALSE) {
  conda <- reticulate::conda_binary()
  if (is.null(conda) || !nzchar(conda)) {
    if (!quiet) message("Installing Miniconda for DrugSigNet.")
    reticulate::install_miniconda()
  }

  python <- tryCatch(reticulate::conda_python(envname), error = function(e) "")
  if (!length(python) || !nzchar(python) || !file.exists(python)) {
    if (!quiet) message("Creating DrugSigNet conda environment: ", envname)
    reticulate::conda_create(
      envname = envname,
      packages = c("python=3.10", "pip"),
      channel = "conda-forge"
    )
    python <- reticulate::conda_python(envname)
  }

  # Once reticulate initializes Python it cannot switch interpreters in the
  # same R session. Fail before installing into an environment that cannot be
  # used, and tell the user exactly how to recover.
  if (reticulate::py_available(initialize = FALSE)) {
    active <- normalizePath(reticulate::py_config()$python, mustWork = FALSE)
    requested <- normalizePath(python, mustWork = FALSE)
    if (!identical(active, requested)) {
      stop(
        "reticulate has already initialized a different Python interpreter: ",
        active, "\nRestart R and call setup_python_dependencies() before using ",
        "reticulate or synapser.",
        call. = FALSE
      )
    }
  }

  Sys.setenv(RETICULATE_PYTHON = python)
  conda_packages <- c(
    "numpy", "pandas", "scipy", "networkx", "joblib", "tqdm", "openpyxl",
    "jinja2", "markupsafe", "kaleido"
  )
  if (isTRUE(include_graph_tool)) conda_packages <- c(conda_packages, "graph-tool")
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

.drugsignet_python_has_modules <- function(python, modules) {
  script <- tempfile(fileext = ".py")
  on.exit(unlink(script), add = TRUE)
  module_literal <- paste(sprintf("%s", encodeString(modules, quote = "\"")), collapse = ", ")
  writeLines(
    paste0(
      "import importlib.util; assert all(importlib.util.find_spec(x) is not None for x in [",
      module_literal, "])"
    ),
    script
  )
  status <- suppressWarnings(system2(python, script, stdout = FALSE, stderr = FALSE))
  identical(status, 0L)
}

.drugsignet_python_packages <- function(include_graph_tool = FALSE) {
  pkgs <- c("numpy", "pandas", "scipy", "networkx", "joblib", "tqdm", "openpyxl", "jinja2", "markupsafe", "kaleido")
  if (isTRUE(include_graph_tool)) {
    pkgs <- c(pkgs, "graph_tool")
  }
  pkgs
}

.drugsignet_require_graph_tool <- function() {
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

.drugsignet_write_reticulate_python <- function(python) {
  renviron <- path.expand("~/.Renviron")
  old <- if (file.exists(renviron)) readLines(renviron, warn = FALSE) else character()
  old <- old[!grepl("^RETICULATE_PYTHON=", old)]
  writeLines(c(old, paste0("RETICULATE_PYTHON=", python)), renviron)
}

.drugsignet_install_synapser <- function(quiet = FALSE) {
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
    build_vignettes = FALSE,
    quiet = quiet
  )
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
  # Network methods are core package functionality, so provision their Python
  # environment on the first interactive attach unless the user explicitly
  # opts out. Non-interactive checks/builds never start network downloads.
  opt <- getOption("DrugSigNet.auto_install_python", interactive())
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
