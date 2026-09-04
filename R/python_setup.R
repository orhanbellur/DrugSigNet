#' Install Python dependencies used by DrugSigNet
#'
#' @description
#' Installs required Python modules for DrugSigNet using `reticulate`.
#'
#' By default, this installs core Python modules plus `kaleido`, which Plotly
#' uses for static sunburst/image export. `graph_tool` is not installed
#' automatically because it is not available from PyPI and usually needs conda
#' or a system package manager.
#'
#' @param envname Name of the Python environment to install into.
#' @param method Installation backend passed to [reticulate::py_install()].
#'   One of `"auto"`, `"virtualenv"`, or `"conda"`.
#' @param include_graph_tool Logical; whether to attempt installing `graph_tool`.
#'   If `NULL` (default), DrugSigNet treats it as `FALSE`. Install `graph_tool`
#'   separately with conda or a system package manager when needed. If `TRUE`
#'   and `method = "conda"`, DrugSigNet installs the conda package
#'   `graph-tool` from the `conda-forge` channel.
#' @param force Logical; if `TRUE`, reinstall requested modules even if already available.
#' @param install_synapser Logical; if `TRUE`, install optional Synapse RAN support
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
    method = c("auto", "virtualenv", "conda"),
    include_graph_tool = NULL,
    force = FALSE,
    install_synapser = FALSE,
    write_renviron = FALSE,
    quiet = FALSE) {
  method <- match.arg(method)

  is_windows <- .drugsignet_is_windows()
  if (is.null(include_graph_tool)) {
    include_graph_tool <- FALSE
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

  if (is_windows && !quiet && graph_tool_requested) {
    warning("graph_tool is not supported on Windows and was skipped.", call. = FALSE)
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

.drugsignet_python_packages <- function(include_graph_tool = FALSE) {
  pkgs <- c("numpy", "pandas", "scipy", "networkx", "joblib", "tqdm", "openpyxl", "jinja2", "markupsafe", "kaleido")
  if (isTRUE(include_graph_tool)) {
    pkgs <- c(pkgs, "graph_tool")
  }
  pkgs
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
  repos <- c(
    synapse = "https://ran.synapse.org",
    CRAN = "https://cloud.r-project.org"
  )

  old_repos <- getOption("repos")
  old_timeout <- getOption("timeout")
  on.exit(options(repos = old_repos, timeout = old_timeout), add = TRUE)
  if (is.null(old_timeout) || is.na(old_timeout)) old_timeout <- 60
  options(repos = repos, timeout = max(1000, old_timeout))
  tryCatch(
    {
      utils::install.packages("synapser", repos = repos, quiet = quiet)
    },
    error = function(e) {
      if (!quiet) {
        message("Synapse R repository installation failed: ", conditionMessage(e))
      }
    }
  )

  if (!.drugsignet_synapser_available()) {
    if (!requireNamespace("remotes", quietly = TRUE)) {
      utils::install.packages("remotes", repos = repos[["CRAN"]], quiet = quiet)
    }
    if (!quiet) {
      message(
        "Installing 'synapser' from its official Sage Bionetworks GitHub repository."
      )
    }
    remotes::install_github(
      "Sage-Bionetworks/synapser",
      dependencies = FALSE,
      upgrade = "never",
      build_vignettes = FALSE,
      quiet = quiet
    )
  }
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

.drugsignet_is_windows <- function() {
  tolower(Sys.info()[["sysname"]]) == "windows"
}

.drugsignet_auto_install_enabled <- function() {
  # Disabled by default to avoid surprising downloads or virtualenv creation
  # during library(DrugSigNet). Users can opt in with an option/env var.
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
