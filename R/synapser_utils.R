# Utilities for optional Synapse support.

.drugsignet_synapser_install_message <- function() {
  paste0(
    "Package 'synapser' is required for this Synapse-backed workflow.\n",
    "Install it with DrugSigNet's setup helper, then retry:\n",
    "  setup_synapser()\n",
    "The helper installs the maintained synapser 3.x release from Sage's official GitHub repository."
  )
}

#' Install optional Synapse support
#'
#' @description
#' Installs and verifies the `synapser` R package used to download
#' DrugSigNet networks, reference databases, and annotation resources. The
#' package is installed from Sage Bionetworks' official GitHub repository. This
#' deliberately avoids the obsolete Synapse RAN release and its incompatible
#' Python bootstrap.
#'
#' DrugSigNet normally calls this helper automatically when a Synapse-backed
#' function is first used. Call it directly to install and validate Synapse
#' support in advance. The helper selects DrugSigNet's conda Python before
#' loading Synapser so reticulate does not create a separate `uv` environment.
#'
#' @param quiet Logical; if `FALSE`, show package installation progress.
#'
#' @return Invisibly returns `TRUE` after `synapser` has been installed and its
#'   namespace can be loaded.
#'
#' @examples
#' \dontrun{
#' setup_synapser()
#' }
#'
#' @export
setup_synapser <- function(quiet = FALSE) {
  # Select DrugSigNet's Python before loading synapser. synapser declares its
  # Python requirements with py_require(), which otherwise lets reticulate
  # initialize a separate uv interpreter without graph_tool.
  .drugsignet_prepare_synapser_python(quiet = quiet)

  if (.drugsignet_synapser_available()) {
    if (!isTRUE(quiet)) {
      message("Package 'synapser' is already installed and loadable.")
    }
    return(invisible(TRUE))
  }

  .drugsignet_install_synapser(quiet = quiet)
  invisible(TRUE)
}

.drugsignet_synapser_package <- function() {
  "synapser"
}

.drugsignet_synapser_available <- function() {
  package <- .drugsignet_synapser_package()
  requireNamespace(package, quietly = TRUE) &&
    utils::packageVersion(package) >= "3.0.0"
}

.drugsignet_require_synapser <- function(purpose) {
  .drugsignet_prepare_synapser_python(quiet = TRUE)

  if (!.drugsignet_synapser_available() && .drugsignet_auto_install_synapser_enabled()) {
    message("Installing Synapse support before attempting to ", purpose, ".")
    setup_synapser()
  }

  load_result <- tryCatch(
    {
      available <- .drugsignet_synapser_available()
      if (!isTRUE(available)) {
        stop("there is no package called 'synapser'", call. = FALSE)
      }
      TRUE
    },
    error = function(e) e
  )

  if (!isTRUE(load_result)) {
    stop(
      "Package 'synapser' is required to ", purpose, ".\n",
      "Load error: ", conditionMessage(load_result), "\n",
      .drugsignet_synapser_install_message(),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.drugsignet_auto_install_synapser_enabled <- function() {
  opt <- getOption("DrugSigNet.auto_install_synapser", TRUE)
  env <- Sys.getenv("DRUGSIGNET_AUTO_INSTALL_SYNAPSER", "")
  if (nzchar(env)) {
    return(tolower(env) %in% c("1", "true", "yes", "y"))
  }
  isTRUE(opt)
}

.drugsignet_synapser_function <- function(name) {
  getExportedValue(.drugsignet_synapser_package(), name)
}

.drugsignet_prepare_synapser_python <- function(quiet = TRUE) {
  tryCatch(
    setup_python_dependencies(
      include_graph_tool = !.drugsignet_is_windows(),
      quiet = quiet
    ),
    error = function(e) {
      stop(
        "Unable to select DrugSigNet's Python environment before loading synapser.\n",
        conditionMessage(e),
        "\nRestart R, load DrugSigNet first, and retry the Synapse operation.",
        call. = FALSE
      )
    }
  )
  invisible(TRUE)
}

.drugsignet_syn_get <- function(entity, ...) {
  syn_get <- .drugsignet_synapser_function("synGet")
  requested <- list(...)
  supported <- names(formals(syn_get))

  # synapser 3.0.0's generated synGet wrapper accepts only `entity`, whereas
  # 2.x exposes downloadFile/downloadLocation/ifcollision. Pass optional
  # arguments only when the installed wrapper supports them. The 3.x default
  # downloads files to Synapse's cache; callers subsequently copy that path to
  # DrugSigNet's requested cache location.
  if (!is.null(supported) && !"..." %in% supported) {
    requested <- requested[names(requested) %in% supported]
  }
  do.call(syn_get, c(list(entity), requested))
}
