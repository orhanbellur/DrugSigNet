# Utilities for optional Synapse support.

.drugsignet_synapser_install_message <- function() {
  paste0(
    "Package 'synapser' is required for this Synapse-backed workflow.\n",
    "Install it with DrugSigNet's setup helper, then retry:\n",
    "  setup_synapser()\n",
    "The helper installs the compatible rjson version before synapser."
  )
}

#' Install optional Synapse support
#'
#' @description
#' Installs and verifies the `synapser` R package used to download
#' DrugSigNet networks, reference databases, and annotation resources. The
#' package is installed from the Synapse R repository after its compatible
#' `rjson` release is installed. A GitHub fallback is available when that
#' repository is temporarily unavailable.
#'
#' DrugSigNet normally calls this helper automatically when a Synapse-backed
#' function is first used. Call it directly to install and validate Synapse
#' support in advance.
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
  requireNamespace(.drugsignet_synapser_package(), quietly = TRUE)
}

.drugsignet_require_synapser <- function(purpose) {
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
