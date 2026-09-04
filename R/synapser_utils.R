# Utilities for optional Synapse support.

.drugsignet_synapser_install_message <- function() {
  paste0(
    "Package 'synapser' is required for this Synapse-backed workflow.\n",
    "Install it with DrugSigNet's setup helper, then retry:\n",
    "  setup_synapser()\n\n",
    "Equivalent manual installation command:\n",
    "install.packages('synapser', repos = c(\n",
    "  synapse = 'https://ran.synapse.org',\n",
    "  CRAN = 'https://cloud.r-project.org'\n",
    "))"
  )
}

#' Install optional Synapse support
#'
#' @description
#' Installs and verifies the `synapser` R package used to download
#' DrugSigNet networks, reference databases, and annotation resources. The
#' package is installed from its official Sage Bionetworks GitHub repository
#' when the legacy Synapse R repository is unavailable.
#'
#' Dependency-aware installation with `remotes` or `devtools` normally installs
#' `synapser` automatically. This helper is a repair path for installations that
#' deliberately skipped dependencies.
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

.drugsignet_synapser_function <- function(name) {
  getExportedValue(.drugsignet_synapser_package(), name)
}
