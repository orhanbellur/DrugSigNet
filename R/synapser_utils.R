# Utilities for optional Synapse support.

.drugsignet_synapser_install_message <- function() {
  paste0(
    "Package 'synapser' is required for this Synapse-backed workflow.\n",
    "Install it from the Synapse R repository, then retry:\n",
    "install.packages('synapser', repos = c(\n",
    "  synapse = 'https://ran.synapse.org',\n",
    "  CRAN = 'https://cloud.r-project.org'\n",
    "))"
  )
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
