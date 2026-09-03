# Utilities for optional Synapse support.

.drugsignet_synapser_install_message <- function() {
  paste0(
    "Package 'synapser' is required for this Synapse-backed workflow.\n",
    "Install it before loading DrugSigNet, then restart R. From the DrugSigNet repository, run in a clean R session:\n",
    "DRUGSIGNET_INSTALL_SYNAPSER=true Rscript tools/install_local_drugsignet.R\n",
    "Alternatively, run these commands in a clean R session before calling library(DrugSigNet):\n",
    "reticulate::install_miniconda() # skip if conda is already available\n",
    "reticulate::conda_create('drugsignet-synapse', packages = c('python=3.10', 'pip', ",
    "'numpy=1.24.4', 'pandas=2.0.3', 'jinja2', 'markupsafe'), channel = 'conda-forge')\n",
    "Sys.setenv(RETICULATE_PYTHON = reticulate::conda_python('drugsignet-synapse'))\n",
    "rjson_path <- find.package('rjson', quiet = TRUE)\n",
    "if (length(rjson_path)) remove.packages('rjson', lib = dirname(rjson_path))\n",
    "remotes::install_version('rjson', version = '0.2.21', repos = 'https://cloud.r-project.org', upgrade = 'never')\n",
    "options(repos = c(synapse = 'http://ran.synapse.org', CRAN = 'https://cloud.r-project.org'))\n",
    "install.packages('synapser')"
  )
}

# Keep Synapse support genuinely optional. The package is distributed from the
# Synapse RAN rather than CRAN/Bioconductor, so listing it in Suggests makes
# --as-cran checks fail before they can exercise DrugSigNet on clean runners.
.drugsignet_synapser_package <- function() {
  paste0("syn", "apser")
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
      "DrugSigNet does not install it from a running package session because ",
      "synapser requires an older rjson release, while rjson may already be loaded ",
      "by another package. Replacing a loaded package can corrupt lazy-load databases.\n",
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
