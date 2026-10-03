#!/usr/bin/env Rscript

# This script runs from configure, before DrugSigNet itself is installed. Load
# only the setup helpers needed to provision the external runtime.
source_dir <- Sys.getenv("DRUGSIGNET_SOURCE_DIR", unset = ".")
source(file.path(source_dir, "R", "python_setup.R"), local = globalenv())

message("DrugSigNet: provisioning the conda Python runtime.")
setup_python_dependencies(
  envname = "r-drugsignet",
  method = "conda",
  include_graph_tool = !.drugsignet_is_windows(),
  install_synapser = FALSE,
  quiet = FALSE
)

installed <- utils::installed.packages()
synapser_installed <- "synapser" %in% rownames(installed) &&
  utils::compareVersion(installed["synapser", "Version"], "3.0.0") >= 0L
if (!synapser_installed) {
  # Staged vignette builds prepend a disposable temp_libpath. Install Synapser
  # into the first writable persistent library so it survives that build and is
  # available to the final DrugSigNet installation.
  libraries <- .libPaths()
  persistent <- libraries[!grepl("temp_libpath|/Rinst|/00LOCK", libraries)]
  writable <- persistent[file.access(persistent, 2L) == 0L]
  if (!length(writable)) {
    user_library <- Sys.getenv("R_LIBS_USER")
    if (!nzchar(user_library)) {
      user_library <- file.path(path.expand("~"), "R", "library")
    }
    dir.create(user_library, recursive = TRUE, showWarnings = FALSE)
    writable <- user_library
  }

  message("DrugSigNet: installing Synapser support into ", writable[[1L]], ".")
  .drugsignet_install_synapser(
    quiet = FALSE,
    verify = FALSE,
    lib = writable[[1L]]
  )
}

message("DrugSigNet: external runtime setup is complete.")
