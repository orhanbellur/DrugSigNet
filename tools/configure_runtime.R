#!/usr/bin/env Rscript

# This script runs from configure, before DrugSigNet itself is installed. Load
# only the setup helpers needed to provision the external runtime.
source_dir <- Sys.getenv("DRUGSIGNET_SOURCE_DIR", unset = ".")
source(file.path(source_dir, "R", "python_setup.R"), local = globalenv())
source(file.path(source_dir, "R", "synapser_utils.R"), local = globalenv())

message("DrugSigNet: provisioning the conda Python runtime.")
setup_python_dependencies(
  envname = "r-drugsignet",
  method = "conda",
  include_graph_tool = !.drugsignet_is_windows(),
  install_synapser = FALSE,
  quiet = FALSE
)

if (!.drugsignet_synapser_available()) {
  message("DrugSigNet: installing Synapser support.")
  .drugsignet_install_synapser(quiet = FALSE)
}

message("DrugSigNet: external runtime setup is complete.")
