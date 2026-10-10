#!/usr/bin/env Rscript

# This script runs from configure, before DrugSigNet itself is installed. Load
# only the setup helpers needed to provision the external runtime.
source_dir <- Sys.getenv("DRUGSIGNET_SOURCE_DIR", unset = ".")
source(file.path(source_dir, "R", "python_setup.R"), local = globalenv())

include_graph_tool <- !.drugsignet_is_windows()
message(
  "DrugSigNet: provisioning the conda Python runtime",
  if (include_graph_tool) " with graph-tool." else " (graph-tool skipped on Windows)."
)
setup_python_dependencies(
  envname = "r-drugsignet",
  method = "conda",
  include_graph_tool = include_graph_tool,
  install_synapser = FALSE,
  write_renviron = TRUE,
  quiet = FALSE
)

message("DrugSigNet: external runtime setup is complete.")
