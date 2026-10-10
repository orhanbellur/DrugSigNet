#!/usr/bin/env Rscript

# Local DrugSigNet installer that mirrors the Docker Python setup.
# Run from the repository root with:
#   Rscript tools/install_local_drugsignet.R
# Optional environment variables:
#   DRUGSIGNET_CONDA_ENV=pyenv
#   DRUGSIGNET_INSTALL_GRAPH_TOOL=true
#   DRUGSIGNET_INSTALL_SYNAPSER=true
#   DRUGSIGNET_WRITE_RENVIRON=true

truthy <- function(x) {
  tolower(x %||% "") %in% c("1", "true", "yes", "y")
}

`%||%` <- function(x, y) {
  if (is.null(x) || !nzchar(x)) y else x
}

pkg_dir <- normalizePath(Sys.getenv("DRUGSIGNET_PKG_DIR", "."), mustWork = TRUE)
envname <- Sys.getenv("DRUGSIGNET_CONDA_ENV", "pyenv")
install_graph_tool <- truthy(Sys.getenv("DRUGSIGNET_INSTALL_GRAPH_TOOL", "true"))
install_synapser <- truthy(Sys.getenv("DRUGSIGNET_INSTALL_SYNAPSER", "false"))
write_renviron <- truthy(Sys.getenv("DRUGSIGNET_WRITE_RENVIRON", "false"))

repos <- c(
  synapse = "http://ran.synapse.org",
  CRAN = "https://cloud.r-project.org"
)
options(repos = repos, timeout = max(1000, getOption("timeout", 60)))

install_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing)) {
    install.packages(missing, repos = repos)
  }
}

install_if_missing(c("BiocManager", "remotes", "reticulate"))

# Use the same rootless micromamba bootstrap as configure/install_github/pak.
source(file.path(pkg_dir, "R", "python_setup.R"), local = globalenv())
runtime <- setup_python_dependencies(
  envname = envname,
  method = "conda",
  include_graph_tool = install_graph_tool,
  write_renviron = write_renviron,
  quiet = FALSE
)
python <- runtime$selected_python
message("RETICULATE_PYTHON=", python)

# Install all package dependencies declared in DESCRIPTION. The explicit block
# below remains useful when this script is run with a restricted dependency set.
message("Installing DrugSigNet R dependencies from DESCRIPTION.")
remotes::install_deps(pkg_dir, dependencies = TRUE, upgrade = "never", repos = repos)

if (install_synapser) {
  message("Installing current synapser from the official Sage repository.")
  remotes::install_github(
    "Sage-Bionetworks/synapser",
    dependencies = TRUE,
    upgrade = "never",
    build_vignettes = FALSE
  )
}

message("Installing DrugSigNet from ", pkg_dir)
remotes::install_local(
  pkg_dir,
  dependencies = FALSE,
  upgrade = "never",
  build_vignettes = FALSE,
  force = TRUE
)

message("Done. Restart R, then set RETICULATE_PYTHON before Synapse/Python workflows if needed:")
message("  Sys.setenv(RETICULATE_PYTHON = '", python, "')")
