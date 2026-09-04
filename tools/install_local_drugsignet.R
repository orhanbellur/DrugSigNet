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
  synapse = "https://ran.synapse.org",
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

conda <- reticulate::conda_binary()
if (is.null(conda)) {
  message("No conda binary found. Installing Miniconda with reticulate.")
  reticulate::install_miniconda()
  conda <- reticulate::conda_binary()
}
if (is.null(conda) || !nzchar(conda)) {
  stop("No conda or mamba executable is available after install_miniconda().", call. = FALSE)
}
message("Using conda executable: ", conda)

run_conda <- function(args) {
  status <- system2(conda, args = args)
  if (!identical(status, 0L)) {
    stop(
      "Conda command failed: ", conda, " ", paste(args, collapse = " "),
      call. = FALSE
    )
  }
}

conda_base <- system2(conda, args = c("info", "--base"), stdout = TRUE)
conda_base <- normalizePath(conda_base[[1]], mustWork = TRUE)
python <- file.path(
  conda_base,
  "envs",
  envname,
  if (.Platform$OS.type == "windows") "python.exe" else file.path("bin", "python")
)

if (!file.exists(python)) {
  message("Creating conda environment: ", envname)
  run_conda(c(
    "create", "--yes", "--name", envname,
    "-c", "conda-forge",
    "python=3.10",
    "pip",
    "numpy=1.24.4",
    "pandas=2.0.3",
    "scipy",
    "openpyxl",
    "tqdm",
    "networkx",
    "joblib",
    "jinja2",
    "markupsafe"
  ))
} else {
  message("Using existing conda environment: ", envname)
}

if (install_graph_tool) {
  message("Installing graph-tool into conda environment: ", envname)
  run_conda(c(
    "install", "--yes", "--name", envname,
    "-c", "conda-forge",
    "graph-tool"
  ))
}

if (!file.exists(python)) {
  stop(
    "Conda environment was requested but Python was not found at: ", python,
    call. = FALSE
  )
}
Sys.setenv(RETICULATE_PYTHON = python)
message("RETICULATE_PYTHON=", python)

message("Installing Python static export dependencies into conda environment: ", envname)
status <- system2(python, args = c("-m", "pip", "install", "--upgrade", "kaleido"))
if (!identical(status, 0L)) {
  stop("Failed to install Python package 'kaleido' into ", envname, call. = FALSE)
}

if (write_renviron) {
  renviron <- path.expand("~/.Renviron")
  line <- paste0("RETICULATE_PYTHON=", python)
  old <- if (file.exists(renviron)) readLines(renviron, warn = FALSE) else character()
  old <- old[!grepl("^RETICULATE_PYTHON=", old)]
  writeLines(c(old, line), renviron)
  message("Wrote RETICULATE_PYTHON to ", renviron)
}

# Install all package dependencies declared in DESCRIPTION. The explicit block
# below remains useful when this script is run with a restricted dependency set.
message("Installing DrugSigNet R dependencies from DESCRIPTION.")
remotes::install_deps(pkg_dir, dependencies = TRUE, upgrade = "never", repos = repos)

if (install_synapser) {
  message("Installing optional synapser support.")
  install.packages("synapser", repos = repos)
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
