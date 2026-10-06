#!/usr/bin/env bash

# One-step DrugSigNet installer for Debian/Ubuntu and Rocker images. This is an
# installation entry point (rather than an R configure hook) so native packages
# are present before R attempts to resolve and compile DrugSigNet dependencies.
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
package_dir="$(cd -- "${script_dir}/.." && pwd)"

if [[ ! -f "${package_dir}/DESCRIPTION" ]]; then
  echo "ERROR: DrugSigNet DESCRIPTION was not found in ${package_dir}." >&2
  exit 1
fi

if ! command -v Rscript >/dev/null 2>&1; then
  echo "ERROR: Rscript is required before DrugSigNet can be installed." >&2
  exit 1
fi

if [[ "$(id -u)" -ne 0 ]]; then
  echo "ERROR: complete Linux installation requires root so pak can install system requirements." >&2
  exit 1
fi

# Reject unsupported R releases before apt or R dependency installation starts.
# Without this guard, an old Bioconductor release can spend a long time building
# a dependency tree that cannot satisfy the current DrugSigNet installation.
Rscript --vanilla - <<'RSCRIPT'
required <- numeric_version("4.5.0")
current <- getRversion()
if (current < required) {
  stop(
    "DrugSigNet requires R >= ", required, "; this image provides R ", current,
    ". Use rocker/rstudio:4.5.2 or the maintained DrugSigNet image.",
    call. = FALSE
  )
}
RSCRIPT

export DRUGSIGNET_PACKAGE_DIR="${package_dir}"
export DRUGSIGNET_INSTALL_SUGGESTS="${DRUGSIGNET_INSTALL_SUGGESTS:-false}"
export RETICULATE_MINICONDA_PATH="${RETICULATE_MINICONDA_PATH:-/opt/drugsignet-miniconda}"

Rscript --vanilla - <<'RSCRIPT'
package_dir <- Sys.getenv("DRUGSIGNET_PACKAGE_DIR")
install_suggests <- tolower(Sys.getenv("DRUGSIGNET_INSTALL_SUGGESTS")) %in%
  c("1", "true", "yes")

cran <- "https://cloud.r-project.org"
options(
  repos = c(CRAN = cran),
  timeout = max(1200, getOption("timeout", 60)),
  pkg.sysreqs = TRUE
)
Sys.setenv(PKG_SYSREQS = "true")

if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak", repos = cran)
}

old_dir <- setwd(package_dir)
on.exit(setwd(old_dir), add = TRUE)
pak::pkg_install(
  "local::.",
  dependencies = if (install_suggests) TRUE else NA,
  upgrade = FALSE,
  ask = FALSE
)

if (!requireNamespace("signatureSearch", quietly = TRUE)) {
  stop(
    "DrugSigNet dependency 'signatureSearch' was not installed. Inspect the ",
    "first Bioconductor download or build error above.",
    call. = FALSE
  )
}

DrugSigNet::check_drugsignet_installation(
  check_python = TRUE,
  check_optional = install_suggests,
  stop_on_error = TRUE
)
RSCRIPT

python="${RETICULATE_MINICONDA_PATH}/envs/r-drugsignet/bin/python"
if [[ ! -x "${python}" ]]; then
  echo "ERROR: the provisioned DrugSigNet Python was not found at ${python}." >&2
  exit 1
fi

# The package is installed by root in a Docker build but used by the rstudio
# account. Persist the shared interpreter selection and make it readable and
# executable without allowing non-root users to mutate the environment.
renviron_site="$(R RHOME)/etc/Renviron.site"
touch "${renviron_site}"
sed -i \
  -e '/^RETICULATE_MINICONDA_PATH=/d' \
  -e '/^RETICULATE_PYTHON=/d' \
  "${renviron_site}"
printf '%s\n' \
  "RETICULATE_MINICONDA_PATH=${RETICULATE_MINICONDA_PATH}" \
  "RETICULATE_PYTHON=${python}" \
  >> "${renviron_site}"
chmod -R a+rX "${RETICULATE_MINICONDA_PATH}"

echo "DrugSigNet and all requested dependencies were installed successfully."
