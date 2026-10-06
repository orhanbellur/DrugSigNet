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

"${script_dir}/install_linux_system_dependencies.sh"

export DRUGSIGNET_PACKAGE_DIR="${package_dir}"
export DRUGSIGNET_INSTALL_SUGGESTS="${DRUGSIGNET_INSTALL_SUGGESTS:-false}"
export RETICULATE_MINICONDA_PATH="${RETICULATE_MINICONDA_PATH:-/opt/drugsignet-miniconda}"

Rscript --vanilla - <<'RSCRIPT'
package_dir <- Sys.getenv("DRUGSIGNET_PACKAGE_DIR")
install_suggests <- tolower(Sys.getenv("DRUGSIGNET_INSTALL_SUGGESTS")) %in%
  c("1", "true", "yes")

cran <- "https://cloud.r-project.org"
options(repos = c(CRAN = cran), timeout = max(1000, getOption("timeout", 60)))

bootstrap <- c("BiocManager", "remotes")
missing <- bootstrap[!vapply(bootstrap, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  install.packages(missing, repos = cran)
}

options(repos = BiocManager::repositories())
dependencies <- if (install_suggests) TRUE else c("Depends", "Imports")
remotes::install_local(
  package_dir,
  dependencies = dependencies,
  upgrade = "never",
  build_vignettes = FALSE,
  force = TRUE
)

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
