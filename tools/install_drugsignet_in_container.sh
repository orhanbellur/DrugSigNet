#!/usr/bin/env bash

# Install DrugSigNet into an already-running rocker/rstudio container that was
# started without ROOT=TRUE. Native packages are added through one explicit
# Docker administrative operation; R, conda, and DrugSigNet are installed as
# the ordinary rstudio user.
set -euo pipefail

container="${1:-rstudio453}"
package_ref="${2:-orhanbellur/DrugSigNet}"
script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
system_helper="${script_dir}/install_linux_system_dependencies.sh"
temporary_helper=""

if ! command -v docker >/dev/null 2>&1; then
  echo "ERROR: docker is required on the host." >&2
  exit 1
fi

if [[ "$(docker inspect --format '{{.State.Running}}' "${container}" 2>/dev/null || true)" != "true" ]]; then
  echo "ERROR: container '${container}' is not running." >&2
  exit 1
fi

# Allow this installer to be streamed directly from GitHub, so users do not
# need to clone the repository merely to repair a stock Rocker container.
if [[ ! -f "${system_helper}" ]]; then
  if ! command -v curl >/dev/null 2>&1; then
    echo "ERROR: curl is required when the repository is not checked out." >&2
    exit 1
  fi
  repository="${package_ref%%@*}"
  if [[ "${repository}" == "${package_ref}" ]]; then
    git_ref="main"
  else
    git_ref="${package_ref#*@}"
  fi
  if [[ ! "${repository}" =~ ^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$ ]]; then
    echo "ERROR: '${package_ref}' is not a supported GitHub package reference." >&2
    exit 1
  fi
  temporary_helper="$(mktemp)"
  trap 'rm -f "${temporary_helper}"' EXIT
  curl --fail --silent --show-error --location \
    "https://raw.githubusercontent.com/${repository}/${git_ref}/tools/install_linux_system_dependencies.sh" \
    --output "${temporary_helper}"
  system_helper="${temporary_helper}"
fi

echo "[DrugSigNet] Installing native dependencies in container ${container}."
docker cp "${system_helper}" \
  "${container}:/tmp/install_drugsignet_system_dependencies.sh"
docker exec --user rstudio "${container}" \
  bash /tmp/install_drugsignet_system_dependencies.sh --check || true
docker exec --user root "${container}" \
  bash /tmp/install_drugsignet_system_dependencies.sh

echo "[DrugSigNet] Installing ${package_ref} as the rstudio user."
docker exec --interactive \
  --user rstudio \
  --env HOME=/home/rstudio \
  --env DRUGSIGNET_SKIP_RUNTIME_SETUP=true \
  --workdir /home/rstudio \
  "${container}" Rscript --vanilla - "${package_ref}" <<'RSCRIPT'
args <- commandArgs(trailingOnly = TRUE)
package_ref <- args[[1L]]
user_lib <- path.expand(Sys.getenv(
  "R_LIBS_USER",
  unset = file.path("~", "R", paste0("library-", getRversion()))
))
dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(user_lib, .libPaths()))

options(
  timeout = max(1200, getOption("timeout", 60)),
  pkg.sysreqs = FALSE,
  pkg.build_vignettes = TRUE
)
Sys.setenv(
  PKG_SYSREQS = "false",
  PKG_BUILD_VIGNETTES = "true",
  DRUGSIGNET_SKIP_RUNTIME_SETUP = "true"
)

if (!requireNamespace("pak", quietly = TRUE)) {
  pak_repo <- sprintf(
    "https://r-lib.github.io/p/pak/stable/%s/%s/%s",
    .Platform$pkgType,
    R.Version()$os,
    R.Version()$arch
  )
  install.packages("pak", lib = user_lib, repos = pak_repo)
}

pak::pkg_install(
  package_ref,
  lib = user_lib,
  dependencies = NA,
  upgrade = FALSE,
  ask = FALSE
)

DrugSigNet::setup_python_dependencies(
  include_graph_tool = TRUE,
  write_renviron = TRUE,
  quiet = FALSE
)

DrugSigNet::check_drugsignet_installation(
  check_python = TRUE,
  check_optional = FALSE,
  stop_on_error = TRUE
)
RSCRIPT

echo "[DrugSigNet] Installation completed successfully in ${container}."
