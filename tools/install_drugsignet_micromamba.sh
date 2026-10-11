#!/usr/bin/env bash

# Build a completely user-owned DrugSigNet runtime. Unlike the ordinary Rocker
# install, R, compilers, native libraries, Java, Python, and graph-tool all come
# from one activated conda-forge environment, so no apt or sudo is required.
set -euo pipefail

package_ref="${1:-orhanbellur/DrugSigNet}"
prefix="${DRUGSIGNET_MAMBA_ENV:-${HOME}/.local/share/drugsignet-micromamba}"
root_prefix="${MAMBA_ROOT_PREFIX:-${HOME}/.local/share/mamba}"
bin_dir="${HOME}/.local/bin"
micromamba="${bin_dir}/micromamba"
script_path="${BASH_SOURCE[0]:-}"
script_dir="$(cd -- "$(dirname -- "${script_path}")" && pwd)"
manifest="${DRUGSIGNET_DEPENDENCY_MANIFEST:-${script_dir}/runtime-dependencies.tsv}"
manifest_url="${DRUGSIGNET_DEPENDENCY_MANIFEST_URL:-}"
environment_file=""
temporary_environment_dir=""
temporary_manifest=""

mkdir -p "${bin_dir}" "${root_prefix}"

if [[ ! -x "${micromamba}" ]]; then
  command -v curl >/dev/null 2>&1 || {
    echo "ERROR: curl is required to bootstrap micromamba." >&2
    exit 1
  }
  echo "[DrugSigNet] Installing micromamba in ${bin_dir}."
  curl -Ls https://micro.mamba.pm/api/micromamba/linux-64/latest \
    | tar -xj -C "${bin_dir}" --strip-components=1 bin/micromamba
  chmod 0755 "${micromamba}"
fi

if [[ ! -f "${manifest}" ]]; then
  if [[ -z "${manifest_url}" ]]; then
    repository="${package_ref%%@*}"
    [[ "${repository}" == "${package_ref}" ]] && git_ref="main" || git_ref="${package_ref#*@}"
    [[ "${repository}" =~ ^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$ ]] || {
      echo "ERROR: unsupported GitHub package reference: ${package_ref}" >&2
      exit 1
    }
    manifest_url="https://raw.githubusercontent.com/${repository}/${git_ref}/tools/runtime-dependencies.tsv"
  fi
  temporary_manifest="$(mktemp)"
  manifest="${temporary_manifest}"
  curl --fail --location --silent --show-error --retry 3 \
    "${manifest_url}" -o "${manifest}"
fi

# Micromamba determines the --file format from its filename. A suffix-less
# mktemp file is interpreted as a plain MatchSpec list, causing YAML lines (and
# eventually an empty line) to fail with "Empty package name". Always provide
# an explicit .yml filename.
temporary_environment_dir="$(mktemp -d)"
cleanup() {
  rm -rf "${temporary_environment_dir}"
  if [[ -n "${temporary_manifest}" ]]; then rm -f "${temporary_manifest}"; fi
}
trap cleanup EXIT
environment_file="${temporary_environment_dir}/environment.yml"
{
  printf '%s\n' 'name: drugsignet' 'channels:' '  - conda-forge' 'dependencies:'
  awk -F '\t' '$1 == "conda" { print "  - " $2 }' "${manifest}"
} > "${environment_file}"

export MAMBA_ROOT_PREFIX="${root_prefix}"
if [[ -f "${prefix}/conda-meta/history" ]]; then
  echo "[DrugSigNet] Updating the existing rootless runtime at ${prefix}."
  mapfile -t conda_packages < <(awk -F '\t' '$1 == "conda" { print $2 }' "${manifest}")
  "${micromamba}" install --yes --prefix "${prefix}" "${conda_packages[@]}"
else
  echo "[DrugSigNet] Creating the rootless runtime at ${prefix}."
  "${micromamba}" create --yes --prefix "${prefix}" --file "${environment_file}"
fi

for executable in R Rscript python xml2-config pkg-config; do
  [[ -x "${prefix}/bin/${executable}" ]] || {
    echo "ERROR: expected runtime executable is missing: ${prefix}/bin/${executable}" >&2
    exit 1
  }
done

# Keep pip outside environment.yml. Micromamba executes an environment file's
# pip section on every update, even when every wheel is already installed. That
# wastes space and can exhaust small home-directory quotas on repeat runs.
mapfile -t pip_packages < <(awk -F '\t' '$1 == "pip" { print $2 }' "${manifest}")
missing_pip_packages=()
for specification in "${pip_packages[@]}"; do
  distribution="${specification%%==*}"
  required_version=""
  if [[ "${specification}" == *"=="* ]]; then required_version="${specification#*==}"; fi
  installed_version="$("${prefix}/bin/python" - "${distribution}" <<'PY'
import importlib.metadata
import sys
try:
    print(importlib.metadata.version(sys.argv[1]))
except importlib.metadata.PackageNotFoundError:
    pass
PY
)"
  if [[ -z "${installed_version}" ]] || \
     { [[ -n "${required_version}" ]] && [[ "${installed_version}" != "${required_version}" ]]; }; then
    missing_pip_packages+=("${specification}")
  fi
done
if ((${#missing_pip_packages[@]})); then
  echo "[DrugSigNet] Installing missing pip packages: ${missing_pip_packages[*]}"
  "${prefix}/bin/python" -m pip install --no-cache-dir "${missing_pip_packages[@]}"
else
  echo "[DrugSigNet] Required pip packages are already installed."
fi

echo "[DrugSigNet] Installing ${package_ref} with the environment's R."
DRUGSIGNET_PACKAGE_REF="${package_ref}" \
DRUGSIGNET_MAMBA_ENV="${prefix}" \
RETICULATE_PYTHON="${prefix}/bin/python" \
XML_CONFIG="${prefix}/bin/xml2-config" \
PKG_CONFIG_PATH="${prefix}/lib/pkgconfig:${prefix}/share/pkgconfig" \
env -u R_HOME -u R_LIBS -u R_LIBS_USER -u R_LIBS_SITE \
  "${micromamba}" run --prefix "${prefix}" Rscript --vanilla - <<'RSCRIPT'
package_ref <- Sys.getenv("DRUGSIGNET_PACKAGE_REF")
prefix <- Sys.getenv("DRUGSIGNET_MAMBA_ENV")
python <- file.path(prefix, "bin", "python")
Sys.setenv(
  RETICULATE_PYTHON = python,
  DRUGSIGNET_SKIP_RUNTIME_SETUP = "true",
  PKG_SYSREQS = "false",
  PKG_BUILD_VIGNETTES = "true"
)
options(
  timeout = max(1200, getOption("timeout", 60)),
  pkg.sysreqs = FALSE,
  pkg.build_vignettes = TRUE
)

expected_library <- normalizePath(file.path(prefix, "lib", "R", "library"), mustWork = FALSE)
active_libraries <- normalizePath(.libPaths(), mustWork = FALSE)
if (!expected_library %in% active_libraries) {
  stop(
    "Micromamba R is not using its own package library. Expected: ",
    expected_library, "; active: ", paste(active_libraries, collapse = ", ")
  )
}
if (!startsWith(normalizePath(R.home(), mustWork = FALSE), normalizePath(prefix))) {
  stop("The installer did not start the Micromamba R runtime: ", R.home())
}

pak::pkg_install(
  package_ref,
  lib = expected_library,
  dependencies = NA,
  upgrade = FALSE,
  ask = FALSE
)

installation_check <- DrugSigNet::check_drugsignet_installation(
  check_python = TRUE,
  check_optional = FALSE,
  stop_on_error = FALSE
)
if (!isTRUE(installation_check$ok)) {
  print(installation_check$packages[!installation_check$packages$available, , drop = FALSE])
  print(installation_check$functions[!installation_check$functions$available, , drop = FALSE])
  print(installation_check$python[!installation_check$python$available, , drop = FALSE])
  stop("DrugSigNet runtime validation failed; missing components are printed above.")
}

renviron <- file.path(prefix, "lib", "R", "etc", "Renviron.site")
lines <- if (file.exists(renviron)) readLines(renviron, warn = FALSE) else character()
lines <- lines[!grepl("^RETICULATE_PYTHON=", lines)]
writeLines(c(lines, paste0("RETICULATE_PYTHON=", python)), renviron)
RSCRIPT

launcher="${bin_dir}/drugsignet-R"
cat > "${launcher}" <<EOF
#!/usr/bin/env bash
export MAMBA_ROOT_PREFIX="${root_prefix}"
exec "${micromamba}" run --prefix "${prefix}" R "\$@"
EOF
chmod 0755 "${launcher}"

cat <<EOF
[DrugSigNet] Rootless runtime installed successfully.
R executable: ${prefix}/bin/R
Python executable: ${prefix}/bin/python
Terminal launcher: ${launcher}

Use this R runtime (not /usr/local/bin/R):
  ${launcher}
EOF
