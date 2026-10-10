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
script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
manifest="${DRUGSIGNET_DEPENDENCY_MANIFEST:-${script_dir}/runtime-dependencies.tsv}"
environment_file=""
temporary_environment=""
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
  repository="${package_ref%%@*}"
  [[ "${repository}" == "${package_ref}" ]] && git_ref="main" || git_ref="${package_ref#*@}"
  [[ "${repository}" =~ ^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$ ]] || {
    echo "ERROR: unsupported GitHub package reference: ${package_ref}" >&2
    exit 1
  }
  temporary_manifest="$(mktemp)"
  manifest="${temporary_manifest}"
  curl -fsSL \
    "https://raw.githubusercontent.com/${repository}/${git_ref}/tools/runtime-dependencies.tsv" \
    -o "${manifest}"
fi

temporary_environment="$(mktemp)"
trap 'rm -f "${temporary_environment}" "${temporary_manifest}"' EXIT
environment_file="${temporary_environment}"
{
  printf '%s\n' 'name: drugsignet' 'channels:' '  - conda-forge' 'dependencies:'
  awk -F '\t' '$1 == "conda" { print "  - " $2 }' "${manifest}"
  if awk -F '\t' '$1 == "pip" { found=1 } END { exit !found }' "${manifest}"; then
    printf '%s\n' '  - pip:'
    awk -F '\t' '$1 == "pip" { print "      - " $2 }' "${manifest}"
  fi
} > "${environment_file}"

export MAMBA_ROOT_PREFIX="${root_prefix}"
echo "[DrugSigNet] Creating/updating the rootless runtime at ${prefix}."
"${micromamba}" create --yes --prefix "${prefix}" --file "${environment_file}"

echo "[DrugSigNet] Installing ${package_ref} with the environment's R."
DRUGSIGNET_PACKAGE_REF="${package_ref}" \
DRUGSIGNET_MAMBA_ENV="${prefix}" \
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

pak::pkg_install(
  package_ref,
  dependencies = NA,
  upgrade = FALSE,
  ask = FALSE
)

DrugSigNet::check_drugsignet_installation(
  check_python = TRUE,
  check_optional = FALSE,
  stop_on_error = TRUE
)

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
