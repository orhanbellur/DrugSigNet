#!/usr/bin/env bash
set -euo pipefail

package_ref="${1:-orhanbellur/DrugSigNet}"
project_dir="${DRUGSIGNET_PIXI_PROJECT:-${HOME}/DrugSigNet-test}"
script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]:-}")" && pwd)"
manifest="${DRUGSIGNET_DEPENDENCY_MANIFEST:-${script_dir}/runtime-dependencies.tsv}"
validation_script="${DRUGSIGNET_VALIDATION_SCRIPT:-${script_dir}/test_installation.R}"
pixi_home="${PIXI_HOME:-${HOME}/.pixi}"
pixi="${pixi_home}/bin/pixi"

mkdir -p "${project_dir}" "${pixi_home}"
if [[ ! -x "${pixi}" ]]; then
  echo "[DrugSigNet] Installing Pixi in ${pixi_home}."
  if [[ -n "${DRUGSIGNET_PIXI_BOOTSTRAP:-}" ]]; then
    PIXI_HOME="${pixi_home}" PIXI_NO_PATH_UPDATE=1 \
      bash "${DRUGSIGNET_PIXI_BOOTSTRAP}"
  elif command -v curl >/dev/null 2>&1; then
    curl -fsSL --retry 3 https://pixi.sh/install.sh |
      PIXI_HOME="${pixi_home}" PIXI_NO_PATH_UPDATE=1 bash
  elif command -v wget >/dev/null 2>&1; then
    wget -qO- https://pixi.sh/install.sh |
      PIXI_HOME="${pixi_home}" PIXI_NO_PATH_UPDATE=1 bash
  else
    echo "ERROR: install curl or wget, or provide DRUGSIGNET_PIXI_BOOTSTRAP." >&2
    exit 127
  fi
fi

[[ -f "${manifest}" ]] || { echo "Missing dependency manifest: ${manifest}" >&2; exit 1; }
[[ -f "${validation_script}" ]] || { echo "Missing validation script: ${validation_script}" >&2; exit 1; }

# Lock only the platform that will actually run this project. A hard-coded
# linux-64 workspace cannot be installed on Apple Silicon, while declaring all
# platforms would make every local install solve unrelated target platforms.
if [[ -n "${DRUGSIGNET_PIXI_PLATFORM:-}" ]]; then
  pixi_platform="${DRUGSIGNET_PIXI_PLATFORM}"
else
  case "$(uname -s)-$(uname -m)" in
    Linux-x86_64|Linux-amd64) pixi_platform="linux-64" ;;
    Linux-aarch64|Linux-arm64) pixi_platform="linux-aarch64" ;;
    Darwin-arm64|Darwin-aarch64) pixi_platform="osx-arm64" ;;
    Darwin-x86_64|Darwin-amd64) pixi_platform="osx-64" ;;
    *)
      echo "ERROR: unsupported Pixi host: $(uname -s) $(uname -m)." >&2
      exit 2
      ;;
  esac
fi
case "${pixi_platform}" in
  linux-64|linux-aarch64|osx-64|osx-arm64) ;;
  *)
    echo "ERROR: unsupported DRUGSIGNET_PIXI_PLATFORM: ${pixi_platform}." >&2
    exit 2
    ;;
esac
echo "[DrugSigNet] Pixi target platform: ${pixi_platform}."

# R entry points select the caller's ABI series. Standalone shell/container
# installations retain the manifest default when no host R is specified.
r_series="${DRUGSIGNET_PIXI_R_VERSION:-$(awk -F '\t' '$1 == "conda" && $2 ~ /^r-base=/ {sub(/^r-base=/, "", $2); print $2; exit}' "${manifest}")}"
if [[ ! "${r_series}" =~ ^[0-9]+\.[0-9]+$ ]]; then
  echo "ERROR: DRUGSIGNET_PIXI_R_VERSION must be an R major.minor series." >&2
  exit 2
fi
echo "[DrugSigNet] Pixi R series: ${r_series} (matching the calling R when supplied)."


# Conda's macOS compilers need the host SDK headers (not supplied by Pixi).
# Persist the SDK in Pixi activation so --clean-env does not discard it.
macos_sdk=""
case "${pixi_platform}" in
  osx-*)
    macos_sdk="${DRUGSIGNET_MACOS_SDKROOT:-}"
    if [[ -z "${macos_sdk}" ]] && command -v xcrun >/dev/null 2>&1; then
      macos_sdk="$(xcrun --sdk macosx --show-sdk-path)"
    fi
    if [[ ! -d "${macos_sdk}/usr/include" ]]; then
      echo "ERROR: macOS SDK headers are unavailable. Install Xcode Command Line Tools with xcode-select --install, or set DRUGSIGNET_MACOS_SDKROOT to a valid SDK." >&2
      exit 1
    fi
    ;;
esac

# Recent Boost.Math headers require C++14, including packages that still
# request CXX11. Keep this compatibility setting inside the managed project.
if [[ "${r_series}" == "4.4" ]]; then
  mkdir -p "${project_dir}/.R"
  printf '%s\n' 'CXX11STD = -std=gnu++14' > "${project_dir}/.R/Makevars-r44"
fi

pixi_toml="${project_dir}/pixi.toml"
{
  printf '%s\n' \
    '[workspace]' \
    'name = "DrugSigNet-test"' \
    'channels = ["conda-forge", "bioconda"]' \
    "platforms = [\"${pixi_platform}\"]" \
    '' \
    '[dependencies]'
  awk -F '\t' -v platform="${pixi_platform}" -v r_series="${r_series}" '$1 == "conda" || $1 == "pixi" || ($1 == "pixi-osx" && platform ~ /^osx-/) {
    # Bioconda does not provide these packages for Apple Silicon. Install
    # them with BiocManager inside Pixi R after the native runtime exists.
    if (platform == "osx-arm64" && ($2 ~ /^bioconductor-/ || $2 == "r-rcdk")) next
    if (r_series == "4.4" && $2 == "r-boutroslabplottinggeneral") next
    spec=$2; name=spec; version="*";
    if (index(spec, "=") > 0) {
      split(spec, parts, "="); name=parts[1]; version=parts[2] ".*"
    }
    if (name == "r-base") version = r_series ".*"
    if (r_series == "4.4" && name == "r-ggplot2") version = "<4"
    printf "\"%s\" = \"%s\"\n", name, version
  }' "${manifest}"
  printf '%s\n' '' '[pypi-dependencies]'
  awk -F '\t' '$1 == "pip" {
    spec=$2; name=spec; version="*";
    if (index(spec, "==") > 0) {
      split(spec, parts, "=="); name=parts[1]; version="==" parts[2]
    }
    printf "\"%s\" = \"%s\"\n", name, version
  }' "${manifest}"
  # rJava configure prefers JAVA_HOME from the process environment over
  # R's javareconf settings. Always bind it to the project OpenJDK.
  printf '\n[activation.env]\nJAVA_HOME = "%s/.pixi/envs/default/lib/jvm"\n' "${project_dir}"
  if [[ "${r_series}" == "4.4" ]]; then
    printf 'R_MAKEVARS_USER = "%s/.R/Makevars-r44"\n' "${project_dir}"
  fi
  if [[ -n "${macos_sdk}" ]]; then
    printf 'SDKROOT = "%s"\nCONDA_BUILD_SYSROOT = "%s"\n' "${macos_sdk}" "${macos_sdk}"
  fi
} > "${pixi_toml}"

# Keep Synapse wholly on the Conda/Bioconda side of Pixi's solver. Mixing the
# older pinned PyPI client into this environment lets Conda pin newer httpx and
# importlib-metadata builds, making the PyPI requirements unsatisfiable. Fail
# before the expensive solve if a future manifest change reintroduces it.
synapse_conda_spec="$(awk '
  /^\[dependencies\]$/ { section = "conda"; next }
  /^\[pypi-dependencies\]$/ { section = "pypi"; next }
  section == "conda" && /^"synapseclient"[[:space:]]*=/ { print; exit }
' "${pixi_toml}")"
synapse_pypi_spec="$(awk '
  /^\[pypi-dependencies\]$/ { section = "pypi"; next }
  section == "pypi" && /^"synapseclient"[[:space:]]*=/ { print; exit }
' "${pixi_toml}")"
if [[ -z "${synapse_conda_spec}" || -n "${synapse_pypi_spec}" ]]; then
  echo "ERROR: Pixi must resolve synapseclient from Bioconda only; refusing a mixed Conda/PyPI solve." >&2
  exit 1
fi
echo "[DrugSigNet] Pixi dependency plan uses Bioconda synapseclient (no PyPI Synapse dependency)."

cp "${validation_script}" "${project_dir}/test_installation.R"
cat > "${project_dir}/DrugSigNet-test.Rproj" <<'EOF'
Version: 1.0
RestoreWorkspace: No
SaveWorkspace: No
AlwaysSaveHistory: No
EOF
cat > "${project_dir}/.Renviron" <<EOF
RETICULATE_PYTHON=${project_dir}/.pixi/envs/default/bin/python
RETICULATE_CONDA=${project_dir}/.pixi/envs/default/bin/conda
CONDA_PREFIX=${project_dir}/.pixi/envs/default
DRUGSIGNET_PIXI_PROJECT=${project_dir}
EOF
cat > "${project_dir}/.Rprofile" <<EOF
local({
  pixi_library <- "${project_dir}/.pixi/envs/default/lib/R/library"
  version_file <- "${project_dir}/.pixi/envs/default/.drugsignet-r-version"
  if (dir.exists(pixi_library) && file.exists(version_file)) {
    runtime_version <- readLines(version_file, warn = FALSE, n = 1L)
    host_minor <- strsplit(as.character(getRversion()), ".", fixed = TRUE)[[1L]][1:2]
    runtime_minor <- if (length(runtime_version)) strsplit(runtime_version, ".", fixed = TRUE)[[1L]][1:2] else character()
    if (identical(host_minor, runtime_minor)) {
      .libPaths(unique(c(pixi_library, .libPaths())))
    }
  }
})
EOF

echo "[DrugSigNet] Resolving and installing the Pixi runtime."
post_link_marker="${project_dir}/.pixi/.drugsignet-post-link-enabled"
runtime_prefix="${project_dir}/.pixi/envs/default"
previous_r=""
if [[ -f "${runtime_prefix}/.drugsignet-r-version" ]]; then
  previous_r="$(awk -F . 'NR == 1 { print $1 "." $2 }' "${runtime_prefix}/.drugsignet-r-version")"
fi
if [[ -d "${runtime_prefix}" && ( ! -f "${post_link_marker}" || "${previous_r}" != "${r_series}" ) ]]; then
  echo "[DrugSigNet] Recreating the project runtime for the requested R series and required post-link scripts."
  rm -rf "${runtime_prefix}"
fi
(
  cd "${project_dir}"
  "${pixi}" config set --local run-post-link-scripts insecure
)
"${pixi}" install --run-post-link-scripts --manifest-path "${pixi_toml}"
[[ -x "${runtime_prefix}/bin/conda" ]] || {
  echo "ERROR: Pixi runtime did not provide the Conda compatibility executable required by reticulate." >&2
  exit 1
}
mkdir -p "$(dirname "${post_link_marker}")"
touch "${post_link_marker}"

# The conda-forge Bash package provides bin/bash but, unlike a normal Linux
# root filesystem, a Pixi prefix may not contain the conventional bin/sh link.
# R's `R CMD ...` launcher explicitly executes `sh`, so create the project-local
# link without touching the host or requiring sudo.
runtime_bin="${project_dir}/.pixi/envs/default/bin"
if [[ ! -x "${runtime_bin}/sh" ]]; then
  if [[ ! -x "${runtime_bin}/bash" ]]; then
    echo "ERROR: Pixi installed neither sh nor bash in ${runtime_bin}." >&2
    exit 127
  fi
  ln -s bash "${runtime_bin}/sh"
  echo "[DrugSigNet] Added the missing project-local sh -> bash runtime link."
fi
# Some R configure scripts call the macOS arch utility. Clean Pixi PATHs
# need an explicit project-local bridge to this standard host executable.
if [[ "${pixi_platform}" == osx-* && ! -x "${runtime_bin}/arch" ]]; then
  [[ -x /usr/bin/arch ]] || { echo "ERROR: macOS arch utility is missing." >&2; exit 1; }
  ln -s /usr/bin/arch "${runtime_bin}/arch"
fi

repository="${package_ref%%@*}"
ref=""
if [[ "${package_ref}" == *"@"* ]]; then
  ref="${package_ref#*@}"
fi
if [[ ! "${repository}" =~ ^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$ ]]; then
  echo "ERROR: package reference must be owner/repository[@git-ref]." >&2
  exit 2
fi

install_work="$(mktemp -d)"
trap 'rm -rf "${install_work}"' EXIT
source_dir="${install_work}/DrugSigNet"
clone_args=(clone --progress --filter=blob:none --single-branch --depth 1)
if [[ -n "${ref}" ]]; then
  clone_args+=(--branch "${ref}")
fi
clone_args+=("https://github.com/${repository}.git" "${source_dir}")

pixi_clean_run() {
  env -u R_HOME -u R_LIBS -u R_LIBS_USER -u R_LIBS_SITE \
    -u JAVA_HOME -u R_JAVA_LD_LIBRARY_PATH \
    "${pixi}" run --clean-env --manifest-path "${pixi_toml}" "$@"
}

echo "[DrugSigNet] Downloading ${package_ref} with Git (pak metadata resolution is skipped)."
pixi_clean_run git "${clone_args[@]}"

# R 4.4/Bioconductor 3.20 need a coherent plotting generation, including
# serialized ggproto defaults. Rebuild extensions against the selected ggplot2.
if [[ "${r_series}" == "4.4" ]]; then
  pixi_clean_run Rscript --vanilla - <<'RSCRIPT'
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2025-04-30"))
install.packages(c("ggplot2", "ggforce", "ggrepel", "ggalluvial", "ggraph",
                   "BoutrosLab.plotting.general"), type = "source")
required <- c("ggplot2", "ggforce", "ggrepel", "ggalluvial", "ggraph", "BoutrosLab.plotting.general")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) || packageVersion("ggplot2") >= "4.0.0") {
  stop("The R 4.4 plotting stack could not be configured consistently.")
}
RSCRIPT
  pixi_clean_run Rscript --vanilla "${source_dir}/.github/scripts/check-r44-plotting.R"
fi

# Use the Bioconductor release associated with the selected Pixi R series.
# Run before building vignettes: they load signatureSearch as well.
if [[ "${pixi_platform}" == "osx-arm64" ]]; then
  echo "[DrugSigNet] Configuring Pixi Java and installing Apple Silicon R dependencies."
  [[ -x "${runtime_prefix}/lib/jvm/bin/java" ]] || {
    echo "ERROR: Pixi OpenJDK is missing from ${runtime_prefix}/lib/jvm." >&2
    exit 1
  }
  # Embed the JVM search path; macOS system shells can strip DYLD_* vars.
  pixi_clean_run R CMD javareconf JAVA_HOME="${runtime_prefix}/lib/jvm" \
    JAVA_LIBS="-L${runtime_prefix}/lib/jvm/lib/server -Wl,-rpath,${runtime_prefix}/lib/jvm/lib/server -ljvm"
  pixi_clean_run Rscript --vanilla - <<'RSCRIPT'
r_series <- paste(strsplit(as.character(getRversion()), ".", fixed = TRUE)[[1L]][1:2], collapse = ".")
cran <- if (r_series == "4.4") "https://packagemanager.posit.co/cran/2025-04-30" else "https://cloud.r-project.org"
options(repos = c(CRAN = cran))
expected_java <- normalizePath(file.path(R.home(), "..", "jvm"), mustWork = TRUE)
actual_java <- normalizePath(Sys.getenv("JAVA_HOME"), mustWork = TRUE)
if (!identical(actual_java, expected_java)) {
  stop("Pixi Java isolation failed: JAVA_HOME must point to ", expected_java)
}
message("Using project OpenJDK: ", actual_java)
bioc_version <- if (r_series == "4.4") "3.20" else as.character(BiocManager::version())
message("Using Pixi R ", getRversion(), " with Bioconductor ", bioc_version)
cran_required <- c("fingerprint", "rcdk", "gdtools")
cran_missing <- cran_required[!vapply(cran_required, requireNamespace, logical(1), quietly = TRUE)]
if (length(cran_missing)) install.packages(cran_missing)
cran_missing <- cran_required[!vapply(cran_required, requireNamespace, logical(1), quietly = TRUE)]
if (length(cran_missing)) {
  stop("Apple Silicon CRAN installation failed: ", paste(cran_missing, collapse = ", "))
}
# Exercise rJava/CDK as well as namespace loading before claiming success.
molecule <- rcdk::parse.smiles("CCO")[[1L]]
fingerprint <- rcdk::get.fingerprint(molecule, type = "extended")
if (!inherits(fingerprint, "fingerprint")) {
  stop("Apple Silicon CDK fingerprint validation failed.")
}
# BH 1.87 from the R 4.4 snapshot has an invalid interprocess template
# expression rejected by Clang 21. BH 1.90 contains the upstream correction.
if (r_series == "4.4" && (!requireNamespace("BH", quietly = TRUE) || packageVersion("BH") < "1.90.0")) {
  install.packages("BH", repos = "https://cloud.r-project.org", type = "source")
}
if (r_series == "4.4" && (!requireNamespace("BH", quietly = TRUE) || packageVersion("BH") < "1.90.0")) {
  stop("Apple Silicon R 4.4 requires BH >= 1.90 for the selected Clang compiler.")
}
required <- c("signatureSearch", "signatureSearchData")
BiocManager::install(version = bioc_version, ask = FALSE, update = FALSE)
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  BiocManager::install(missing, version = bioc_version, ask = FALSE, update = FALSE)
}
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Apple Silicon Bioconductor installation failed: ", paste(missing, collapse = ", "))
}
RSCRIPT
fi

install_source="${source_dir}"
if [[ "${DRUGSIGNET_BUILD_VIGNETTES:-false}" =~ ^(1|true|TRUE|yes|YES|on|ON)$ ]]; then
  echo "[DrugSigNet] Building the source package and HTML vignettes."
  (
    cd "${install_work}"
    pixi_clean_run R CMD build --no-manual "${source_dir}"
  )
  install_source="$(find "${install_work}" -maxdepth 1 -name 'DrugSigNet_*.tar.gz' -print -quit)"
  [[ -n "${install_source}" ]] || { echo "ERROR: R CMD build did not create a package archive." >&2; exit 1; }
else
  echo "[DrugSigNet] Installing DrugSigNet directly (vignette rebuild disabled)."
fi

pixi_clean_run R CMD INSTALL \
  --library="${project_dir}/.pixi/envs/default/lib/R/library" \
  "${install_source}"

echo "[DrugSigNet] Validating the installed package and Python runtime."
pixi_clean_run Rscript --vanilla "${project_dir}/test_installation.R"
touch "${project_dir}/.pixi/envs/default/.drugsignet-runtime-validated"

cat <<EOF
[DrugSigNet] Pixi project created successfully: ${project_dir}
Manifest: ${pixi_toml}
Lock file: ${project_dir}/pixi.lock
Environment: ${project_dir}/.pixi
RStudio project: ${project_dir}/DrugSigNet-test.Rproj
Start R with:
  ${pixi} run --manifest-path ${pixi_toml} R
EOF
