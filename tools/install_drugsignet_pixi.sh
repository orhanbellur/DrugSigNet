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

pixi_toml="${project_dir}/pixi.toml"
{
  printf '%s\n' \
    '[workspace]' \
    'name = "DrugSigNet-test"' \
    'channels = ["conda-forge", "bioconda"]' \
    'platforms = ["linux-64"]' \
    '' \
    '[dependencies]'
  awk -F '\t' '$1 == "conda" || $1 == "pixi" {
    spec=$2; name=spec; version="*";
    if (index(spec, "=") > 0) {
      split(spec, parts, "="); name=parts[1]; version=parts[2] ".*"
    }
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
} > "${pixi_toml}"
cp "${validation_script}" "${project_dir}/test_installation.R"
cat > "${project_dir}/DrugSigNet-test.Rproj" <<'EOF'
Version: 1.0
RestoreWorkspace: No
SaveWorkspace: No
AlwaysSaveHistory: No
EOF
cat > "${project_dir}/.Renviron" <<EOF
RETICULATE_PYTHON=${project_dir}/.pixi/envs/default/bin/python
DRUGSIGNET_PIXI_PROJECT=${project_dir}
EOF

echo "[DrugSigNet] Resolving and installing the Pixi runtime."
"${pixi}" install --manifest-path "${pixi_toml}"
echo "[DrugSigNet] Installing and validating ${package_ref}."
install_started_at=${SECONDS}
env -u R_HOME -u R_LIBS -u R_LIBS_USER -u R_LIBS_SITE \
  "${pixi}" run --clean-env --manifest-path "${pixi_toml}" \
    Rscript --vanilla "${project_dir}/test_installation.R" "${package_ref}" &
install_pid=$!

# pak suppresses its animated progress display when Rscript is launched from a
# non-interactive RStudio/system2 process. Print a heartbeat independently so a
# long metadata download or source build never looks stalled.
(
  while sleep 15; do
  if kill -0 "${install_pid}" 2>/dev/null; then
    elapsed=$((SECONDS - install_started_at))
    printf '[DrugSigNet] Installation still running (%dm %02ds elapsed).\n' \
      "$((elapsed / 60))" "$((elapsed % 60))"
    else
      exit 0
  fi
  done
) &
heartbeat_pid=$!

set +e
wait "${install_pid}"
install_status=$?
kill "${heartbeat_pid}" 2>/dev/null
wait "${heartbeat_pid}" 2>/dev/null
set -e
if ((install_status != 0)); then
  echo "ERROR: DrugSigNet installation/validation failed with status ${install_status}." >&2
  exit "${install_status}"
fi

cat <<EOF
[DrugSigNet] Pixi project created successfully: ${project_dir}
Manifest: ${pixi_toml}
Lock file: ${project_dir}/pixi.lock
Environment: ${project_dir}/.pixi
Start R with:
  ${pixi} run --manifest-path ${pixi_toml} R
EOF
