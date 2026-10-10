#!/usr/bin/env bash

# Detect and install the operating-system prerequisites needed to build
# DrugSigNet and its R dependencies on Debian/Ubuntu.
set -euo pipefail

mode="${1:-install}"
if [[ "${mode}" != "install" && "${mode}" != "--check" ]]; then
  echo "Usage: $0 [--check]" >&2
  exit 2
fi

if ! command -v apt-get >/dev/null 2>&1 || \
   ! command -v dpkg-query >/dev/null 2>&1; then
  echo "ERROR: this helper supports Debian/Ubuntu images with apt-get." >&2
  exit 1
fi

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
manifest="${DRUGSIGNET_DEPENDENCY_MANIFEST:-${script_dir}/runtime-dependencies.tsv}"
[[ -r "${manifest}" ]] || { echo "ERROR: dependency manifest not found: ${manifest}" >&2; exit 1; }
mapfile -t packages < <(awk -F '\t' '$1 == "apt" { print $2 }' "${manifest}")
(( ${#packages[@]} > 0 )) || { echo "ERROR: no apt dependencies in ${manifest}" >&2; exit 1; }

missing=()
for package in "${packages[@]}"; do
  status="$(dpkg-query -W -f='${Status}' "${package}" 2>/dev/null || true)"
  if [[ "${status}" != "install ok installed" ]]; then
    missing+=("${package}")
  fi
done

if ((${#missing[@]} == 0)); then
  echo "DrugSigNet Linux system dependencies are already installed."
  exit 0
fi

printf 'Missing DrugSigNet Linux system dependencies (%d):\n' "${#missing[@]}"
printf '  %s\n' "${missing[@]}"

if [[ "${mode}" == "--check" ]]; then
  exit 1
fi

if [[ "$(id -u)" -ne 0 ]]; then
  cat >&2 <<'EOF'
ERROR: missing distribution packages cannot be installed without root-equivalent
privilege. Run this file through `docker exec --user root`, use passwordless
sudo, or install the packages while building the image.
EOF
  exit 1
fi

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y --no-install-recommends "${missing[@]}"
R CMD javareconf
rm -rf /var/lib/apt/lists/*

echo "DrugSigNet Linux system dependencies were installed successfully."
