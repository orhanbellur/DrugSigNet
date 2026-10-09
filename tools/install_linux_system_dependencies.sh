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

packages=(
  build-essential ca-certificates cmake curl default-jdk default-jre gfortran git
  libbz2-dev libcairo2-dev libcurl4-openssl-dev libfontconfig1-dev
  libfreetype6-dev libfribidi-dev libgit2-dev libglpk-dev libgmp3-dev
  libgsl-dev libharfbuzz-dev libicu-dev libjpeg-dev liblzma-dev libmpfr-dev
  libpng-dev libsqlite3-dev libssh2-1-dev libssl-dev libtiff-dev
  libudunits2-dev libuv1-dev libx11-dev libxml2-dev libxt-dev pkg-config wget
  xz-utils zlib1g-dev
)

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
