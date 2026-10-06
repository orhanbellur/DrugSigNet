#!/usr/bin/env bash

# Install the operating-system prerequisites needed to build DrugSigNet and its
# R dependencies on Debian/Ubuntu (including rocker/rstudio images).
set -euo pipefail

if ! command -v apt-get >/dev/null 2>&1; then
  echo "ERROR: this helper supports Debian/Ubuntu images with apt-get." >&2
  exit 1
fi

if [[ "$(id -u)" -ne 0 ]]; then
  echo "ERROR: run this helper as root (for Docker, switch to USER root)." >&2
  exit 1
fi

export DEBIAN_FRONTEND=noninteractive

apt-get update
apt-get install -y --no-install-recommends \
  build-essential \
  ca-certificates \
  cmake \
  curl \
  default-jdk \
  default-jre \
  gfortran \
  git \
  libbz2-dev \
  libcairo2-dev \
  libcurl4-openssl-dev \
  libfontconfig1-dev \
  libfreetype6-dev \
  libfribidi-dev \
  libgit2-dev \
  libglpk-dev \
  libgmp3-dev \
  libgsl-dev \
  libharfbuzz-dev \
  libicu-dev \
  libjpeg-dev \
  liblzma-dev \
  libmpfr-dev \
  libpng-dev \
  libsqlite3-dev \
  libssh2-1-dev \
  libssl-dev \
  libtiff-dev \
  libudunits2-dev \
  libx11-dev \
  libxml2-dev \
  libxt-dev \
  pkg-config \
  wget \
  zlib1g-dev

R CMD javareconf
rm -rf /var/lib/apt/lists/*

echo "DrugSigNet Linux system dependencies are installed."
