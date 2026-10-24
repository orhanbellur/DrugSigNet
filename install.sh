#!/usr/bin/env bash
# Unified Linux/HPC installer. All dependency names come from the shared TSV.
set -euo pipefail
root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
export DRUGSIGNET_DEPENDENCY_MANIFEST="${root}/tools/runtime-dependencies.tsv"
exec "${root}/tools/install_drugsignet_micromamba.sh" "${1:-orhanbellur/DrugSigNet}"
