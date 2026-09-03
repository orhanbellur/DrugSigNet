#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
host_arch="$(uname -m)"

case "${host_arch}" in
  arm64|aarch64) platform="linux/arm64" ;;
  x86_64|amd64) platform="linux/amd64" ;;
  *)
    echo "Unsupported macOS architecture: ${host_arch}" >&2
    exit 1
    ;;
esac

docker buildx build \
  --load \
  --platform "${platform}" \
  --build-arg BUILD_VIGNETTES=true \
  --file "${repo_root}/docker/linux/Dockerfile" \
  --tag drugsignet:linux \
  "${repo_root}"

docker buildx build \
  --load \
  --platform "${platform}" \
  --file "${repo_root}/docker/macos/Dockerfile" \
  --tag drugsignet:macos \
  "${repo_root}"
