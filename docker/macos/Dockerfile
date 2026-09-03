# DrugSigNet image metadata for Docker Desktop on macOS.
#
# Build docker/linux/Dockerfile as `drugsignet:linux` first. That image supports
# both Apple Silicon (linux/arm64) and Intel Macs (linux/amd64), and includes
# required R packages such as wordcloud.
ARG BASE_IMAGE=drugsignet:linux
FROM ${BASE_IMAGE}

LABEL org.opencontainers.image.title="DrugSigNet for macOS Docker Desktop" \
      org.opencontainers.image.description="DrugSigNet Linux container packaged for Docker Desktop on macOS" \
      org.opencontainers.image.vendor="DrugSigNet"
