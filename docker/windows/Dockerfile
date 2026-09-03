# DrugSigNet image metadata for Docker Desktop on Windows.
#
# DrugSigNet uses Linux-only graph-tool dependencies, so Docker Desktop must be
# configured to use Linux containers. Build docker/linux/Dockerfile as
# `drugsignet:linux` before building this image. The base image includes
# required R packages such as wordcloud.
ARG BASE_IMAGE=drugsignet:linux
FROM ${BASE_IMAGE}

LABEL org.opencontainers.image.title="DrugSigNet for Windows Docker Desktop" \
      org.opencontainers.image.description="DrugSigNet Linux container packaged for Docker Desktop on Windows" \
      org.opencontainers.image.vendor="DrugSigNet"
