# Linux Docker image for DrugSigNet.
# Uses the official Bioconductor RStudio image to avoid rebuilding the full
# Bioconductor system dependency stack from a plain rocker image.
FROM bioconductor/bioconductor_docker:RELEASE_3_22

ARG INSTALL_SUGGESTS=false
ARG BUILD_VIGNETTES=true
ARG TARGETARCH

ENV DEBIAN_FRONTEND=noninteractive \
    INSTALL_SUGGESTS=${INSTALL_SUGGESTS} \
    BUILD_VIGNETTES=${BUILD_VIGNETTES} \
    MAKEFLAGS=-j1 \
    RETICULATE_PYTHON=/opt/conda/envs/drugsignet/bin/python \
    LD_LIBRARY_PATH=/opt/conda/envs/drugsignet/lib:/opt/conda/lib \
    LD_PRELOAD=/opt/conda/envs/drugsignet/lib/libcrypto.so.3:/opt/conda/envs/drugsignet/lib/libssl.so.3 \
    PATH=/opt/conda/bin:$PATH

USER root

# Keep apt additions minimal: the Bioconductor base image already includes most
# R/Bioconductor system libraries. The headless JDK is needed for rJava/rcdk.
RUN apt-get update && apt-get install -y --no-install-recommends \
    bzip2 \
    ca-certificates \
    default-jdk-headless \
    git \
    wget \
  && R CMD javareconf \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Install R build helpers and the Bioconductor dependency that remotes does not
# reliably resolve from a local package install. Posit Package Manager provides
# prebuilt Linux packages for CRAN packages such as vroom/readr; using it avoids the
# source-build failure that prevented signatureSearch from loading.
RUN R -q -e "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/noble/latest'), BioC_mirror = 'https://bioconductor.org', timeout = 1000); if (!requireNamespace('BiocManager', quietly = TRUE)) install.packages('BiocManager', Ncpus = 1); install.packages(c('remotes', 'devtools', 'pkgdown', 'rmarkdown', 'reticulate', 'vroom', 'readr', 'readxl', 'plotly', 'wordcloud'), Ncpus = 1); options(repos = BiocManager::repositories()); BiocManager::install(c('BiocParallel', 'BiocFileCache', 'signatureSearch', 'signatureSearchData'), dependencies = c('Depends', 'Imports'), ask = FALSE, update = FALSE, Ncpus = 1); stopifnot(requireNamespace('readr', quietly = TRUE), requireNamespace('signatureSearch', quietly = TRUE), requireNamespace('signatureSearchData', quietly = TRUE), requireNamespace('plotly', quietly = TRUE), requireNamespace('wordcloud', quietly = TRUE))"

# Install Miniforge and create the Python environment used by reticulate-backed
# network methods. graph-tool is installed from conda-forge because it is not
# available from PyPI.
RUN set -eux; \
    case "${TARGETARCH:-amd64}" in \
      amd64) miniforge_arch="x86_64" ;; \
      arm64) miniforge_arch="aarch64" ;; \
      *) echo "Unsupported TARGETARCH: ${TARGETARCH}" >&2; exit 1 ;; \
    esac; \
    wget -q "https://github.com/conda-forge/miniforge/releases/download/24.7.1-2/Miniforge3-24.7.1-2-Linux-${miniforge_arch}.sh" -O /tmp/miniforge.sh; \
    bash /tmp/miniforge.sh -b -p /opt/conda; \
    rm /tmp/miniforge.sh; \
    conda config --set always_yes yes --set changeps1 no; \
    conda config --set channel_priority strict; \
    conda create -n drugsignet -c conda-forge \
      python=3.10 \
      graph-tool \
      jinja2 \
      joblib \
      markupsafe \
      networkx \
      numpy=1.24 \
      openpyxl \
      pandas=2.0.3 \
      pip \
      scipy \
      tqdm; \
    /opt/conda/envs/drugsignet/bin/python -m pip install --no-cache-dir \
      synapseclient==4.4.2 \
      kaleido \
      openpyxl==3.1.5; \
    /opt/conda/envs/drugsignet/bin/python -c \
      "import os, tempfile, openpyxl, pandas as pd; handle, path = tempfile.mkstemp(suffix='.xlsx'); os.close(handle); pd.DataFrame({'value': [1]}).to_excel(path, index=False); assert pd.read_excel(path).iloc[0, 0] == 1; os.unlink(path); print('openpyxl', openpyxl.__version__, 'pandas', pd.__version__)"; \
    conda clean -afy; \
    chown -R rstudio:rstudio /opt/conda

# Configure R/reticulate to use the DrugSigNet Python environment.
RUN echo "RETICULATE_PYTHON=${RETICULATE_PYTHON}" >> /usr/local/lib/R/etc/Renviron.site \
  && echo "LD_LIBRARY_PATH=${LD_LIBRARY_PATH}" >> /usr/local/lib/R/etc/Renviron.site \
  && echo "LD_PRELOAD=${LD_PRELOAD}" >> /usr/local/lib/R/etc/Renviron.site \
  && R -q -e "py <- reticulate::py_config(); stopifnot(normalizePath(py[['python']]) == normalizePath(Sys.getenv('RETICULATE_PYTHON')), reticulate::py_module_available('graph_tool'), reticulate::py_module_available('pip'), reticulate::py_module_available('jinja2'), reticulate::py_module_available('markupsafe'), reticulate::py_module_available('synapseclient'), reticulate::py_module_available('kaleido'), reticulate::py_module_available('openpyxl'), reticulate::py_module_available('pandas'))"

# Install Synapse R client and verify required packages.
RUN R -q -e "Sys.setenv(RETICULATE_PYTHON = Sys.getenv('RETICULATE_PYTHON'), LD_LIBRARY_PATH = Sys.getenv('LD_LIBRARY_PATH')); install.packages('synapser', repos = c('https://ran.synapse.org', 'https://cloud.r-project.org'), Ncpus = 1); stopifnot(all(c('synapser', 'readxl', 'wordcloud') %in% rownames(utils::installed.packages())))"

# Verify Synapse packages for both root and the RStudio user.
RUN R -q -e "Sys.setenv(RETICULATE_PYTHON = Sys.getenv('RETICULATE_PYTHON'), LD_LIBRARY_PATH = Sys.getenv('LD_LIBRARY_PATH')); library(synapser); library(readxl)" \
  && su -s /bin/bash rstudio -c "R -q -e \"Sys.setenv(RETICULATE_PYTHON = '/opt/conda/envs/drugsignet/bin/python', LD_LIBRARY_PATH = '/opt/conda/envs/drugsignet/lib:/opt/conda/lib', LD_PRELOAD = '/opt/conda/envs/drugsignet/lib/libcrypto.so.3:/opt/conda/envs/drugsignet/lib/libssl.so.3'); library(synapser); library(readxl); library(wordcloud); py <- reticulate::py_config(); stopifnot(normalizePath(py[['python']]) == normalizePath(Sys.getenv('RETICULATE_PYTHON')), reticulate::py_module_available('openpyxl'), reticulate::py_module_available('pandas'))\""

# Prevent RStudio from suspending sessions because of inactivity or browser
# disconnection. Useful for long-running DrugSigNet analyses.
RUN mkdir -p /etc/rstudio \
  && touch /etc/rstudio/rsession.conf \
  && sed -i \
       -e '/^session-timeout-minutes=/d' \
       -e '/^session-disconnected-timeout-minutes=/d' \
       /etc/rstudio/rsession.conf \
  && printf '%s\n' \
       'session-timeout-minutes=0' \
       'session-disconnected-timeout-minutes=0' \
       >> /etc/rstudio/rsession.conf

WORKDIR /home/rstudio/DrugSigNet
COPY --chown=rstudio:rstudio . /home/rstudio/DrugSigNet

# Verify that the package source is available, install DrugSigNet, and only
# then build the vignettes against the installed package. Keeping these as
# separate steps ensures that vignette chunks containing library(DrugSigNet)
# resolve the package in the same way they do for an end user.
RUN test -f DESCRIPTION || \
    (printf '%s\n' \
       "ERROR: DESCRIPTION was not found in the Docker build context." \
       "Build from the DrugSigNet repository root:" \
       "  docker build -f docker/linux/Dockerfile -t drugsignet:linux ." >&2; \
     find /home/rstudio/DrugSigNet -maxdepth 2 -name DESCRIPTION -print >&2; \
     exit 1) \
  && R -q -e "deps <- if (tolower(Sys.getenv('INSTALL_SUGGESTS')) %in% c('1', 'true', 'yes')) TRUE else c('Depends', 'Imports'); remotes::install_local('.', dependencies = deps, upgrade = 'never', build_vignettes = FALSE)" \
  && R -q -e "library(DrugSigNet); packageVersion('DrugSigNet')" \
  && if [ "$(printf '%s' "${BUILD_VIGNETTES}" | tr '[:upper:]' '[:lower:]')" = "true" ]; then \
       R -q -e "tools::buildVignettes(dir = '.', tangle = TRUE)"; \
     else \
       echo "Skipping vignette build (BUILD_VIGNETTES=${BUILD_VIGNETTES})."; \
     fi

EXPOSE 8787
CMD ["/init"]
