# DrugSigNet

[![License: MIT + file LICENSE](https://img.shields.io/badge/license-MIT%20%2B%20file%20LICENSE-blue.svg)](https://cran.r-project.org/web/licenses/MIT%20%2B%20file%20LICENSE)
[![](https://img.shields.io/badge/devel%20version-1.0.0-black.svg)](https://github.com/compneurobio/DrugSigNet)
[![R build status](https://github.com/compneurobio/DrugSigNet/workflows/rworkflows/badge.svg)](https://github.com/compneurobio/DrugSigNet/actions)
[![Docker Hub](https://img.shields.io/badge/Docker%20Hub-orhan1117-2496ED?logo=docker&logoColor=white)](https://hub.docker.com/u/orhan1117)

<img src="vignettes/images/DrugSigNet_hex.png" width="100px" align="right" />

DrugSigNet is an R package for reproducible *in silico* drug repurposing using disease signatures, biological networks, and consensus ranking.

The package supports three common analysis scenarios: signature-based drug search from differential expression data, network-based prioritization from disease genes, and integrated workflows that combine both evidence types. DrugSigNet also provides tools for drug annotation, enrichment analysis, and visualization of prioritized candidates.

## Why DrugSigNet?

DrugSigNet helps users move from disease evidence to prioritized and interpretable drug candidates in a modular workflow. It provides:

- **Signature-based drug search** using CMAP, LINCS, gCMAP, and correlation-based scoring.
- **Network-based prioritization** using TrustRank, harmonic centrality, degree centrality, network proximity, and diffusion-based scoring.
- **Consensus ranking** using CRank, Dowdall, and Robust Rank Aggregation.
- **Drug annotation** for mechanisms of action, indications, ATC classes, drug status, blood-brain-barrier information, clinical trials, and adverse events.
- **Visualization helpers** for ranked candidates, method agreement, overlaps, enrichment, and annotation summaries.
- **Export and reporting** with multi-sheet Excel output and HTML/PDF analysis reports.

## Choosing a workflow

| If you have... | Start with... | Main output |
|---|---|---|
| A signed differential expression signature with `Entrez` and `FC` columns | `drugSignaturePipeline()` | Signature-based drug rankings and consensus results |
| Disease-associated genes but no signed expression signature | `drugNetworkPipeline()` | Network-based drug rankings from disease genes and interaction networks |
| Both expression and disease-gene evidence | `drugRepurposingPipeline(mode = "both")` | Integrated signature-network rankings |
| A candidate drug list from any source | `annotate_drugs()` | Drug metadata, mechanisms, indications, status, safety, and trial context |
| Completed rankings or annotations | Visualization helpers or `run_visualization = TRUE` | Plots for method agreement, overlap, enrichment, annotation, and similarity |
| A completed pipeline result | `write_pipeline_results()` or `write_report()` | Excel workbooks or HTML/PDF analysis reports |

For a first test run, set `run_drug_annotation = FALSE` and `run_visualization = FALSE`. Re-enable annotation and visualization after confirming that the core ranking workflow runs successfully.

## Installation

DrugSigNet requires **R 4.5.0 or newer**. R 4.0.5 is tied to an obsolete
Bioconductor release and cannot resolve the current `signatureSearch`
dependency stack. Upgrade the R runtime before installing DrugSigNet; changing
`build = FALSE` does not repair an incompatible Bioconductor dependency graph.

Check the runtime before installation:

```r
R.version.string
stopifnot(getRversion() >= "4.5.0")
```

For a bare Debian/Ubuntu/Rocker container, the following development install
requires root or passwordless sudo. On supported Linux distributions, `pak`
resolves and installs system requirements such as `zlib1g-dev` before compiling
the R dependencies:

```r
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak", repos = sprintf(
    "https://r-lib.github.io/p/pak/stable/%s/%s/%s",
    .Platform$pkgType,
    R.Version()$os,
    R.Version()$arch
  ))
}

options(timeout = max(1200, getOption("timeout", 60)))
options(pkg.sysreqs = TRUE, pkg.build_vignettes = TRUE)
Sys.setenv(PKG_SYSREQS = "true", PKG_BUILD_VIGNETTES = "true")

pak::pkg_install(
  "orhanbellur/DrugSigNet",
  dependencies = NA,
  upgrade = FALSE,
  ask = FALSE
)
```

This block is **not** a rootless installer. On an unmodified
`rocker/rstudio:4.5.3` session running as `rstudio`, do not set
`pkg.sysreqs = TRUE`: pak will correctly attempt `sudo apt-get` and fail. There
is no R option that grants permission to the Debian package manager. A rootless,
R-only installation is supported only after the native layer has been prepared
(for example with `Dockerfile.runtime`), in which case use
`pkg.sysreqs = FALSE`.

`pkg.build_vignettes = TRUE` is required for GitHub/source installations made
with `pak`. Without it, `browseVignettes("DrugSigNet")` can display an index
derived from the source files even though the linked HTML files were not
installed, resulting in an RStudio Server “requested page was not found” error.
Reinstall with this option enabled if an existing installation has that symptom.

To install a branch, append the branch after `@` while retaining the GitHub
owner and repository. For example:

```r
pak::pkg_install(
  "orhanbellur/DrugSigNet@codex/verify-drugsignet-installation-dependencies-etqtuy",
  dependencies = NA,
  upgrade = FALSE,
  ask = FALSE
)
```

Do not pass only `"codex/verify-drugsignet-installation-dependencies-etqtuy"`.
`pak` interprets a two-component reference as `owner/repository`, so that value
asks GitHub for an owner named `codex` and a repository named after the branch.

`signatureSearch` remains a declared DrugSigNet import and is installed
automatically; no separate dependency-install command is required.
`dependencies = NA` installs the required dependencies without pulling every
development `Suggests` package. The longer timeout prevents large annotation
downloads from failing at R's 60-second default.

Packages used automatically by annotation, enrichment, and the default
visualization payload (`enrichR`, `data.table`, `ggalluvial`, `ggforce`,
`plotly`, and `wordcloud`) are hard imports. Consequently, the normal
installation includes them and a pipeline requested with
`run_drug_annotation = TRUE` or `run_visualization = TRUE` does not fail later
because an automatically invoked feature was classified as optional.

Parallel R tasks use base R's PSOCK clusters on Linux, macOS, Windows,
containers, and scheduler-allocated HPC sessions. Each worker is a clean R
process with the parent library paths and required namespaces loaded explicitly.
This intentionally avoids Unix fork workers, which inherit HDF5, Java, and
reticulate native state and can block or crash. Worker counts are capped by the
task count and physical cores; single-worker runs remain sequential. On an HPC
system, request CPU cores from the scheduler first and set `n_workers` no higher
than that allocation.

The implementation uses `parallel::makeCluster(type = "PSOCK")` together with
`parallel::parLapply()`. This is the portable base-R pattern on Linux, macOS,
and Windows. It is equivalent in transport to `makePSOCKcluster()`; explicitly
naming the type makes the cross-platform contract clear. Native-library or
memory failures inside a task still require the checkpoint/recovery safeguards
described below—the PSOCK transport alone cannot make an HDF5 backend
process-safe.

Signature methods execute in strict reference order: every CMAP-reference
method completes before the LINCS2 phase starts. When `n_workers > 1`, both
phases use independent PSOCK workers. The cluster is stopped and recreated
after every batch, releasing HDF5 handles and resident memory before the next
batch. A parallel request made with the default reference mode is automatically
resolved to immutable local frozen reference files before workers start. This
avoids concurrent ExperimentHub/cache initialization, which can kill a worker
and break its PSOCK connection. Execution is capped at two workers. Because LINCS2 is approximately
5.9 GB, the Docker container or HPC job must have enough memory for two LINCS2
workers and their working matrices; otherwise the operating system can still
terminate a worker with `error reading from connection`. Each completed method
is atomically checkpointed before its result is returned over the PSOCK socket.
If a worker exits, DrugSigNet reloads completed checkpoints and retries only the
unfinished methods sequentially, rather than discarding the entire analysis or
rerunning work that already finished.

Automatic Linux system-package installation requires either a root R process
or passwordless `sudo`. In a Dockerfile, run the command after `USER root`.
When neither privilege is available, `pak` reports the required OS packages but
cannot modify the host operating system.

Native Windows does not provide the Python `graph-tool` dependency. Calling
`drugNetworkPipeline()` or an individual network-search method on native
Windows therefore issues a warning and returns without starting the analysis,
rather than failing during Python initialization. Use the DrugSigNet Docker
image or Windows Subsystem for Linux (WSL) for network-based analyses. Signature
methods remain available on native Windows.

For an interactive `rocker/rstudio` container, opt in to passwordless sudo when
starting the container:

```bash
docker rm -f rstudio453 2>/dev/null || true
docker run -d \
  --name rstudio453 \
  -p 8787:8787 \
  -e PASSWORD=1234 \
  -e ROOT=TRUE \
  rocker/rstudio:4.5.3
```

Then sign in as `rstudio` and run the `pak::pkg_install()` command above. If an
already-running container was started without `ROOT=TRUE`, recreate it with
that setting or perform the installation as root with `docker exec --user root`.
The message `Executing sudo ... apt-get` followed by `System command 'sudo'
failed` means the RStudio user was not granted that privilege; it is not an R or
Bioconductor dependency-resolution error.

`ROOT=TRUE` does not make the RStudio session run as root. Rocker's startup
script adds the normal `rstudio` account to passwordless sudoers. Confirm that
the recreated container received the setting before starting a large install:

```bash
docker exec --user rstudio rstudio453 whoami
docker exec --user rstudio rstudio453 sudo -n true
```

The first command must print `rstudio`, and the second must exit successfully.
Environment variables cannot be added retroactively to an already-running
container, so `docker restart rstudio453` is not sufficient; remove and recreate
it. Also write `--name`, not `\--name`.

If `ROOT=TRUE` and sudo are not allowed, a running stock
`rocker/rstudio:4.5.3` container cannot install missing `apt` packages. Build a
runtime base once, with the native dependencies included, and keep the running
RStudio session unprivileged:

```bash
docker buildx build --load \
  --file docker/linux/Dockerfile.runtime \
  --tag drugsignet-runtime:4.5.3 .

docker run -d \
  --name rstudio453 \
  -p 8787:8787 \
  -e PASSWORD=1234 \
  drugsignet-runtime:4.5.3
```

No `ROOT=TRUE` is used at runtime. In R, install to the user library with
`pkg.sysreqs = FALSE`; DrugSigNet's R dependencies and user-owned Python/conda
runtime are still installed by the package workflow, while the immutable OS
layer is already present. This separation is required by Linux: neither an R
package nor its configure script can grant itself permission to modify `/usr`.

An existing stock Rocker container does not need to be discarded if the caller
can use Docker. Copy the dependency helper into it and execute only that setup
step as container root:

Do not reference the installer through `raw.githubusercontent.com/.../main`
until the commit adding it has actually been merged into `main`; GitHub
correctly returns HTTP 404 for a path that does not yet exist on that branch.
Use the script from the checked-out branch containing this change. No RStudio
Console preparation is required: the installer detects and installs missing OS
packages through Docker, installs DrugSigNet as `rstudio`, provisions
Python/conda, and validates the finished runtime.

The same operation cannot be initiated exclusively from the R Console in a
stock container started without `ROOT=TRUE`. That R process has neither sudo
authorization nor access to the host Docker daemon, and creating another shell
or R script does not change its Linux credentials. Mounting the Docker socket
into RStudio would make the session root-equivalent to the host and is not a
safe workaround. Therefore at least the one host-terminal bootstrap command
above is required; all later R, Python, and validation work is automatic.

Consequently, an “install every missing Debian package from one ordinary R
command” API is intentionally not provided: it would either fail at `apt-get`
or require silently granting the R session root-equivalent access. The supported
one-command R design starts from `docker/linux/Dockerfile.runtime`, where native
libraries are already part of the image; then an ordinary user can run only
`pak::pkg_install()` with `pkg.sysreqs = FALSE`. For the unmodified stock Rocker
image, one authorized host/container setup step is irreducible.

Conda/Mamba solves the rootless **Python** portion: it can be installed below
the user's home directory and provides `graph-tool` plus its Python-side native
libraries without sudo. It does not install Debian packages or update dpkg, so
it cannot make pak's `libxml2-dev`, `default-jdk`, or similar apt requirements
become installed. Building R packages against conda headers would require the R
compiler, Java, pkg-config, include paths, link paths, and runtime loader all to
come from one activated conda environment. Mixing those libraries into Rocker's
already-running system R is not treated as a supported installation because it
can create ABI and loader mismatches. A fully conda-managed R stack is a
different runtime, not an in-place repair of `rocker/rstudio`.

DrugSigNet uses one dependency manifest for all supported installation paths:
`tools/runtime-dependencies.tsv`. The RStudio entry point (`install.R`), HPC
entry point (`install.sh`), and Docker builds consume that same reviewed list,
so their apt, Conda, and pip dependencies cannot silently diverge.

For a completely rootless runtime, run the unified HPC/Linux entry point. It
keeps R 4.5, compilers, Java, native libraries, R/Bioconductor packages,
Python, and `graph-tool` in one user-owned prefix:

```bash
./install.sh orhanbellur/DrugSigNet
~/.local/bin/drugsignet-R
```

The same rootless installation can be initiated from an RStudio Console with
the single public R entry point:

```r
source("https://raw.githubusercontent.com/orhanbellur/DrugSigNet/main/install.R")
install_drugsignet("orhanbellur/DrugSigNet")
```

### Automatic project-local Pixi installation

Users who want a portable project directory containing `pixi.toml`,
`pixi.lock`, `.pixi/`, and `test_installation.R` can use the Pixi entry point.
The call bootstraps Pixi when necessary, builds the manifest from the canonical
runtime dependency list, resolves the lock file, creates the environment,
installs DrugSigNet, and runs the installation check automatically:

```r
branch <- "main"
source(paste0(
  "https://raw.githubusercontent.com/orhanbellur/DrugSigNet/",
  branch,
  "/install-pixi.R"
))
install_drugsignet_pixi(
  package_ref = paste0("orhanbellur/DrugSigNet@", branch),
  project_dir = "~/DrugSigNet-test"
)
```

After completion, start the isolated R runtime with
`~/.pixi/bin/pixi run --manifest-path ~/DrugSigNet-test/pixi.toml R`. Repeating
the installer reuses the lock file and `.pixi` environment when the generated
manifest has not changed.

If DrugSigNet is already installed in the current RStudio R library, the shell
command is not required for Python-backed network and Synapse operations.
DrugSigNet automatically discovers
`~/DrugSigNet-test/.pixi/envs/default/bin/python` (or the project selected by
`DRUGSIGNET_PIXI_PROJECT` / `options(DrugSigNet.pixi_project = ...)`) on first
use and selects it before reticulate initializes. The explicit `pixi run ... R`
command remains the fully isolated option when R packages or native libraries
must also come from Pixi.

Minimal containers such as `rocker/rstudio:4.5.3` may not include the `curl`
command-line program. The R entry point now selects `curl`, then `wget`, and
finally R's own `utils::download.file()` automatically. The final fallback also
downloads the Pixi bootstrap through R, so the same installation call works in
an unmodified Rocker container without first running `apt-get install curl`.

#### Why this uses a rootless prefix instead of `fakeroot`

`fakeroot` only simulates ownership and permission metadata for processes that
create archives or Debian packages; it does not grant permission to write into
`/usr`, update dpkg's real database, run privileged package-maintainer scripts,
or make host R safely load libraries unpacked into an arbitrary fake root.
Likewise, `fakechroot`/PRoot-style filesystem redirection would create a second
userspace root whose loader paths must be entered explicitly; it cannot repair
the already-running host R process.

The supported no-sudo equivalent is therefore the generated Pixi prefix. During
`install_drugsignet_pixi()`, compilers, Java, R, Python, `graph-tool`, and native
libraries such as libxml2, GLPK, GSL, Cairo, ICU, libcurl, OpenSSL, and HDF5 are
resolved below `~/DrugSigNet-test/.pixi/`; nothing is written to `/usr` or the
host dpkg database. The DrugSigNet package is then built by that prefix's R and
validated before the installer returns. In other words, Pixi provides the
useful isolation expected from a “fake root”, but with a consistent package
solver, runtime loader paths, and lock file.

The current R process cannot replace its own R executable or loaded native
libraries. If reticulate has not yet initialized, however, it can select Pixi
Python directly and network loading can continue in the same RStudio session.
If reticulate already initialized another Python, restart R once; DrugSigNet
will discover Pixi automatically after restart. DrugSigNet recognizes a
complete Pixi `RETICULATE_PYTHON` runtime and will not create the unrelated
Micromamba `r-drugsignet` environment.

For a conventional `devtools::install_github()` installation, the first
Python-backed operation still provisions the dedicated `r-drugsignet` Conda
environment. If an earlier interrupted attempt left only a partial directory,
DrugSigNet detects the missing `conda-meta/history`, removes that incomplete
dedicated prefix, and recreates it before installing Python packages.

Install only hard dependencies unless signature-based analyses are needed.
Using `dependencies = TRUE` asks remotes to download every suggested package
and large Bioconductor annotation database, which is unnecessary for network
workflows and is prone to the default 60-second download timeout:

```r
options(timeout = max(1200, getOption("timeout", 60)))
remotes::install_github(
  "orhanbellur/DrugSigNet",
  ref = "main",
  dependencies = NA,
  upgrade = "never",
  build_vignettes = TRUE
)
```

`signatureSearch` is now an optional dependency. Install it separately with
`BiocManager::install("signatureSearch")` before running signature-based
methods; network loading and network analysis do not require it.

For a local checkout, `source("install.R")` provides the same entry point. For
a development branch, use the exact pushed branch in both the raw URL and
`package_ref`. This installer must be used **instead of** starting with
`pak::pkg_install()`: pak resolves and builds R dependencies before the target
package's configure hook can run, which is too late to supply their missing
native libraries.

The function deliberately runs the installer in a child process. The active
RStudio process cannot replace its own R executable or already-loaded shared
libraries. After installation, select
`~/.local/share/drugsignet-micromamba/bin/R` as the RStudio R version and restart
the session, or use `~/.local/bin/drugsignet-R` in the Terminal.

For remote installations, the R helper streams the shell bootstrap directly
from GitHub with `curl` rather than storing another copy through RStudio's
download handler. This avoids partial `.rs.downloadFile`/`readRDS` downloads
when the home-directory quota is nearly full. The runtime installation itself
still needs enough free space; clear stale Micromamba and pip caches before
retrying if the system reports `Disk quota exceeded`.

This does not modify apt or `/usr`. The resulting package belongs to the
Micromamba R library and must be run with that R executable; it will not appear
in an already-running Rocker `/usr/local/bin/R` session.

The installer writes its generated Conda specification to a file named
`environment.yml`. Micromamba uses the filename extension to distinguish a
YAML environment from a plain MatchSpec list; using an extension-less temporary
file results in `Error parsing MatchSpec "": Empty package name`.

Before invoking the environment's R, the installer removes inherited `R_HOME`,
`R_LIBS`, `R_LIBS_USER`, and `R_LIBS_SITE` values. This prevents a parent
RStudio session from redirecting the Micromamba R process back into the host R
library. Native builds are also directed to the environment's `xml2-config`
and pkg-config metadata so packages such as XML cannot accidentally mix `/usr`
headers with Micromamba libraries.

The final health check resolves API functions directly from the installed
DrugSigNet namespace, so validation does not depend on attaching the package
with `library()`. If validation fails, the installer prints the exact missing
R packages, exports, and Python modules before exiting.

Repeated runs update an existing Micromamba prefix instead of recreating it.
pip requirements are checked by installed distribution/version and installed
with `--no-cache-dir` only when missing; they are deliberately excluded from
the generated Conda YAML because Micromamba otherwise reruns pip on every
environment update, which can exhaust HPC or home-directory disk quotas.

From that local repository checkout, run:

```bash
./tools/install_drugsignet_in_container.sh \
  rstudio453 \
  orhanbellur/DrugSigNet@codex/verify-drugsignet-installation-dependencies-cnvtok
```

This one command installs the native layer, then runs pak and the runtime health
check as `rstudio`. The copied Bash file first uses `dpkg-query` to list only
missing libraries; `apt-get` receives only that missing set. Detection is
unprivileged, while installation is executed by the Docker daemon with
`--user root`, so it does not depend on sudo inside the container. The
equivalent manual native-dependency steps are:

```bash
docker cp tools/install_linux_system_dependencies.sh \
  rstudio453:/tmp/install_drugsignet_system_dependencies.sh
docker exec --user root rstudio453 \
  bash /tmp/install_drugsignet_system_dependencies.sh
docker exec --user rstudio rstudio453 \
  test -r /usr/include/zlib.h
```

These are **host-terminal commands**, not RStudio Console commands. A stock
container started without `ROOT=TRUE` cannot fix this from its R Console. In
particular, setting `pkg.sysreqs = TRUE` forces pak to run `sudo apt-get` and
reproduces `System command 'sudo' failed`. After the host-side dependency step,
restart the R installation with both `pkg.sysreqs = FALSE` and
`PKG_SYSREQS=false`; do not repeat the failing configuration.

The RStudio session remains `rstudio` and the container was not started with
`ROOT=TRUE`. This does use the Docker daemon's existing ability to execute one
administrative command inside the container. Afterwards set
`pkg.sysreqs = FALSE`, unset `DRUGSIGNET_SKIP_RUNTIME_SETUP`, and install
DrugSigNet into the user library. Container filesystem changes disappear when
the container is removed; use `docker commit` or build `Dockerfile.runtime` if
the repaired environment must be reproducible.

On an HPC cluster, Docker/Apptainer/Singularity containers commonly run as the
calling user and deliberately provide neither root nor passwordless `sudo`. If
the prebuilt image already contains DrugSigNet's operating-system and Python
runtime dependencies, do **not** enable pak system-requirement installation.
Install only the R package into a writable user library:

```r
user_lib <- path.expand(Sys.getenv(
  "R_LIBS_USER",
  unset = file.path("~", "R", paste0("library-", getRversion()))
))
dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(user_lib, .libPaths()))

options(
  timeout = max(1200, getOption("timeout", 60)),
  pkg.sysreqs = FALSE,
  pkg.build_vignettes = TRUE
)
Sys.setenv(
  PKG_SYSREQS = "false",
  PKG_BUILD_VIGNETTES = "true",
  DRUGSIGNET_SKIP_RUNTIME_SETUP = "true"
)

pak::pkg_install(
  "orhanbellur/DrugSigNet@your-branch",
  lib = user_lib,
  dependencies = NA,
  upgrade = FALSE,
  ask = FALSE
)
```

Use `dependencies = NA` for a runtime installation; `dependencies = TRUE`
requests all development and optional `Suggests` packages, which explains the
279-package/803 MB plan in the reported log. Only set
`DRUGSIGNET_SKIP_RUNTIME_SETUP=true` when the prebuilt image already contains
the validated conda/Python runtime. If native dependencies are genuinely
missing, they must be added by the image builder or HPC administrator; an
unprivileged R process cannot install them.

The `pak` plan marks unavailable native packages with `✖`. Those entries are
not harmless when a dependency must be built from source. For example,
`Rhdf5lib` checks for `zlib.h` during its configure step, so
`configure: error: couldn't find zlib library` proves that the running HPC image
does **not** contain the required zlib development files. Disabling pak's
system-requirement installer avoids the `sudo` error, but cannot manufacture a
missing header. Verify the exact runtime image before installing:

```bash
test -r /usr/include/zlib.h || echo "missing zlib1g-dev"
dpkg-query -W zlib1g-dev libxml2-dev libcurl4-openssl-dev default-jdk
```

If that check fails, rebuild the image from this repository. The maintained
Dockerfile now installs the complete native dependency set explicitly rather
than assuming it is inherited from the Bioconductor base:

```bash
docker buildx build --load \
  --file docker/linux/Dockerfile \
  --tag drugsignet:linux .
```

Then transfer that exact immutable image/digest to the cluster. Merely using the
same R version or image name does not guarantee that the HPC runtime is running
the locally built dependency layer.

There is no way for an ordinary Linux process to install `apt` packages without
root-equivalent privilege; allowing that would bypass the operating system's
security boundary. If the environment is a single-user Docker container (not a
restricted Apptainer/Singularity HPC job), the maintained image offers an
explicit opt-in that keeps R/RStudio non-root while granting passwordless sudo
for installation commands:

```bash
docker buildx build --load \
  --build-arg ENABLE_PASSWORDLESS_SUDO=true \
  --file docker/linux/Dockerfile \
  --tag drugsignet:interactive .
```

Inside that container, leave `pkg.sysreqs = TRUE`; pak can then elevate only
when it invokes the OS package manager. This is still root-equivalent access,
even though the R process runs as `rstudio`, and must not be enabled in a shared
or multi-tenant image. Apptainer/Singularity commonly removes or blocks setuid
escalation, so this option cannot override an HPC security policy. In that case,
the only supported solution is to preinstall native dependencies in the image
or ask the administrator to provide them.

The bootstrap uses pak's self-contained pre-built repository rather than the
CRAN source tarball. This is necessary on a bare Rocker image: compiling pak's
embedded `curl` package from source already needs `libcurl4-openssl-dev`, while
the pre-built pak binary has no system-library bootstrap dependency.

`R CMD INSTALL` deliberately does not create a conda environment. Configure
hooks can run while pak is merely packaging a GitHub checkout, and external
runtime mutation there can make source-package creation fail. After a normal
installation, call `DrugSigNet::setup_python_dependencies()` explicitly for
Python-backed methods. For a rootless installation that also supplies native
libraries, use the all-Micromamba installer above and restart RStudio with its
R executable.

The three-platform `rworkflows` workflow performs a second source installation
after `R CMD check` and validates the separately provisioned runtime.

For an environment whose system requirements are already provisioned, `pak`
can also install all optional development dependencies:

```r
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak", repos = sprintf(
    "https://r-lib.github.io/p/pak/stable/%s/%s/%s",
    .Platform$pkgType,
    R.Version()$os,
    R.Version()$arch
  ))
}

options(pkg.build_vignettes = TRUE)
Sys.setenv(PKG_BUILD_VIGNETTES = "true")

pak::pak(
  "compneurobio/DrugSigNet",
  dependencies = TRUE,
  upgrade = FALSE
)
```

Load the package:

```r
library(DrugSigNet)
```

For Python-backed methods, explicitly run
`DrugSigNet::setup_python_dependencies()` after installing the R package. It
creates a dedicated `r-drugsignet` conda environment containing the Python
dependencies, including conda-forge's `graph-tool` on supported platforms.
Run it before reticulate initializes another Python interpreter, then restart
R. Package loading never downloads an external runtime unless you explicitly
set `options(DrugSigNet.auto_install_python = TRUE)` before attaching it.
DrugSigNet runs Synapse operations through an isolated child Python process;
the R process never imports `synapseclient` or its OpenSSL bindings. DrugSigNet
also verifies this environment immediately before its
first graph-backed operation, so a missing `graph_tool` now produces recovery
instructions rather than a Python `ModuleNotFoundError`.

### Optional dependencies

Some workflows require additional setup:

- **Synapse-backed data** requires a Synapse personal access token. DrugSigNet
  installs the maintained Python `synapseclient` into its conda environment and
  invokes it out of process. This avoids the OpenSSL collision caused by loading
  conda's SSL extension inside an RStudio process that already loaded Ubuntu's
  `libcrypto`. To validate
  Synapse support in advance, use:

  ```r
  DrugSigNet::setup_synapser()
  ```

  The legacy function name is retained for compatibility; the helper now
  validates the isolated Python client rather than loading the Synapser R
  namespace.
- **Graph-tool-backed network methods** use the automatically provisioned
  `r-drugsignet` conda environment. You can repair or validate it explicitly
  with `DrugSigNet::setup_python_dependencies()` and
  `DrugSigNet::check_drugsignet_installation(stop_on_error = TRUE)`.
- **Fully reproducible local execution** is supported through Docker.

For local Python, Synapse, and Docker details, see [`DOCKER.md`](DOCKER.md), the vignettes, and the helper script `tools/install_local_drugsignet.R` in the repository.

## Quick start

### Signature-based workflow

Run a small signature-based workflow from a two-column disease signature.

```r
library(DrugSigNet)

signature_input <- data.frame(
  Entrez = c("7157", "5290", "1956", "7422", "4609", "207"),
  FC = c(2.1, -1.4, 1.8, -2.2, 1.3, -1.7)
)

signature_result <- drugSignaturePipeline(
  signature_input = signature_input,
  top_k = 50,
  n_workers = 1,
  run_drug_annotation = FALSE,
  run_visualization = FALSE
)
```

### Network-based workflow

Run a network-based workflow from disease genes.

```r
disease_genes <- c("TP53", "PIK3CA", "EGFR", "VEGFA", "AKT1")

network_result <- drugNetworkPipeline(
  disease_genes = disease_genes,
  run_all_network_methods = FALSE,
  top_k = 50,
  run_drug_annotation = FALSE,
  run_visualization = FALSE
)
```

### Integrated workflow

Combine signature and disease-gene evidence in one workflow.

```r
repurposing_result <- drugRepurposingPipeline(
  signature_input = signature_input,
  disease_genes = disease_genes,
  mode = "both",
  run_all_network_methods = FALSE,
  top_k = 50,
  run_drug_annotation = FALSE,
  run_visualization = FALSE
)
```

## Input requirements

| Workflow component | Required input | Notes |
|---|---|---|
| Signature workflow | Data frame with `Entrez` and `FC` columns | Include both positive and negative fold-change values |
| Network workflow | Non-empty disease gene vector or data frame | Custom networks can be supplied with `ppi_network` and `drug_target_network` |
| Custom PPI network | Data frame with `gene1` and `gene2` columns | Use identifiers consistent with disease genes and drug targets |
| Custom drug-target network | Data frame with `ID`, `Drug`, `Target`, and `Group` columns | Targets should use the same identifier system as the PPI network |
| Annotation workflow | Character vector or data frame of drug names | Synapse-backed annotation data may require `SYNAPSE_AUTH_TOKEN` |
| Visualization workflow | Ranking, annotation, enrichment, or similarity tables | Required columns depend on the selected plotting function |

## Main functions

| Function | Purpose |
|---|---|
| `drugSignaturePipeline()` | Run signature-based drug repurposing from differential expression signatures |
| `drugNetworkPipeline()` | Run network-based prioritization from disease genes and interaction networks |
| `drugRepurposingPipeline()` | Run signature, network, or integrated workflows |
| `annotate_drugs()` | Add drug metadata, mechanisms, indications, safety, and trial information |
| `CRank()`, `Dowdall()`, `RRA()` | Aggregate method-specific rankings into consensus rankings |
| Visualization helpers | Generate plots for ranked drugs, method agreement, overlaps, enrichment, and annotation summaries |
| `write_pipeline_results()` | Export available pipeline tables to a multi-sheet Excel workbook |
| `write_report()` | Generate HTML/PDF reports from one or more pipeline results |

## Export and reporting

Completed pipeline results can be exported to Excel or summarized as an
HTML/PDF report:

```r
write_pipeline_results(
  result_obj = repurposing_result,
  file_path = "DrugSigNet_results.xlsx",
  top_n = 100
)

write_report(
  object = repurposing_result,
  file = "DrugSigNet_report",
  device = "html",
  interactive = TRUE
)
```

Set `write_results = TRUE` in `write_report()` to create the Excel export
alongside the report. See the complete case-study vignette for additional
examples and reporting options.

## Documentation

After installation, open the package vignettes with:

```r
browseVignettes("DrugSigNet")
```

If RStudio Server shows the vignette index but its `/library/DrugSigNet/doc/`
links return HTTP 404, the files may still be installed correctly while the
server refuses that system-library route. Open a session-local copy instead:

```r
browse_drugsignet_vignette()                  # vignette index
browse_drugsignet_vignette("getting-started") # one vignette
```

This copies the installed documentation and its assets to the R session's
temporary directory before passing it to the configured RStudio viewer.
The maintained Linux installer also makes the root-installed package directory
world-readable and traversable for the unprivileged RStudio Server session.

Recommended reading order:

1. [Getting started](vignettes/getting-started.Rmd)
2. [Choosing an analysis strategy](vignettes/choosing-analysis-strategy.Rmd)
3. [Signature methods](vignettes/signature-methods.Rmd)
4. [Network methods](vignettes/network-methods.Rmd)
5. [Rank aggregation](vignettes/rank-aggregation.Rmd)
6. [Drug annotation](vignettes/drug-annotation.Rmd)
7. [Visualization](vignettes/visualization.Rmd)
8. [Signature workflow](vignettes/signature-workflow.Rmd)
9. [Network workflow](vignettes/network-workflow.Rmd)
10. [Complete case study](vignettes/complete-case-study.Rmd)

For function-level documentation, load the package and use:

```r
?drugSignaturePipeline
?drugNetworkPipeline
?drugRepurposingPipeline
?write_pipeline_results
?write_report
```

## Docker

Docker is recommended when users want a reproducible DrugSigNet environment without manually installing R, system libraries, Python modules, or optional network dependencies.

### Use a pre-built Docker image

Pre-built DrugSigNet Docker images for Linux, macOS, and Windows are available from the [`orhan1117` Docker Hub account](https://hub.docker.com/u/orhan1117). Pull the image corresponding to your platform:

**Linux**

```bash
docker pull orhan1117/drugsignet:linux
```

**macOS**

```bash
docker pull orhan1117/drugsignet:macos
```

**Windows**

```powershell
docker pull orhan1117/drugsignet:windows
```

The `docker pull` command downloads the selected image to the local Docker installation. If the image is already present, Docker checks for and retrieves a newer version when available.

### Run RStudio Server

After pulling the image, start the container for your platform.

**Linux**

```bash
docker run --rm -p 8787:8787 -e PASSWORD=drugsignet orhan1117/drugsignet:linux
```

**macOS**

```bash
docker run --rm -p 8787:8787 -e PASSWORD=drugsignet orhan1117/drugsignet:macos
```

**Windows (PowerShell)**

```powershell
docker run --rm -p 8787:8787 -e PASSWORD=drugsignet orhan1117/drugsignet:windows
```

Open <http://localhost:8787> in a web browser and sign in with username `rstudio` and password `drugsignet`.

To run R directly instead of RStudio Server, use:

```bash
docker run --rm -it orhan1117/drugsignet:linux R
```

Replace `linux` with `mac` or `windows` when using the corresponding image.

### Build the image locally

Users who prefer to build DrugSigNet from source can build the Linux image from the repository root:

```bash
docker buildx build --load -f docker/linux/Dockerfile -t drugsignet:linux .
```

The maintained image uses R 4.5.x with its matching Bioconductor release and
checks the complete installation while building. Pulling
`rocker/rstudio:4.5.2` only downloads the base image: it does not execute the
`apt-get`, Java configuration, R-package, or Python-package installation steps
in a Dockerfile that starts `FROM rocker/rstudio:4.5.2`. Therefore, the same R
version can work in a derived image and fail in the bare Rocker image. Use the
maintained build above, or build—not merely pull—your complete custom
Dockerfile.

For a custom Debian, Ubuntu, or Rocker image, a single root command installs
the native prerequisites, R/Bioconductor dependencies, DrugSigNet, its Python
runtime, and validates the result:

```dockerfile
COPY . /opt/DrugSigNet
RUN /opt/DrugSigNet/tools/install_drugsignet_linux.sh
```

This must be an outer installation entry point rather than a package
`configure` hook because R installs dependencies before running DrugSigNet's
hook. The package's `SystemRequirements` field also records the native
requirements; see [`DOCKER.md`](DOCKER.md) for the complete Rocker example.

For platform-specific builds, configuration, and troubleshooting, see [`DOCKER.md`](DOCKER.md).
The repository includes `docker/macos/build.sh` for Apple Silicon and Intel
macOS hosts, plus `docker/windows/build.ps1` for Docker Desktop on Windows.

## Synapse authentication

Some DrugSigNet annotation resources are hosted on Synapse and require a
Synapse Personal Access Token (PAT).

For instructions on creating a token and configuring it securely in R, see the
[Getting Started vignette](vignettes/getting-started.Rmd).

Synapse authentication is optional and is not required for the basic
signature- or network-based workflows.

## Citation

If DrugSigNet contributes to your analysis or publication, please cite the package. Citation metadata is available in [`CITATION.cff`](CITATION.cff).

## Issues and feature requests

Please report bugs, questions, and feature requests through [GitHub issues](https://github.com/compneurobio/DrugSigNet/issues).
