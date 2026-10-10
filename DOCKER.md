# DrugSigNet Docker images

DrugSigNet containers use a Linux userspace because Bioconductor and the
`graph-tool` dependency are supported by the Linux base image. macOS and
Windows users run this image through Docker Desktop; these are not native macOS
or Windows containers.

The required R dependency set, including `data.table`, `ggalluvial`, `ggforce`,
and `wordcloud` for the visualization vignette, is installed in the Linux base
image and inherited by the macOS and Windows Docker Desktop images.
DrugSigNet vignettes are built as part of the source-package installation, so
the generated HTML is copied into the installed package's `doc` directory and
is available through `browseVignettes("DrugSigNet")` in RStudio Server.

All build commands use the repository root as the Docker build context.

## Supported R version

The current DrugSigNet release requires R 4.5.0 or newer. In particular,
`rocker/rstudio:4.0.5` is not supported: it selects the historical
Bioconductor release associated with R 4.0, whose `XVector`, `GenomicRanges`,
`HDF5Array`, and `signatureSearch` dependency chain is not interchangeable with
the current release. Use `rocker/rstudio:4.5.2` or the maintained
`bioconductor/bioconductor_docker:RELEASE_3_22` image.

An error such as `libxml2.so.2: cannot open shared object file` is a separate
operating-system dependency failure. It means an already-installed binary R
package was linked to a shared library that is absent from the running image.
The one-step Linux installer below installs that native runtime, but it cannot
make an unsupported R/Bioconductor release compatible with the current
package.

If the log says that `reactome.db` or `org.Hs.eg.db` reached a 60-second
timeout, that timeout is the first actionable error. The later failures for
`XVector`, `GenomicRanges`, `HDF5Array`, and `signatureSearch` are cascading
results. The maintained installer raises the timeout and installs
the declared R and Bioconductor dependencies through the normal DrugSigNet
installation with a single worker. No separate `BiocManager::install()` call is
needed.

## A pulled Rocker image is only the base image

`docker pull rocker/rstudio:4.5.2` and `FROM rocker/rstudio:4.5.2` select the
same R 4.5.2 base. However, pulling the image does **not** execute the later
`RUN apt-get ...` and `RUN R CMD javareconf` instructions from a custom
Dockerfile. Those instructions create a new derived image containing the
compilers, JDK, headers, and native libraries needed to build DrugSigNet's R
dependencies. Consequently, a package can install in the derived image and
fail in a container started directly from the pulled base even though
`R.version.string` is identical.

Do not install the package interactively into a container created directly
from the Rocker base and treat that container as the deliverable. Container
changes are ephemeral, hard to reproduce, and still omit dependencies unless
every provisioning step was run. Build a derived image from a versioned
Dockerfile instead.

In particular, DrugSigNet cannot offer an R-level flag that makes a stock
Rocker container rootless-installable while native packages are missing.
Package metadata can describe and detect those requirements, but neither pak
nor `R CMD INSTALL` can authorize dpkg changes. Any design promising to install
missing `apt` packages from an ordinary `rstudio` process would either fail or
hide root-equivalent credentials inside the R session.

### One-step installation, including native prerequisites

Use the provided one-step installer when native dependencies must be installed
as part of the same installation operation. It installs operating-system
packages first, then R/Bioconductor dependencies, DrugSigNet, its Python
runtime, and finally runs the installation health check. Any failure aborts the
operation. The installer provisions Python under `/opt`, persists
`RETICULATE_PYTHON` in R's site environment, and grants read/execute access so
the `rstudio` account uses the same validated environment that root created.

This orchestration must start outside `R CMD INSTALL`: R resolves dependency
packages *before* running DrugSigNet's `configure` script, and some dependencies
need a compiler, Java, or headers during that earlier phase. In addition, a
normal R package installation has no root permission to run `apt-get`. Calling
`apt-get` from `configure` would therefore be both too late and unsafe. The
one-step entry point preserves correct ordering while still requiring only one
installation command.

For Debian, Ubuntu, and Rocker images, the repository provides one maintained
installer. It invokes `pak` as root so system requirements are installed before
R dependencies are compiled, then provisions and validates the Python runtime.
The installer bootstraps pak from its self-contained pre-built repository, so
installing pak itself does not require `libcurl` headers:

```dockerfile
FROM rocker/rstudio:4.5.2

USER root
WORKDIR /opt/DrugSigNet
COPY . .

RUN ./tools/install_drugsignet_linux.sh
```

For installation from an interactive RStudio session instead of at image build
time, the `rstudio` user needs passwordless sudo. Rocker enables that only when
the container is started with `ROOT=TRUE`:

```bash
docker rm -f rstudio453 2>/dev/null || true
docker run -d \
  --name rstudio453 \
  -p 8787:8787 \
  -e PASSWORD=1234 \
  -e ROOT=TRUE \
  rocker/rstudio:4.5.3
```

Without `ROOT=TRUE`, pak can correctly calculate the missing apt packages but
its `sudo apt-get` step cannot execute. Prefer build-time installation for a
reproducible image; use `ROOT=TRUE` only when interactive installation is
required.

The `ROOT` variable is processed only during container initialization. Adding
it later or merely restarting the old container does not update sudoers. Verify
the newly created container before running pak:

```bash
docker exec --user rstudio rstudio453 whoami
docker exec --user rstudio rstudio453 sudo -n true
```

The RStudio process remains the `rstudio` user; only commands invoked through
sudo receive root-equivalent privileges. The second command must return exit
status zero. Use the Docker option `--name` without a leading backslash.

### Runtime installation without ROOT=TRUE

It is not possible to add Debian/Ubuntu packages to a running stock container
without root, sudo, or an equivalent container capability. For environments
where `ROOT=TRUE` is prohibited, build the provided runtime base instead:

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

The image build installs native headers into the immutable image layer. Rocker's
init process then starts the RStudio session as its normal `rstudio` account;
the container does not need `ROOT=TRUE`. Use `pkg.sysreqs = FALSE` for the later
interactive pak installation because the OS layer is already complete; R
dependencies and the user-owned conda/Python runtime can still be provisioned
during DrugSigNet installation.

### Repairing an already-running stock Rocker container

If `rstudio453` is already running without `ROOT=TRUE`, a Docker user can install
the native layer with one explicit administrative operation while leaving the
RStudio session unprivileged. From the DrugSigNet repository root on the Docker
host, run:

The installer must first exist on the selected Git branch. Do not use a
`raw.githubusercontent.com/.../main` URL before the installer commit is merged;
an HTTP 404 means precisely that the file is absent from `main`, not that Docker
or curl is misconfigured. From a checkout of the branch containing the
installer, use:

```bash
./tools/install_drugsignet_in_container.sh \
  rstudio453 \
  orhanbellur/DrugSigNet@codex/verify-drugsignet-installation-dependencies-cnvtok
```

If `docker run` reports that `rstudio453` is already in use, do not run a second
container. Confirm or start the existing one instead:

```bash
docker inspect --format '{{.State.Running}}' rstudio453
docker start rstudio453  # needed only when the first command prints false
```

An RStudio Console running as `rstudio` cannot execute this administrative step
by itself when the container was created without `ROOT=TRUE`. `system()`,
`system2()`, a generated Bash file, and an R package configure hook all retain
the same unprivileged Linux identity. Exposing `/var/run/docker.sock` to the
RStudio session would effectively grant host-root control and is deliberately
not supported. Use the single host-terminal bootstrap command instead.

This is an architectural constraint, not an installer feature still to be
implemented. A rootless R-only installer can install files below the user's home
directory, including R packages and conda environments, but it cannot modify
the dpkg database or `/usr`. Therefore the supported minimum-step choices are:

1. stock Rocker plus one host bootstrap command; or
2. `Dockerfile.runtime` plus one R `pak::pkg_install()` call.

There is no third safe mode that adds missing `apt` packages from an
unprivileged RStudio session.

### What Conda/Mamba can and cannot replace

Conda or Mamba can be installed in a user-writable directory without root and
is the supported mechanism for DrugSigNet's Python environment, including
`graph-tool`. It does not register Debian packages in dpkg, so pak will still
report apt requirements such as `libxml2-dev`, `libcurl4-openssl-dev`, and
`default-jdk` as missing.

It is theoretically possible to create a separate environment containing R,
compilers, Java, headers, and every shared library, then run that environment's
R after full conda activation. That replaces the Rocker R runtime; it does not
repair the already-running `/usr/local/bin/R`. Pointing system R at an arbitrary
mixture of conda headers and libraries is unsupported because compiler flags,
RPATH/loader state, Java configuration, and ABI versions must remain coherent.
DrugSigNet therefore uses conda for Python/`graph-tool`, but retains the native
image layer for R package system requirements.

If replacing system R is acceptable, the repository also provides a fully
rootless Micromamba stack:

```bash
./tools/install_drugsignet_micromamba.sh orhanbellur/DrugSigNet
~/.local/bin/drugsignet-R
```

It may also be started directly from the existing RStudio Console without a
repository checkout:

```r
source(paste0(
  "https://raw.githubusercontent.com/orhanbellur/DrugSigNet/main/",
  "tools/install_drugsignet_micromamba.R"
))
install_drugsignet_micromamba("orhanbellur/DrugSigNet")
```

This initiates installation from RStudio but does not install into the active
system-R process. A running R process cannot swap its executable or unload its
native runtime safely. Select the resulting
`~/.local/share/drugsignet-micromamba/bin/R` in RStudio and restart, or use the
generated Terminal launcher.

`environment-micromamba.yml` keeps R 4.5, compiler toolchains, Java, native
libraries, Python, and `graph-tool` in the same prefix. The installer disables
pak's apt integration and validates DrugSigNet using the environment's own R and
Python. This environment must not be mixed with `/usr/local/bin/R`. To use it in
RStudio Server, select/configure the environment's R executable and restart the
R session; otherwise use the generated terminal launcher.

The helper validates that the container is running, installs native packages
by detecting the missing `dpkg` entries and passing only those packages to
`apt-get` through `docker exec --user root`. It therefore does not call sudo
inside the container. It then installs DrugSigNet, its required R packages, and
its Python/conda runtime as `rstudio`, finishing with
`check_drugsignet_installation()`. To perform the native step manually instead:

```bash
docker cp tools/install_linux_system_dependencies.sh \
  rstudio453:/tmp/install_drugsignet_system_dependencies.sh
docker exec --user root rstudio453 \
  bash /tmp/install_drugsignet_system_dependencies.sh
docker exec --user rstudio rstudio453 \
  test -r /usr/include/zlib.h
```

Run those commands in the Docker **host terminal**, not in the RStudio Console.
The R settings `pkg.sysreqs = TRUE` and `PKG_SYSREQS=true` instruct pak to call
`sudo`; they do not grant sudo permission. Repeating those settings in a stock
container created without `ROOT=TRUE` will always reproduce `System command
'sudo' failed`. Once the host-side native installation completes, use
`pkg.sysreqs = FALSE` and `PKG_SYSREQS=false` for the R installation.

Then install from RStudio with `pkg.sysreqs = FALSE` and
`PKG_SYSREQS=false`, because the OS layer is now complete. After installation,
run `DrugSigNet::setup_python_dependencies()` explicitly to create the
user-owned Python/conda runtime. This procedure does not grant sudo
to `rstudio`, but `docker exec --user root` is still a root-equivalent operation
performed by the Docker daemon.

Changes made this way belong to the writable container layer and are lost when
the container is removed. For a repeatable image, use `Dockerfile.runtime`. As a
temporary snapshot, the repaired container can be saved with:

```bash
docker commit rstudio453 drugsignet-runtime:4.5.3-repaired
```

### Prebuilt images on an HPC cluster

HPC container runtimes normally execute as the submitting user even when the
image was originally built with Docker. They do not grant the container process
Docker's build-time root identity or passwordless `sudo`. Consequently,
`PKG_SYSREQS=true` makes pak correctly discover apt requirements and then fail
at `sudo apt-get update`.

When the image already contains the required native and Python dependencies,
disable system provisioning and install into a writable user library:

```r
user_lib <- path.expand(Sys.getenv(
  "R_LIBS_USER",
  unset = file.path("~", "R", paste0("library-", getRversion()))
))
dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(user_lib, .libPaths()))

options(pkg.sysreqs = FALSE, pkg.build_vignettes = TRUE)
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

If the image does not already contain the system dependencies, disabling
sysreq installation will only move the failure to compilation or package load.
Rebuild the image with `tools/install_drugsignet_linux.sh` as root, or ask the
cluster administrator to provide a corrected image.

In a pak plan, `✖ zlib1g-dev` means the development headers are not visible in
the running container. A later `Rhdf5lib` failure containing `zlib.h... no` and
`couldn't find zlib library` confirms that this is a genuinely incomplete image,
not a pak false alarm. Check the image used by the scheduled job before spending
time downloading the R dependency graph:

```bash
test -r /usr/include/zlib.h || echo "missing zlib1g-dev"
dpkg-query -W zlib1g-dev libxml2-dev libcurl4-openssl-dev default-jdk
```

An unprivileged job cannot correct a failed check. Rebuild
`docker/linux/Dockerfile` or run `tools/install_drugsignet_linux.sh` during an
image build as root, then deploy that exact image digest to the cluster. The
maintained Dockerfile installs the complete declared native dependency set
explicitly so it does not depend on incidental contents of its base image.

### Optional passwordless sudo for single-user Docker

Linux cannot install distribution packages as an ordinary user without some
root-equivalent capability. For a trusted, single-user Docker container, the
image can keep the R/RStudio process unprivileged while permitting pak to use
passwordless sudo:

```bash
docker buildx build --load \
  --build-arg ENABLE_PASSWORDLESS_SUDO=true \
  --file docker/linux/Dockerfile \
  --tag drugsignet:interactive .
```

Then `options(pkg.sysreqs = TRUE)` works from the `rstudio` account because
pak's `sudo sh -c apt-get ...` command can elevate. This opt-in deliberately
grants the account root-equivalent control of the container. Do not enable it
for a shared or multi-tenant service.

This cannot bypass an HPC runtime policy. Apptainer/Singularity installations
usually execute with the submitter's host privileges and disable privilege
escalation, even if `/etc/sudoers` exists inside the image. Such jobs must use an
image with the native dependencies already installed, or an administrator must
provide the dependencies.

When installing directly from GitHub with `pak`, a branch is part of the full
package reference: `owner/repository@branch`. For example,
`orhanbellur/DrugSigNet@codex/my-branch` is valid, while `codex/my-branch` is
interpreted as a different GitHub owner and repository and cannot identify a
DrugSigNet branch.

This is base-image independent within the supported Debian/Ubuntu family: the
same installer works in a bare Rocker image or another Debian-derived image. It
cannot be portable to every Linux distribution because their package names and
package managers differ.

Set `DRUGSIGNET_INSTALL_SUGGESTS=true` on the `RUN` instruction when all
optional packages are required. By default, the installer installs `Depends`
and `Imports`, which are the dependencies required to load and run DrugSigNet.

For a reproducible installation, build the maintained image:

```bash
docker buildx build --load \
  --file docker/linux/Dockerfile \
  --tag drugsignet:linux \
  .
```

The maintained image pairs R 4.5.x with Bioconductor 3.22, fails early unless
the base supplies R 4.5.x, and runs
`check_drugsignet_installation()` after package installation. This detects
missing required R packages and Python modules during the image build instead
of during an analysis. It is the recommended deliverable because it includes
the system libraries, JDK configuration, CRAN and Bioconductor packages, conda
environment, `graph-tool`, and Synapse client setup. An `apt-get` list alone
only satisfies the system-library layer; it does not install those R and Python
dependencies.

DrugSigNet package installation does not mutate the Python runtime from its
configure hook. Image builders provision Python explicitly, and the final
health check ensures that provisioning really completed. For the most reproducible and
already pinned setup, prefer the maintained `docker/linux/Dockerfile` rather
than maintaining an alternative yourself.

## Linux

```bash
docker buildx build --load \
  --build-arg BUILD_VIGNETTES=true \
  --file docker/linux/Dockerfile \
  --tag drugsignet:linux \
  .
```

## macOS

Install Docker Desktop, start it, and run:

```bash
./docker/macos/build.sh
```

The script detects Apple Silicon and Intel hosts, builds the corresponding
`linux/arm64` or `linux/amd64` base image, and creates `drugsignet:macos`.

## Windows

Install Docker Desktop, select **Linux containers**, start PowerShell in the
repository root, and run:

```powershell
.\docker\windows\build.ps1
```

The script builds a `linux/amd64` image and creates
`drugsignet:windows`. Windows on Arm can run this image through Docker Desktop's
amd64 emulation.

## Run RStudio Server

Use the platform tag produced above (`linux`, `macos`, or `windows`):

```bash
docker run --rm -p 8787:8787 \
  -e PASSWORD=drugsignet \
  drugsignet:macos
```

Open <http://localhost:8787>, sign in as `rstudio`, and use the password passed
through `PASSWORD`.

To persist projects between container runs, mount a host directory at
`/home/rstudio/projects`. For example on macOS or Linux:

```bash
docker run --rm -p 8787:8787 \
  -e PASSWORD=drugsignet \
  -v "$PWD/projects:/home/rstudio/projects" \
  drugsignet:macos
```

In PowerShell, use `${PWD}` and the Windows image tag:

```powershell
docker run --rm -p 8787:8787 `
  -e PASSWORD=drugsignet `
  -v "${PWD}\projects:/home/rstudio/projects" `
  drugsignet:windows
```

## Build options

Vignettes are built by default while DrugSigNet is installed. Set
`BUILD_VIGNETTES=false` to skip that step. Set `INSTALL_SUGGESTS=true` if all
optional development dependencies are also required; the dependencies needed
by the included vignettes are installed by the image independently. For a
manual build with all suggested packages:

```bash
docker buildx build --load \
  --build-arg INSTALL_SUGGESTS=true \
  --build-arg BUILD_VIGNETTES=true \
  --file docker/linux/Dockerfile \
  --tag drugsignet:linux \
  .
```

To verify the generated vignettes in a built image:

```bash
docker run --rm drugsignet:linux \
  Rscript -e 'print(list.files(system.file("doc", package="DrugSigNet"), pattern="[.]html$"))'
```

## Troubleshooting

- Compare both images directly when diagnosing a custom build. The first
  command should show the missing system tools or libraries in the bare image;
  the second verifies the derived image rather than merely its base:

  ```bash
  docker run --rm rocker/rstudio:4.5.2 \
    bash -lc 'R --version | head -1; command -v gfortran; command -v javac'
  docker run --rm YOUR_DERIVED_IMAGE R -q -e \
    'library(DrugSigNet); DrugSigNet::check_drugsignet_installation(stop_on_error=TRUE)'
  ```
- Inspect the failing `R CMD INSTALL` output from its first `ERROR:` line. A
  message about a missing header, compiler, Java, or shared library indicates a
  system dependency; “package ... is not available” generally indicates an R
  repository or R/Bioconductor-version problem.
- Run `docker buildx inspect --bootstrap` if no BuildKit builder is active.
- On macOS, do not force `linux/amd64` on Apple Silicon unless emulation is
  specifically required; the Linux Dockerfile supports `linux/arm64`.
- On Windows, verify Docker Desktop is using Linux containers. Native Windows
  containers cannot provide the Linux `graph-tool` environment used here.
- The first build downloads R, Bioconductor, conda, Python, and package
  dependencies and can take considerable time.
- If an older cached image reports that pandas cannot find `openpyxl`, rebuild
  the Linux base without cache so the pinned Excel dependency and reticulate
  checks are applied:

  ```bash
  docker buildx build --no-cache --load \
    --file docker/linux/Dockerfile \
    --tag drugsignet:linux \
    .
  ```

  Then rerun the macOS or Windows platform build script to refresh its wrapper
  image.
