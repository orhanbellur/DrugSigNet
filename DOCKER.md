# DrugSigNet Docker images

DrugSigNet containers use a Linux userspace because Bioconductor and the
`graph-tool` dependency are supported by the Linux base image. macOS and
Windows users run this image through Docker Desktop; these are not native macOS
or Windows containers.

The required R dependency set, including `data.table`, `ggalluvial`, `ggforce`,
and `wordcloud` for the visualization vignette, is installed in the Linux base
image and inherited by the macOS and Windows Docker Desktop images.
DrugSigNet is installed before its vignettes are built, so vignette calls to
`library(DrugSigNet)` use the installed package. Vignette output is generated
in the package source's `doc` directory during the image build. The build uses
the base-R `tools::buildVignettes()` function, avoiding a second dependency
resolution and package installation pass.

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
`signatureSearch` and its required Bioconductor dependency graph serially before
building DrugSigNet.

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
helper containing the complete system dependency list. A custom Rocker image
can use it before installing the R package:

```dockerfile
FROM rocker/rstudio:4.5.2

USER root
WORKDIR /opt/DrugSigNet
COPY . .

RUN ./tools/install_drugsignet_linux.sh
```

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

DrugSigNet's install-time configuration provisions the non-system Python
runtime after the R dependencies have been resolved. The final health check
ensures that provisioning really completed. For the most reproducible and
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

Vignettes are built by default after DrugSigNet is installed. Set
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
  bash -lc 'find /home/rstudio/DrugSigNet/doc -maxdepth 1 -type f -print'
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
