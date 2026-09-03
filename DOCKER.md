# DrugSigNet Docker images

DrugSigNet containers use a Linux userspace because Bioconductor and the
`graph-tool` dependency are supported by the Linux base image. macOS and
Windows users run this image through Docker Desktop; these are not native macOS
or Windows containers.

The required R dependency set, including `wordcloud`, is installed in the Linux
base image and inherited by the macOS and Windows Docker Desktop images.
DrugSigNet is installed before its vignettes are built, so vignette calls to
`library(DrugSigNet)` use the installed package. Vignette output is generated
in the package source's `doc` directory during the image build. The build uses
the base-R `tools::buildVignettes()` function, avoiding a second dependency
resolution and package installation pass.

All build commands use the repository root as the Docker build context.

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
