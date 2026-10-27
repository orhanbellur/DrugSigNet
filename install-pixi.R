.drugsignet_r_minor <- function(version) {
  paste(strsplit(as.character(numeric_version(version)), ".", fixed = TRUE)[[1L]][1:2], collapse = ".")
}

.drugsignet_pixi_platform <- function(system = Sys.info()[["sysname"]], arch = R.version$arch) {
  if (!system %in% c("Darwin", "Linux")) return(NULL)
  architecture <- if (arch %in% c("arm64", "aarch64")) "arm64" else if (arch == "x86_64") "x86_64" else "unsupported"
  platform <- c("Darwin-arm64" = "osx-arm64", "Darwin-x86_64" = "osx-64",
                "Linux-arm64" = "linux-aarch64", "Linux-x86_64" = "linux-64")
  result <- unname(platform[paste(system, architecture, sep = "-")])
  if (is.na(result)) stop("Unsupported host R architecture: ", arch, call. = FALSE)
  result
}

# Packages compiled for another R minor version must stay in their own runtime.
.drugsignet_configure_pixi_library <- function(pixi_library, runtime_version,
                                              host_version = getRversion()) {
  compatible <- identical(.drugsignet_r_minor(runtime_version), .drugsignet_r_minor(host_version))
  if (compatible) {
    .libPaths(unique(c(pixi_library, .libPaths())))
  } else {
    target <- normalizePath(pixi_library, mustWork = FALSE)
    .libPaths(.libPaths()[normalizePath(.libPaths(), mustWork = FALSE) != target])
  }
  compatible
}

# Build a separate copy for the R interpreter that launched this installer.
# Never reuse packages compiled in the Pixi R library for a different R minor.
.drugsignet_install_host_package <- function(package_ref, host_library,
                                              build_vignettes, repository_root = NULL) {
  if (getRversion() < "4.4.0") stop("Host installation requires R >= 4.4.0.", call. = FALSE)
  if ("DrugSigNet" %in% loadedNamespaces()) {
    stop("Restart R before installing DrugSigNet into the host library.", call. = FALSE)
  }
  dir.create(host_library, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(host_library) || file.access(host_library, 2L) != 0L) {
    stop("Host R library is not writable: ", host_library, call. = FALSE)
  }
  old_options <- options()
  old_environment <- Sys.getenv(c("CONDA_PREFIX", "R_LIBS", "R_LIBS_USER", "R_LIBS_SITE"), unset = NA_character_)
  on.exit({
    options(old_options)
    for (name in names(old_environment)) {
      if (is.na(old_environment[[name]])) Sys.unsetenv(name)
      else do.call(Sys.setenv, setNames(list(old_environment[[name]]), name))
    }
  }, add = TRUE)
  Sys.unsetenv(names(old_environment))
  cran <- if (.drugsignet_r_minor(getRversion()) == "4.4") {
    "https://packagemanager.posit.co/cran/2025-04-30"
  } else "https://cloud.r-project.org"
  options(repos = c(CRAN = cran), timeout = max(1200, getOption("timeout", 60)))
  .libPaths(unique(c(host_library, .libPaths())))
  bootstrap <- c("BiocManager", "remotes", if (isTRUE(build_vignettes)) c("knitr", "rmarkdown"))
  for (package in bootstrap) {
    if (!requireNamespace(package, quietly = TRUE)) {
      utils::install.packages(package, lib = host_library, repos = cran)
    }
  }
  repositories <- if (.drugsignet_r_minor(getRversion()) == "4.4") {
    BiocManager::repositories(version = "3.20")
  } else BiocManager::repositories()
  repositories["CRAN"] <- cran
  options(repos = repositories)
  message("Installing a separate DrugSigNet copy for host R ", getRversion(),
          " into ", host_library, ". Native system dependencies must already be available.")
  arguments <- list(lib = host_library, dependencies = NA, upgrade = "never",
                    build_vignettes = build_vignettes, force = TRUE)
  tryCatch({
    if (is.null(repository_root)) {
      do.call(remotes::install_github, c(list(repo = package_ref), arguments))
    } else {
      do.call(remotes::install_local, c(list(path = repository_root), arguments))
    }
    if (!requireNamespace("DrugSigNet", quietly = TRUE) ||
        !identical(normalizePath(dirname(find.package("DrugSigNet"))), normalizePath(host_library))) {
      stop("DrugSigNet did not load from the requested host library.")
    }
  }, error = function(error) {
    stop("Host R installation failed: ", conditionMessage(error),
         "\nThe validated Pixi installation is still available. Install missing host system dependencies and retry.",
         call. = FALSE)
  })
  invisible(normalizePath(host_library))
}

# Persist access to a completed Pixi runtime for future host-R sessions.
.drugsignet_persist_pixi_host_config <- function(project_dir, pixi_python,
                                                  pixi_library, validation_marker,
                                                  runtime_version = getRversion(),
                                                  config_dir = path.expand("~")) {
  renviron <- file.path(config_dir, ".Renviron")
  environment <- if (file.exists(renviron)) readLines(renviron, warn = FALSE) else character()
  environment <- environment[!grepl(
    "^(DRUGSIGNET_PIXI_PROJECT|RETICULATE_PYTHON|RETICULATE_CONDA|CONDA_PREFIX|DRUGSIGNET_PIXI_VALIDATION_MARKER)=",
    environment
  )]
  writeLines(c(
    environment,
    paste0("DRUGSIGNET_PIXI_PROJECT=", project_dir),
    paste0("RETICULATE_PYTHON=", pixi_python),
    paste0("RETICULATE_CONDA=", file.path(project_dir, ".pixi", "envs", "default", "bin", "conda")),
    paste0("CONDA_PREFIX=", file.path(project_dir, ".pixi", "envs", "default")),
    paste0("DRUGSIGNET_PIXI_VALIDATION_MARKER=", validation_marker)
  ), renviron)

  rprofile <- file.path(config_dir, ".Rprofile")
  profile <- if (file.exists(rprofile)) readLines(rprofile, warn = FALSE) else character()
  begin <- "# >>> DrugSigNet Pixi >>>"
  end <- "# <<< DrugSigNet Pixi <<<"
  keep <- logical(length(profile))
  inside <- FALSE
  for (index in seq_along(profile)) {
    if (identical(profile[[index]], begin)) inside <- TRUE
    keep[[index]] <- !inside
    if (identical(profile[[index]], end)) inside <- FALSE
  }
  profile <- profile[keep]
  block <- c(
    begin,
    "local({",
    paste0("  pixi_library <- ", encodeString(pixi_library, quote = '"')),
    paste0("  runtime_minor <- ", encodeString(.drugsignet_r_minor(runtime_version), quote = '"')),
    '  host_minor <- paste(strsplit(as.character(getRversion()), ".", fixed = TRUE)[[1L]][1:2], collapse = ".")',
    "  if (identical(host_minor, runtime_minor) && dir.exists(pixi_library)) {",
    "    .libPaths(unique(c(pixi_library, .libPaths())))",
    "  }",
    "})",
    end
  )
  writeLines(c(profile, block), rprofile)
  invisible(TRUE)
}

# Create a locked, project-local Pixi runtime and install DrugSigNet into it.
install_drugsignet_pixi <- function(
    package_ref = "orhanbellur/DrugSigNet",
    project_dir = file.path(path.expand("~"), "DrugSigNet-test"),
    build_vignettes = FALSE,
    repository_root = if (file.exists("tools/install_drugsignet_pixi.sh")) "." else NULL,
    install_host = FALSE,
    host_library = file.path(path.expand("~"), "R", "DrugSigNet",
                             .drugsignet_r_minor(getRversion()), "library")) {
  if (!is.logical(install_host) || length(install_host) != 1L || is.na(install_host)) {
    stop("install_host must be TRUE or FALSE.", call. = FALSE)
  }
  if (getRversion() < "4.4.0") {
    stop("DrugSigNet requires R >= 4.4.0.", call. = FALSE)
  }
  repository <- sub("@.*$", "", package_ref)
  ref <- if (grepl("@", package_ref, fixed = TRUE)) sub("^[^@]+@", "", package_ref) else "main"
  if (!grepl("^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$", repository)) {
    stop("package_ref must have the form 'owner/repository[@git-ref]'.", call. = FALSE)
  }
  environment <- c(
    paste0("DRUGSIGNET_PIXI_PROJECT=", path.expand(project_dir)),
    paste0("DRUGSIGNET_PIXI_R_VERSION=", .drugsignet_r_minor(getRversion())),
    paste0("DRUGSIGNET_BUILD_VIGNETTES=", if (isTRUE(build_vignettes)) "true" else "false")
  )
  platform <- .drugsignet_pixi_platform()
  if (!is.null(platform)) environment <- c(environment, paste0("DRUGSIGNET_PIXI_PLATFORM=", platform))
  if (is.null(repository_root)) {
    base <- paste0("https://raw.githubusercontent.com/", repository, "/", ref)
    if (nzchar(Sys.which("curl")) || nzchar(Sys.which("wget"))) {
      fetch_file <- if (nzchar(Sys.which("curl"))) {
        'curl -fsSL --retry 3 "$1/%s" -o "$work/%s";'
      } else {
        'wget -qO "$work/%2$s" "$1/%1$s";'
      }
      fetch_stdout <- if (nzchar(Sys.which("curl"))) {
        'curl -fsSL --retry 3 "$1/tools/install_drugsignet_pixi.sh"'
      } else {
        'wget -qO- "$1/tools/install_drugsignet_pixi.sh"'
      }
      command <- paste(
        "set -o pipefail; work=$(mktemp -d); trap 'rm -rf \"$work\"' EXIT;",
        sprintf(fetch_file, "tools/runtime-dependencies.tsv", "deps.tsv"),
        sprintf(fetch_file, "tools/test_installation.R", "test.R"),
        fetch_stdout, "|",
        "DRUGSIGNET_DEPENDENCY_MANIFEST=\"$work/deps.tsv\"",
        "DRUGSIGNET_VALIDATION_SCRIPT=\"$work/test.R\" bash -s -- \"$2\""
      )
      status <- system2("bash", c("-c", shQuote(command), "pixi-bootstrap", shQuote(base), shQuote(package_ref)), env = environment)
    } else {
      work <- tempfile("drugsignet-pixi-bootstrap-")
      dir.create(work)
      on.exit(unlink(work, recursive = TRUE, force = TRUE), add = TRUE)
      files <- c(
        manifest = "tools/runtime-dependencies.tsv",
        validation = "tools/test_installation.R",
        installer = "tools/install_drugsignet_pixi.sh"
      )
      paths <- file.path(work, basename(files))
      names(paths) <- names(files)
      for (index in seq_along(files)) {
        utils::download.file(
          paste0(base, "/", files[[index]]), paths[[index]],
          mode = "wb", quiet = FALSE
        )
      }
      pixi_bootstrap <- file.path(work, "pixi-install.sh")
      utils::download.file(
        "https://pixi.sh/install.sh", pixi_bootstrap,
        mode = "wb", quiet = FALSE
      )
      status <- system2(
        "bash", c(shQuote(paths[["installer"]]), shQuote(package_ref)),
        env = c(
          environment,
          paste0("DRUGSIGNET_DEPENDENCY_MANIFEST=", paths[["manifest"]]),
          paste0("DRUGSIGNET_VALIDATION_SCRIPT=", paths[["validation"]]),
          paste0("DRUGSIGNET_PIXI_BOOTSTRAP=", pixi_bootstrap)
        )
      )
    }
  } else {
    installer <- file.path(repository_root, "tools", "install_drugsignet_pixi.sh")
    status <- system2("bash", c(shQuote(installer), shQuote(package_ref)), env = environment)
  }
  if (!identical(status, 0L)) stop("DrugSigNet Pixi installer exited with status ", status, ".", call. = FALSE)
  project_dir <- normalizePath(project_dir, mustWork = FALSE)
  pixi_python <- file.path(project_dir, ".pixi", "envs", "default", "bin", "python")
  if (!file.exists(pixi_python)) {
    stop("Pixi completed without creating its Python executable: ", pixi_python, call. = FALSE)
  }
  Sys.setenv(
    DRUGSIGNET_PIXI_PROJECT = project_dir,
    RETICULATE_PYTHON = pixi_python,
    RETICULATE_CONDA = file.path(
      project_dir, ".pixi", "envs", "default", "bin", "conda"
    ),
    CONDA_PREFIX = file.path(project_dir, ".pixi", "envs", "default"),
    DRUGSIGNET_PIXI_VALIDATION_MARKER = file.path(
      project_dir, ".pixi", "envs", "default", ".drugsignet-runtime-validated"
    )
  )
  pixi_library <- file.path(
    project_dir, ".pixi", "envs", "default", "lib", "R", "library"
  )
  if (!dir.exists(pixi_library)) {
    stop("Pixi completed without creating its R package library: ", pixi_library, call. = FALSE)
  }
  validation_marker <- Sys.getenv("DRUGSIGNET_PIXI_VALIDATION_MARKER")
  if (!file.exists(validation_marker)) {
    stop("Pixi runtime validation marker is missing: ", validation_marker, call. = FALSE)
  }
  version_file <- file.path(project_dir, ".pixi", "envs", "default", ".drugsignet-r-version")
  runtime_version <- if (file.exists(version_file)) readLines(version_file, warn = FALSE, n = 1L) else character()
  if (length(runtime_version) != 1L || !grepl("^[0-9]+[.][0-9]+([.][0-9]+)?$", runtime_version)) {
    stop("Pixi R version metadata is missing or invalid; rerun the installer.", call. = FALSE)
  }
  if (!identical(.drugsignet_r_minor(runtime_version), .drugsignet_r_minor(getRversion())) &&
      !isTRUE(install_host)) {
    stop("Pixi installed R ", runtime_version, " but the calling R uses ", getRversion(),
         ". The installer must provide the same R major/minor series; no foreign R library was activated.",
         call. = FALSE)
  }
  compatible <- if (isTRUE(install_host)) FALSE else {
    .drugsignet_configure_pixi_library(pixi_library, runtime_version)
  }
  if (compatible && !requireNamespace("DrugSigNet", quietly = FALSE)) {
    stop("DrugSigNet could not load in the current R session. Native-library details are printed above. ",
         "Use install_host = TRUE to build a separate copy for this host R.", call. = FALSE)
  }
  host_installed <- FALSE
  if (isTRUE(install_host) && !compatible) {
    host_library <- normalizePath(path.expand(host_library), mustWork = FALSE)
    prefix <- paste0(normalizePath(file.path(project_dir, ".pixi"), mustWork = FALSE), "/")
    if (startsWith(paste0(host_library, "/"), prefix)) {
      stop("host_library must be outside the Pixi environment.", call. = FALSE)
    }
    .drugsignet_install_host_package(package_ref, path.expand(host_library),
                                     build_vignettes, repository_root)
    host_installed <- TRUE
  }
  .drugsignet_persist_pixi_host_config(
    project_dir, pixi_python,
    if (host_installed) path.expand(host_library) else pixi_library,
    validation_marker,
    runtime_version = if (host_installed) getRversion() else runtime_version
  )
  if (host_installed) {
    .drugsignet_persist_pixi_host_config(
      project_dir, pixi_python, host_library, validation_marker,
      runtime_version = getRversion(), config_dir = project_dir
    )
    message("DrugSigNet is installed separately for host R ", getRversion(),
            " and Pixi R ", runtime_version,
            ". You can now run library(DrugSigNet) in this R session.")
    return(invisible(project_dir))
  }
  message(
    "DrugSigNet's Pixi library and Python selection are configured for this R session; ",
    "library(DrugSigNet) can be called directly. The generated RStudio project ",
    "and the managed ~/.Rprofile entry restore the same library on later sessions. ",
    "Network calculations use separate Python processes to avoid host native-library conflicts. This does not replace the running R executable. For a fully isolated R use:\n  ",
    file.path(path.expand("~"), ".pixi", "bin", "pixi"),
    " run --manifest-path ", file.path(project_dir, "pixi.toml"), " R"
  )
  invisible(project_dir)
}

if (sys.nframe() == 0L) install_drugsignet_pixi()
