.drugsignet_r_minor <- function(version) {
  paste(strsplit(as.character(numeric_version(version)), ".", fixed = TRUE)[[1L]][1:2], collapse = ".")
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
    repository_root = if (file.exists("tools/install_drugsignet_pixi.sh")) "." else NULL) {
  repository <- sub("@.*$", "", package_ref)
  ref <- if (grepl("@", package_ref, fixed = TRUE)) sub("^[^@]+@", "", package_ref) else "main"
  if (!grepl("^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$", repository)) {
    stop("package_ref must have the form 'owner/repository[@git-ref]'.", call. = FALSE)
  }
  environment <- c(
    paste0("DRUGSIGNET_PIXI_PROJECT=", path.expand(project_dir)),
    paste0("DRUGSIGNET_BUILD_VIGNETTES=", if (isTRUE(build_vignettes)) "true" else "false")
  )
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
  compatible <- .drugsignet_configure_pixi_library(pixi_library, runtime_version)
  if (compatible && !requireNamespace("DrugSigNet", quietly = TRUE)) {
    stop("DrugSigNet could not load in the matching host R; start the validated Pixi R runtime.", call. = FALSE)
  }
  .drugsignet_persist_pixi_host_config(
    project_dir, pixi_python, pixi_library, validation_marker, runtime_version = runtime_version
  )
  if (!compatible) {
    message(
      "DrugSigNet was installed and validated in Pixi R ", runtime_version,
      ". This session uses R ", getRversion(),
      "; use Pixi R for this project so compiled packages use the matching R version:\n  ",
      file.path(path.expand("~"), ".pixi", "bin", "pixi"),
      " run --manifest-path ", shQuote(file.path(project_dir, "pixi.toml")), " R"
    )
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
