# Persist access to a completed Pixi runtime for future host-R sessions.
.drugsignet_persist_pixi_host_config <- function(project_dir, pixi_python,
                                                  pixi_library, validation_marker) {
  renviron <- path.expand("~/.Renviron")
  environment <- if (file.exists(renviron)) readLines(renviron, warn = FALSE) else character()
  environment <- environment[!grepl(
    "^(DRUGSIGNET_PIXI_PROJECT|RETICULATE_PYTHON|RETICULATE_CONDA|DRUGSIGNET_PIXI_VALIDATION_MARKER)=",
    environment
  )]
  writeLines(c(
    environment,
    paste0("DRUGSIGNET_PIXI_PROJECT=", project_dir),
    paste0("RETICULATE_PYTHON=", pixi_python),
    paste0("RETICULATE_CONDA=", file.path(project_dir, ".pixi", "envs", "default", "bin", "conda")),
    paste0("DRUGSIGNET_PIXI_VALIDATION_MARKER=", validation_marker)
  ), renviron)

  rprofile <- path.expand("~/.Rprofile")
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
    "  if (dir.exists(pixi_library)) {",
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
  .libPaths(unique(c(pixi_library, .libPaths())))
  if (!requireNamespace("DrugSigNet", quietly = TRUE)) {
    stop(
      "DrugSigNet is installed in Pixi but cannot be loaded by the current R. ",
      "Open ", file.path(project_dir, "DrugSigNet-test.Rproj"),
      " or start Pixi R as printed below.",
      call. = FALSE
    )
  }
  validation_marker <- Sys.getenv("DRUGSIGNET_PIXI_VALIDATION_MARKER")
  if (!file.exists(validation_marker)) {
    stop("Pixi runtime validation marker is missing: ", validation_marker, call. = FALSE)
  }
  .drugsignet_persist_pixi_host_config(
    project_dir, pixi_python, pixi_library, validation_marker
  )
  message(
    "DrugSigNet's Pixi library and Python are now active in the current R session; ",
    "library(DrugSigNet) can be called directly. The generated RStudio project ",
    "and the managed ~/.Rprofile entry restore the same library on later sessions. ",
    "For a fully isolated R use:\n  ",
    file.path(path.expand("~"), ".pixi", "bin", "pixi"),
    " run --manifest-path ", file.path(project_dir, "pixi.toml"), " R"
  )
  invisible(project_dir)
}

if (sys.nframe() == 0L) install_drugsignet_pixi()
