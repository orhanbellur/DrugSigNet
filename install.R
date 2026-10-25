# Unified RStudio entry point for the rootless installer. Keep the remote path
# self-contained: after this file has been sourced, no second R script needs to
# be downloaded or evaluated in the active RStudio process.
install_drugsignet <- function(
    package_ref = "orhanbellur/DrugSigNet",
    repository_root = if (file.exists("tools/install_drugsignet_micromamba.sh")) "." else NULL,
    prefix = file.path(path.expand("~"), ".local", "share", "drugsignet-micromamba")) {
  if (!identical(.Platform$OS.type, "unix")) {
    stop("The Micromamba rootless installer currently supports Linux only.", call. = FALSE)
  }

  if (is.null(repository_root)) {
    repository <- sub("@.*$", "", package_ref)
    ref <- if (grepl("@", package_ref, fixed = TRUE)) {
      sub("^[^@]+@", "", package_ref)
    } else {
      "main"
    }
    if (!grepl("^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$", repository)) {
      stop("package_ref must have the form 'owner/repository[@git-ref]'.", call. = FALSE)
    }

    raw_base <- paste0("https://raw.githubusercontent.com/", repository, "/", ref)
    installer_url <- paste0(raw_base, "/tools/install_drugsignet_micromamba.sh")
    manifest_url <- paste0(raw_base, "/tools/runtime-dependencies.tsv")
    bootstrap_command <- paste(
      "set -o pipefail;",
      "curl --fail --location --silent --show-error --retry 3 \"$1\" |",
      "bash -s -- \"$2\""
    )
    message("Streaming the rootless Micromamba installer to a child process.")
    status <- system2(
      "bash",
      c(
        "-c", shQuote(bootstrap_command), "drugsignet-bootstrap",
        shQuote(installer_url), shQuote(package_ref)
      ),
      env = c(
        paste0("DRUGSIGNET_MAMBA_ENV=", path.expand(prefix)),
        paste0("DRUGSIGNET_DEPENDENCY_MANIFEST_URL=", manifest_url)
      )
    )
  } else {
    installer <- normalizePath(file.path(
      repository_root, "tools", "install_drugsignet_micromamba.sh"
    ), mustWork = TRUE)
    manifest <- normalizePath(file.path(
      repository_root, "tools", "runtime-dependencies.tsv"
    ), mustWork = TRUE)
    message("Starting the rootless Micromamba installation in a child process.")
    status <- system2(
      "bash",
      c(shQuote(installer), shQuote(package_ref)),
      env = c(
        paste0("DRUGSIGNET_MAMBA_ENV=", path.expand(prefix)),
        paste0("DRUGSIGNET_DEPENDENCY_MANIFEST=", manifest)
      )
    )
  }

  if (!identical(status, 0L)) {
    stop("The Micromamba DrugSigNet installer exited with status ", status, ".", call. = FALSE)
  }

  r_binary <- file.path(path.expand(prefix), "bin", "R")
  message(
    "DrugSigNet was installed in a separate rootless R runtime:\n  ",
    r_binary,
    "\nThe current RStudio session still uses:\n  ", R.home("bin"),
    "\nSelect the Micromamba R executable in RStudio and restart the session, ",
    "or run ~/.local/bin/drugsignet-R in the Terminal."
  )
  invisible(r_binary)
}

if (sys.nframe() == 0L) install_drugsignet()
