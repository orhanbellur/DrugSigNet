# Start the fully rootless Micromamba installation from an existing RStudio
# Console. The installation runs in a child process because the current R
# process cannot replace its own executable or loaded native libraries.
install_drugsignet_micromamba <- function(
    package_ref = "orhanbellur/DrugSigNet",
    repository_root = NULL,
    prefix = file.path(path.expand("~"), ".local", "share", "drugsignet-micromamba")) {
  if (!identical(.Platform$OS.type, "unix")) {
    stop("The Micromamba rootless installer currently supports Linux only.", call. = FALSE)
  }

  if (is.null(repository_root)) {
    repository <- sub("@.*$", "", package_ref)
    git_ref <- if (grepl("@", package_ref, fixed = TRUE)) {
      sub("^[^@]+@", "", package_ref)
    } else {
      "main"
    }
    if (!grepl("^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$", repository)) {
      stop("package_ref must have the form 'owner/repository[@git-ref]'.", call. = FALSE)
    }

    raw_base <- paste0("https://raw.githubusercontent.com/", repository, "/", git_ref)
    installer_url <- paste0(raw_base, "/tools/install_drugsignet_micromamba.sh")
    manifest_url <- paste0(raw_base, "/tools/runtime-dependencies.tsv")
    # Stream the shell bootstrap instead of asking RStudio's download handler
    # to create another temporary copy. This avoids .rs.downloadFile failures
    # when a previous installation has left the user's disk quota nearly full.
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
