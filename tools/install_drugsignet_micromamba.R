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

    repository_root <- tempfile("drugsignet-bootstrap-")
    dir.create(file.path(repository_root, "tools"), recursive = TRUE)
    on.exit(unlink(repository_root, recursive = TRUE, force = TRUE), add = TRUE)
    raw_base <- paste0("https://raw.githubusercontent.com/", repository, "/", git_ref)
    files <- c("tools/install_drugsignet_micromamba.sh", "environment-micromamba.yml")
    for (file in files) {
      utils::download.file(
        paste0(raw_base, "/", file), file.path(repository_root, file),
        mode = "wb", quiet = FALSE
      )
    }
  }

  installer <- normalizePath(file.path(
    repository_root, "tools", "install_drugsignet_micromamba.sh"
  ), mustWork = TRUE)
  normalizePath(file.path(repository_root, "environment-micromamba.yml"), mustWork = TRUE)

  message("Starting the rootless Micromamba installation in a child process.")
  status <- system2(
    "bash",
    c(shQuote(installer), shQuote(package_ref)),
    env = paste0("DRUGSIGNET_MAMBA_ENV=", path.expand(prefix))
  )
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
