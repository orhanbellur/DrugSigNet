# Unified RStudio entry point for the rootless installer.
install_drugsignet <- function(
    package_ref = "orhanbellur/DrugSigNet",
    repository_root = if (file.exists("tools/install_drugsignet_micromamba.R")) "." else NULL,
    prefix = file.path(path.expand("~"), ".local", "share", "drugsignet-micromamba")) {
  if (is.null(repository_root)) {
    repository <- sub("@.*$", "", package_ref)
    ref <- if (grepl("@", package_ref, fixed = TRUE)) sub("^[^@]+@", "", package_ref) else "main"
    bootstrap <- tempfile(fileext = ".R")
    on.exit(unlink(bootstrap), add = TRUE)
    utils::download.file(paste0(
      "https://raw.githubusercontent.com/", repository, "/", ref,
      "/tools/install_drugsignet_micromamba.R"
    ), bootstrap, mode = "wb")
  } else {
    bootstrap <- file.path(repository_root, "tools", "install_drugsignet_micromamba.R")
  }
  bootstrap_environment <- new.env(parent = environment())
  base::sys.source(bootstrap, envir = bootstrap_environment)
  if (!exists(
    "install_drugsignet_micromamba",
    envir = bootstrap_environment,
    mode = "function",
    inherits = FALSE
  )) {
    stop(
      "The bootstrap script did not define install_drugsignet_micromamba(). ",
      "Verify that package_ref names an existing GitHub branch or tag.",
      call. = FALSE
    )
  }
  bootstrap_installer <- base::get(
    "install_drugsignet_micromamba",
    envir = bootstrap_environment,
    inherits = FALSE
  )
  bootstrap_installer(
    package_ref = package_ref,
    repository_root = repository_root,
    prefix = prefix
  )
}

if (sys.nframe() == 0L) install_drugsignet()
