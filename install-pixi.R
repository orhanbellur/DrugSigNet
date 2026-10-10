# Create a locked, project-local Pixi runtime and install DrugSigNet into it.
install_drugsignet_pixi <- function(
    package_ref = "orhanbellur/DrugSigNet",
    project_dir = file.path(path.expand("~"), "DrugSigNet-test"),
    repository_root = if (file.exists("tools/install_drugsignet_pixi.sh")) "." else NULL) {
  repository <- sub("@.*$", "", package_ref)
  ref <- if (grepl("@", package_ref, fixed = TRUE)) sub("^[^@]+@", "", package_ref) else "main"
  if (!grepl("^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$", repository)) {
    stop("package_ref must have the form 'owner/repository[@git-ref]'.", call. = FALSE)
  }
  environment <- paste0("DRUGSIGNET_PIXI_PROJECT=", path.expand(project_dir))
  if (is.null(repository_root)) {
    base <- paste0("https://raw.githubusercontent.com/", repository, "/", ref)
    command <- paste(
      "set -o pipefail; work=$(mktemp -d); trap 'rm -rf \"$work\"' EXIT;",
      "curl -fsSL --retry 3 \"$1/tools/runtime-dependencies.tsv\" -o \"$work/deps.tsv\";",
      "curl -fsSL --retry 3 \"$1/tools/test_installation.R\" -o \"$work/test.R\";",
      "curl -fsSL --retry 3 \"$1/tools/install_drugsignet_pixi.sh\" |",
      "DRUGSIGNET_DEPENDENCY_MANIFEST=\"$work/deps.tsv\"",
      "DRUGSIGNET_VALIDATION_SCRIPT=\"$work/test.R\" bash -s -- \"$2\""
    )
    status <- system2("bash", c("-c", shQuote(command), "pixi-bootstrap", shQuote(base), shQuote(package_ref)), env = environment)
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
    RETICULATE_PYTHON = pixi_python
  )
  message(
    "The current R session now points Synapse subprocesses to Pixi Python. ",
    "DrugSigNet itself was installed into Pixi R; start that R with:\n  ",
    file.path(path.expand("~"), ".pixi", "bin", "pixi"),
    " run --manifest-path ", file.path(project_dir, "pixi.toml"), " R"
  )
  invisible(project_dir)
}

if (sys.nframe() == 0L) install_drugsignet_pixi()
