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

# Keep libraries for different R installations separate, even within one series.
.drugsignet_default_host_library <- function() {
  identity <- tempfile("drugsignet-r-engine-")
  on.exit(unlink(identity), add = TRUE)
  writeLines(c(normalizePath(R.home()), R.version$platform, .drugsignet_r_minor(getRversion())), identity)
  key <- substr(unname(tools::md5sum(identity)), 1L, 12L)
  file.path(path.expand("~"), "R", "DrugSigNet", .drugsignet_r_minor(getRversion()), key, "library")
}

.drugsignet_remove_pixi_library <- function(library) {
  target <- normalizePath(library, mustWork = FALSE)
  .libPaths(.libPaths()[normalizePath(.libPaths(), mustWork = FALSE) != target])
  invisible(NULL)
}

# Probe foreign native libraries in a child of the HOST R, never in the live
# RStudio process: an incompatible macOS libR can terminate R with SIGSEGV.
.drugsignet_can_share_runtime <- function(library, prefix,
                                          system = Sys.info()[["sysname"]],
                                          r_home = R.home()) {
  if (identical(normalizePath(r_home, mustWork = FALSE),
                normalizePath(file.path(prefix, "lib", "R"), mustWork = FALSE))) return(TRUE)
  if (identical(system, "Darwin")) return(FALSE)
  script <- tempfile("drugsignet-host-probe-", fileext = ".R")
  log <- tempfile("drugsignet-host-probe-", fileext = ".log")
  on.exit(unlink(c(script, log)), add = TRUE)
  writeLines(c(
    'args <- commandArgs(trailingOnly = TRUE)',
    'library <- args[[1L]]',
    'cpp <- c("/usr/lib/x86_64-linux-gnu/libstdc++.so.6", "/usr/lib/aarch64-linux-gnu/libstdc++.so.6")',
    'cpp <- cpp[file.exists(cpp)]',
    'if (length(cpp)) dyn.load(cpp[[1L]], local = FALSE)',
    '.libPaths(unique(c(library, args[-1L])))',
    'stopifnot(requireNamespace("DrugSigNet", quietly = FALSE))',
    'stopifnot(identical(normalizePath(dirname(getNamespaceInfo(asNamespace("DrugSigNet"), "path"))), normalizePath(library)))',
    'stopifnot(requireNamespace("signatureSearch", quietly = FALSE))'
  ), script)
  status <- system2(file.path(r_home, "bin", "Rscript"),
                    c("--vanilla", shQuote(script), shQuote(unique(c(library, .libPaths())))),
                    stdout = log, stderr = log,
                    env = c("DRUGSIGNET_AUTO_INSTALL_PYTHON=false", "DRUGSIGNET_SKIP_RUNTIME_SETUP=true"))
  if (!identical(status, 0L)) {
    message("Pixi R packages are incompatible with this host R; installing native host packages.")
    if (file.exists(log)) message(paste(readLines(log, warn = FALSE), collapse = "\n"))
    return(FALSE)
  }
  TRUE
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
                                              build_vignettes, repository_root = NULL,
                                              runtime_prefix = NULL, source_archive = NULL,
                                              full_runtime = FALSE) {
  if (getRversion() < "4.4.0") stop("Host installation requires R >= 4.4.0.", call. = FALSE)
  if ("DrugSigNet" %in% loadedNamespaces()) {
    stop("Restart R before installing DrugSigNet into the host library.", call. = FALSE)
  }
  dir.create(host_library, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(host_library) || file.access(host_library, 2L) != 0L) {
    stop("Host R library is not writable: ", host_library, call. = FALSE)
  }
  old_options <- options()
  old_environment <- Sys.getenv(c("CONDA_PREFIX", "R_LIBS", "R_LIBS_USER", "R_LIBS_SITE",
                                  "JAVA_HOME", "R_JAVA_LD_LIBRARY_PATH", "R_MAKEVARS_USER", "RSTUDIO_PANDOC"), unset = NA_character_)
  on.exit({
    options(old_options)
    for (name in names(old_environment)) {
      if (is.na(old_environment[[name]])) Sys.unsetenv(name)
      else do.call(Sys.setenv, setNames(list(old_environment[[name]]), name))
    }
  }, add = TRUE)
  Sys.unsetenv(c("CONDA_PREFIX", "R_LIBS", "R_LIBS_USER", "R_LIBS_SITE"))
  cran <- if (.drugsignet_r_minor(getRversion()) == "4.4") {
    "https://packagemanager.posit.co/cran/2025-04-30"
  } else "https://cloud.r-project.org"
  options(repos = c(CRAN = cran), timeout = max(1200, getOption("timeout", 60)))
  .libPaths(unique(c(host_library, .libPaths())))
  if (isTRUE(full_runtime) && Sys.info()[["sysname"]] == "Darwin" && grepl("R.framework", R.home(), fixed = TRUE)) {
    options(pkgType = "both", install.packages.compile.from.source = "always",
            install.packages.check.source = "no")
  }
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
  if (isTRUE(full_runtime)) {
    if (is.null(runtime_prefix) || is.null(source_archive) || !file.exists(source_archive)) {
      stop("The matching package source archive is missing; rerun the Pixi installer.", call. = FALSE)
    }
    if (isTRUE(build_vignettes)) {
      pandoc <- file.path(runtime_prefix, "bin")
      if (!file.exists(file.path(pandoc, "pandoc"))) {
        stop("The validated runtime is missing Pandoc for host vignette builds.", call. = FALSE)
      }
      Sys.setenv(RSTUDIO_PANDOC = pandoc)
    }
    descriptor <- file.path(runtime_prefix, "lib", "R", "library", "DrugSigNet", "DESCRIPTION")
    imports <- read.dcf(descriptor, fields = "Imports")[[1L]]
    core <- trimws(sub("\\s*\\(.*$", "", strsplit(imports, ",", fixed = TRUE)[[1L]]))
    required <- unique(c(core, "knitr", "rmarkdown", "rcdk", "fingerprint",
                         "BoutrosLab.plotting.general", "ggforce", "ggalluvial", "wordcloud"))
    required <- setdiff(required, rownames(utils::installed.packages(priority = "base")))
    java <- file.path(runtime_prefix, "lib", "jvm")
    Sys.setenv(JAVA_HOME = java, R_JAVA_LD_LIBRARY_PATH = file.path(java, "lib", "server"))
    # Bake the project JVM path into a library compiled by the HOST R.
    includes <- paste0("-I", file.path(java, c("include", paste0("include/", if (Sys.info()[["sysname"]] == "Darwin") "darwin" else "linux"))))
    java_libs <- paste0("-L", file.path(java, "lib", "server"),
                        " -Wl,-rpath,", file.path(java, "lib", "server"), " -ljvm")
    configure <- paste0("JAVA_HOME=", shQuote(java),
                         " JAVA=", shQuote(file.path(java, "bin", "java")),
                         " JAVAC=", shQuote(file.path(java, "bin", "javac")),
                         " JAR=", shQuote(file.path(java, "bin", "jar")),
                         " JAVA_CPPFLAGS=", shQuote(paste(includes, collapse = " ")),
                         " JAVA_LIBS=", shQuote(java_libs))
    # DrugSigNet uses JNI (R -> Java), not JRI (Java -> embedded R).
    # Avoid linking a second R engine and its optional embedding libraries.
    utils::install.packages("rJava", lib = host_library, repos = cran,
                            type = "source", configure.vars = configure,
                            configure.args = "--disable-jri")
    if (!file.exists(file.path(host_library, "rJava", "DESCRIPTION"))) {
      stop("Native rJava installation failed; inspect the compiler output above.", call. = FALSE)
    }
    installed <- rownames(utils::installed.packages(lib.loc = .libPaths()))
    missing <- setdiff(required, installed)
    if (length(missing)) utils::install.packages(missing, lib = host_library, repos = repositories, dependencies = NA)
    if (.drugsignet_r_minor(getRversion()) == "4.4") {
      makevars <- tempfile("drugsignet-native-Makevars-")
      on.exit(unlink(makevars), add = TRUE)
      user_makevars <- old_environment[["R_MAKEVARS_USER"]]
      if (is.na(user_makevars) || !nzchar(user_makevars)) user_makevars <- path.expand("~/.R/Makevars")
      settings <- if (file.exists(user_makevars)) readLines(user_makevars, warn = FALSE) else character()
      writeLines(c(settings, "CXX11STD = -std=gnu++14"), makevars)
      Sys.setenv(R_MAKEVARS_USER = makevars)
      utils::install.packages("BH", lib = host_library, repos = "https://cloud.r-project.org", type = "source")
    }
    bioc_version <- if (.drugsignet_r_minor(getRversion()) == "4.4") "3.20" else as.character(BiocManager::version())
    BiocManager::install(c("signatureSearch", "signatureSearchData"), lib = host_library,
                         version = bioc_version, ask = FALSE, update = FALSE)
  }
  message("Installing a separate DrugSigNet copy for host R ", getRversion(),
          " into ", host_library, ". Native system dependencies must already be available.")
  arguments <- list(lib = host_library, dependencies = NA, upgrade = "never",
                    build_vignettes = build_vignettes, force = TRUE)
  tryCatch({
    if (!is.null(source_archive)) {
      do.call(remotes::install_local, c(list(path = source_archive), arguments))
    } else if (is.null(repository_root)) {
      do.call(remotes::install_github, c(list(repo = package_ref), arguments))
    } else {
      do.call(remotes::install_local, c(list(path = repository_root), arguments))
    }
    if (isTRUE(full_runtime)) {
      stopifnot(requireNamespace("signatureSearch", quietly = FALSE))
      molecule <- rcdk::parse.smiles("CCO")[[1L]]
      stopifnot(inherits(rcdk::get.fingerprint(molecule, type = "extended"), "fingerprint"))
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
  managed_library <- file.path(project_dir, ".pixi", "envs", "default", "lib", "R", "library")
  managed_home <- file.path(project_dir, ".pixi", "envs", "default", "lib", "R")
  quote_r <- function(value) encodeString(normalizePath(value, mustWork = FALSE), quote = '"')
  block <- c(
    begin,
    "local({",
    paste0("  selected_library <- ", quote_r(pixi_library)),
    paste0("  pixi_library <- ", quote_r(managed_library)),
    paste0("  pixi_home <- ", quote_r(managed_home)),
    paste0("  installed_home <- ", quote_r(R.home())),
    paste0("  runtime_minor <- ", encodeString(.drugsignet_r_minor(runtime_version), quote = '"')),
    '  host_minor <- paste(strsplit(as.character(getRversion()), ".", fixed = TRUE)[[1L]][1:2], collapse = ".")',
    '  current_home <- normalizePath(R.home(), mustWork = FALSE)',
    '  if (identical(current_home, pixi_home)) {',
    '    if (!identical(selected_library, pixi_library)) .libPaths(.libPaths()[normalizePath(.libPaths(), mustWork = FALSE) != selected_library])',
    '  } else {',
    '    .libPaths(.libPaths()[!normalizePath(.libPaths(), mustWork = FALSE) %in% c(pixi_library, selected_library)])',
    '    if (identical(current_home, installed_home) && identical(host_minor, runtime_minor) && dir.exists(selected_library)) {',
    '      .libPaths(unique(c(selected_library, .libPaths())))',
    '    }',
    '  }',
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
    host_library = .drugsignet_default_host_library()) {
  if (!is.logical(install_host) || length(install_host) != 1L || is.na(install_host)) {
    stop("install_host must be TRUE or FALSE.", call. = FALSE)
  }
  if (getRversion() < "4.4.0") {
    stop("DrugSigNet requires R >= 4.4.0.", call. = FALSE)
  }
  planned_prefix <- file.path(path.expand(project_dir), ".pixi", "envs", "default")
  if (!identical(normalizePath(R.home(), mustWork = FALSE),
                 normalizePath(file.path(planned_prefix, "lib", "R"), mustWork = FALSE))) {
    .drugsignet_remove_pixi_library(file.path(planned_prefix, "lib", "R", "library"))
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
  prefix <- file.path(project_dir, ".pixi", "envs", "default")
  native_host <- isTRUE(install_host) || !.drugsignet_can_share_runtime(pixi_library, prefix)
  host_installed <- FALSE
  if (native_host) {
    .drugsignet_remove_pixi_library(pixi_library)
    host_library <- normalizePath(path.expand(host_library), mustWork = FALSE)
    managed <- paste0(normalizePath(file.path(project_dir, ".pixi"), mustWork = FALSE), "/")
    if (startsWith(paste0(host_library, "/"), managed)) {
      stop("host_library must be outside the Pixi environment.", call. = FALSE)
    }
    .drugsignet_install_host_package(
      package_ref, host_library, build_vignettes, repository_root,
      runtime_prefix = prefix, source_archive = file.path(project_dir, "DrugSigNet-source.tar.gz"),
      full_runtime = TRUE
    )
    host_installed <- TRUE
  } else {
    .drugsignet_configure_pixi_library(pixi_library, runtime_version)
    if (!requireNamespace("DrugSigNet", quietly = FALSE)) {
      stop("DrugSigNet could not load in the current R after the native-library check.", call. = FALSE)
    }
  }
  .drugsignet_persist_pixi_host_config(
    project_dir, pixi_python,
    if (host_installed) path.expand(host_library) else pixi_library,
    validation_marker,
    runtime_version = if (host_installed) getRversion() else runtime_version
  )
  .drugsignet_persist_pixi_host_config(
    project_dir, pixi_python, if (host_installed) host_library else pixi_library,
    validation_marker, runtime_version = if (host_installed) getRversion() else runtime_version,
    config_dir = project_dir
  )
  if (host_installed) {
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
