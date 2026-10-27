test_that("Pixi library activation respects the host R minor version", {
  entry <- new.env(parent = globalenv())
  sys.source(drugsignet_repository_file("install-pixi.R"), envir = entry)
  library <- tempfile("pixi-library-")
  dir.create(library)
  on.exit(unlink(library, recursive = TRUE), add = TRUE)
  withr::local_libpaths(.libPaths())
  expect_true(entry$.drugsignet_configure_pixi_library(library, as.character(getRversion())))
  expect_identical(.libPaths()[1L], normalizePath(library))
  expect_false(entry$.drugsignet_configure_pixi_library(library, "9.9.0"))
  expect_false(normalizePath(library) %in% .libPaths())
  expect_identical(entry$.drugsignet_r_minor("4.5.2"), entry$.drugsignet_r_minor("4.5.3"))
})

test_that("persisted Pixi profiles preserve user settings and guard the R version", {
  entry <- new.env(parent = globalenv())
  sys.source(drugsignet_repository_file("install-pixi.R"), envir = entry)
  config <- tempfile("pixi-host-config-")
  dir.create(config)
  on.exit(unlink(config, recursive = TRUE), add = TRUE)
  library <- file.path(config, "library")
  dir.create(library)
  withr::local_libpaths(.libPaths())
  profile <- file.path(config, ".Rprofile")
  writeLines("# Existing user settings", profile)
  for (version in c("9.9.0", as.character(getRversion()))) {
    entry$.drugsignet_persist_pixi_host_config(
      config, file.path(config, "python"), library, file.path(config, "validated"),
      runtime_version = version, config_dir = config
    )
    code <- readLines(profile)
    expect_identical(code[1L], "# Existing user settings")
    expect_equal(sum(code == "# >>> DrugSigNet Pixi >>>"), 1L)
    expect_silent(sys.source(profile, envir = new.env(parent = globalenv())))
    expect_identical(normalizePath(library) %in% .libPaths(), version != "9.9.0")
  }
})

test_that("unexpected cross-version runtimes are rejected without loading foreign R packages", {
  entry <- new.env(parent = globalenv())
  sys.source(drugsignet_repository_file("install-pixi.R"), envir = entry)
  project <- tempfile("pixi-completed-runtime-")
  prefix <- file.path(project, ".pixi", "envs", "default")
  dir.create(file.path(prefix, "bin"), recursive = TRUE)
  dir.create(file.path(prefix, "lib", "R", "library"), recursive = TRUE)
  file.create(file.path(prefix, "bin", "python"))
  file.create(file.path(prefix, ".drugsignet-runtime-validated"))
  version <- file.path(prefix, ".drugsignet-r-version")
  writeLines("9.9.0", version)
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  withr::local_libpaths(.libPaths())
  withr::local_envvar(c(DRUGSIGNET_PIXI_PROJECT = NA, RETICULATE_PYTHON = NA,
                       RETICULATE_CONDA = NA, CONDA_PREFIX = NA,
                       DRUGSIGNET_PIXI_VALIDATION_MARKER = NA))
  entry$system2 <- function(...) 0L
  entry$requireNamespace <- function(...) stop("Unsafe cross-version host load")
  persisted <- FALSE
  entry$.drugsignet_persist_pixi_host_config <- function(...) persisted <<- TRUE
  expect_error(
    entry$install_drugsignet_pixi(project_dir = project, repository_root = project),
    "must provide the same R major/minor"
  )
  expect_false(persisted)
  expect_false(normalizePath(file.path(prefix, "lib", "R", "library")) %in% .libPaths())
  unlink(version)
  expect_error(entry$install_drugsignet_pixi(project_dir = project, repository_root = project),
               "version metadata is missing")
})

test_that("opt-in host installation uses a separate library and persists both entry points", {
  entry <- new.env(parent = globalenv())
  sys.source(drugsignet_repository_file("install-pixi.R"), envir = entry)
  project <- tempfile("pixi dual runtime ")
  prefix <- file.path(project, ".pixi", "envs", "default")
  dir.create(file.path(prefix, "bin"), recursive = TRUE)
  dir.create(file.path(prefix, "lib", "R", "library"), recursive = TRUE)
  file.create(file.path(prefix, "bin", "python"))
  file.create(file.path(prefix, ".drugsignet-runtime-validated"))
  writeLines("9.9.0", file.path(prefix, ".drugsignet-r-version"))
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  withr::local_libpaths(.libPaths())
  withr::local_envvar(c(DRUGSIGNET_PIXI_PROJECT = NA, RETICULATE_PYTHON = NA,
                       RETICULATE_CONDA = NA, CONDA_PREFIX = NA,
                       DRUGSIGNET_PIXI_VALIDATION_MARKER = NA))
  entry$system2 <- function(...) 0L
  entry$requireNamespace <- function(...) stop("Foreign namespace must not load")
  host <- file.path(project, "host library")
  pixi_library <- file.path(prefix, "lib", "R", "library")
  .libPaths(c(pixi_library, .libPaths()))
  calls <- list()
  entry$.drugsignet_install_host_package <- function(package_ref, host_library,
                                                     build_vignettes, repository_root, ...) {
    expect_false(normalizePath(pixi_library) %in% .libPaths())
    calls$install <<- list(package_ref, host_library, build_vignettes, repository_root)
  }
  entry$.drugsignet_persist_pixi_host_config <- function(..., runtime_version, config_dir = "host") {
    calls[[config_dir]] <<- list(arguments = list(...), version = runtime_version)
  }
  expect_message(result <- entry$install_drugsignet_pixi(
    project_dir = project, repository_root = project, install_host = TRUE,
    host_library = host, build_vignettes = TRUE
  ), "You can now run library")
  expect_identical(result, normalizePath(project))
  expect_identical(calls$install, list("orhanbellur/DrugSigNet", host, TRUE, project))
  expect_identical(calls$host$version, getRversion())
  expect_identical(calls[[normalizePath(project)]]$version, getRversion())
  expect_identical(calls$host$arguments[[3L]], host)
  expect_false(normalizePath(file.path(prefix, "lib", "R", "library")) %in% .libPaths())
  expect_error(entry$install_drugsignet_pixi(project_dir = project,
    repository_root = project, install_host = TRUE,
    host_library = file.path(prefix, "lib", "R", "library")), "outside the Pixi")
  expect_error(entry$install_drugsignet_pixi(install_host = NA), "TRUE or FALSE")
  entry$.drugsignet_install_host_package <- function(...) stop("host build failure")
  expect_error(entry$install_drugsignet_pixi(project_dir = project,
    repository_root = project, install_host = TRUE, host_library = host), "host build failure")
  expect_true(file.exists(file.path(prefix, ".drugsignet-runtime-validated")))
})

test_that("host dependency builds isolate Conda and restore caller settings on failure", {
  skip_if_not_installed("BiocManager")
  skip_if_not_installed("remotes")
  entry <- new.env(parent = globalenv())
  sys.source(drugsignet_repository_file("install-pixi.R"), envir = entry)
  library <- tempfile("host-r-build-")
  dir.create(library)
  on.exit(unlink(library, recursive = TRUE), add = TRUE)
  withr::local_libpaths(.libPaths())
  withr::local_envvar(c(CONDA_PREFIX = "/foreign-pixi", R_LIBS_USER = "/foreign-library"))
  withr::local_options(repos = c(CRAN = "https://example.invalid"), timeout = 123)
  repositories <- NULL
  testthat::local_mocked_bindings(
    repositories = function(version = NULL) {
      repositories <<- version
      c(BioCsoft = "https://bioconductor.org/packages/3.20/bioc", CRAN = "@CRAN@")
    }, .package = "BiocManager"
  )
  testthat::local_mocked_bindings(
    install_local = function(path, lib, dependencies, upgrade, build_vignettes, force) {
      expect_identical(path, "source-fixture")
      expect_identical(lib, library)
      expect_identical(dependencies, NA)
      expect_identical(upgrade, "never")
      expect_true(build_vignettes)
      expect_true(force)
      expect_identical(Sys.getenv("CONDA_PREFIX"), "")
      expect_identical(Sys.getenv("R_LIBS_USER"), "")
      if (entry$.drugsignet_r_minor(getRversion()) == "4.4") {
        expect_identical(unname(getOption("repos")[["CRAN"]]),
                         "https://packagemanager.posit.co/cran/2025-04-30")
      }
      stop("missing host compiler")
    }, .package = "remotes"
  )
  # The package may already be loaded by the test runner; use a controlled view
  # to exercise dependency configuration without unloading user namespaces.
  entry$loadedNamespaces <- function() setdiff(base::loadedNamespaces(), "DrugSigNet")
  expect_error(entry$.drugsignet_install_host_package("owner/repo@ref", library, TRUE,
                                                    "source-fixture"),
               "validated Pixi installation is still available")
  expect_identical(getOption("repos"), c(CRAN = "https://example.invalid"))
  expect_identical(getOption("timeout"), 123)
  expect_identical(Sys.getenv("CONDA_PREFIX"), "/foreign-pixi")
  expect_identical(Sys.getenv("R_LIBS_USER"), "/foreign-library")
  if (entry$.drugsignet_r_minor(getRversion()) == "4.4") expect_identical(repositories, "3.20")
})

test_that("the caller R series selects Pixi and activates it in the same session", {
  entry <- new.env(parent = globalenv())
  sys.source(drugsignet_repository_file("install-pixi.R"), envir = entry)
  project <- tempfile("pixi matching runtime ")
  prefix <- file.path(project, ".pixi", "envs", "default")
  library <- file.path(prefix, "lib", "R", "library")
  dir.create(file.path(prefix, "bin"), recursive = TRUE)
  dir.create(library, recursive = TRUE)
  file.create(file.path(prefix, "bin", "python"))
  file.create(file.path(prefix, ".drugsignet-runtime-validated"))
  writeLines("4.4.3", file.path(prefix, ".drugsignet-r-version"))
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  withr::local_libpaths(.libPaths())
  withr::local_envvar(c(DRUGSIGNET_PIXI_PROJECT = NA, RETICULATE_PYTHON = NA,
                       RETICULATE_CONDA = NA, CONDA_PREFIX = NA,
                       DRUGSIGNET_PIXI_VALIDATION_MARKER = NA))
  entry$getRversion <- function() numeric_version("4.4.0")
  environment <- NULL
  entry$system2 <- function(..., env) { environment <<- env; 0L }
  entry$.drugsignet_can_share_runtime <- function(...) TRUE
  entry$requireNamespace <- function(package, quietly) {
    expect_identical(package, "DrugSigNet")
    expect_identical(.libPaths()[1L], normalizePath(library))
    TRUE
  }
  entry$.drugsignet_install_host_package <- function(...) stop("No second installation should be needed")
  entry$.drugsignet_persist_pixi_host_config <- function(...) TRUE
  for (versions in list(c("4.4.0", "4.4.3"), c("4.5.3", "4.5.3"), c("4.6.0", "4.6.1"))) {
    entry$getRversion <- function() numeric_version(versions[[1L]])
    writeLines(versions[[2L]], file.path(prefix, ".drugsignet-r-version"))
    expect_message(result <- entry$install_drugsignet_pixi(project_dir = project,
      repository_root = project), "can be called directly")
    expect_identical(result, normalizePath(project))
    expect_true(paste0("DRUGSIGNET_PIXI_R_VERSION=", entry$.drugsignet_r_minor(versions[[1L]])) %in% environment)
  }
  entry$getRversion <- function() numeric_version("4.3.3")
  entry$system2 <- function(...) stop("Installer must not launch")
  expect_error(entry$install_drugsignet_pixi(), "requires R >= 4.4.0")
})

test_that("Pixi architecture follows the calling R, including Intel R on Apple Silicon", {
  entry <- new.env(parent = globalenv())
  sys.source(drugsignet_repository_file("install-pixi.R"), envir = entry)
  expect_identical(entry$.drugsignet_pixi_platform("Darwin", "aarch64"), "osx-arm64")
  expect_identical(entry$.drugsignet_pixi_platform("Darwin", "x86_64"), "osx-64")
  expect_identical(entry$.drugsignet_pixi_platform("Linux", "x86_64"), "linux-64")
  expect_identical(entry$.drugsignet_pixi_platform("Linux", "aarch64"), "linux-aarch64")
  expect_error(entry$.drugsignet_pixi_platform("Linux", "unknown"), "Unsupported host R")
})

test_that("native ABI probe failures are isolated from the live R session", {
  entry <- new.env(parent = globalenv())
  sys.source(drugsignet_repository_file("install-pixi.R"), envir = entry)
  before <- .libPaths()
  entry$system2 <- function(command, args, stdout, stderr, ...) {
    expect_true("--vanilla" %in% args)
    writeLines("Rcpp.so: CXXABI_1.3.15 not found; child terminated", stdout)
    139L
  }
  expect_message(
    compatible <- entry$.drugsignet_can_share_runtime("/candidate/library", "/candidate", system = "Linux", r_home = "/host/R"),
    "native host packages"
  )
  expect_false(compatible)
  expect_identical(.libPaths(), before)
  entry$system2 <- function(...) stop("Foreign macOS libraries must not be loaded")
  expect_false(entry$.drugsignet_can_share_runtime("/candidate/library", "/candidate", system = "Darwin", r_home = "/Framework/R"))
  expect_true(entry$.drugsignet_can_share_runtime("/candidate/library", "/candidate", system = "Darwin", r_home = "/candidate/lib/R"))
})

test_that("startup profiles never mix host-native and Pixi R engines", {
  entry <- new.env(parent = globalenv())
  sys.source(drugsignet_repository_file("install-pixi.R"), envir = entry)
  project <- tempfile("native-profile-")
  native <- file.path(project, "native-library")
  pixi <- file.path(project, ".pixi", "envs", "default", "lib", "R", "library")
  dir.create(native, recursive = TRUE)
  dir.create(pixi, recursive = TRUE)
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  withr::local_libpaths(.libPaths())
  entry$.drugsignet_persist_pixi_host_config(project, "python", native, "validated", config_dir = project)
  profile <- file.path(project, ".Rprofile")
  .libPaths(c(pixi, .libPaths()))
  sys.source(profile, envir = new.env(parent = globalenv()))
  expect_identical(.libPaths()[1L], normalizePath(native))
  expect_false(normalizePath(pixi) %in% .libPaths())
  .libPaths(c(pixi, .libPaths()))
  own <- new.env(parent = globalenv())
  own$R.home <- function(...) dirname(pixi)
  sys.source(profile, envir = own)
  expect_false(normalizePath(native) %in% .libPaths())
  expect_true(normalizePath(pixi) %in% .libPaths())
  .libPaths(c(native, pixi, .libPaths()))
  other <- new.env(parent = globalenv())
  other$R.home <- function(...) "/another/R/installation"
  sys.source(profile, envir = other)
  expect_false(normalizePath(native) %in% .libPaths())
  expect_false(normalizePath(pixi) %in% .libPaths())
})

test_that("host libraries are keyed to the actual R installation", {
  entry <- new.env(parent = globalenv())
  sys.source(drugsignet_repository_file("install-pixi.R"), envir = entry)
  first <- entry$.drugsignet_default_host_library()
  expect_identical(first, entry$.drugsignet_default_host_library())
  other <- tempfile("other-r-home-")
  dir.create(other)
  on.exit(unlink(other, recursive = TRUE), add = TRUE)
  entry$R.home <- function(...) other
  expect_false(identical(first, entry$.drugsignet_default_host_library()))
})

test_that("ABI-incompatible runtimes automatically install for the original R without a flag", {
  entry <- new.env(parent = globalenv())
  sys.source(drugsignet_repository_file("install-pixi.R"), envir = entry)
  project <- tempfile("automatic-native-")
  prefix <- file.path(project, ".pixi", "envs", "default")
  pixi <- file.path(prefix, "lib", "R", "library")
  native <- file.path(project, "native")
  dir.create(file.path(prefix, "bin"), recursive = TRUE)
  dir.create(pixi, recursive = TRUE)
  file.create(file.path(prefix, "bin", "python"))
  file.create(file.path(prefix, ".drugsignet-runtime-validated"))
  writeLines(as.character(getRversion()), file.path(prefix, ".drugsignet-r-version"))
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  withr::local_libpaths(.libPaths())
  withr::local_envvar(c(DRUGSIGNET_PIXI_PROJECT = NA, RETICULATE_PYTHON = NA,
                       RETICULATE_CONDA = NA, CONDA_PREFIX = NA,
                       DRUGSIGNET_PIXI_VALIDATION_MARKER = NA))
  .libPaths(c(pixi, .libPaths()))
  entry$system2 <- function(...) 0L
  entry$.drugsignet_can_share_runtime <- function(...) FALSE
  entry$requireNamespace <- function(...) stop("The live R must not probe foreign DLLs")
  installed <- FALSE
  entry$.drugsignet_install_host_package <- function(package_ref, host_library, build_vignettes,
                                                     repository_root, runtime_prefix, source_archive,
                                                     full_runtime) {
    installed <<- TRUE
    expect_identical(host_library, native)
    expect_identical(runtime_prefix, prefix)
    expect_identical(source_archive, file.path(project, "DrugSigNet-source.tar.gz"))
    if (requireNamespace("remotes", quietly = TRUE)) {
      writeLines("archive fixture", source_archive)
      downloaded <- remotes:::remote_download(remotes:::local_remote(source_archive))
      on.exit(unlink(dirname(downloaded), recursive = TRUE), add = TRUE)
      expect_true(file.exists(downloaded))
      expect_identical(readLines(downloaded), "archive fixture")
    }
    expect_true(full_runtime)
    expect_false(normalizePath(pixi) %in% .libPaths())
    dir.create(native)
    .libPaths(c(native, .libPaths()))
  }
  entry$.drugsignet_persist_pixi_host_config <- function(...) TRUE
  expect_message(result <- entry$install_drugsignet_pixi(project_dir = project,
    repository_root = project, host_library = native), "You can now run library")
  expect_true(installed)
  expect_identical(result, normalizePath(project))
  expect_identical(.libPaths()[1L], normalizePath(native))
})

test_that("native Java configure variables use real compiler paths rather than literal quotes", {
  skip_if_not_installed("BiocManager")
  skip_if_not_installed("remotes")
  entry <- new.env(parent = globalenv())
  sys.source(drugsignet_repository_file("install-pixi.R"), envir = entry)
  entry$.drugsignet_prepare_host_sdk <- function(...) character()
  project <- tempfile("native-java-")
  dir.create(file.path(project, "bin"), recursive = TRUE)
  file.create(file.path(project, "bin", "pandoc"))
  withr::local_envvar(c(RSTUDIO_PANDOC = "/original-pandoc"))
  descriptor <- file.path(project, "lib", "R", "library", "DrugSigNet", "DESCRIPTION")
  dir.create(dirname(descriptor), recursive = TRUE)
  writeLines(c("Package: DrugSigNet", "Imports: stats"), descriptor)
  archive <- file.path(project, "DrugSigNet-source.tar.gz")
  file.create(archive)
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  withr::local_libpaths(.libPaths())
  entry$loadedNamespaces <- function() setdiff(base::loadedNamespaces(), "DrugSigNet")
  testthat::local_mocked_bindings(
    repositories = function(...) c(CRAN = "https://example.invalid"), .package = "BiocManager"
  )
  testthat::local_mocked_bindings(
    install.packages = function(pkgs, ..., configure.vars = NULL, configure.args = NULL) {
      if (!identical(pkgs, "rJava")) return(invisible(NULL))
      expect_identical(configure.args, "--disable-jri")
      expect_identical(Sys.getenv("RSTUDIO_PANDOC"), file.path(project, "bin"))
      # R passes this string through a shell. Embedded quotes in CPPFLAGS
      # remain literal compiler-path characters after variable expansion.
      command <- paste(configure.vars,
                       "; printf '%s\\n' \"$JAVA\" \"$JAVA_CPPFLAGS\" \"$JAVA_LIBS\"")
      values <- system2("sh", c("-c", shQuote(command)), stdout = TRUE)
      java <- file.path(project, "lib", "jvm")
      expect_identical(values[[1L]], file.path(java, "bin", "java"))
      expect_match(values[[2L]], paste0("-I", java, "/include"), fixed = TRUE)
      expect_false(grepl("['\"]", values[[2L]]))
      expect_false(grepl("['\"]", values[[3L]]))
      stop("configure captured")
    }, .package = "utils"
  )
  expect_error(entry$.drugsignet_install_host_package("owner/repo", file.path(project, "native"),
    TRUE, runtime_prefix = project, source_archive = archive, full_runtime = TRUE), "configure captured")
  expect_identical(Sys.getenv("RSTUDIO_PANDOC"), "/original-pandoc")
})


test_that("missing native headers are detected without installing OS packages", {
  entry <- new.env(parent = globalenv())
  sys.source(drugsignet_repository_file("install-pixi.R"), envir = entry)
  entry$Sys.which <- function(...) ""
  expect_identical(entry$.drugsignet_missing_host_headers(character(), "Linux"),
                   c("fontconfig", "freetype2", "libpng", "libxml-2.0", "zlib"))
  expect_identical(entry$.drugsignet_missing_host_headers(
    c("systemfonts", "png", "XML", "XVector", "Rhdf5lib"), "Linux"), character())
  expect_identical(entry$.drugsignet_missing_host_headers(character(), "Darwin"), character())
  entry$Sys.which <- function(...) "/native/pkg-config"
  entry$system2 <- function(command, args, ...) {
    expect_identical(command, "/native/pkg-config")
    if (grepl("libpng", args[[2L]], fixed = TRUE)) 1L else 0L
  }
  expect_identical(entry$.drugsignet_missing_host_headers(character(), "Linux"), "libpng")
})

test_that("native SDK dependency traversal excludes libc and compiler runtimes", {
  entry <- new.env(parent = globalenv())
  sys.source(drugsignet_repository_file("install-pixi.R"), envir = entry)
  graph <- list("libpng-dev" = c("libpng16-16", "zlib1g-dev", "libc6-dev", "<libc-dev>"),
                "libpng16-16" = c("zlib1g", "libc6"),
                "zlib1g-dev" = c("zlib1g", "libc6-dev"),
                "zlib1g" = c("libgcc-s1", "libstdc++6", "libgfortran5", "libgomp1", "libc6"))
  packages <- entry$.drugsignet_native_sdk_packages("libpng-dev", function(package) graph[[package]])
  expect_setequal(packages, c("libpng-dev", "libpng16-16", "zlib1g-dev", "zlib1g"))
  expect_identical(length(packages), length(unique(packages)))
})

test_that("the private native SDK downloads and extracts without OS installation", {
  entry <- new.env(parent = globalenv())
  sys.source(drugsignet_repository_file("install-pixi.R"), envir = entry)
  project <- tempfile("rootless-sdk-")
  prefix <- file.path(project, "pixi")
  host <- file.path(project, "host", "library")
  dir.create(file.path(prefix, "bin"), recursive = TRUE)
  file.create(file.path(prefix, "bin", "pkg-config"))
  dir.create(host, recursive = TRUE)
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  withr::local_envvar(c(PATH = Sys.getenv("PATH"), CPATH = NA, LIBRARY_PATH = NA,
    CPPFLAGS = NA, LDFLAGS = NA, PKG_CONFIG_PATH = NA, PKG_CONFIG_LIBDIR = NA,
    PKG_CONFIG_SYSROOT_DIR = NA))
  original_directory <- getwd()
  unpacked <- FALSE
  calls <- list()
  entry$.drugsignet_missing_host_headers <- function(...) {
    if (unpacked && nzchar(Sys.getenv("PKG_CONFIG_SYSROOT_DIR"))) character() else "libpng"
  }
  entry$Sys.which <- function(commands) setNames(paste0("/native/", commands), commands)
  entry$file.exists <- function(path) ifelse(path %in% c("/etc/os-release", "/etc/apt/sources.list"), TRUE, base::file.exists(path))
  entry$readLines <- function(con, ...) {
    if (identical(con, "/etc/os-release")) return(c("ID=ubuntu", 'VERSION_ID="22.04"'))
    base::readLines(con, ...)
  }
  entry$file.copy <- function(from, to, ...) {
    writeLines("deb https://archive.ubuntu.com/ubuntu jammy main", to)
    TRUE
  }
  entry$file.symlink <- function(from, to) { writeLines(from, to); TRUE }
  entry$system2 <- function(command, args, stdout = NULL, ...) {
    if (endsWith(command, "/dpkg")) return("amd64")
    calls[[length(calls) + 1L]] <<- list(command = command, args = args)
    if (endsWith(command, "/apt-cache")) {
      writeLines(if ("policy" %in% args) "  Candidate: 1.0" else "  Depends: libc6", stdout)
    } else if (endsWith(command, "/apt-get")) {
      if ("download" %in% args) file.create("native-files_1.0_amd64.deb")
      writeLines("Downloaded", stdout)
    } else if (endsWith(command, "/dpkg-deb")) {
      expect_identical(args[[1L]], "--extract")
      root <- Sys.getenv("PKG_CONFIG_SYSROOT_DIR")
      dir.create(file.path(root, "usr", "bin"), recursive = TRUE)
      writeLines(c("#!/bin/sh", "prefix=/usr", 'echo /usr/include/libpng16'),
                 file.path(root, "usr", "bin", "libpng-config"))
      unpacked <<- TRUE
      writeLines("Extracted", stdout)
    }
    0L
  }
  settings <- entry$.drugsignet_prepare_host_sdk(prefix, host)
  expect_identical(getwd(), original_directory)
  expect_true(any(grepl("-Wl,-rpath,", settings, fixed = TRUE)))
  expect_true(all(vapply(calls, function(call) !any(call$args %in% c("install", "sudo")), logical(1))))
  apt <- Filter(function(call) endsWith(call$command, "/apt-get"), calls)
  expect_identical(length(apt), 2L)
  expect_true(any(apt[[1L]]$args == "update"))
  expect_true(any(apt[[2L]]$args == "download"))
  expect_true(any(grepl("Dir::State=", apt[[1L]]$args, fixed = TRUE)))
  expect_true(any(grepl("Dir::Cache=", apt[[1L]]$args, fixed = TRUE)))
  root <- Sys.getenv("PKG_CONFIG_SYSROOT_DIR")
  tools <- file.path(root, "tools")
  expect_setequal(list.files(tools), c("pkg-config", "libpng-config"))
  expect_identical(readLines(file.path(tools, "libpng-config"))[[1L]], "#!/bin/sh")
  expect_match(readLines(file.path(tools, "libpng-config"))[[3L]],
               file.path(root, "usr", "include"), fixed = TRUE)
  expect_true(file.exists(file.path(root, ".ready")))
})
