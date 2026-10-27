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
  calls <- list()
  entry$.drugsignet_install_host_package <- function(package_ref, host_library,
                                                     build_vignettes, repository_root) {
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
  entry$requireNamespace <- function(package, quietly) {
    expect_identical(package, "DrugSigNet")
    expect_identical(.libPaths()[1L], normalizePath(library))
    TRUE
  }
  entry$.drugsignet_install_host_package <- function(...) stop("No second installation should be needed")
  entry$.drugsignet_persist_pixi_host_config <- function(...) TRUE
  expect_message(result <- entry$install_drugsignet_pixi(project_dir = project,
    repository_root = project), "can be called directly")
  expect_identical(result, normalizePath(project))
  expect_true("DRUGSIGNET_PIXI_R_VERSION=4.4" %in% environment)
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
