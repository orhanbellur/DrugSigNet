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

test_that("cross-version installation succeeds without loading foreign R packages", {
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
  expect_message(
    result <- entry$install_drugsignet_pixi(project_dir = project, repository_root = project),
    "use Pixi R for this project"
  )
  expect_identical(result, normalizePath(project))
  expect_true(persisted)
  expect_false(normalizePath(file.path(prefix, "lib", "R", "library")) %in% .libPaths())
  unlink(version)
  expect_error(entry$install_drugsignet_pixi(project_dir = project, repository_root = project),
               "version metadata is missing")
})
