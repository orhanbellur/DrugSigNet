test_that("Pixi installs Apple Silicon Bioconductor packages before vignette builds", {
  bash <- Sys.which("bash")
  skip_if(!nzchar(bash), "Bash is required to exercise the shell installer")
  root <- drugsignet_repository_root()
  for (case in list(c("osx-arm64", "4.4"), c("osx-arm64", "4.5"),
                    c("osx-64", "4.4"), c("linux-64", "4.4"), c("linux-64", "4.5"),
                    c("linux-64", "4.4", "4.5"), c("linux-64", "4.6"),
                    c("linux-aarch64", "4.4"))) {
    platform <- case[[1L]]
    r_series <- case[[2L]]
    source_r_deps <- platform %in% c("osx-arm64", "linux-aarch64") || !r_series %in% c("4.4", "4.5")
    project <- tempfile("pixi-platform-")
    dir.create(project)
    on.exit(unlink(project, recursive = TRUE), add = TRUE)
    sdk <- file.path(project, "MacOSX.sdk")
    dir.create(file.path(sdk, "usr", "include"), recursive = TRUE)
    runtime <- file.path(project, ".pixi", "envs", "default", "bin")
    dir.create(runtime, recursive = TRUE)
    file.create(file.path(project, ".pixi", ".drugsignet-post-link-enabled"))
    previous_series <- if (length(case) == 3L) case[[3L]] else r_series
    writeLines(paste0(previous_series, ".3"), file.path(dirname(runtime), ".drugsignet-r-version"))
    file.create(file.path(project, "user-results.txt"))
    file.create(file.path(dirname(runtime), "old-package-sentinel"))
    for (name in c("conda", "bash")) {
      writeLines(c("#!/bin/sh", "exit 0"), file.path(runtime, name))
      Sys.chmod(file.path(runtime, name), "0755")
    }
    java <- file.path(dirname(runtime), "lib", "jvm", "bin")
    dir.create(java, recursive = TRUE)
    writeLines(c("#!/bin/sh", "exit 0"), file.path(java, "java"))
    Sys.chmod(file.path(java, "java"), "0755")
    pixi_home <- file.path(project, "fake-pixi")
    dir.create(file.path(pixi_home, "bin"), recursive = TRUE)
    fake <- file.path(pixi_home, "bin", "pixi")
    writeLines(c(
      "#!/bin/bash", "set -eu",
      'printf "%s\\n" "$*" >> "$DRUGSIGNET_PIXI_PROJECT/calls"',
      'if [[ "$*" == *"R CMD javareconf"* ]]; then printf "%s\\n" "$@" > "$DRUGSIGNET_PIXI_PROJECT/java-arguments"; fi',
      'if [[ "$1" == install ]]; then',
      '  bin="$DRUGSIGNET_PIXI_PROJECT/.pixi/envs/default/bin"',
      '  if [[ ! -d "$bin" ]]; then',
      '    mkdir -p "$bin"',
      '    for name in bash conda; do printf "#!/bin/sh\\nexit 0\\n" > "$bin/$name"; chmod +x "$bin/$name"; done',
      '  fi',
      'fi',
      'if [[ "$*" == *"Rscript --vanilla -"* ]]; then',
      '  cat > "$DRUGSIGNET_PIXI_PROJECT/last.R"',
      '  if grep -q bioc_version "$DRUGSIGNET_PIXI_PROJECT/last.R"; then',
      '    cp "$DRUGSIGNET_PIXI_PROJECT/last.R" "$DRUGSIGNET_PIXI_PROJECT/bioconductor.R"',
      '  elif grep -q ggplot2 "$DRUGSIGNET_PIXI_PROJECT/last.R"; then',
      '    cp "$DRUGSIGNET_PIXI_PROJECT/last.R" "$DRUGSIGNET_PIXI_PROJECT/plotting.R"',
      '  else',
      '    cp "$DRUGSIGNET_PIXI_PROJECT/last.R" "$DRUGSIGNET_PIXI_PROJECT/boutros.R"',
      '  fi',
      'fi',
      '# Stop at the build boundary: no network, packages, or host changes.',
      'if [[ "$*" == *"R CMD build"* ]]; then exit 73; fi',
      'if [[ "$*" == *"Rscript --vanilla -" && -n "${JAVA_HOME:-}${R_JAVA_LD_LIBRARY_PATH:-}" ]]; then exit 74; fi',
      'exit 0'
    ), fake)
    Sys.chmod(fake, "0755")
    output <- suppressWarnings(system2(bash, c(
      shQuote(file.path(root, "tools", "install_drugsignet_pixi.sh")),
      "orhanbellur/DrugSigNet@fixture"
    ), stdout = TRUE, stderr = TRUE, env = c(
      paste0("PIXI_HOME=", shQuote(pixi_home)),
      paste0("DRUGSIGNET_PIXI_PROJECT=", shQuote(project)),
      paste0("DRUGSIGNET_PIXI_PLATFORM=", platform),
      paste0("DRUGSIGNET_PIXI_R_VERSION=", r_series),
      paste0("DRUGSIGNET_MACOS_SDKROOT=", shQuote(sdk)),
      "JAVA_HOME=/host-java-must-not-leak",
      "R_JAVA_LD_LIBRARY_PATH=/host-java-must-not-leak/lib/server",
      "DRUGSIGNET_BUILD_VIGNETTES=true"
    )))
    expect_equal(attr(output, "status"), 73L, info = paste(output, collapse = "\n"))
    expect_true(file.exists(file.path(project, "user-results.txt")))
    expect_identical(file.exists(file.path(dirname(runtime), "old-package-sentinel")), previous_series == r_series)
    manifest <- readLines(file.path(project, "pixi.toml"))
    expect_true(any(grepl(paste0('"r-base" = "', r_series, '.*"'), manifest, fixed = TRUE)))
    expect_true(any(grepl("[activation.env]", manifest, fixed = TRUE)))
    expect_true(any(grepl(paste0('JAVA_HOME = "', project, '/.pixi/envs/default/lib/jvm"'), manifest, fixed = TRUE)))
    if (startsWith(platform, "osx-")) {
      expect_true(any(grepl(paste0('SDKROOT = "', sdk, '"'), manifest, fixed = TRUE)))
      expect_true(any(grepl(paste0('CONDA_BUILD_SYSROOT = "', sdk, '"'), manifest, fixed = TRUE)))
    }
    expect_identical(any(grepl('"libintl-devel"', manifest, fixed = TRUE)), startsWith(platform, "osx-"))
    expect_identical(file.exists(file.path(runtime, "arch")), startsWith(platform, "osx-"))
    expect_identical(any(grepl('"r-boutroslabplottinggeneral"', manifest, fixed = TRUE)), r_series == "4.5")
    expect_identical(file.exists(file.path(project, "plotting.R")), r_series == "4.4")
    expect_identical(file.exists(file.path(project, "boutros.R")), !r_series %in% c("4.4", "4.5"))
    if (!r_series %in% c("4.4", "4.5")) {
      expect_false(any(grepl('"r-wordcloud"', manifest, fixed = TRUE)))
      extra <- paste(readLines(file.path(project, "boutros.R")), collapse = "\n")
      expect_match(extra, '"wordcloud"', fixed = TRUE)
      expect_match(extra, 'fields = "Imports"', fixed = TRUE)
      expect_lt(as.integer(regexpr('install.packages("codetools"', extra, fixed = TRUE)),
                as.integer(regexpr("install.packages(required", extra, fixed = TRUE)))
    }
    expect_identical(any(grepl("R_MAKEVARS_USER =", manifest, fixed = TRUE)), r_series == "4.4")
    if (r_series == "4.4") {
      expect_identical(readLines(file.path(project, ".R", "Makevars-r44")), "CXX11STD = -std=gnu++14")
    }
    if (r_series == "4.4") {
      expect_true(any(grepl('"r-ggplot2" = "<4"', manifest, fixed = TRUE)))
      plotting <- paste(readLines(file.path(project, "plotting.R")), collapse = "\n")
      expect_match(plotting, "2025-04-30", fixed = TRUE)
      expect_match(plotting, 'type = "source"', fixed = TRUE)
      expect_match(plotting, "BoutrosLab.plotting.general", fixed = TRUE)
    }
    bioc <- grepl('^"bioconductor-', manifest)
    expect_equal(sum(bioc), if (source_r_deps) 0L else 2L)
    expect_equal(any(grepl('^"r-rcdk"', manifest)), !source_r_deps)
    script <- file.path(project, "bioconductor.R")
    expect_identical(file.exists(script), source_r_deps)
    if (source_r_deps) {
      profile <- file.path(project, ".Rprofile")
      library <- file.path(dirname(runtime), "lib", "R", "library")
      dir.create(library, recursive = TRUE)
      version_file <- file.path(dirname(runtime), ".drugsignet-r-version")
      unlink(version_file)
      previous <- .libPaths()
      withr::local_libpaths(previous)
      expect_silent(sys.source(profile, envir = new.env(parent = globalenv())))
      expect_false(normalizePath(library) %in% .libPaths())
      writeLines("9.9.0", version_file)
      sys.source(profile, envir = new.env(parent = globalenv()))
      expect_false(normalizePath(library) %in% .libPaths())
      writeLines(as.character(getRversion()), version_file)
      sys.source(profile, envir = new.env(parent = globalenv()))
      expect_false(normalizePath(library) %in% .libPaths())
      own_runtime <- new.env(parent = globalenv())
      own_runtime$R.home <- function(...) file.path(dirname(runtime), "lib", "R")
      sys.source(profile, envir = own_runtime)
      expect_true(normalizePath(library) %in% .libPaths())
      .libPaths(previous)
      expect_silent(parse(script))
      code <- paste(readLines(script), collapse = "\n")
      expect_match(code, 'version = bioc_version', fixed = TRUE)
      expect_match(code, 'if (r_series == "4.4") "3.20"', fixed = TRUE)
      expect_match(code, 'install.packages("BH", repos = "https://cloud.r-project.org"', fixed = TRUE)
      expect_match(code, '"signatureSearchData"', fixed = TRUE)
      expect_match(code, 'stop("Pixi Bioconductor installation failed:', fixed = TRUE)
      expect_match(code, 'install.packages(cran_missing)', fixed = TRUE)
      expect_match(code, 'rcdk::get.fingerprint', fixed = TRUE)
      expect_match(code, 'Pixi Java isolation failed:', fixed = TRUE)
      expect_match(code, 'stop("Pixi CRAN installation failed:', fixed = TRUE)
      calls <- readLines(file.path(project, "calls"))
      java_args <- readLines(file.path(project, "java-arguments"))
      expect_lt(which(java_args == "env"), which(java_args == "R"))
      expect_identical(utils::tail(java_args, 3L), c("R", "CMD", "javareconf"))
      flags <- grep("^JAVA_LIBS=", java_args, value = TRUE)
      expect_length(flags, 1L)
      expect_match(flags, " -Wl,-rpath,", fixed = TRUE)
      expect_lt(grep("R CMD javareconf", calls), utils::tail(grep("Rscript --vanilla -$", calls), 1L))
      expect_lt(utils::tail(grep("Rscript --vanilla -$", calls), 1L), grep("R CMD build", calls))
    }
  }
})
