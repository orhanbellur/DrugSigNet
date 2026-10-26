test_that("Pixi installs Apple Silicon Bioconductor packages before vignette builds", {
  bash <- Sys.which("bash")
  skip_if(!nzchar(bash), "Bash is required to exercise the shell installer")
  root <- drugsignet_repository_root()
  for (platform in c("osx-arm64", "osx-64", "linux-64")) {
    project <- tempfile("pixi-platform-")
    dir.create(project)
    on.exit(unlink(project, recursive = TRUE), add = TRUE)
    sdk <- file.path(project, "MacOSX.sdk")
    dir.create(file.path(sdk, "usr", "include"), recursive = TRUE)
    runtime <- file.path(project, ".pixi", "envs", "default", "bin")
    dir.create(runtime, recursive = TRUE)
    file.create(file.path(project, ".pixi", ".drugsignet-post-link-enabled"))
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
      'if [[ "$*" == *"Rscript --vanilla -" ]]; then',
      '  cat > "$DRUGSIGNET_PIXI_PROJECT/bioconductor.R"',
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
      paste0("DRUGSIGNET_MACOS_SDKROOT=", shQuote(sdk)),
      "JAVA_HOME=/host-java-must-not-leak",
      "R_JAVA_LD_LIBRARY_PATH=/host-java-must-not-leak/lib/server",
      "DRUGSIGNET_BUILD_VIGNETTES=true"
    )))
    expect_equal(attr(output, "status"), 73L, info = paste(output, collapse = "\n"))
    manifest <- readLines(file.path(project, "pixi.toml"))
    expect_true(any(grepl("[activation.env]", manifest, fixed = TRUE)))
    expect_true(any(grepl(paste0('JAVA_HOME = "', project, '/.pixi/envs/default/lib/jvm"'), manifest, fixed = TRUE)))
    if (startsWith(platform, "osx-")) {
      expect_true(any(grepl(paste0('SDKROOT = "', sdk, '"'), manifest, fixed = TRUE)))
      expect_true(any(grepl(paste0('CONDA_BUILD_SYSROOT = "', sdk, '"'), manifest, fixed = TRUE)))
    }
    expect_identical(any(grepl('"libintl-devel"', manifest, fixed = TRUE)), startsWith(platform, "osx-"))
    expect_identical(file.exists(file.path(runtime, "arch")), startsWith(platform, "osx-"))
    bioc <- grepl('^"bioconductor-', manifest)
    expect_equal(sum(bioc), if (platform == "osx-arm64") 0L else 2L)
    expect_equal(any(grepl('^"r-rcdk"', manifest)), platform != "osx-arm64")
    script <- file.path(project, "bioconductor.R")
    expect_identical(file.exists(script), platform == "osx-arm64")
    if (platform == "osx-arm64") {
      profile <- file.path(project, ".Rprofile")
      library <- file.path(dirname(runtime), "lib", "R", "library")
      dir.create(library, recursive = TRUE)
      version_file <- file.path(dirname(runtime), ".drugsignet-r-version")
      previous <- .libPaths()
      withr::local_libpaths(previous)
      expect_silent(sys.source(profile, envir = new.env(parent = globalenv())))
      expect_false(normalizePath(library) %in% .libPaths())
      writeLines("9.9.0", version_file)
      sys.source(profile, envir = new.env(parent = globalenv()))
      expect_false(normalizePath(library) %in% .libPaths())
      writeLines(as.character(getRversion()), version_file)
      sys.source(profile, envir = new.env(parent = globalenv()))
      expect_true(normalizePath(library) %in% .libPaths())
      .libPaths(previous)
      expect_silent(parse(script))
      code <- paste(readLines(script), collapse = "\n")
      expect_match(code, 'version = "3.22"', fixed = TRUE)
      expect_match(code, '"signatureSearchData"', fixed = TRUE)
      expect_match(code, 'stop("Apple Silicon Bioconductor installation failed:', fixed = TRUE)
      expect_match(code, 'install.packages(cran_missing)', fixed = TRUE)
      expect_match(code, 'rcdk::get.fingerprint', fixed = TRUE)
      expect_match(code, 'Pixi Java isolation failed:', fixed = TRUE)
      expect_match(code, 'stop("Apple Silicon CRAN installation failed:', fixed = TRUE)
      calls <- readLines(file.path(project, "calls"))
      expect_lt(grep("R CMD javareconf", calls), grep("Rscript --vanilla -$", calls))
      expect_lt(grep("Rscript --vanilla -$", calls), grep("R CMD build", calls))
    }
  }
})
