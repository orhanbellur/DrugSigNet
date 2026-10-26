test_that("Pixi installs Apple Silicon Bioconductor packages before vignette builds", {
  bash <- Sys.which("bash")
  skip_if(!nzchar(bash), "Bash is required to exercise the shell installer")
  root <- drugsignet_repository_root()
  for (platform in c("osx-arm64", "osx-64", "linux-64")) {
    project <- tempfile("pixi-platform-")
    dir.create(project)
    on.exit(unlink(project, recursive = TRUE), add = TRUE)
    runtime <- file.path(project, ".pixi", "envs", "default", "bin")
    dir.create(runtime, recursive = TRUE)
    file.create(file.path(project, ".pixi", ".drugsignet-post-link-enabled"))
    for (name in c("conda", "bash")) {
      writeLines(c("#!/bin/sh", "exit 0"), file.path(runtime, name))
      Sys.chmod(file.path(runtime, name), "0755")
    }
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
      "DRUGSIGNET_BUILD_VIGNETTES=true"
    )))
    expect_equal(attr(output, "status"), 73L, info = paste(output, collapse = "\n"))
    manifest <- readLines(file.path(project, "pixi.toml"))
    bioc <- grepl('^"bioconductor-', manifest)
    expect_equal(sum(bioc), if (platform == "osx-arm64") 0L else 2L)
    script <- file.path(project, "bioconductor.R")
    expect_identical(file.exists(script), platform == "osx-arm64")
    if (platform == "osx-arm64") {
      expect_silent(parse(script))
      code <- paste(readLines(script), collapse = "\n")
      expect_match(code, 'version = "3.22"', fixed = TRUE)
      expect_match(code, '"signatureSearchData"', fixed = TRUE)
      expect_match(code, 'stop("Apple Silicon Bioconductor installation failed:', fixed = TRUE)
      calls <- readLines(file.path(project, "calls"))
      expect_lt(grep("Rscript --vanilla -$", calls), grep("R CMD build", calls))
    }
  }
})
