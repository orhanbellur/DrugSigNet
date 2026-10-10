test_that("shared runtime manifest drives every installer", {
  root <- testthat::test_path("..", "..")
  manifest <- read.delim(
    file.path(root, "tools", "runtime-dependencies.tsv"),
    comment.char = "#", header = FALSE,
    col.names = c("manager", "package")
  )

  expect_true(all(c("apt", "conda", "pip") %in% manifest$manager))
  expect_true(all(c("libglpk-dev", "zlib1g-dev") %in% manifest$package))
  expect_true(all(c("r-base=4.5", "graph-tool", "libxml2-devel") %in% manifest$package))
  expect_true(all(c(
    "bash", "coreutils", "diffutils", "file", "findutils", "gawk", "git",
    "grep", "gzip", "patch", "sed", "tar", "which"
  ) %in% manifest$package[manifest$manager == "conda"]))
  expect_true("r-boutroslabplottinggeneral" %in%
    manifest$package[manifest$manager == "pixi"])
  expect_true(all(c(
    "r-data.table", "r-dplyr", "r-enrichr", "r-ggplot2", "r-igraph",
    "r-plotly", "r-rcdk", "r-reticulate", "r-tidyr", "r-wordcloud"
  ) %in% manifest$package[manifest$manager == "conda"]))

  consumers <- c(
    "install.sh", "tools/install_linux_system_dependencies.sh",
    "tools/install_drugsignet_micromamba.sh", "docker/linux/Dockerfile",
    "docker/linux/Dockerfile.runtime"
  )
  contents <- vapply(consumers, function(path) {
    paste(readLines(file.path(root, path), warn = FALSE), collapse = "\n")
  }, character(1))
  expect_true(all(grepl("runtime-dependencies.tsv", contents, fixed = TRUE)))

  micromamba_installer <- contents[["tools/install_drugsignet_micromamba.sh"]]
  expect_match(micromamba_installer, "environment[.]yml")
  expect_match(micromamba_installer, "mktemp -d", fixed = TRUE)
  expect_match(micromamba_installer, "env -u R_HOME", fixed = TRUE)
  expect_match(micromamba_installer, "XML_CONFIG=", fixed = TRUE)
  expect_match(micromamba_installer, '"${micromamba}" install', fixed = TRUE)
  expect_match(micromamba_installer, "pip install --no-cache-dir", fixed = TRUE)
  expect_match(micromamba_installer, "importlib.metadata.version", fixed = TRUE)
  expect_match(micromamba_installer, "DRUGSIGNET_DEPENDENCY_MANIFEST_URL", fixed = TRUE)
  expect_match(micromamba_installer, 'BASH_SOURCE[0]:-', fixed = TRUE)
  expect_false(grepl("printf '%s\\n' '  - pip:'", micromamba_installer, fixed = TRUE))
})
