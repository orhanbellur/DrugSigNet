test_that("shared runtime manifest drives every installer", {
  root <- testthat::test_path("..", "..")
  manifest <- read.delim(
    file.path(root, "tools", "runtime-dependencies.tsv"),
    comment.char = "#", header = FALSE,
    col.names = c("manager", "package")
  )

  expect_true(all(c("apt", "conda", "pip") %in% manifest$manager))
  expect_true(all(c("libglpk-dev", "zlib1g-dev") %in% manifest$package))
  expect_true(all(c("r-base=4.5", "graph-tool") %in% manifest$package))

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
})
