# Repository-only tests exercise bootstrap and CI files that are deliberately
# excluded from the source package.  They run in a Git checkout, while package
# checks skip them explicitly instead of assuming tests/testthat/../../ is the
# checkout root (it is the *.Rcheck directory during R CMD check).
drugsignet_repository_root <- function() {
  candidates <- unique(normalizePath(c(
    Sys.getenv("DRUGSIGNET_SOURCE_DIR"),
    getwd(),
    file.path(getwd(), ".."),
    file.path(getwd(), "..", "..")
  )[nzchar(c(
    Sys.getenv("DRUGSIGNET_SOURCE_DIR"),
    getwd(),
    file.path(getwd(), ".."),
    file.path(getwd(), "..", "..")
  ))], mustWork = FALSE))

  matches <- candidates[
    file.exists(file.path(candidates, ".git", "HEAD")) &
      file.exists(file.path(candidates, "DESCRIPTION"))
  ]
  if (!length(matches)) {
    testthat::skip(
      "repository-only bootstrap/CI files are unavailable in the built package"
    )
  }
  matches[[1L]]
}

drugsignet_repository_file <- function(...) {
  file.path(drugsignet_repository_root(), ...)
}
