#' Open an installed DrugSigNet vignette in RStudio Server
#'
#' Copies the installed vignette documentation to a session-owned temporary
#' directory before opening it. This avoids the RStudio Server `/library/`
#' route, which can return HTTP 404 for packages installed system-wide even
#' when their HTML vignette files exist and are readable from R.
#'
#' @param vignette Vignette basename, without `.html`. Use `"index"` for the
#'   complete vignette list.
#' @param launch Logical; open the copied HTML with the configured R viewer.
#' @return Invisibly, the copied HTML file path.
#' @export
browse_drugsignet_vignette <- function(vignette = "index", launch = TRUE) {
  if (!is.character(vignette) || length(vignette) != 1L || is.na(vignette)) {
    stop("`vignette` must be one non-missing character string.", call. = FALSE)
  }
  vignette <- sub("[.]html$", "", vignette)
  if (!grepl("^[A-Za-z0-9_-]+$", vignette)) {
    stop("`vignette` must be a vignette basename without a path.", call. = FALSE)
  }

  source_dir <- system.file("doc", package = "DrugSigNet")
  source_file <- file.path(source_dir, paste0(vignette, ".html"))
  if (!nzchar(source_dir) || !file.exists(source_file)) {
    stop(
      "Installed vignette not found: ", vignette,
      ". Reinstall DrugSigNet with `pkg.build_vignettes = TRUE`.",
      call. = FALSE
    )
  }

  target_dir <- file.path(tempdir(), "DrugSigNet-vignettes")
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  copied <- file.copy(
    list.files(source_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE),
    target_dir,
    recursive = TRUE,
    overwrite = TRUE
  )
  if (!all(copied)) {
    stop("Could not copy the installed vignette files to the session directory.", call. = FALSE)
  }
  target_file <- file.path(target_dir, basename(source_file))

  if (isTRUE(launch)) {
    viewer <- getOption("viewer")
    if (is.function(viewer)) viewer(target_file) else utils::browseURL(target_file)
  }
  invisible(target_file)
}
