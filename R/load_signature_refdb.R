#' @title Download and Cache Frozen Signature Reference Databases
#'
#' @description
#' Downloads frozen DrugSigNet signature reference databases from Synapse and
#' caches them in the user cache directory.
#'
#' @details
#' `load_signature_refdb()` retrieves a fixed HDF5 reference database for
#' signature-based drug searching. The returned file path can be supplied to
#' DrugSigNet signature methods, such as `cmap_method()`, `lincs_method()`,
#' `gcmap_method()`, and `correlation_method()`, when a reproducible frozen
#' reference database is preferred over package defaults.
#'
#' Cached files are stored under `tools::R_user_dir("DrugSigNet", "cache")`.
#' On each call, the function compares the cached metadata with the current
#' Synapse file version. If the cached version is current, the local file is
#' reused. If the remote version has changed, or if `force = TRUE`, the file is
#' downloaded again and the cache metadata is updated.
#'
#' When `validate_hdf5 = TRUE`, the downloaded or cached file is checked with
#' `rhdf5::h5ls()` before the path is returned.
#'
#' @param refdb Reference database to download. One of `"cmap"` or `"lincs2"`.
#' @param force Logical; if `TRUE`, redownload the file even when the cached
#'   Synapse version is current. Default is `FALSE`.
#' @param auth_token Optional Synapse authentication token. If `NULL`, the
#'   `SYNAPSE_AUTH_TOKEN` environment variable is used.
#' @param validate_hdf5 Logical; whether to validate the cached or downloaded
#'   file with `rhdf5::h5ls()` before returning it. Default is `TRUE`.
#'
#' @return
#' Character path to the cached HDF5 reference database.
#'
#' @examples
#' \dontrun{
#' cmap_ref <- load_signature_refdb(
#'   refdb = "cmap",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#'
#' res <- cmap_method(
#'   upset = c("7157", "1956", "5290"),
#'   downset = c("7422", "4318", "348"),
#'   ref_db = cmap_ref
#' )
#'
#' lincs_ref <- load_signature_refdb(
#'   refdb = "lincs2",
#'   force = TRUE,
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#' }
#'
#' @importFrom tools R_user_dir
#' @export
load_signature_refdb <- function(refdb = c("cmap", "lincs2"),
                                 force = FALSE,
                                 auth_token = NULL,
                                 validate_hdf5 = TRUE) {
  refdb <- match.arg(refdb)

  synapse_ids <- c(
    cmap = "syn75455481",
    lincs2 = "syn75456257"
  )
  synapse_data_id <- synapse_ids[[refdb]]

  cache_dir <- file.path(
    tools::R_user_dir("DrugSigNet", which = "cache"),
    "signature_reference_databases"
  )
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  local_file <- file.path(cache_dir, paste0(refdb, ".h5"))
  meta_file <- file.path(cache_dir, paste0(refdb, "_metadata.rds"))

  .drugsignet_require_synapser(
    "download DrugSigNet signature reference databases"
  )

  if (isTRUE(validate_hdf5) && !requireNamespace("rhdf5", quietly = TRUE)) {
    stop(
      "Package 'rhdf5' is required when validate_hdf5 = TRUE.\n",
      "Install it with BiocManager::install('rhdf5') or set validate_hdf5 = FALSE.",
      call. = FALSE
    )
  }

  if (is.null(auth_token)) {
    auth_token <- Sys.getenv("SYNAPSE_AUTH_TOKEN")
  }

  if (identical(auth_token, "")) {
    stop(
      "No Synapse authentication token was provided.\n\n",
      "Use:\n",
      "  load_signature_refdb(refdb = '", refdb, "', auth_token = 'your_token_here')\n\n",
      "or save the token in ~/.Renviron as:\n",
      "  SYNAPSE_AUTH_TOKEN=your_token_here\n",
      call. = FALSE
    )
  }

  .drugsignet_synapser_function("synLogin")(authToken = auth_token)

  syn_meta <- .drugsignet_synapser_function("synGet")(synapse_data_id, downloadFile = FALSE)
  remote_version <- as.integer(syn_meta$properties$versionNumber)
  remote_id <- as.character(syn_meta$properties$id)

  local_meta <- NULL
  if (file.exists(meta_file)) {
    local_meta <- readRDS(meta_file)
  }

  cache_is_current <-
    file.exists(local_file) &&
    !is.null(local_meta) &&
    identical(local_meta$synapse_id, remote_id) &&
    identical(as.integer(local_meta$versionNumber), remote_version)

  if (cache_is_current && !force) {
    message(
      "Using cached DrugSigNet signature reference database: ",
      local_file,
      " | refdb: ", refdb,
      " | Synapse version: ", remote_version
    )
    if (isTRUE(validate_hdf5)) {
      invisible(rhdf5::h5ls(local_file))
    }
    return(local_file)
  }

  if (!file.exists(local_file)) {
    message("No local cache found. Downloading ", refdb, " reference database from Synapse.")
  } else if (!cache_is_current) {
    message(
      "Synapse reference database has changed. Updating local cache from version ",
      if (!is.null(local_meta$versionNumber)) local_meta$versionNumber else "unknown",
      " to version ", remote_version, "."
    )
  } else if (force) {
    message("force = TRUE. Redownloading ", refdb, " reference database from Synapse.")
  }

  if (file.exists(local_file)) {
    unlink(local_file)
  }

  syn_file <- .drugsignet_synapser_function("synGet")(
    synapse_data_id,
    downloadLocation = cache_dir,
    ifcollision = "overwrite.local"
  )

  downloaded_path <- tryCatch(syn_file$path, error = function(e) NULL)
  if (is.null(downloaded_path) || !file.exists(downloaded_path)) {
    stop(
      "The Synapse file was not downloaded.\n\n",
      "Check that ", synapse_data_id,
      " is the correct file ID and that your account has DOWNLOAD permission.",
      call. = FALSE
    )
  }

  downloaded_path_norm <- normalizePath(downloaded_path, mustWork = TRUE)
  local_file_norm <- normalizePath(local_file, mustWork = FALSE)

  if (!identical(downloaded_path_norm, local_file_norm)) {
    ok <- file.rename(downloaded_path, local_file)
    if (!ok) {
      ok <- file.copy(downloaded_path, local_file, overwrite = TRUE)
    }
    if (!ok || !file.exists(local_file)) {
      stop(
        "Downloaded file could not be moved/copied to the local cache path:\n",
        local_file,
        call. = FALSE
      )
    }
  }

  cache_candidates <- Sys.glob(file.path(cache_dir, paste0(refdb, "*")))
  cache_candidates <- normalizePath(cache_candidates, winslash = "/", mustWork = FALSE)
  canonical_files <- normalizePath(c(local_file, meta_file), winslash = "/", mustWork = FALSE)
  stale <- setdiff(cache_candidates, canonical_files)
  stale <- stale[file.exists(stale)]
  if (length(stale) > 0) {
    suppressWarnings(file.remove(stale))
  }

  if (isTRUE(validate_hdf5)) {
    tryCatch(
      rhdf5::h5ls(local_file),
      error = function(e) {
        stop(
          "The downloaded file exists but could not be opened as an HDF5 file:\n",
          local_file,
          "\n\nOriginal error:\n",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }

  saveRDS(
    list(
      refdb = refdb,
      synapse_id = remote_id,
      versionNumber = remote_version,
      downloaded_at = as.character(Sys.time()),
      local_file = local_file,
      file_size = file.info(local_file)$size
    ),
    meta_file
  )

  message(
    "DrugSigNet signature reference database cached at: ", local_file,
    " | refdb: ", refdb,
    " | Synapse version: ", remote_version,
    " | size: ", round(file.info(local_file)$size / 1024^2, 2), " MB"
  )

  local_file
}


.resolve_signature_refdb <- function(ref_db,
                                     signature_refdb_mode = c("default", "frozen", "frozen_force"),
                                     auth_token = NULL,
                                     validate_hdf5 = TRUE) {
  signature_refdb_mode <- match.arg(signature_refdb_mode)
  if (identical(signature_refdb_mode, "default")) {
    return(ref_db)
  }

  refdb_key <- basename(ref_db)
  refdb_key <- sub("\\.h5$", "", refdb_key, ignore.case = TRUE)
  if (!refdb_key %in% c("cmap", "lincs2")) {
    stop(
      "Frozen signature reference databases are only available for 'cmap' and 'lincs2'.",
      call. = FALSE
    )
  }

  load_signature_refdb(
    refdb = refdb_key,
    force = identical(signature_refdb_mode, "frozen_force"),
    auth_token = auth_token,
    validate_hdf5 = validate_hdf5
  )
}
