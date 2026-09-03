#' @title Load DrugSigNet Drug Annotation Data
#'
#' @description
#' Downloads DrugSigNet drug annotation data from Synapse and caches it in the
#' user cache directory.
#'
#' @details
#' `load_synapse_annotations()` retrieves the DrugSigNet drug annotation
#' reference object used by annotation helper functions. The object contains
#' annotation tables for indications, mechanisms of action, ATC codes, clinical
#' trials, adverse events, synonyms, SMILES strings, approval status, and
#' blood-brain barrier classifications.
#'
#' Cached files are stored under `tools::R_user_dir("DrugSigNet", "cache")`.
#' On each call, the function compares the cached metadata with the current
#' Synapse file version. If the cached version is current, the local file is
#' reused. If the remote version has changed, or if `force = TRUE`, the file is
#' downloaded again and the cache metadata is updated.
#'
#' This function requires the optional `synapser` package and access to the
#' DrugSigNet Synapse annotation file.
#'
#' @param force Logical; if `TRUE`, redownload the Synapse annotation file even
#'   when a current local cache is available. Default is `FALSE`.
#' @param auth_token Optional Synapse authentication token. If `NULL`, the
#'   `SYNAPSE_AUTH_TOKEN` environment variable is used.
#'
#' @return
#' The DrugSigNet drug annotation object loaded from the local cache or
#' downloaded from Synapse.
#'
#' @examples
#' \dontrun{
#' annotation <- load_synapse_annotations(
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#'
#' annotation <- load_synapse_annotations(
#'   force = TRUE,
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#' }
#'
#' @export
load_synapse_annotations <- function(force = FALSE, auth_token = NULL) {

  synapse_data_id <- "syn76297686"

  cache_dir <- tools::R_user_dir("DrugSigNet", which = "cache")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  local_file <- file.path(cache_dir, "drug_annotation.rds")
  meta_file  <- file.path(cache_dir, "drug_annotation_metadata.rds")

  .drugsignet_require_synapser("download DrugSigNet annotation data")

  if (is.null(auth_token)) {
    auth_token <- Sys.getenv("SYNAPSE_AUTH_TOKEN")
  }

  if (identical(auth_token, "")) {
    stop(
      "No Synapse authentication token was provided.\n\n",
      "Use:\n",
      "  load_synapse_annotations(auth_token = 'your_token_here')\n\n",
      "or save the token in ~/.Renviron as:\n",
      "  SYNAPSE_AUTH_TOKEN=your_token_here\n",
      call. = FALSE
    )
  }

  .drugsignet_synapser_function("synLogin")(authToken = auth_token)

  # Check latest Synapse metadata without downloading the file
  syn_meta <- .drugsignet_synapser_function("synGet")(
    synapse_data_id,
    downloadFile = FALSE
  )

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
      "Using cached DrugSigNet annotation data: ",
      local_file,
      " | Synapse version: ",
      remote_version
    )
    return(readRDS(local_file))
  }

  if (!file.exists(local_file)) {
    message("No local cache found. Downloading DrugSigNet annotation data from Synapse.")
  } else if (!cache_is_current) {
    message(
      "Synapse data has changed. Updating local cache from version ",
      if (!is.null(local_meta$versionNumber)) local_meta$versionNumber else "unknown",
      " to version ",
      remote_version,
      "."
    )
  } else if (force) {
    message("force = TRUE. Redownloading DrugSigNet annotation data from Synapse.")
  }

  # Remove old local cache before redownload
  if (file.exists(local_file)) {
    unlink(local_file)
  }

  # Download newest version
  syn_file <- .drugsignet_synapser_function("synGet")(
    synapse_data_id,
    downloadLocation = cache_dir,
    ifcollision = "overwrite.local"
  )

  downloaded_path <- tryCatch(
    syn_file$path,
    error = function(e) NULL
  )

  if (is.null(downloaded_path) || !file.exists(downloaded_path)) {
    stop(
      "The Synapse file was not downloaded.\n\n",
      "Check that syn75156649 is the file ID and that your account has DOWNLOAD permission.",
      call. = FALSE
    )
  }

  downloaded_path_norm <- normalizePath(downloaded_path, mustWork = TRUE)
  local_file_norm <- normalizePath(local_file, mustWork = FALSE)

  if (!identical(downloaded_path_norm, local_file_norm)) {
    file.copy(downloaded_path, local_file, overwrite = TRUE)
  }

  # Remove duplicate suffixed files if Synapse/R created them
  cache_candidates <- Sys.glob(file.path(cache_dir, "drug_annotation*.rds"))
  cache_candidates <- normalizePath(cache_candidates, winslash = "/", mustWork = FALSE)
  canonical <- normalizePath(local_file, winslash = "/", mustWork = FALSE)
  stale <- setdiff(cache_candidates, canonical)
  stale <- stale[file.exists(stale)]

  if (length(stale) > 0) {
    suppressWarnings(file.remove(stale))
  }

  # Save local metadata for future version checks
  saveRDS(
    list(
      synapse_id = remote_id,
      versionNumber = remote_version,
      downloaded_at = as.character(Sys.time()),
      local_file = local_file
    ),
    meta_file
  )

  message(
    "DrugSigNet annotation data cached at: ",
    local_file,
    " | Synapse version: ",
    remote_version
  )

  readRDS(local_file)
}

#' Load DrugSigNet data from Synapse
#'
#' @description
#' Deprecated compatibility wrapper for `load_synapse_annotations()`.
#'
#' @param force Logical; if `TRUE`, re-download the Synapse annotation file.
#' @param auth_token Optional Synapse personal access token.
#' @return The DrugSigNet drug annotation object.
#' @keywords internal
load_synapse_data <- function(force = FALSE, auth_token = NULL) {
  .Deprecated("load_synapse_annotations")
  load_synapse_annotations(force = force, auth_token = auth_token)
}
