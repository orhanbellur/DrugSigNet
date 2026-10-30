# signatureSearch accesses ExperimentHub even with a local HDF5 reference (for
# reference resolution and target annotation). Initialize it before PSOCK starts,
# and serialize initialization across independent DrugSigNet R sessions.
.prepare_signature_hub <- function(refdb = character(),
                                   cache = ExperimentHub::getExperimentHubOption("CACHE"),
                                   open_hub = ExperimentHub::ExperimentHub) {
  cache <- path.expand(cache)
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(cache)) stop("Cannot create ExperimentHub cache: ", cache, call. = FALSE)
  cache <- normalizePath(cache, winslash = "/", mustWork = TRUE)
  lock <- filelock::lock(paste0(cache, ".drugsignet-init.lock"), timeout = 120000)
  if (is.null(lock)) {
    stop("Timed out waiting for ExperimentHub cache initialization: ", cache, call. = FALSE)
  }
  on.exit(filelock::unlock(lock), add = TRUE)

  .repair_signature_hub_duplicates(cache)
  hub <- open_hub(cache = cache, ask = FALSE)
  # EH3228 supplies signatureSearch's target annotations. Resolving it here
  # prevents simultaneous first downloads by CMAP/LINCS result annotation.
  resources <- c(cmap = "EH3223", cmap_expr = "EH3224", lincs = "EH3226",
                 lincs_expr = "EH3227", lincs2 = "EH7297")
  ids <- unique(c("EH3228", unname(resources[refdb[refdb %in% names(resources)]])))
  for (id in ids) hub[[id]]
  invisible(TRUE)
}

.signature_hub_sqlite_valid <- function(path) {
  if (!file.exists(path)) return(FALSE)
  tryCatch({
    connection <- DBI::dbConnect(RSQLite::SQLite(), path, flags = RSQLite::SQLITE_RO,
                                 synchronous = NULL)
    on.exit(DBI::dbDisconnect(connection), add = TRUE)
    identical(DBI::dbGetQuery(connection, "PRAGMA quick_check")[[1L]], "ok") &&
      length(DBI::dbListTables(connection)) > 0L
  }, error = function(e) FALSE)
}

.repair_signature_hub_duplicates <- function(cache) {
  bfc <- BiocFileCache::BiocFileCache(cache = cache, ask = FALSE)
  records <- as.data.frame(BiocFileCache::bfcquery(
    bfc, "experimenthub.sqlite3", field = "rname", exact = TRUE
  ))
  if (nrow(records) < 2L) return(invisible(FALSE))

  valid <- vapply(records$rpath, .signature_hub_sqlite_valid, logical(1))
  if (!any(valid)) {
    stop("Duplicate ExperimentHub metadata registrations have no readable SQLite copy. ",
         "Cache was preserved: ", cache, call. = FALSE)
  }
  # Prefer the most recently modified readable metadata database. Only duplicate
  # hub metadata records are removed; EH resources and HDF5 files are untouched.
  candidates <- which(valid)
  keep <- candidates[order(file.info(records$rpath[candidates])$mtime,
                           decreasing = TRUE, na.last = TRUE)][1L]
  backup <- tempfile("drugsignet-metadata-backup-", tmpdir = cache)
  if (!dir.create(backup)) stop("Cannot back up ExperimentHub metadata.", call. = FALSE)
  saveRDS(records, file.path(backup, "records.rds"))
  existing <- which(file.exists(records$rpath))
  saved <- file.path(backup, paste0(records$rid[existing], ".sqlite3"))
  if (!all(file.copy(records$rpath[existing], saved, copy.date = TRUE))) {
    stop("Cannot back up ExperimentHub metadata; cache was not modified.", call. = FALSE)
  }
  BiocFileCache::bfcremove(bfc, records$rid[-keep])
  # Older caches can register one physical file more than once. bfcremove then
  # removes that file too, so restore the retained entry from its saved copy.
  if (!file.exists(records$rpath[keep])) {
    retained <- saved[match(keep, existing)]
    if (!file.copy(retained, records$rpath[keep])) {
      stop("Cannot restore retained ExperimentHub metadata. Backup: ", backup, call. = FALSE)
    }
  }
  message("[DrugSigNet] Repaired duplicate ExperimentHub metadata registrations; backup: ", backup)
  invisible(TRUE)
}
