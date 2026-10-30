hub_cache_fixture <- function() {
  cache <- tempfile("hub-cache-")
  dir.create(cache)
  bfc <- BiocFileCache::BiocFileCache(cache, ask = FALSE)
  source <- tempfile(fileext = ".sqlite3")
  connection <- DBI::dbConnect(RSQLite::SQLite(), source)
  DBI::dbWriteTable(connection, "metadata", data.frame(value = "retained"))
  DBI::dbDisconnect(connection)
  first <- BiocFileCache::bfcadd(bfc, "experimenthub.sqlite3", source, rtype = "local")
  second <- BiocFileCache::bfcadd(bfc, "experimenthub.sqlite3", source, rtype = "local")
  resource <- BiocFileCache::bfcadd(bfc, "EH3228", source, rtype = "local")
  unlink(source)
  list(cache = cache, bfc = bfc, paths = c(first, second), resource = resource)
}

test_that("duplicate hub metadata is repaired without deleting downloaded resources", {
  fixture <- hub_cache_fixture()
  on.exit(unlink(fixture$cache, recursive = TRUE), add = TRUE)
  checksum <- tools::md5sum(fixture$resource)
  # Reproduce the actual AnnotationHub error with two metadata registrations.
  update <- getFromNamespace(".updateHubDB", "AnnotationHub")
  expect_error(update(fixture$bfc, "ExperimentHub", "unused", NULL, TRUE),
               "Corrupt Cache: sqlite file")
  expect_message(.repair_signature_hub_duplicates(fixture$cache), "Repaired duplicate")
  records <- BiocFileCache::bfcquery(fixture$bfc, "experimenthub.sqlite3", field = "rname", exact = TRUE)
  expect_equal(nrow(records), 1L)
  expect_true(.signature_hub_sqlite_valid(records$rpath))
  expect_identical(update(fixture$bfc, "ExperimentHub", "unused", NULL, TRUE), records$rpath)
  expect_identical(tools::md5sum(fixture$resource), checksum)
  backup <- list.dirs(fixture$cache, recursive = FALSE, full.names = TRUE)
  backup <- backup[grepl("drugsignet-metadata-backup-", basename(backup))]
  expect_length(backup, 1L)
  expect_equal(nrow(readRDS(file.path(backup, "records.rds"))), 2L)
  expect_length(list.files(backup, pattern = "sqlite3$"), 2L)
  expect_false(.repair_signature_hub_duplicates(fixture$cache))
})

test_that("invalid duplicate copies and unrelated failures do not erase the cache", {
  fixture <- hub_cache_fixture()
  on.exit(unlink(fixture$cache, recursive = TRUE), add = TRUE)
  for (path in fixture$paths) writeLines("not SQLite", path)
  expect_error(.repair_signature_hub_duplicates(fixture$cache), "no readable SQLite copy")
  expect_equal(nrow(BiocFileCache::bfcquery(fixture$bfc, "experimenthub.sqlite3",
                                          field = "rname", exact = TRUE)), 2L)
  expect_true(all(file.exists(c(fixture$paths, fixture$resource))))
})

test_that("a missing duplicate does not prevent retention of the readable copy", {
  fixture <- hub_cache_fixture()
  on.exit(unlink(fixture$cache, recursive = TRUE), add = TRUE)
  unlink(fixture$paths[1L])
  expect_message(.repair_signature_hub_duplicates(fixture$cache), "Repaired duplicate")
  remaining <- BiocFileCache::bfcquery(fixture$bfc, "experimenthub.sqlite3",
                                      field = "rname", exact = TRUE)
  expect_identical(remaining$rpath, unname(fixture$paths[2L]))
  expect_true(.signature_hub_sqlite_valid(remaining$rpath))
  expect_true(file.exists(fixture$resource))
})

test_that("preflight warms annotations and only requested hub references", {
  cache <- tempfile("hub-preflight-")
  on.exit(unlink(c(cache, paste0(cache, ".drugsignet-init.lock")), recursive = TRUE), add = TRUE)
  requested <- character()
  # An environment with an active binding records actual resource access.
  hub <- new.env(parent = emptyenv())
  for (id in c("EH3228", "EH3223", "EH7297")) {
    local({
      key <- id
      makeActiveBinding(key, function() { requested <<- c(requested, key); "local-file" }, hub)
    })
  }
  open <- function(cache, ask) { expect_false(ask); hub }
  .prepare_signature_hub(c("cmap", "lincs2", "cmap"), cache = cache, open_hub = open)
  expect_identical(requested, c("EH3228", "EH3223", "EH7297"))
  requested <- character()
  .prepare_signature_hub("/local/frozen.h5", cache = cache, open_hub = open)
  expect_identical(requested, "EH3228")
  expect_error(.prepare_signature_hub(cache = cache,
    open_hub = function(...) stop("network unavailable")), "network unavailable")
  # Failure releases the lock and allows the next attempt.
  expect_true(.prepare_signature_hub(cache = cache, open_hub = open))
})

test_that("independent PSOCK processes serialize first-time hub initialization", {
  cache <- tempfile("hub-parallel-")
  log <- tempfile("hub-events-")
  on.exit(unlink(c(cache, paste0(cache, ".drugsignet-init.lock"), log), recursive = TRUE), add = TRUE)
  cluster <- parallel::makePSOCKcluster(2L)
  on.exit(parallel::stopCluster(cluster), add = TRUE)
  prepare <- .prepare_signature_hub
  repair <- .repair_signature_hub_duplicates
  results <- parallel::parLapply(cluster, 1:2, function(id, cache, log, prepare, repair) {
    # Supply the production repair routine in a worker-local environment, without
    # requiring the newly edited package to have been installed on that worker.
    scope <- new.env(parent = environment(prepare))
    scope$.repair_signature_hub_duplicates <- repair
    environment(prepare) <- scope
    prepare(cache = cache, open_hub = function(...) {
      cat(paste0("start-", id, "\n"), file = log, append = TRUE)
      Sys.sleep(0.15)
      cat(paste0("end-", id, "\n"), file = log, append = TRUE)
      list(EH3228 = "already-cached")
    })
  }, cache, log, prepare, repair)
  expect_true(all(unlist(results)))
  events <- readLines(log)
  expect_match(events[1], "^start-")
  expect_identical(sub("start", "end", events[1]), events[2])
  expect_identical(sub("start", "end", events[3]), events[4])
})
