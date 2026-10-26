# R 4.5's hard-coded WorldTimeAPI/WorldClockAPI queries can fail even when the
# runner clock is correct. Verify the clock independently before replacing only
# that external query. R CMD check still validates package file timestamps.

clock_response_interval <- function(headers, status, url) {
  if (!startsWith(url, "https://") || status < 200L || status >= 300L) {
    stop("Clock source must return a successful HTTPS response: ", url)
  }
  blocks <- strsplit(headers, "\r?\n\r?\n", perl = TRUE)[[1L]]
  blocks <- blocks[grepl("^HTTP/", blocks)]
  if (!length(blocks)) stop("Missing HTTP response headers: ", url)
  lines <- strsplit(utils::tail(blocks, 1L), "\r?\n", perl = TRUE)[[1L]]
  header <- function(name, required = TRUE) {
    values <- lines[startsWith(tolower(lines), paste0(tolower(name), ":"))]
    if (!length(values) && !required) return(NULL)
    if (length(values) != 1L) stop("Missing or duplicate ", name, " header: ", url)
    trimws(sub("^[^:]+:", "", values))
  }
  date <- header("Date")
  if (!grepl("^[A-Z][a-z]{2}, [0-9]{2} [A-Z][a-z]{2} [0-9]{4} [0-9:]{8} GMT$", date)) {
    stop("Invalid HTTP Date header: ", url)
  }
  previous_locale <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", previous_locale), add = TRUE)
  Sys.setlocale("LC_TIME", "C")
  remote <- as.POSIXct(date, format = "%a, %d %b %Y %H:%M:%S GMT", tz = "UTC")
  if (is.na(remote)) stop("Unparseable HTTP Date header: ", url)
  age <- header("Age", required = FALSE)
  age_seconds <- 0
  if (!is.null(age)) {
    if (!grepl("^[0-9]+$", age) || !is.finite(as.numeric(age))) {
      stop("Invalid HTTP Age header: ", url)
    }
    age_seconds <- as.numeric(age)
  }
  # Some CDNs preserve the origin Date; others refresh it while retaining
  # content Age. Bound the current time by both interpretations. Both sources
  # must jointly identify a narrow interval, independently of the local clock.
  c(lower = as.numeric(remote), upper = as.numeric(remote) + age_seconds)
}

fetch_clock_headers <- function(url) {
  executable <- Sys.which("curl")
  if (!nzchar(executable)) stop("curl is required for CI clock verification")
  error_file <- tempfile("clock-request-")
  on.exit(unlink(error_file), add = TRUE)
  output <- system2(executable, c(
    "--silent", "--show-error", "--head", "--proto", shQuote("=https"),
    "--connect-timeout", "10", "--max-time", "20",
    "--header", shQuote("Cache-Control: no-cache, no-store, max-age=0"),
    "--header", shQuote("Pragma: no-cache"), "--url", shQuote(url)
  ), stdout = TRUE, stderr = error_file)
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stop("HTTPS clock request failed: ", paste(readLines(error_file, warn = FALSE), collapse = " "))
  }
  output <- sub("\r$", "", output)
  status_lines <- grep("^HTTP/[0-9.]+ [0-9]{3}", output, value = TRUE)
  if (!length(status_lines)) stop("Missing HTTP clock-source status: ", url)
  status <- as.integer(strsplit(trimws(utils::tail(status_lines, 1L)), "[[:space:]]+")[[1L]][[2L]])
  list(headers = paste(output, collapse = "\r\n"), status = status, url = url)
}

check_system_clock <- function(fetch = fetch_clock_headers, now = Sys.time) {
  sources <- c("https://api.github.com/rate_limit", "https://www.google.com/generate_204")
  nonce <- basename(tempfile("drugsignet-clock-"))
  windows <- vapply(sources, function(source) {
    url <- paste0(source, "?drugsignet_clock_probe=", nonce)
    before <- now()
    response <- fetch(url)
    after <- now()
    remote <- clock_response_interval(response$headers, response$status, response$url)
    duration <- as.numeric(difftime(after, before, units = "secs"))
    if (!is.finite(duration) || duration < 0 || duration > 25) {
      stop("Clock changed or request took too long: ", source)
    }
    # Include response latency and HTTP Date's one-second resolution.
    window <- c(remote[["lower"]] - as.numeric(after) - 1,
                remote[["upper"]] - as.numeric(before) + 1)
    message("HTTPS clock source ", source, ": possible offset [",
            round(window[[1L]], 1L), ", ", round(window[[2L]], 1L), "] seconds")
    window
  }, numeric(2L))
  bounds <- c(max(windows[1L, ]), min(windows[2L, ]))
  if (any(!is.finite(bounds)) || bounds[[1L]] > bounds[[2L]]) {
    stop("Independent HTTPS clock sources disagree")
  }
  if (diff(bounds) > 60) stop("Cached clock responses leave the time too uncertain")
  if (max(abs(bounds)) > 300) stop("Runner clock differs from verified HTTPS time by more than five minutes")
  message("System clock verified against both independent HTTPS sources.")
  message("Verified clock offset lies in [", round(bounds[[1L]], 1L), ", ", round(bounds[[2L]], 1L), "] seconds.")
  invisible(bounds)
}

prepare_check_clock <- function(...) {
  check_system_clock(...)
  # Set only after both verified responses pass. No unchecked fallback exists.
  Sys.setenv(
    "_R_CHECK_SYSTEM_CLOCK_" = "FALSE",
    "_R_CHECK_FUTURE_FILE_TIMESTAMPS_" = "TRUE"
  )
  message("R CMD check file-timestamp validation remains enabled; its duplicate external clock query is replaced by the verified guard.")
}
