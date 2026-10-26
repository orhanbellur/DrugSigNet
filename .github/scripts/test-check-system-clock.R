source(".github/scripts/check-system-clock.R")

expect_error <- function(code, pattern) {
  error <- tryCatch({ force(code); NULL }, error = identity)
  stopifnot(inherits(error, "error"), grepl(pattern, conditionMessage(error)))
}

reference <- as.POSIXct("2026-10-26 12:00:00", tz = "UTC")
headers <- "HTTP/2 200\r\nDate: Mon, 26 Oct 2026 12:00:00 GMT\r\n\r\n"
response <- function(url, text = headers, status = 200L) {
  list(headers = text, status = status, url = url)
}
fetch <- function(url) response(url)
now <- function() reference
stopifnot(identical(unname(check_system_clock(fetch, now)), c(-1, 1)))
stopifnot(identical(clock_response_interval(headers, 204L, "https://example.org"), c(lower = as.numeric(reference), upper = as.numeric(reference))))
stopifnot(identical(clock_response_interval(
  sub("\r\n\r\n$", "\r\nAge: 20\r\n\r\n", headers), 200L, "https://example.org"
), c(lower = as.numeric(reference), upper = as.numeric(reference) + 20)))
# Proxy and informational headers must not supply the origin's time.
proxied <- paste0("HTTP/1.1 200 Connection established\r\nDate: Mon, 01 Jan 2001 00:00:00 GMT\r\n\r\n", headers)
stopifnot(identical(clock_response_interval(proxied, 200L, "https://example.org"), c(lower = as.numeric(reference), upper = as.numeric(reference))))
expect_error(clock_response_interval(headers, 503L, "https://example.org"), "successful HTTPS")
expect_error(clock_response_interval(headers, 200L, "http://example.org"), "successful HTTPS")
expect_error(clock_response_interval("HTTP/2 200\r\n\r\n", 200L, "https://example.org"), "Date header")
expect_error(clock_response_interval(sub("Date:", "Date: invalid", headers), 200L, "https://example.org"), "Invalid HTTP Date")
expect_error(clock_response_interval(sub("\r\n\r\n$", "\r\nDate: duplicate\r\n\r\n", headers), 200L, "https://example.org"), "duplicate Date")
for (age in c("-1", "invalid")) {
  expect_error(clock_response_interval(sub("\r\n\r\n$", paste0("\r\nAge: ", age, "\r\n\r\n"), headers), 200L, "https://example.org"), "HTTP Age")
}
for (drift in c(-301, 301)) {
  expect_error(check_system_clock(fetch, function() reference + drift), "five minutes")
}
# Cached Date plus a valid Age must give the current reference time.
fetch_cached <- function(url) {
  text <- if (grepl("google", url)) headers else sub("12:00:00", "11:30:00", headers)
  response(url, sub("\\r\\n\\r\\n$", "\r\nAge: 1800\r\n\r\n", text))
}
stopifnot(identical(unname(check_system_clock(fetch_cached, now)), c(-1, 1)))
fetch_uncertain <- function(url) response(url, sub("\\r\\n\\r\\n$", "\r\nAge: 1800\r\n\r\n", headers))
expect_error(check_system_clock(fetch_uncertain, now), "too uncertain")
fetch_stale <- function(url) response(url, sub("12:00:00", "11:30:00", headers))
expect_error(check_system_clock(fetch_stale, now), "five minutes")
fetch_disagree <- function(url) {
  response(url, if (grepl("google", url)) sub("12:00:00", "12:02:00", headers) else headers)
}
expect_error(check_system_clock(fetch_disagree, now), "sources disagree")
expect_error(check_system_clock(function(url) stop("TLS failure"), now), "TLS failure")
for (duration in c(-1, 26)) {
  calls <- 0L
  changing_clock <- function() {
    calls <<- calls + 1L
    reference + if (calls %% 2L == 0L) duration else 0
  }
  expect_error(check_system_clock(fetch, changing_clock), "Clock changed")
}


variables <- c("_R_CHECK_SYSTEM_CLOCK_", "_R_CHECK_FUTURE_FILE_TIMESTAMPS_")
previous <- Sys.getenv(variables, unset = NA_character_)
restore <- function() {
  Sys.unsetenv(variables)
  for (name in variables[!is.na(previous)]) do.call(Sys.setenv, stats::setNames(list(previous[[name]]), name))
}
tryCatch({
  Sys.unsetenv(variables)
  expect_error(prepare_check_clock(function(url) stop("TLS failure"), now), "TLS failure")
  stopifnot(all(is.na(Sys.getenv(variables, unset = NA_character_))))
  prepare_check_clock(fetch, now)
  stopifnot(identical(unname(Sys.getenv(variables)), c("FALSE", "TRUE")))
}, finally = restore())
# Exercise the transport boundary: verify TLS-preserving curl arguments and
# parse the origin HTTP/2 status after a proxy CONNECT response.
transport <- new.env(parent = globalenv())
sys.source(".github/scripts/check-system-clock.R", envir = transport)
transport$Sys.which <- function(command) "/mock/curl"
transport$system2 <- function(command, args, stdout, stderr) {
  stopifnot(identical(command, "/mock/curl"), "--head" %in% args,
            "--proto" %in% args, any(grepl("=https", args, fixed = TRUE)),
            !any(args %in% c("--insecure", "-k", "--location", "-L")))
  c("HTTP/1.1 200 Connection established\r", "\r", "HTTP/2 204\r",
    "Date: Mon, 26 Oct 2026 12:00:00 GMT\r", "\r")
}
result <- transport$fetch_clock_headers("https://example.org")
stopifnot(identical(result$status, 204L))
stopifnot(identical(clock_response_interval(result$headers, result$status, result$url),
                    c(lower = as.numeric(reference), upper = as.numeric(reference))))
transport$system2 <- function(command, args, stdout, stderr) {
  writeLines("SSL certificate verification failed", stderr)
  structure(character(), status = 60L)
}
expect_error(transport$fetch_clock_headers("https://example.org"), "SSL certificate verification failed")

message("CI clock-validation regression checks passed.")
