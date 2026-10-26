# R 4.5 checks the system clock against these external services. Diagnose the
# original requests without replacing their responses or disabling the check.
urls <- c(
  "https://worldtimeapi.org/api/timezone/etc/UTC",
  "http://worldtimeapi.org/api/timezone/etc/UTC",
  "http://worldclockapi.com/api/json/utc/now"
)

options(timeout = 20)
for (url in urls) {
  message("R CMD check time-service probe: ", url)
  response <- tryCatch(
    readLines(url, warn = FALSE),
    error = function(error) {
      message("Request failed: ", conditionMessage(error))
      NULL
    }
  )
  if (is.null(response)) next
  field <- if (grepl("worldclockapi", url, fixed = TRUE)) "currentDateTime" else "datetime"
  pattern <- paste0('.*"', field, '":"([^Z]*).*')
  remote <- as.POSIXct(
    gsub(pattern, "\\1", response), tz = "UTC",
    format = if (identical(field, "datetime")) "%Y-%m-%dT%H:%M:%S" else "%Y-%m-%dT%H:%M"
  )
  if (!length(remote) || is.na(remote[[1L]])) {
    message("Response did not contain a time parseable by R CMD check.")
  } else {
    message("Remote UTC: ", format(remote[[1L]], tz = "UTC", usetz = TRUE))
    message("Local clock difference (seconds): ",
            round(as.numeric(difftime(Sys.time(), remote[[1L]], units = "secs"))))
  }
}
