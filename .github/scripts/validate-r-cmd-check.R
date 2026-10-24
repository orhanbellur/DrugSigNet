# R CMD check must complete without errors or warnings. Notices remain visible
# in the check log but are not promoted to failures by this validator.

logs <- list.files(
  ".",
  pattern = "^00check\\.log$",
  recursive = TRUE,
  full.names = TRUE
)
logs <- logs[grepl("\\.Rcheck[/\\\\]00check\\.log$", logs)]

if (length(logs) != 1L) {
  stop(
    "Expected exactly one R CMD check log, found ", length(logs), ": ",
    if (length(logs)) paste(logs, collapse = ", ") else "none",
    call. = FALSE
  )
}

lines <- readLines(logs[[1L]], warn = FALSE, encoding = "UTF-8")
error_lines <- grep("^\\* checking .* \\.\\.\\. ERROR$", lines, value = TRUE)
warning_indices <- grep("^\\* checking .* \\.\\.\\. WARNING$", lines)
status_lines <- grep("^Status:", lines, value = TRUE)

test_outputs <- list.files(
  dirname(logs[[1L]]),
  pattern = "testthat\\.Rout(?:\\.fail)?$",
  recursive = TRUE,
  full.names = TRUE
)

diagnostic_tail <- function() {
  source <- if (length(test_outputs)) test_outputs[[1L]] else logs[[1L]]
  output <- readLines(source, warn = FALSE, encoding = "UTF-8")
  paste0(
    "\nDiagnostic tail from ", source, ":\n",
    paste(utils::tail(output, 80L), collapse = "\n")
  )
}

if (length(error_lines)) {
  stop(
    "R CMD check reported ERRORs:\n", paste(error_lines, collapse = "\n"),
    diagnostic_tail(),
    call. = FALSE
  )
}

if (length(warning_indices)) {
  stop(
    "R CMD check reported WARNINGs:\n",
    paste(lines[warning_indices], collapse = "\n"),
    diagnostic_tail(),
    call. = FALSE
  )
}

if (length(status_lines) && !grepl("OK$", status_lines[[length(status_lines)]])) {
  stop(
    "R CMD check reported a failing status: ",
    status_lines[[length(status_lines)]],
    diagnostic_tail(),
    call. = FALSE
  )
}

if (!identical(Sys.getenv("RWORKFLOWS_OUTCOME"), "success")) {
  stop(
    "The R CMD check process failed without writing a standard ERROR/WARNING ",
    "summary (for example, a test process may have crashed).",
    diagnostic_tail(),
    call. = FALSE
  )
}

message("R CMD check passed with 0 ERRORs and 0 WARNINGs.")
