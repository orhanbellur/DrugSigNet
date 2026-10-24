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

if (length(error_lines)) {
  stop(
    "R CMD check reported ERRORs:\n", paste(error_lines, collapse = "\n"),
    call. = FALSE
  )
}

if (length(warning_indices)) {
  stop(
    "R CMD check reported WARNINGs:\n",
    paste(lines[warning_indices], collapse = "\n"),
    call. = FALSE
  )
}

if (!identical(Sys.getenv("RWORKFLOWS_OUTCOME"), "success")) {
  stop("rworkflows failed outside R CMD check", call. = FALSE)
}

message("R CMD check passed with 0 ERRORs and 0 WARNINGs.")
