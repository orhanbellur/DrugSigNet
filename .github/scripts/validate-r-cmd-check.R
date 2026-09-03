# Admit only DrugSigNet's immutable cell_info2 R CMD check warning.

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

if (!length(warning_indices)) {
  if (!identical(Sys.getenv("RWORKFLOWS_OUTCOME"), "success")) {
    stop(
      "rworkflows failed for a reason other than an allowed R CMD check warning",
      call. = FALSE
    )
  }
  message("R CMD check passed with 0 ERRORs and 0 WARNINGs.")
  quit(status = 0L)
}

if (length(warning_indices) != 1L) {
  stop(
    "R CMD check reported ", length(warning_indices),
    " WARNINGs; only the cell_info2 warning is allowed",
    call. = FALSE
  )
}

warning_index <- warning_indices[[1L]]
expected_header <- "* checking data for non-ASCII characters ... WARNING"
if (!identical(lines[[warning_index]], expected_header)) {
  stop("Unexpected R CMD check warning: ", lines[[warning_index]], call. = FALSE)
}

later_checks <- which(
  seq_along(lines) > warning_index & startsWith(lines, "* checking ")
)
next_check <- if (length(later_checks)) later_checks[[1L]] else length(lines) + 1L
warning_block <- paste(lines[seq.int(warning_index, next_check - 1L)], collapse = "\n")
if (!grepl("(?<![A-Za-z0-9_])cell_info2(?![A-Za-z0-9_])", warning_block, perl = TRUE)) {
  stop("The non-ASCII data warning did not identify cell_info2", call. = FALSE)
}

message(
  "R CMD check has 0 ERRORs and only the allowed cell_info2 ",
  "non-ASCII data warning."
)
