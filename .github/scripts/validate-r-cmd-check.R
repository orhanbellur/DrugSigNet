# R CMD check must complete without errors, warnings, or notes.

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
error_lines <- grep("^\\* checking .* ERROR$", lines, value = TRUE)
warning_indices <- grep("^\\* checking .* WARNING$", lines)
note_indices <- grep("^\\* checking .* NOTE$", lines)
status_lines <- grep("^Status:", lines, value = TRUE)

# Show the check's own diagnostic blocks, including source file names and
# undefined symbols. Test output alone cannot explain package metadata notes.
problem_indices <- grep(
  "^\\* checking .* \\.\\.\\.(?: \\[[^]]+\\])? (?:ERROR|WARNING|NOTE)$",
  lines, perl = TRUE
)
if (length(problem_indices)) {
  blocks <- vapply(problem_indices, function(index) {
    following <- which(seq_along(lines) > index & grepl("^\\* (checking|DONE)", lines))
    end <- if (length(following)) following[[1L]] - 1L else length(lines)
    paste(lines[seq.int(index, end)], collapse = "\n")
  }, character(1L))
  message("R CMD check diagnostic blocks:\n", paste(blocks, collapse = "\n\n"))
}

test_outputs <- list.files(
  dirname(logs[[1L]]),
  pattern = "testthat\\.Rout(?:\\.fail)?$",
  recursive = TRUE,
  full.names = TRUE
)
if (any(grepl("\\.fail$", test_outputs))) {
  test_outputs <- c(
    test_outputs[grepl("\\.fail$", test_outputs)],
    test_outputs[!grepl("\\.fail$", test_outputs)]
  )
}

diagnostic_tail <- function() {
  source <- if (length(test_outputs)) test_outputs[[1L]] else logs[[1L]]
  output <- readLines(source, warn = FALSE, encoding = "UTF-8")

  # A crashed test process can write the useful error and native traceback
  # hundreds of lines before the final "Execution halted" line. A plain tail
  # therefore hides the cause precisely when diagnostics matter most. Preserve
  # the complete output when it is reasonably small; otherwise combine context
  # around fatal markers with the final lines and include original line numbers.
  if (length(output) <= 400L) {
    selected <- seq_along(output)
  } else {
    marker_pattern <- paste(
      c(
        "Segmentation fault",
        "\\*\\*\\* caught segfault",
        "^Traceback:",
        "^Error(?: in|:)",
        "^Execution halted$",
        "^── (?:Error|Failure)",
        "^══ Failed",
        "\\[ FAIL"
      ),
      collapse = "|"
    )
    marker_indices <- grep(
      marker_pattern,
      output,
      ignore.case = TRUE,
      perl = TRUE
    )
    marker_indices <- unique(c(
      utils::head(marker_indices, 10L),
      utils::tail(marker_indices, 10L)
    ))
    context_indices <- unlist(lapply(marker_indices, function(index) {
      seq.int(max(1L, index - 20L), min(length(output), index + 20L))
    }), use.names = FALSE)
    final_indices <- seq.int(max(1L, length(output) - 119L), length(output))
    selected <- sort(unique(c(context_indices, final_indices)))
  }

  numbered_output <- sprintf("%6d | %s", selected, output[selected])
  paste0(
    "\nDiagnostic tail and fatal-error context from ", source, ":\n",
    paste(numbered_output, collapse = "\n")
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

if (length(note_indices)) {
  stop(
    "R CMD check reported NOTEs:\n",
    paste(lines[note_indices], collapse = "\n"),
    call. = FALSE
  )
}

if (length(status_lines) != 1L || !identical(status_lines, "Status: OK")) {
  stop(
    "R CMD check reported a failing status: ",
    if (length(status_lines)) paste(status_lines, collapse = "; ") else "missing Status summary",
    diagnostic_tail(),
    call. = FALSE
  )
}

if (!identical(Sys.getenv("RWORKFLOWS_OUTCOME"), "success")) {
  stop(
    "The R CMD check process failed without writing a standard ERROR/WARNING/NOTE ",
    "summary (for example, a test process may have crashed).",
    diagnostic_tail(),
    call. = FALSE
  )
}

message("R CMD check passed with 0 ERRORs, 0 WARNINGs, and 0 NOTEs.")
