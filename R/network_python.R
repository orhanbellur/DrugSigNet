# Network Python runs out of process: RStudio may already have loaded a system
# libstdc++ that cannot satisfy graph-tool's Conda/Pixi C++ ABI requirements.
# The file protocol keeps large network tables out of JSON and never evaluates
# user-provided Python expressions.
.drugsignet_network_python_call <- function(module, function_name, args,
                                            result_index = NULL, discard = FALSE) {
  python <- .drugsignet_discover_pixi_python()
  if (!nzchar(python) && reticulate::py_available(initialize = FALSE)) {
    python <- reticulate::py_config()$python
  }
  if (!nzchar(python) || !file.exists(python)) {
    stop("Configure DrugSigNet's Python runtime before running network methods.", call. = FALSE)
  }
  worker <- system.file("Python", "network_worker.py", package = "DrugSigNet")
  if (!nzchar(worker) || !file.exists(worker)) stop("Missing network Python worker.", call. = FALSE)
  work_dir <- tempfile("DrugSigNet_python_")
  dir.create(work_dir, recursive = TRUE)
  on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)
  na_value <- paste0("__DS_NA_", basename(work_dir), "__")
  encoded <- lapply(seq_along(args), function(i) {
    value <- args[[i]]
    if (!is.data.frame(value)) return(value)
    types <- lapply(value, function(column) {
      if (is.logical(column)) "boolean" else if (is.integer(column)) "Int64" else
        if (is.numeric(column)) "float64" else "string"
    })
    value[] <- lapply(value, function(column) if (is.factor(column)) as.character(column) else column)
    path <- file.path(work_dir, paste0("input_", i, ".csv"))
    utils::write.csv(value, path, row.names = FALSE, na = na_value, fileEncoding = "UTF-8")
    list(kind = "data.frame", path = path, types = types)
  })
  names(encoded) <- names(args)
  request <- file.path(work_dir, "request.json")
  response <- file.path(work_dir, "response.json")
  jsonlite::write_json(list(module = module, function_name = function_name,
    args = encoded, result_index = result_index, discard = discard,
    na_value = na_value), request, auto_unbox = TRUE, null = "null", digits = NA)
  hash_seed <- Sys.getenv("PYTHONHASHSEED", unset = "0")
  if (!nzchar(hash_seed)) hash_seed <- "0"
  output <- suppressWarnings(.drugsignet_python_system2(python, c(shQuote(worker),
    "--request", shQuote(request), "--response", shQuote(response)), stdout = TRUE, stderr = TRUE,
    extra_env = paste0("PYTHONHASHSEED=", hash_seed)))
  status <- attr(output, "status") %||% 0L
  result <- if (file.exists(response)) tryCatch(
    jsonlite::fromJSON(response, simplifyVector = FALSE),
    error = function(e) list(ok = FALSE, error = paste("Invalid worker response:", conditionMessage(e)))
  ) else NULL
  if (status != 0L || is.null(result) || !isTRUE(result$ok)) {
    detail <- if (!is.null(result$error)) result$error else paste(utils::tail(output, 30L), collapse = "\n")
    stop("Network Python worker failed (", module, "::", function_name, ") using ",
      python, ":\n", detail, call. = FALSE)
  }
  if (!identical(result$kind, "data.frame")) return(result$value)
  schema <- result$types
  classes <- vapply(schema, function(type) switch(type, number = "numeric",
    boolean = "logical", "character"), character(1))
  table <- utils::read.csv(result$path, colClasses = unname(classes),
    check.names = FALSE, stringsAsFactors = FALSE, na.strings = na_value, fileEncoding = "UTF-8")
  for (column in names(schema)[vapply(schema, identical, logical(1), "list")]) {
    table[[column]] <- lapply(table[[column]], function(value) {
      if (is.na(value)) NULL else jsonlite::fromJSON(value)
    })
  }
  table
}
