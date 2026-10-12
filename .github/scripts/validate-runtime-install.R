configured_python <- Sys.getenv("RETICULATE_PYTHON")
if (!nzchar(configured_python) || !file.exists(configured_python)) {
  stop(
    "RETICULATE_PYTHON was not persisted by runtime provisioning: ",
    configured_python,
    call. = FALSE
  )
}

reticulate::use_python(configured_python, required = TRUE)
python_config <- reticulate::py_config()
configured_normalized <- normalizePath(configured_python, mustWork = TRUE)
initialized_normalized <- normalizePath(python_config$python, mustWork = TRUE)
if (!identical(configured_normalized, initialized_normalized)) {
  stop(
    "reticulate initialized the wrong Python interpreter. Configured: ",
    configured_normalized, "; initialized: ", initialized_normalized,
    call. = FALSE
  )
}
message(
  "Validated reticulate interpreter: configured=", configured_normalized,
  "; initialized=", initialized_normalized
)

library(DrugSigNet)

result <- DrugSigNet::check_drugsignet_installation(
  check_python = TRUE,
  check_optional = FALSE,
  stop_on_error = TRUE
)

required_everywhere <- c(
  "numpy", "pandas", "scipy", "networkx", "joblib", "tqdm", "openpyxl",
  "jinja2", "markupsafe", "kaleido", "synapseclient"
)
missing <- setdiff(required_everywhere, result$python$module[result$python$available])
if (length(missing)) {
  stop("Missing required Python modules: ", paste(missing, collapse = ", "))
}

is_windows <- identical(.Platform$OS.type, "windows")
if (is_windows && "graph_tool" %in% result$python$module) {
  stop("graph_tool must not be requested by the native Windows installation")
}
if (!is_windows) {
  graph_status <- result$python$available[result$python$module == "graph_tool"]
  if (length(graph_status) != 1L || !isTRUE(graph_status)) {
    stop("graph_tool is required and must be importable outside Windows")
  }
}

vignette_names <- c(
  "getting-started", "choosing-analysis-strategy", "signature-methods",
  "network-methods", "rank-aggregation", "drug-annotation",
  "visualization", "signature-workflow", "network-workflow",
  "complete-case-study"
)
vignette_files <- system.file(
  "doc", paste0(vignette_names, ".html"), package = "DrugSigNet"
)
missing_vignettes <- vignette_names[!nzchar(vignette_files) | !file.exists(vignette_files)]
if (length(missing_vignettes)) {
  stop(
    "Installed DrugSigNet HTML vignettes are missing: ",
    paste(missing_vignettes, collapse = ", "),
    call. = FALSE
  )
}

message(
  "Validated DrugSigNet runtime on ", Sys.info()[["sysname"]],
  if (is_windows) " (graph_tool skipped)." else " (graph_tool available).",
  " Validated ", length(vignette_files), " installed HTML vignettes."
)
