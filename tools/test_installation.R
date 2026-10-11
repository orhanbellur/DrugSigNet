python <- Sys.which("python")
if (!nzchar(python)) stop("Pixi environment does not contain Python.")
conda <- Sys.which("conda")
if (!nzchar(conda)) {
  stop("Pixi environment does not contain reticulate's Conda compatibility executable.")
}

prefix <- Sys.getenv("CONDA_PREFIX")
if (!nzchar(prefix)) {
  # `pixi run --clean-env` intentionally does not guarantee CONDA_PREFIX.
  # Derive the environment prefix from the Python executable instead of
  # rejecting an otherwise valid Pixi runtime.
  prefix <- dirname(dirname(python))
}
if (!dir.exists(prefix)) {
  stop("Unable to determine the Pixi environment prefix from Python: ", python)
}
expected_library <- normalizePath(file.path(prefix, "lib", "R", "library"), mustWork = FALSE)
active_libraries <- normalizePath(.libPaths(), mustWork = FALSE)
if (!expected_library %in% active_libraries) {
  stop(
    "Pixi R is not using its own package library. Expected: ",
    expected_library, "; active: ", paste(active_libraries, collapse = ", ")
  )
}
if (!startsWith(normalizePath(R.home(), mustWork = FALSE), normalizePath(prefix))) {
  stop("The validation script did not start Pixi R: ", R.home())
}

Sys.setenv(
  RETICULATE_PYTHON = python,
  RETICULATE_CONDA = conda,
  DRUGSIGNET_SKIP_RUNTIME_SETUP = "true"
)
if (!requireNamespace("DrugSigNet", quietly = TRUE)) {
  stop("DrugSigNet was not installed into the Pixi R library: ", expected_library)
}
required_signature_packages <- c("BiocManager", "signatureSearch")
missing_signature_packages <- required_signature_packages[!vapply(
  required_signature_packages, requireNamespace, quietly = TRUE,
  FUN.VALUE = logical(1)
)]
if (length(missing_signature_packages)) {
  stop(
    "Pixi runtime is missing signature-pipeline R packages: ",
    paste(missing_signature_packages, collapse = ", ")
  )
}
reticulate::use_python(python, required = TRUE)
tryCatch(
  reticulate::import("graph_tool", delay_load = FALSE),
  error = function(e) {
    stop(
      "reticulate could not bind to Pixi graph_tool during validation: ",
      conditionMessage(e),
      call. = FALSE
    )
  }
)
check <- DrugSigNet::check_drugsignet_installation(
  check_python = TRUE,
  check_optional = FALSE,
  stop_on_error = FALSE
)
if (!isTRUE(check$ok)) {
  print(check$packages[!check$packages$available, , drop = FALSE])
  print(check$functions[!check$functions$available, , drop = FALSE])
  print(check$python[!check$python$available, , drop = FALSE])
  stop("DrugSigNet Pixi validation failed; missing components are printed above.")
}
message("DrugSigNet installation and its Pixi runtime were validated successfully.")
