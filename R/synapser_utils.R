# Utilities for optional Synapse support.

.drugsignet_synapser_install_message <- function() {
  paste0(
    "Package 'synapser' is required for this Synapse-backed workflow.\n",
    "Install it with DrugSigNet's setup helper, then retry:\n",
    "  setup_synapser()\n",
    "The helper installs the maintained synapser 3.x release from Sage's official GitHub repository."
  )
}

#' Install optional Synapse support
#'
#' @description
#' Installs and verifies the isolated Python `synapseclient` used to download
#' DrugSigNet networks, reference databases, and annotation resources. Synapse
#' operations run in a child Python process so conda's OpenSSL libraries never
#' collide with libraries already loaded into the R/RStudio process.
#'
#' DrugSigNet normally calls this helper automatically when a Synapse-backed
#' function is first used. Call it directly to install and validate Synapse
#' support in advance. The helper selects DrugSigNet's conda Python before
#' running the client so reticulate does not create a separate `uv` environment.
#'
#' @param quiet Logical; if `FALSE`, show package installation progress.
#'
#' @return Invisibly returns `TRUE` after `synapseclient` is available.
#'
#' @examples
#' \dontrun{
#' setup_synapser()
#' }
#'
#' @export
setup_synapser <- function(quiet = FALSE) {
  # Keep the historical public helper name, but validate the out-of-process
  # Python client rather than loading the Synapser R namespace.
  .drugsignet_prepare_synapser_python(quiet = quiet)
  python <- Sys.getenv("RETICULATE_PYTHON")
  if (!nzchar(python) || !.drugsignet_python_has_modules(python, "synapseclient")) {
    stop("Python Synapse client installation failed.", call. = FALSE)
  }
  if (!isTRUE(quiet)) message("DrugSigNet's isolated Synapse client is ready.")
  invisible(TRUE)
}

.drugsignet_synapser_package <- function() {
  "synapser"
}

.drugsignet_synapser_available <- function() {
  package <- .drugsignet_synapser_package()
  requireNamespace(package, quietly = TRUE) &&
    utils::packageVersion(package) >= "3.0.0"
}

.drugsignet_require_synapser <- function(purpose) {
  .drugsignet_prepare_synapser_python(quiet = TRUE)
  python <- Sys.getenv("RETICULATE_PYTHON")
  if (!nzchar(python) ||
      !.drugsignet_python_has_modules(python, "synapseclient")) {
    stop(
      "Python package 'synapseclient' is required to ", purpose, ".\n",
      "Run setup_python_dependencies(force = TRUE) and retry.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.drugsignet_auto_install_synapser_enabled <- function() {
  opt <- getOption("DrugSigNet.auto_install_synapser", TRUE)
  env <- Sys.getenv("DRUGSIGNET_AUTO_INSTALL_SYNAPSER", "")
  if (nzchar(env)) {
    return(tolower(env) %in% c("1", "true", "yes", "y"))
  }
  isTRUE(opt)
}

.drugsignet_synapser_function <- function(name) {
  if (identical(name, "synLogin")) {
    return(function(authToken) {
      options(DrugSigNet.synapse_auth_token = authToken)
      invisible(TRUE)
    })
  }
  getExportedValue(.drugsignet_synapser_package(), name)
}

.drugsignet_prepare_synapser_python <- function(quiet = TRUE) {
  tryCatch(
    setup_python_dependencies(
      include_graph_tool = !.drugsignet_is_windows(),
      quiet = quiet
    ),
    error = function(e) {
      stop(
        "Unable to select DrugSigNet's Python environment before loading synapser.\n",
        conditionMessage(e),
        "\nRestart R, load DrugSigNet first, and retry the Synapse operation.",
        call. = FALSE
      )
    }
  )
  invisible(TRUE)
}

.drugsignet_syn_get <- function(entity, ...) {
  args <- list(...)
  python <- Sys.getenv("RETICULATE_PYTHON")
  script <- system.file("Python", "synapse_client.py", package = "DrugSigNet")
  token <- getOption("DrugSigNet.synapse_auth_token", Sys.getenv("SYNAPSE_AUTH_TOKEN"))
  old_token <- Sys.getenv("SYNAPSE_AUTH_TOKEN", unset = NA_character_)
  on.exit({
    if (is.na(old_token)) Sys.unsetenv("SYNAPSE_AUTH_TOKEN") else Sys.setenv(SYNAPSE_AUTH_TOKEN = old_token)
  }, add = TRUE)
  Sys.setenv(SYNAPSE_AUTH_TOKEN = token)

  download <- !identical(args$downloadFile, FALSE)
  location <- args$downloadLocation %||% ""
  output <- system2(
    python,
    c(shQuote(script), shQuote(entity), if (download) "true" else "false", shQuote(location)),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(output, "status") %||% 0L
  if (!identical(as.integer(status), 0L)) {
    stop("Synapse Python client failed: ", paste(output, collapse = "\n"), call. = FALSE)
  }
  json_lines <- output[grepl("^\\{", output)]
  if (!length(json_lines)) {
    stop("Synapse Python client returned no result: ", paste(output, collapse = "\n"), call. = FALSE)
  }
  result <- jsonlite::fromJSON(tail(json_lines, 1L), simplifyVector = TRUE)
  list(properties = result$properties, path = result$path)
}

`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x
