#' Check DrugSigNet Installation Health
#'
#' @description
#' Performs a lightweight post-installation smoke check for DrugSigNet. This is
#' useful after installing from Git with `remotes::install_git()` to confirm that
#' core R dependencies, key exported functions, optional Synapse support, and
#' Python modules used by reticulate-backed workflows are available.
#'
#' @param check_python Logical; if `TRUE`, check Python modules used by
#'   DrugSigNet through `reticulate`.
#' @param check_synapser Logical; if `TRUE`, require the optional `synapser`
#'   package to be installed and loadable.
#' @param check_optional Logical; if `TRUE`, check optional reporting and
#'   plotting packages such as `plotly`, `kableExtra`, and `tinytex`.
#' @param install_missing_synapser Logical; if `TRUE` and `check_synapser = TRUE`,
#'   attempt to install/load `synapser` through the DrugSigNet Synapse helper
#'   before reporting check results.
#' @param stop_on_error Logical; if `TRUE`, stop when any requested check fails.
#'
#' @return
#' A list with logical `ok` plus data frames for package, function, and Python
#' module checks.
#'
#' @examples
#' \dontrun{
#' setup_synapser()
#' check_drugsignet_installation(check_synapser = TRUE, stop_on_error = TRUE)
#' }
#'
#' @export
check_drugsignet_installation <- function(check_python = TRUE,
                                          check_synapser = FALSE,
                                          check_optional = TRUE,
                                          install_missing_synapser = FALSE,
                                          stop_on_error = FALSE) {
  required_packages <- c(
    "dplyr", "ggplot2", "methods", "reticulate", "signatureSearch",
    "tibble", "tidyr", "utils", "wordcloud"
  )
  optional_packages <- c("kableExtra", "plotly", "tinytex")
  if (isTRUE(check_synapser)) {
    optional_packages <- c(optional_packages, "synapser")
  }

  if (isTRUE(check_synapser) && isTRUE(install_missing_synapser)) {
    tryCatch(
      {
        if (!.drugsignet_synapser_available()) {
          .drugsignet_install_synapser(quiet = FALSE)
        }
        .drugsignet_require_synapser("run Synapse-backed workflows")
      },
      error = function(e) warning(conditionMessage(e), call. = FALSE)
    )
  }

  package_names <- c(required_packages, if (isTRUE(check_optional) || isTRUE(check_synapser)) optional_packages else character(0))
  package_status <- data.frame(
    package = package_names,
    required = package_names %in% required_packages | (package_names == "synapser" & isTRUE(check_synapser)),
    available = vapply(package_names, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1)),
    stringsAsFactors = FALSE
  )

  function_names <- c(
    "drugSignaturePipeline", "drugNetworkPipeline", "drugRepurposingPipeline",
    "annotate_drugs", "get_drug_trials", "plot_drug_indications",
    "plot_drug_hierarchy", "write_report", "setup_python_dependencies"
  )
  function_status <- data.frame(
    function_name = function_names,
    available = vapply(function_names, exists, FUN.VALUE = logical(1), mode = "function"),
    stringsAsFactors = FALSE
  )

  synapser_status <- data.frame(check = character(0), available = logical(0), stringsAsFactors = FALSE)
  if (isTRUE(check_synapser)) {
    synapser_status <- data.frame(
      check = c("namespace", "synLogin", "synGet"),
      available = c(
        .drugsignet_synapser_available(),
        tryCatch(is.function(.drugsignet_synapser_function("synLogin")), error = function(e) FALSE),
        tryCatch(is.function(.drugsignet_synapser_function("synGet")), error = function(e) FALSE)
      ),
      stringsAsFactors = FALSE
    )
  }

  python_status <- data.frame(module = character(0), available = logical(0), stringsAsFactors = FALSE)
  if (isTRUE(check_python)) {
    py_modules <- .drugsignet_python_packages(include_graph_tool = FALSE)
    if (requireNamespace("reticulate", quietly = TRUE)) {
      python_status <- data.frame(
        module = py_modules,
        available = vapply(py_modules, reticulate::py_module_available, FUN.VALUE = logical(1)),
        stringsAsFactors = FALSE
      )
    } else {
      python_status <- data.frame(
        module = py_modules,
        available = FALSE,
        stringsAsFactors = FALSE
      )
    }
  }

  required_packages_ok <- all(package_status$available[package_status$required])
  functions_ok <- all(function_status$available)
  python_ok <- !isTRUE(check_python) || all(python_status$available)
  synapser_ok <- !isTRUE(check_synapser) || all(synapser_status$available)
  ok <- isTRUE(required_packages_ok && functions_ok && python_ok && synapser_ok)

  result <- list(
    ok = ok,
    packages = package_status,
    functions = function_status,
    python = python_status,
    synapser = synapser_status
  )
  class(result) <- c("DrugSigNetInstallationCheck", class(result))

  if (isTRUE(stop_on_error) && !ok) {
    stop("DrugSigNet installation check failed. Inspect the returned check tables with `stop_on_error = FALSE`.", call. = FALSE)
  }

  result
}
