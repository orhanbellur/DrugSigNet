# Internal method registry for signature-based drug searching.
# Centralizes method names, method families, and reference database labels so
# drugSignaturePipeline() does not need to duplicate string parsing logic.

.signature_method_registry <- function() {
  method_family <- c("CMAP", "LINCS", "gCMAP", "Correlation")
  ref_label <- c("CMAP", "LINCS2")
  registry <- expand.grid(
    family = method_family,
    ref_label = ref_label,
    stringsAsFactors = FALSE
  )

  registry$method <- paste(registry$family, registry$ref_label, sep = "_")
  registry$ref_db <- c(CMAP = "cmap", LINCS2 = "lincs2")[registry$ref_label]
  registry$reference_method <- paste("CMAP", registry$ref_label, sep = "_")

  registry[, c("method", "family", "ref_label", "ref_db", "reference_method")]
}

.signature_method_config <- function(method, registry = .signature_method_registry()) {
  config <- registry[registry$method == method, , drop = FALSE]
  if (nrow(config) != 1L) {
    stop("Unknown signature method: ", method, call. = FALSE)
  }
  config[1, , drop = TRUE]
}

.signature_method_names <- function(registry = .signature_method_registry()) {
  registry$method
}

.signature_method_families <- function(registry = .signature_method_registry()) {
  unique(registry$family)
}
