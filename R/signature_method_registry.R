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

.resolve_signature_worker_count <- function(requested, signature_refdb_mode = "default") {
  requested <- suppressWarnings(as.integer(requested))
  if (length(requested) != 1L || is.na(requested) || requested < 1L) {
    stop("`n_workers` must be a positive integer.", call. = FALSE)
  }
  if (requested > 2L) {
    message(
      "[DrugSigNet] Signature parallelism is capped at two workers to bound ",
      "HDF5 working-set memory."
    )
    return(2L)
  }
  requested
}

.group_signature_tasks_by_refdb <- function(tasks, ref_order = c("cmap", "lincs2")) {
  if (!length(tasks)) return(list())
  task_refs <- vapply(tasks, `[[`, character(1), "db_key")
  unknown_refs <- setdiff(unique(task_refs), ref_order)
  ordered_refs <- c(ref_order[ref_order %in% task_refs], unknown_refs)
  stats::setNames(
    lapply(ordered_refs, function(ref) tasks[task_refs == ref]),
    ordered_refs
  )
}

.signature_group_worker_count <- function(db_key, requested) {
  requested
}

# Execute one self-contained signature-search task. Keeping this worker at
# namespace scope avoids serializing the complete drugSignaturePipeline()
# execution environment for every PSOCK submission.
.run_signature_method_task <- function(task) {
  method <- task$method
  message(sprintf(
    "[DrugSigNet] Signature worker %d start: %s (%s)",
    Sys.getpid(), method, task$db_key
  ))
  started <- proc.time()[["elapsed"]]

  tryCatch({
    sig <- task$signature
    result <- .with_signature_search_attached({
      if (identical(task$family, "CMAP")) {
        cmap_method(
          upset = as.character(sig$up[, 1]),
          downset = as.character(sig$down[, 1]),
          ref_db = task$ref_db,
          chunk_size = task$chunk_size
        )
      } else if (identical(task$family, "LINCS")) {
        lincs_method(
          upset = as.character(sig$up[, 1]),
          downset = as.character(sig$down[, 1]),
          ref_db = task$ref_db,
          chunk_size = task$chunk_size
        )
      } else if (identical(task$family, "gCMAP")) {
        gcmap_method(
          signature_matrix = sig$exp,
          ref_db = task$ref_db,
          higher = min(sig$exp[sig$exp > 0]),
          lower = max(sig$exp[sig$exp < 0]),
          padj = if (identical(task$db_key, "cmap")) task$padj else NULL,
          chunk_size = task$chunk_size
        )
      } else if (identical(task$family, "Correlation")) {
        correlation_method(
          signature_matrix = sig$exp,
          ref_db = task$ref_db,
          chunk_size = task$chunk_size
        )
      } else {
        stop("Unknown signature method family: ", task$family, call. = FALSE)
      }
    })

    message(sprintf(
      "[DrugSigNet] Signature worker %d completed: %s (%.1f sec)",
      Sys.getpid(), method, proc.time()[["elapsed"]] - started
    ))
    result
  }, error = function(e) {
    message(sprintf(
      "[DrugSigNet] Signature worker %d failed: %s: %s",
      Sys.getpid(), method, conditionMessage(e)
    ))
    structure(
      list(method = method, message = conditionMessage(e)),
      class = "DrugSigNetSignatureMethodError"
    )
  })
}
