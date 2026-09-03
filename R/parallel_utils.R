#' Run pipeline tasks sequentially or in parallel
#'
#' Internal helper shared by pipeline functions to choose a fork/PSOCK backend,
#' cap worker counts, show simple progress, and fall back to sequential
#' execution if parallel execution fails.
#'
#' @param tasks A list or vector of task inputs.
#' @param FUN Function applied to each task.
#' @param n_workers Requested number of parallel workers.
#' @param label Character label used in messages.
#' @param task_label Optional function returning a printable task label.
#' @param psock_packages Character vector of packages loaded on PSOCK workers.
#' @param allow_psock Logical; whether PSOCK workers may be used when fork
#'   workers are unavailable. If `FALSE`, PSOCK-only environments run tasks
#'   sequentially to avoid separate-session namespace/version drift.
#' @param fallback Logical; whether to retry sequentially on parallel failure.
#' @param progress Logical; whether to show a text progress bar.
#' @return A list of task results, preserving task names.
#' @keywords internal
run_pipeline_tasks <- function(tasks,
                               FUN,
                               n_workers = 1L,
                               label = "Pipeline task",
                               task_label = NULL,
                               psock_packages = character(),
                               allow_psock = TRUE,
                               fallback = TRUE,
                               progress = TRUE) {
  if (missing(tasks) || length(tasks) == 0) {
    return(list())
  }
  if (!is.function(FUN)) {
    stop("`FUN` must be a function.", call. = FALSE)
  }

  backend_info <- detect_pipeline_parallel_backend()
  n_workers <- resolve_pipeline_n_workers_count(n_workers, length(tasks))

  if (identical(label, "Network centrality method") && n_workers > 1L) {
    message(
      "[DrugSigNet] Network centrality methods use Python/reticulate backends; ",
      "forcing sequential execution for stability."
    )
    n_workers <- 1L
  }

  if (!isTRUE(allow_psock) && identical(backend_info$backend, "psock") && n_workers > 1L) {
    message(sprintf(
      "[DrugSigNet] %s: PSOCK parallelism disabled for this task type; running sequentially to avoid worker namespace drift.",
      label
    ))
    n_workers <- 1L
  }

  if (backend_info$is_hpc) {
    message("[DrugSigNet] HPC environment detected; using PSOCK cluster for stability.")
  }
  if (backend_info$is_docker && identical(backend_info$backend, "psock")) {
    message("[DrugSigNet] Container environment detected; exporting .libPaths() to workers.")
  }

  message(sprintf("[DrugSigNet] %s: running %d task(s) (n_workers=%d).", label, length(tasks), n_workers))

  run_sequential <- function() {
    pb <- NULL
    if (isTRUE(progress)) {
      pb <- utils::txtProgressBar(min = 0, max = length(tasks), style = 3)
      on.exit(close(pb), add = TRUE)
    }

    out <- vector("list", length(tasks))
    names(out) <- names(tasks)
    for (idx in seq_along(tasks)) {
      item <- tasks[[idx]]
      item_label <- format_pipeline_task_label(item, idx, task_label)
      message(sprintf("[DrugSigNet] %s start: %s", label, item_label))
      elapsed <- system.time({
        out[[idx]] <- FUN(item)
      })
      message(sprintf("[DrugSigNet] %s completed: %s (%.1f sec)", label, item_label, unname(elapsed[["elapsed"]])))
      if (!is.null(pb)) utils::setTxtProgressBar(pb, idx)
    }
    out
  }

  run_parallel <- function() {
    if (n_workers <= 1L) {
      return(run_sequential())
    }

    pb <- NULL
    if (isTRUE(progress)) {
      pb <- utils::txtProgressBar(min = 0, max = length(tasks), style = 3)
      on.exit(close(pb), add = TRUE)
    }

    batches <- split(seq_along(tasks), ceiling(seq_along(tasks) / n_workers))
    out <- vector("list", length(tasks))
    names(out) <- names(tasks)
    done <- 0L

    if (identical(backend_info$backend, "fork")) {
      for (batch in batches) {
        batch_res <- parallel::mclapply(
          batch,
          FUN = function(i) FUN(tasks[[i]]),
          mc.cores = n_workers,
          mc.preschedule = FALSE
        )
        out[batch] <- batch_res
        done <- done + length(batch)
        if (!is.null(pb)) utils::setTxtProgressBar(pb, done)
      }
      return(out)
    }

    cl <- parallel::makePSOCKcluster(n_workers)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterCall(cl, function(paths, packages, work_dir) {
      .libPaths(paths)

      local_pkg <- NULL
      desc_file <- file.path(work_dir, "DESCRIPTION")
      if (file.exists(desc_file)) {
        desc <- tryCatch(base::read.dcf(desc_file), error = function(e) NULL)
        if (!is.null(desc) && "Package" %in% colnames(desc)) {
          local_pkg <- desc[1, "Package"]
        }
      }

      for (pkg in packages) {
        loaded_from_source <- FALSE
        if (!is.null(local_pkg) && identical(pkg, local_pkg) && requireNamespace("pkgload", quietly = TRUE)) {
          loaded_from_source <- tryCatch({
            pkgload::load_all(work_dir, quiet = TRUE)
            TRUE
          }, error = function(e) FALSE)
        }

        if (!isTRUE(loaded_from_source)) {
          loadNamespace(pkg)
        }
      }
      NULL
    }, .libPaths(), psock_packages, getwd())

    for (batch in batches) {
      batch_res <- parallel::parLapplyLB(cl, batch, function(i) FUN(tasks[[i]]))
      out[batch] <- batch_res
      done <- done + length(batch)
      if (!is.null(pb)) utils::setTxtProgressBar(pb, done)
    }
    out
  }

  if (n_workers <= 1L) {
    out <- run_sequential()
  } else if (isTRUE(fallback)) {
    out <- tryCatch(
      {
        parallel_out <- run_parallel()
        if (any(vapply(parallel_out, is.null, logical(1)))) {
          stop("one or more parallel tasks returned NULL", call. = FALSE)
        }
        parallel_out
      },
      error = function(e) {
        warning(
          sprintf(
            "Parallel execution failed/interrupted; retrying sequentially. Original error: %s",
            conditionMessage(e)
          ),
          call. = FALSE
        )
        run_sequential()
      }
    )
  } else {
    out <- run_parallel()
  }

  message(sprintf("[DrugSigNet] %s: task execution complete.", label))
  out
}

#' Detect a safe backend for pipeline task execution
#' @keywords internal
detect_pipeline_parallel_backend <- function() {
  is_windows <- identical(.Platform$OS.type, "windows")
  is_macos <- identical(Sys.info()[["sysname"]], "Darwin")
  env_names <- names(Sys.getenv())
  is_hpc <- any(env_names %in% c("SLURM_JOB_ID", "PBS_JOBID", "LSB_JOBID", "SGE_ROOT"))
  is_docker <- file.exists("/.dockerenv")
  if (!is_docker && file.exists("/proc/1/cgroup")) {
    cgroup <- readLines("/proc/1/cgroup", warn = FALSE)
    is_docker <- any(grepl("docker|kubepods|containerd", cgroup, ignore.case = TRUE))
  }
  backend <- if (is_windows || is_macos || is_hpc) "psock" else "fork"
  list(
    backend = backend,
    is_hpc = is_hpc,
    is_docker = is_docker,
    is_macos = is_macos,
    is_windows = is_windows
  )
}

#' Resolve requested pipeline worker count
#' @keywords internal
resolve_pipeline_n_workers_count <- function(requested_n_workers, task_count) {
  detected_cores <- suppressWarnings(parallel::detectCores(logical = FALSE))
  if (!is.numeric(detected_cores) || is.na(detected_cores) || detected_cores < 1) {
    detected_cores <- 1L
  }
  requested_n_workers <- suppressWarnings(as.integer(requested_n_workers))
  if (!is.numeric(requested_n_workers) || is.na(requested_n_workers) || requested_n_workers < 1) {
    requested_n_workers <- 1L
  }
  as.integer(max(1L, min(requested_n_workers, as.integer(task_count), as.integer(detected_cores))))
}

#' Format a pipeline task label
#' @keywords internal
format_pipeline_task_label <- function(task, idx, task_label = NULL) {
  if (is.function(task_label)) {
    out <- task_label(task)
  } else if (!is.null(names(task)) && "method_name" %in% names(task)) {
    out <- task[["method_name"]]
  } else if (is.character(task) && length(task) == 1) {
    out <- task
  } else {
    out <- sprintf("task_%d", idx)
  }
  out <- as.character(out)[1]
  if (is.na(out) || !nzchar(out)) sprintf("task_%d", idx) else out
}
