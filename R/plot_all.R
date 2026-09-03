#' @title Plot Multiple DrugSigNet Visualizations
#'
#' @description
#' Runs multiple DrugSigNet plotting helpers in one call.
#'
#' @details
#' `plot_all()` is a convenience wrapper for generating several DrugSigNet plots
#' from a named list of inputs. Each list element name must match one supported
#' plot key. Each element can either be the main input object for that plot
#' or a named list of arguments passed directly to the corresponding plotting
#' function.
#'
#' Supported plot keys are `drug_status`, `drug_indications`,
#' `drug_similarity`, `drug_hierarchy`, `top_k_hits`, `top_k_overlap`,
#' `enriched_terms`, `rank_agreement`, and `rank_distribution_scatter`.
#'
#' If `output_dir` is provided, file names are generated automatically using
#' `file_prefix` and the plot key unless a plot-specific `file_name` is supplied.
#' Failed plots are collected in the returned `errors` list when
#' `continue_on_error = TRUE`.
#'
#' @inheritParams plot_top_k_overlap
#' @param plot_inputs Named list of plot inputs. Names must be supported plot
#'   keys. Each value can be a data frame, matrix, or a named list of arguments
#'   for the underlying plotting function.
#' @param output_dir Optional output directory for saved plots. If `NULL`, each
#'   plotting function uses its default saving behavior.
#' @param file_prefix Optional prefix added to generated file names when
#'   `output_dir` is provided.
#' @param continue_on_error Logical; if `TRUE`, failed plots are collected and
#'   processing continues. If `FALSE`, the first failure stops execution.
#' @param verbose Logical; if `TRUE`, progress messages are printed.
#'
#' @return
#' A named list with two elements:
#' \describe{
#'   \item{plots}{Successfully generated plot objects or plot results.}
#'   \item{errors}{Named list of error messages for failed plots.}
#' }
#'
#' @examples
#' \dontrun{
#' rank_df <- data.frame(
#'   Drug = c("drug_a", "drug_b", "drug_c"),
#'   Status = c("positive", NA, "positive"),
#'   Method1 = c(1, 2, 3),
#'   Method2 = c(2, 1, 3)
#' )
#'
#' indication_df <- data.frame(
#'   Drug = c("drug_a", "drug_b"),
#'   indication = c("Cancer|Tumor", "Diabetes")
#' )
#'
#' res <- plot_all(
#'   plot_inputs = list(
#'     top_k_hits = list(
#'       drug_ranks_df = rank_df,
#'       plottype = "Barplot"
#'     ),
#'     top_k_overlap = rank_df,
#'     drug_indications = indication_df
#'   ),
#'   file_type = "pdf",
#'   output_dir = "plots",
#'   file_prefix = "DrugSigNet_"
#' )
#'
#' res$plots
#' res$errors
#' }
#'
#' @export
plot_all <- function(plot_inputs,
                     file_type = c("pdf", "png", "jpeg", "svg"),
                     output_dir = NULL,
                     file_prefix = "",
                     continue_on_error = TRUE,
                     verbose = TRUE) {
  if (!is.list(plot_inputs) || is.null(names(plot_inputs)) || any(!nzchar(names(plot_inputs)))) {
    stop("`plot_inputs` must be a named list.", call. = FALSE)
  }

  file_type <- match.arg(file_type)

  supported <- c(
    "drug_status", "drug_indications", "drug_similarity", "drug_hierarchy",
    "top_k_hits", "top_k_overlap", "enriched_terms", "rank_agreement",
    "rank_distribution_scatter"
  )

  unknown <- setdiff(names(plot_inputs), supported)
  if (length(unknown) > 0) {
    stop(
      "Unsupported plot key(s): ", paste(unknown, collapse = ", "),
      ". Supported keys are: ", paste(supported, collapse = ", "), ".",
      call. = FALSE
    )
  }

  if (!is.null(output_dir)) {
    if (!is.character(output_dir) || length(output_dir) != 1 || !nzchar(output_dir)) {
      stop("`output_dir` must be NULL or a non-empty character path.", call. = FALSE)
    }
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }
  }

  as_arg_list <- function(x, main_arg) {
    if (is.list(x) && !is.data.frame(x) && !is.matrix(x)) {
      x
    } else {
      stats::setNames(list(x), main_arg)
    }
  }

  build_file_name <- function(key) {
    if (is.null(output_dir)) {
      return(NULL)
    }
    file.path(output_dir, paste0(file_prefix, key))
  }

  results <- list()
  errors <- list()

  for (key in names(plot_inputs)) {
    if (isTRUE(verbose)) {
      message("Generating plot: ", key)
    }

    fn <- switch(
      key,
      drug_status = plot_drug_status,
      drug_indications = plot_drug_indications,
      drug_similarity = plot_drug_similarity,
      drug_hierarchy = plot_drug_hierarchy,
      top_k_hits = plot_top_k_hits,
      top_k_overlap = plot_top_k_overlap,
      enriched_terms = plot_enriched_terms,
      rank_agreement = plot_rank_agreement,
      rank_distribution_scatter = plot_rank_distribution
    )

    main_arg <- switch(
      key,
      drug_status = "data_df",
      drug_indications = "data_df",
      drug_similarity = "similarity_matrix",
      drug_hierarchy = "data_df",
      top_k_hits = "drug_ranks_df",
      top_k_overlap = "drug_ranks_df",
      enriched_terms = "data_df",
      rank_agreement = "drug_ranks_df",
      rank_distribution_scatter = "drug_ranks_df"
    )

    args <- as_arg_list(plot_inputs[[key]], main_arg = main_arg)

    if (is.null(args$file_type)) {
      args$file_type <- file_type
    }
    if (is.null(args$file_name)) {
      args$file_name <- build_file_name(key)
    }

    out <- tryCatch(
      do.call(fn, args),
      error = function(e) e
    )

    if (inherits(out, "error")) {
      errors[[key]] <- conditionMessage(out)
      if (!isTRUE(continue_on_error)) {
        stop("Failed while generating '", key, "': ", conditionMessage(out), call. = FALSE)
      }
    } else {
      results[[key]] <- out
    }
  }

  list(plots = results, errors = errors)
}
