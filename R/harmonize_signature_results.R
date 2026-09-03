#' Aggregate signature rank tables with one rank aggregation method
#'
#' This implementation is intentionally compatible with the original
#' Harmonize_Signature_Network() signature harmonization behaviour:
#'   - input rows are preserved as-is (no duplicate collapsing),
#'   - missing rank_score values remain NA (no NA -> 0 replacement),
#'   - the identifier remains `perturbation` during the first aggregation stage,
#'   - rank input columns are generated dynamically as rank_score_1 ... rank_score_n.
#'
#' @param method Rank aggregation method. One of \code{"CRank"},
#'   \code{"Dowdall"}, or \code{"RRA"}.
#' @param ... Processed signature rank tables containing \code{perturbation}
#'   and \code{rank_score} columns.
#' @param rank_inputs Optional list of processed signature rank tables. When
#'   supplied, \code{...} is ignored.
#' @param prior Prior passed to \code{CRank()}.
#' @param num_bin Number of bins passed to \code{CRank()}.
#' @param ties_method Ties method passed to rank aggregation methods.
#'
#' @return A \code{RankAggregation} object.
#' @export
calculate_rank_aggregation <- function(method,
                                       ...,
                                       rank_inputs = NULL,
                                       prior = 0.093,
                                       num_bin = 200,
                                       ties_method = "max") {
  method <- match.arg(method, choices = c("CRank", "Dowdall", "RRA"))

  if (is.null(rank_inputs)) {
    rank_inputs <- list(...)
  }

  if (!is.list(rank_inputs) || length(rank_inputs) < 2L) {
    stop("At least two processed signature rank tables are required.", call. = FALSE)
  }

  .calculate_multi_signature_rank_aggregation(
    method = method,
    rank_inputs = rank_inputs,
    prior = prior,
    num_bin = num_bin,
    ties_method = ties_method
  )
}


#' Harmonize multiple signature drug-search results
#'
#' Aggregates processed signature drug-search rankings from two or more
#' signature pipeline results. The function is generic: the supplied signature
#' results may have any names and there may be any number of them. For each
#' requested signature family, the original aggregation across all requested
#' reference databases and results is retained. In addition, each complete
#' method/reference combination is aggregated across results. For example,
#' `CMAP_CMAP` from result 1 is aggregated with `CMAP_CMAP` from result 2, and
#' the returned `PairwiseAcrossResults$CMAP_CMAP` entry contains its `CRank`,
#' `Dowdall`, and `RRA` results. These additional entries are kept in a separate
#' list and do not participate in or change the existing `Pairwise` or
#' second-stage `Harmonized` results.
#'
#' The first-stage aggregation deliberately reproduces the behaviour of the
#' original Harmonize_Signature_Network() implementation. In particular, it
#' does not collapse duplicate perturbations and does not replace missing values
#' with zero before CRank/Dowdall/RRA.
#'
#' @param signature_results Named list of signature pipeline results. Each entry
#'   may be a \code{DrugSearchingPipeline} object or a list containing
#'   \code{DrugSearching$Processed}.
#' @param datasets Character vector of signature method families to harmonize.
#' @param reference_suffixes Character vector of processed-result suffixes to
#'   harmonize separately for every dataset.
#' @param methods Character vector of rank aggregation methods. Supported values
#'   are \code{"CRank"}, \code{"Dowdall"}, and \code{"RRA"}.
#' @param prior Prior passed to \code{CRank()}.
#' @param num_bin Number of bins passed to \code{CRank()}.
#' @param ties_method Ties method passed to rank aggregation methods.
#'
#' @details A second-stage \code{Harmonized} aggregation is produced only when at least
#' two signature families are available. With one family there is no second
#' independent ranking to aggregate, so \code{Harmonized} is an empty list.
#'
#' @return A list with \code{Pairwise}, \code{PairwiseAcrossResults}, and
#'   \code{Harmonized} entries. \code{Pairwise} retains the original
#'   family-level results. \code{PairwiseAcrossResults} is keyed by complete
#'   processed-method names such as \code{CMAP_CMAP}; each entry contains the
#'   requested CRank, Dowdall, and RRA objects. RRA result tables consistently
#'   use \code{Drug} as the identifier column.
#' @export
harmonize_signature_results <- function(
    signature_results,
    datasets = c("CMAP", "LINCS", "gCMAP", "Correlation"),
    reference_suffixes = c("CMAP", "LINCS2"),
    methods = c("CRank", "Dowdall", "RRA"),
    prior = 0.093,
    num_bin = 200,
    ties_method = "max") {

  if (missing(signature_results) || is.null(signature_results)) {
    stop("`signature_results` must be a named list of signature pipeline results.", call. = FALSE)
  }

  if (!is.list(signature_results) || length(signature_results) == 0L) {
    stop("`signature_results` must be a non-empty list.", call. = FALSE)
  }

  if (is.null(names(signature_results))) {
    names(signature_results) <- rep("", length(signature_results))
  }

  missing_result_names <- is.na(names(signature_results)) | !nzchar(names(signature_results))
  if (any(missing_result_names)) {
    names(signature_results)[missing_result_names] <-
      paste0("Signature", which(missing_result_names))
  }

  if (!is.character(datasets) || !length(datasets)) {
    stop("`datasets` must be a non-empty character vector.", call. = FALSE)
  }

  if (!is.character(reference_suffixes) || !length(reference_suffixes)) {
    stop("`reference_suffixes` must be a non-empty character vector.", call. = FALSE)
  }

  methods <- match.arg(
    methods,
    choices = c("CRank", "Dowdall", "RRA"),
    several.ok = TRUE
  )

  harmonized_prefix <- .signature_result_suffix(names(signature_results))

  processed_by_result <- lapply(signature_results, .signature_processed_results)
  processed_by_result <- Filter(Negate(is.null), processed_by_result)

  if (length(processed_by_result) == 0L) {
    stop("No processed signature drug-search results were found.", call. = FALSE)
  }

  pairwise_rank_aggregation <- list()
  across_results_rank_aggregation <- list()

  # Preserve the original family-level aggregation exactly: for each dataset,
  # collect every requested reference suffix from every supplied result.
  for (dataset in datasets) {
    rank_inputs <- .collect_signature_rank_inputs(
      processed_by_result = processed_by_result,
      dataset = dataset,
      reference_suffixes = reference_suffixes
    )

    if (length(rank_inputs) < 2L) {
      warning(
        "Skipping signature harmonization for ", dataset,
        " because fewer than two processed rank tables were available.",
        call. = FALSE
      )
      next
    }

    pairwise_rank_aggregation[[dataset]] <- stats::setNames(
      lapply(methods, function(method) {
        calculate_rank_aggregation(
          method = method,
          rank_inputs = rank_inputs,
          prior = prior,
          num_bin = num_bin,
          ties_method = ties_method
        )
      }),
      methods
    )

    for (method in methods) {
      score_col <- .signature_score_column(method)
      out_col <- paste0(dataset, "_", method)
      result_df <- pairwise_rank_aggregation[[dataset]][[method]]@result
      if (identical(method, "RRA")) {
        result_df <- .standardize_pairwise_signature_result(result_df, method)
      }
      pairwise_rank_aggregation[[dataset]][[method]]@result <- result_df %>%
        dplyr::rename(!!out_col := dplyr::all_of(score_col))
    }
  }

  # Additionally aggregate each complete method/reference name across results.
  # These entries are returned in a separate list and are deliberately excluded
  # from the existing pairwise and second-stage harmonization results.
  for (dataset in datasets) {
    for (reference_suffix in reference_suffixes) {
      method_name <- paste0(dataset, "_", reference_suffix)
      rank_inputs <- .collect_signature_rank_inputs(
        processed_by_result = processed_by_result,
        method_name = method_name
      )

      if (length(rank_inputs) < 2L) {
        warning(
          "Skipping signature harmonization for ", method_name,
          " because fewer than two processed rank tables were available.",
          call. = FALSE
        )
        next
      }

      across_results_rank_aggregation[[method_name]] <- stats::setNames(
        lapply(methods, function(method) {
          calculate_rank_aggregation(
            method = method,
            rank_inputs = rank_inputs,
            prior = prior,
            num_bin = num_bin,
            ties_method = ties_method
          )
        }),
        methods
      )

      for (method in methods) {
        score_col <- .signature_score_column(method)
        out_col <- paste0(method_name, "_", method)

        across_results_rank_aggregation[[method_name]][[method]]@result <-
          .standardize_pairwise_signature_result(
            result_df = across_results_rank_aggregation[[method_name]][[method]]@result,
            method = method
          ) %>%
          dplyr::rename(!!out_col := dplyr::all_of(score_col))
      }
    }
  }

  if (length(pairwise_rank_aggregation) == 0L) {
    return(list(
      Pairwise = list(),
      PairwiseAcrossResults = across_results_rank_aggregation,
      Harmonized = list()
    ))
  }

  # Preserve the previous second-stage input: only original family-level
  # pairwise results participate, so adding exact-method results cannot change
  # the existing Harmonized values.
  available_datasets <- intersect(datasets, names(pairwise_rank_aggregation))

  harmonized_methods <- list()
  # A second-stage aggregation needs at least two independently aggregated
  # family rankings. Passing a single family would leave CRank with only one
  # numeric rank column, which is not a valid aggregation input.
  if (length(available_datasets) >= 2L) {
    harmonized_methods <- stats::setNames(
      lapply(methods, function(method) {
        combined_ranks <- lapply(available_datasets, function(dataset) {
          .prepare_pairwise_signature_result(
            result_df = pairwise_rank_aggregation[[dataset]][[method]]@result,
            method = method,
            score_col = paste0(dataset, "_", method)
          )
        })

        combined_ranks <- Reduce(
          function(x, y) dplyr::full_join(x, y, by = "Drug"),
          combined_ranks
        )

        switch(
          method,
          CRank = CRank(
            input_data = combined_ranks,
            ties_method = ties_method,
            prior = prior,
            num_bin = num_bin,
            num_iter = 1000,
            reverse = TRUE
          ),
          Dowdall = Dowdall(
            input_data = combined_ranks,
            ties_method = ties_method,
            reverse = FALSE
          ),
          RRA = RRA(
            input_data = combined_ranks,
            full = TRUE,
            exact = FALSE,
            ties_method = ties_method,
            reverse = FALSE
          )
        )
      }),
      methods
    )
  }

  # Standardize only the returned final harmonized tables. This does not alter
  # the values passed into the aggregation algorithms.
  for (method in names(harmonized_methods)) {
    score_col <- .signature_score_column(method)
    out_col <- paste0(harmonized_prefix, "_", method)

    harmonized_methods[[method]]@result <-
      .standardize_harmonized_signature_result(
        result_df = harmonized_methods[[method]]@result,
        method = method,
        score_col = score_col
      ) %>%
      dplyr::rename(!!out_col := dplyr::all_of(score_col))
  }

  list(
    Pairwise = pairwise_rank_aggregation,
    PairwiseAcrossResults = across_results_rank_aggregation,
    Harmonized = harmonized_methods
  )
}


# Extract the processed signature tables from one supplied result object.
.signature_processed_results <- function(x) {
  if (.is_drug_searching_pipeline(x)) {
    x <- list(
      DrugSearching = x@DrugSearching,
      RankAggregation = x@RankAggregation
    )
  }

  if (!is.list(x)) {
    return(NULL)
  }

  processed <- x$DrugSearching$Processed

  if (is.null(processed) && !is.null(x$Processed)) {
    processed <- x$Processed
  }

  if (!is.list(processed) || length(processed) == 0L) {
    return(NULL)
  }

  processed
}


# Collect all available rank inputs dynamically.
#
# Ordering for family-level collection remains result first, then reference
# suffix, matching the previous implementation. When `method_name` is supplied,
# only that exact method is collected across results.
.collect_signature_rank_inputs <- function(processed_by_result,
                                           dataset = NULL,
                                           reference_suffixes = NULL,
                                           method_name = NULL) {
  rank_inputs <- list()

  for (result_name in names(processed_by_result)) {
    processed <- processed_by_result[[result_name]]
    method_names <- if (!is.null(method_name)) {
      method_name
    } else {
      paste0(dataset, "_", reference_suffixes)
    }

    for (current_method in method_names) {
      df <- processed[[current_method]]

      if (is.null(df) || !is.data.frame(df) || !nrow(df)) {
        next
      }

      if (!all(c("perturbation", "rank_score") %in% names(df))) {
        warning(
          "Skipping `", result_name, "$", current_method,
          "` because it does not contain `perturbation` and `rank_score` columns.",
          call. = FALSE
        )
        next
      }

      input_name <- paste(result_name, current_method, sep = "_")
      rank_inputs[[input_name]] <- df
    }
  }

  rank_inputs
}


# Standardize pairwise identifiers, including RRA's current `Drug` column.
.standardize_pairwise_signature_result <- function(result_df, method) {
  id_candidates <- switch(
    method,
    CRank = c("Drug", "perturbation", "Name", "Item"),
    Dowdall = c("Drug", "perturbation", "Name", "Item"),
    RRA = c("Drug", "Name", "Item", "perturbation"),
    stop("Unknown method: ", method, call. = FALSE)
  )
  id_col <- id_candidates[id_candidates %in% names(result_df)][1L]
  if (is.na(id_col) || !length(id_col)) {
    stop("Could not identify the drug column in the ", method,
         " pairwise result.", call. = FALSE)
  }
  if (!identical(id_col, "Drug")) {
    result_df <- dplyr::rename(result_df, Drug = dplyr::all_of(id_col))
  }
  result_df
}


# Build the prefix used for returned harmonized score columns from the actual
# names supplied by the caller; no DEG/DEP-specific name is hardcoded.
.signature_result_suffix <- function(result_names) {
  result_names <- trimws(as.character(result_names))
  result_names <- result_names[!is.na(result_names) & nzchar(result_names)]

  if (!length(result_names)) {
    result_names <- "Signature"
  }

  result_names <- unique(result_names)
  result_names <- gsub("[^A-Za-z0-9_]+", "_", result_names)
  result_names <- gsub("_+", "_", result_names)
  result_names <- gsub("^_|_$", "", result_names)
  result_names <- result_names[nzchar(result_names)]

  if (!length(result_names)) {
    result_names <- "Signature"
  }

  paste(result_names, collapse = "_")
}


# Dynamically generate exactly the same style of first-stage score-column names
# used by the original implementation: rank_score_1, ..., rank_score_n.
.signature_rank_input_columns <- function(rank_inputs) {
  paste0("rank_score_", seq_along(rank_inputs))
}


# Return the native score-column name produced by each aggregation method.
.signature_score_column <- function(method) {
  switch(
    method,
    CRank = "CRank",
    Dowdall = "Dowdall_rank",
    RRA = "RRA_rank",
    stop("Unknown method: ", method, call. = FALSE)
  )
}


# First-stage aggregation.
#
# IMPORTANT FOR LEGACY COMPATIBILITY:
#   * no group_by()/summarise()
#   * no duplicate removal
#   * no max(rank_score)
#   * no NA -> 0 conversion
#   * no perturbation -> Drug rename before aggregation
.calculate_multi_signature_rank_aggregation <- function(method,
                                                        rank_inputs,
                                                        prior,
                                                        num_bin,
                                                        ties_method) {
  if (!is.list(rank_inputs) || length(rank_inputs) < 2L) {
    stop("At least two rank input tables are required.", call. = FALSE)
  }

  input_cols <- .signature_rank_input_columns(rank_inputs)

  prepared_inputs <- lapply(seq_along(rank_inputs), function(i) {
    rank_inputs[[i]] %>%
      dplyr::select(perturbation, rank_score) %>%
      dplyr::rename(!!input_cols[[i]] := rank_score)
  })

  combined_data <- Reduce(
    function(x, y) dplyr::full_join(x, y, by = "perturbation"),
    prepared_inputs
  ) %>%
    as.data.frame()

  switch(
    method,
    CRank = CRank(
      input_data = combined_data,
      reverse = FALSE,
      prior = prior,
      num_bin = num_bin,
      num_iter = 1000,
      ties_method = ties_method
    ),
    Dowdall = Dowdall(
      input_data = combined_data,
      ties_method = ties_method,
      reverse = TRUE
    ),
    RRA = RRA(
      input_data = combined_data,
      full = TRUE,
      exact = FALSE,
      ties_method = ties_method,
      reverse = TRUE
    ),
    stop("Unknown method: ", method, call. = FALSE)
  )
}


# Prepare one pairwise result for second-stage harmonization while preserving
# the old method-specific identifier conventions.
.prepare_pairwise_signature_result <- function(result_df,
                                               method,
                                               score_col) {
  if (!is.data.frame(result_df)) {
    stop("Pairwise aggregation result must be a data.frame.", call. = FALSE)
  }

  if (!score_col %in% names(result_df)) {
    stop("Expected score column `", score_col, "` was not found.", call. = FALSE)
  }

  id_candidates <- switch(
    method,
    CRank = c("Drug", "perturbation", "Name", "Item"),
    Dowdall = c("perturbation", "Drug", "Name", "Item"),
    RRA = c("Drug", "Name", "Item", "perturbation"),
    stop("Unknown method: ", method, call. = FALSE)
  )

  id_col <- id_candidates[id_candidates %in% names(result_df)][1L]

  if (is.na(id_col) || !length(id_col)) {
    stop(
      "Could not identify the drug column in the ", method,
      " pairwise result.",
      call. = FALSE
    )
  }

  result_df %>%
    dplyr::select(dplyr::all_of(c(id_col, score_col))) %>%
    dplyr::rename(Drug = dplyr::all_of(id_col))
}


# Standardize the final returned harmonized result to Drug + score while
# leaving the actual aggregation computation untouched.
.standardize_harmonized_signature_result <- function(result_df,
                                                     method,
                                                     score_col) {
  if (!is.data.frame(result_df)) {
    stop("Harmonized aggregation result must be a data.frame.", call. = FALSE)
  }

  if (!score_col %in% names(result_df)) {
    stop("Expected score column `", score_col, "` was not found.", call. = FALSE)
  }

  id_candidates <- switch(
    method,
    CRank = c("Drug", "perturbation", "Name", "Item"),
    Dowdall = c("perturbation", "Drug", "Name", "Item"),
    RRA = c("Drug", "Name", "Item", "perturbation"),
    stop("Unknown method: ", method, call. = FALSE)
  )

  id_col <- id_candidates[id_candidates %in% names(result_df)][1L]

  if (is.na(id_col) || !length(id_col)) {
    stop(
      "Could not identify the drug column in the ", method,
      " harmonized result.",
      call. = FALSE
    )
  }

  result_df %>%
    dplyr::select(dplyr::all_of(c(id_col, score_col))) %>%
    dplyr::rename(Drug = dplyr::all_of(id_col))
}
