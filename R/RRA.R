#' @title RRA Rank Aggregation Method
#'
#' @description
#' Aggregates multiple ranked drug lists using the Robust Rank Aggregation
#' (RRA) algorithm.
#'
#' @details
#' `RRA()` combines rankings from at least two numeric columns in `input_data`.
#' The first column is treated as the item identifier, typically `Drug`, and the
#' remaining numeric columns are converted to ranks before aggregation.
#'
#' Robust Rank Aggregation identifies items that appear consistently near the top
#' of multiple ranked lists more often than expected by chance. The algorithm
#' compares the observed ranks against a null model in which ranks are randomly
#' ordered. For each item, it calculates a rho score that reflects how
#' unexpectedly high the item appears across the input rankings.
#'
#' In DrugSigNet, ranks are normalized before applying the RRA scoring procedure.
#' The resulting rho score is reported as `RRA_pval`, and items with smaller
#' `RRA_pval` values receive better final `RRA_rank` values.
#'
#' Missing values are preserved during rank conversion and ignored by the RRA
#' scoring procedure. The final output is sorted by increasing RRA p-value.
#'
#' For pipe-friendly use, a data frame can be supplied as `object`; in that case,
#' it is used as `input_data`.
#'
#' @param object Optional `RankAggregation` object. For pipe-friendly use, this
#'   can also be a data frame used as `input_data`.
#' @param input_data Data frame containing items and ranking scores. The first
#'   column should contain item identifiers, and at least two additional columns
#'   must be numeric.
#' @param full Logical; whether to return the full RRA output. Default is
#'   `TRUE`.
#' @param exact Logical; whether to calculate exact p-values from rho scores.
#'   Default is `FALSE`.
#' @param ties_method Method used to resolve ties during rank conversion. One of
#'   `"max"`, `"min"`, `"average"`, `"first"`, `"last"`, `"random"`, or
#'   `"dense"`. Default is `"max"`.
#' @param reverse Logical; if `FALSE`, smaller input values are treated as
#'   better, which is appropriate when numeric columns already represent ranks.
#'   If `TRUE`, larger input values are treated as better. Default is `FALSE`.
#'
#' @return
#' A `RankAggregation` object containing RRA results in `object@result`,
#' including `Drug`, `RRA_pval`, and `RRA_rank`.
#'
#' @examples
#' \dontrun{
#' rank_df <- data.frame(
#'   Drug = c("drug_a", "drug_b", "drug_c"),
#'   Method1 = c(1, 2, 3),
#'   Method2 = c(2, 1, 3)
#' )
#'
#' res <- RRA(
#'   input_data = rank_df,
#'   ties_method = "max",
#'   full = TRUE,
#'   exact = FALSE,
#'   reverse = FALSE
#' )
#'
#' # Pipe-friendly use
#' res_pipe <- RRA(rank_df, reverse = FALSE)
#' }
#'
#' @references
#' Kolde R, Laur S, Adler P, Vilo J. Robust rank aggregation for gene list
#' integration and meta-analysis. \emph{Bioinformatics}. 2012;28(4):573-580.
#' \doi{10.1093/bioinformatics/btr709}
#'
#' PMID: 22247279; PMCID: PMC3278763.
#'
#' @importFrom dplyr dense_rank mutate arrange ungroup rename
#' @importFrom RobustRankAggreg aggregateRanks rhoScores
#' @importFrom tibble column_to_rownames
#' @export
setGeneric("RRA", function(object = NULL, input_data, full = TRUE,
                           exact = FALSE, ties_method = "max",
                           reverse = FALSE) {
  if (missing(input_data) && is.data.frame(object)) {
    input_data <- object
    object <- NULL
  }
  if (missing(input_data) || !is.data.frame(input_data)) {
    stop("`input_data` must be a data frame and cannot be missing.")
  }

  if (is.null(object)) {
    object <- RankAggregation(
      result = data.frame(),
      input_data = input_data,
      rank_aggregation = "RRA",
      ties_method = ties_method,
      reverse = reverse,
      full = full,
      exact = exact
    )
  }

  standardGeneric("RRA")
})

#' @rdname RRA
setMethod(
  "RRA",
  signature = "RankAggregation",
  function(object) {
    params <- object@parameters
    input_data <- params$input_data

    # Identify numeric columns
    numeric_cols <- sapply(input_data, is.numeric)

    # Validate input data
    if (sum(numeric_cols) < 2) {
      stop("`input_data` must have at least two numeric columns for RRA.")
    }

    # Replace NA values and apply ranking
    rank_matrix <- input_data
    rank_matrix[, numeric_cols] <- lapply(rank_matrix[, numeric_cols], function(x) {
      if (params$ties_method == "dense") {
        if (params$reverse) {
          ifelse(is.na(x), NA, dplyr::dense_rank(-x)) # Preserve NA values
        } else {
          ifelse(is.na(x), NA, dplyr::dense_rank(x)) # Preserve NA values
        }
      } else {
        if (params$reverse) {
          rank(-x, ties.method = params$ties_method, na.last = "keep")
        } else {
          rank(x, ties.method = params$ties_method, na.last = "keep")
        }
      }
    })

    # Convert the first column to row names
    rank_matrix <- rank_matrix %>%
      data.frame() %>%
      `rownames<-`(NULL) %>%
      tibble::column_to_rownames(var = names(rank_matrix)[1])

    # Normalize ranks
    max_val <- max(rank_matrix, na.rm = TRUE)
    normalized_ranks <- rank_matrix / max_val

    # Apply rhoScores function
    if (!exists("rhoScores")) {
      stop("Function `rhoScores` is not defined or available in the environment.")
    }
    rra_scores <- apply(normalized_ranks, 1, function(r) RobustRankAggreg::rhoScores(r = r, exact = params$exact))

    # Create results dataframe
    rra_results <- data.frame(Drug = rownames(rank_matrix), Score = rra_scores)
    rra_results <- rra_results[order(rra_results$Score), ] # Sort by score

    # Rank RRA scores
    rra_results <- rra_results %>%
      mutate(RRA_rank = if (params$ties_method == "dense") {
        dplyr::dense_rank(Score)
      } else {
        rank(Score, ties.method = params$ties_method, na.last = "keep")
      }) %>%
      rename(RRA_pval = Score)

    # Assign results back to the object
    object@result <- rra_results
    rownames(object@result) <- NULL

    object@parameters <- filterParametersByMethod(object)
    return(object)


    return(rra_results)
  }
)

#' Helper Function to Compute Ranks for a Vector
#'
#' @description
#' This function computes ranks for a numeric vector, with optional tie-breaking methods and reverse ordering.
#'
#' @param x A numeric vector to rank.
#' @param ties_method A character string specifying the method to handle ties ("average", "first", "last", "random", "dense").
#' @param reverse A logical indicating if rankings should be reversed.
#'
#' @return A numeric vector of ranks with ties handled and optional reversal applied.
#' @keywords internal
.transform_to_ranks <- function(x, ties_method, reverse) {
  if (reverse) x <- -x
  if (ties_method == "dense") {
    return(dplyr::dense_rank(x))
  } else {
    return(rank(x, ties.method = ties_method, na.last = "keep"))
  }
}
