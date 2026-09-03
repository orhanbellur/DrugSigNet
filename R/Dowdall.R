#' @title Dowdall Rank Aggregation Method
#'
#' @description
#' Aggregates multiple ranked drug lists using the Dowdall method.
#'
#' @details
#' `Dowdall()` combines rankings from at least two numeric columns in
#' `input_data`. The first column is treated as the item identifier, typically
#' `Drug`, and the remaining numeric columns are converted to ranks before
#' aggregation.
#'
#' The Dowdall method is a positional voting system that assigns each item a
#' reciprocal rank score (`1 / rank`) within each input ranking. These reciprocal
#' scores are summed across ranking columns to produce a consensus score. Items
#' ranked highly across several methods receive larger Dowdall scores and better
#' final consensus ranks.
#'
#' In DrugSigNet, reciprocal rank scores are scaled to the range `[0, 1]` before
#' summation so that different ranking methods contribute comparably.
#'
#' Missing numeric values are replaced with zero before ranking. Values equal to
#' zero are treated as unranked entries and contribute zero to the final
#' consensus score.
#'
#' For pipe-friendly use, a data frame can be supplied as `object`; in that case,
#' it is used as `input_data`.
#'
#' @param object Optional `RankAggregation` object. For pipe-friendly use, this
#'   can also be a data frame used as `input_data`.
#' @param input_data Data frame containing items and ranking scores. The first
#'   column should contain item identifiers, and at least two additional columns
#'   must be numeric.
#' @param ties_method Method used to resolve ties during rank conversion. One of
#'   `"max"`, `"min"`, `"average"`, `"first"`, `"last"`, `"random"`, or
#'   `"dense"`. Default is `"max"`.
#' @param reverse Logical; if `FALSE`, smaller input values are treated as
#'   better, which is appropriate when numeric columns already represent ranks.
#'   If `TRUE`, larger input values are treated as better. Default is `FALSE`.
#'
#' @return
#' A `RankAggregation` object containing the Dowdall aggregation results in
#' `object@result`.
#'
#' @examples
#' \dontrun{
#' rank_df <- data.frame(
#'   Drug = c("drug_a", "drug_b", "drug_c"),
#'   Method1 = c(1, 2, 3),
#'   Method2 = c(2, 1, 3)
#' )
#'
#' res <- Dowdall(
#'   input_data = rank_df,
#'   ties_method = "max",
#'   reverse = FALSE
#' )
#'
#' # Pipe-friendly use
#' res_pipe <- Dowdall(rank_df, reverse = FALSE)
#' }
#'
#' @references
#' Reilly B. Social choice in the south seas: Electoral innovation and the
#' Borda count in the Pacific Island countries.
#' \emph{International Political Science Review}. 2002;23(4):355-372.
#' \doi{10.1177/0192512102023004002}
#'
#' @importFrom dplyr mutate dense_rank
#' @export
setGeneric("Dowdall", function(object = NULL, input_data, ties_method = "max", reverse = FALSE) {
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
      rank_aggregation = "Dowdall",
      ties_method = ties_method,
      reverse = reverse
    )
  }
  standardGeneric("Dowdall")
})

#' Dowdall for a RankAggregation Object
#'
#' @param object A `RankAggregation` object containing the input rankings and
#'   Dowdall parameters.
#' @return The supplied `RankAggregation` object with aggregation results stored
#'   in its `result` slot.
setMethod(
  "Dowdall",
  signature = "RankAggregation",
  function(object) {
    # Access parameters from the object
    params <- object@parameters
    input_data <- params$input_data # Access input_data from the parameters list
    ties_method <- params$ties_method
    reverse <- params$reverse

    # Validate numeric columns
    numeric_cols <- sapply(input_data, is.numeric)
    if (sum(numeric_cols) < 2) {
      stop("The `input_data` must have at least 2 numeric columns.")
    }

    # Compute ranks and scale data
    rankagg_result <- input_data %>% .Dowdall_score(., ties_method = ties_method, reverse = reverse)

    # Assign results to the object
    object@result <- rankagg_result

    # Filter the `parameters` slot dynamically
    object@parameters <- filterParametersByMethod(object)
    return(object)
  }
)


# Helper Function to Compute Ranks
.Dowdall_score <- function(data_input, ties_method, reverse) {
  # Helper function to scale a numeric vector to a range of 0-1
  scale_to_range <- function(x) {
    range_diff <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
    if (range_diff == 0) {
      return(rep(0, length(x))) # Handle constant columns
    }
    return((x - min(x, na.rm = TRUE)) / range_diff)
  }

  # Identify numeric columns
  numeric_cols <- sapply(data_input, is.numeric)

  # Replace NA values with 0 for numeric columns
  data_input[, numeric_cols] <- lapply(data_input[, numeric_cols], function(x) ifelse(is.na(x), 0, x))

  # Apply ranking and scaling transformation
  data_input[, numeric_cols] <- lapply(data_input[, numeric_cols], function(x) {
    if (all(x == 0, na.rm = TRUE)) {
      # If all values are zero, return a vector of zeros
      return(rep(0, length(x)))
    } else {
      # Apply ranking logic
      rank_val <- ifelse(
        x == 0,
        0, # Assign zero if the value is zero
        if (ties_method == "dense") {
          # Dense rank
          if (reverse) {
            dplyr::dense_rank(-x)
          } else {
            dplyr::dense_rank(x)
          }
        } else {
          # Standard rank
          if (reverse) {
            rank(-x, ties.method = ties_method, na.last = "keep")
          } else {
            rank(x, ties.method = ties_method, na.last = "keep")
          }
        }
      )

      # Transform rank to 1/rank for non-zero values
      transformed_rank <- ifelse(rank_val != 0, 1 / rank_val, 0)

      # Scale the transformed ranks to a range of 0-1
      return(scale_to_range(transformed_rank))
    }
  })

  # Calculate Dowdall_score as the row-wise sum of numeric columns
  data_input$Dowdall_score <- rowSums(data_input[, numeric_cols], na.rm = TRUE)

  # Add a ranked column for Dowdall_score
  data_input$Dowdall_rank <- if (ties_method == "dense") {
    dplyr::dense_rank(-data_input$Dowdall_score)
  } else {
    rank(-data_input$Dowdall_score, ties.method = ties_method, na.last = "keep")
  }

  return(data_input)
}
