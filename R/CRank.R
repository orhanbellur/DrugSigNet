#' @title CRank Rank Aggregation Method
#'
#' @description
#' Aggregates multiple ranked drug lists using the CRank algorithm.
#'
#' @details
#' `CRank()` combines rankings from at least two numeric columns in `input_data`.
#' The first column is treated as the item identifier, typically `Drug`, and the
#' remaining numeric columns are converted to ranks before running the
#' Python-based CRank implementation.
#'
#' CRank is a Bayesian rank aggregation algorithm that estimates a consensus
#' ranking from multiple input rankings. It models the probability that an item
#' belongs to the top-ranked group across different methods and uses iterative
#' inference to update this probability until convergence. Items consistently
#' ranked highly across methods receive better consensus ranks.
#'
#' In DrugSigNet, CRank is used to integrate evidence across multiple
#' drug-ranking methods, such as signature-based or network-based scores, into a
#' single consensus ranking.
#'
#' Missing numeric values are replaced with zero before ranking. Values equal to
#' zero are treated as unranked entries and remain zero in the CRank input.
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
#' @param prior Numeric prior used by CRank. Default is `0.093`.
#' @param num_bin Number of bins used by CRank. Default is `200`.
#' @param num_iter Number of CRank iterations. Default is `1000`.
#' @param reverse Logical; if `TRUE`, smaller input values are treated as better,
#'   which is appropriate when numeric columns already represent ranks. If
#'   `FALSE`, larger input values are treated as better. Default is `FALSE`.
#'
#' @return
#' A `RankAggregation` object containing the aggregated CRank results in
#' `object@result` and convergence information in `object@parameters`.
#'
#' @examples
#' \dontrun{
#' rank_df <- data.frame(
#'   Drug = c("drug_a", "drug_b", "drug_c"),
#'   Method1 = c(1, 2, 3),
#'   Method2 = c(2, 1, 3)
#' )
#'
#' res <- CRank(
#'   input_data = rank_df,
#'   ties_method = "max",
#'   prior = 0.093,
#'   num_bin = 200,
#'   num_iter = 1000,
#'   reverse = TRUE
#' )
#'
#' # Pipe-friendly use
#' res_pipe <- CRank(rank_df, reverse = TRUE)
#' }
#'
#' @references
#' Zitnik M, Sosič R, Leskovec J. Prioritizing network communities.
#' \emph{Nature Communications}. 2018;9:2544.
#' \doi{10.1038/s41467-018-04948-5}
#'
#' @importFrom dplyr mutate dense_rank
#' @importFrom openxlsx write.xlsx
#' @importFrom reticulate py_run_file
#' @export

setGeneric(
  "CRank",
  function(object = NULL, input_data, ties_method = "max", prior = 0.093, num_bin = 200,
           num_iter = 1000, reverse = FALSE) {
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
        rank_aggregation = "CRank",
        input_data = input_data,
        ties_method = ties_method,
        prior = prior,
        num_bin = num_bin,
        num_iter = num_iter,
        reverse = reverse
      )
    }
    standardGeneric("CRank")
  }
)

#' @rdname CRank
setMethod(
  "CRank",
  signature = "RankAggregation",
  function(object) {
    # Extract parameters from the object
    params <- object@parameters
    input_data <- as.data.frame(params$input_data)
    rownames(input_data) <- NULL

    # Identify numeric columns
    numeric_cols <- sapply(input_data, is.numeric)

    # Validate numeric columns
    if (sum(numeric_cols) < 2) {
      stop("The `input_data` must have at least two numeric columns for CRank aggregation.")
    }

    # Create a temporary file for CRank input
    crank_input_file <- tempfile(fileext = ".xlsx")
    on.exit(unlink(crank_input_file), add = TRUE)

    if (any(input_data[, numeric_cols] > 0, na.rm = TRUE)) {
      # Prepare CRank input
      crank_input <- .CRank_input(input_data, ties_method = params$ties_method, reverse = params$reverse)

      # Write to a temporary Excel file
      openxlsx::write.xlsx(crank_input, crank_input_file)

      # Locate the Python script dynamically
      CRank_path <- system.file("Python", "CRank.py", package = "DrugSigNet")
      if (!file.exists(CRank_path)) {
        stop("Unable to find Python script: CRank.py in the package directory.")
      }

      # Call Python CRank with error handling
      tryCatch(
        {
          # Declare transitive requirements before evaluating the script so
          # reticulate's managed Python environment includes scipy on Linux.
          if ("py_require" %in% getNamespaceExports("reticulate")) {
            reticulate::py_require(c("numpy", "pandas", "scipy", "openpyxl"))
          }
          py <- reticulate::py_run_file(CRank_path)
          CRank_res <- py$CRank(
            file_name = crank_input_file, sheet_name = "Sheet 1",
            ties_method = params$ties_method, prior = params$prior,
            num_bin = as.integer(params$num_bin), num_iter = as.integer(params$num_iter)
          )
        },
        error = function(e) {
          stop("Error executing Python script: ", e$message)
        }
      )

      # Process results
      rankagg_result <- CRank_res[[1]] #%>% dplyr::mutate(Drug = tolower(Drug))
    } else {
      # Handle non-numeric or invalid data
      rankagg_result <- data.frame(Drug = input_data[, 1]) %>%
        dplyr::mutate(CRank = nrow(input_data))
      CRank_res <- list(rankagg_result, TRUE, 1, 1)
    }

    # Assign results to the object
    object@result <- rankagg_result
    object@parameters$converged <- CRank_res[[2]]
    object@parameters$correlation <- CRank_res[[3]]

    # Filter the `parameters` slot dynamically
    object@parameters <- filterParametersByMethod(object)

    # Return the updated object
    return(object)
  }
)

# Helper Function to Prepare CRank Input Data
.CRank_input <- function(input_data, ties_method, reverse) {
  # Replace NA values in numeric columns with 0
  numeric_columns <- sapply(input_data, is.numeric)
  input_data[numeric_columns] <- lapply(input_data[numeric_columns], function(x) ifelse(is.na(x), 0, x))

  # Apply ranking based on the reverse parameter
  rank_fun <- if (reverse) function(x) -x else function(x) x
  apply_ranking <- function(x) {
    if (ties_method == "dense") {
      return(ifelse(x > 0, dplyr::dense_rank(rank_fun(x)), 0))
    } else {
      return(ifelse(x > 0, rank(rank_fun(x), ties.method = ties_method, na.last = "keep"), 0))
    }
  }

  # Apply ranking transformation to numeric columns
  input_data[numeric_columns] <- lapply(input_data[numeric_columns], apply_ranking)

  return(input_data)
}
