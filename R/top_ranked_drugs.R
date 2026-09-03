#' @title Extract Top-Ranked Drugs Across Rank Columns
#'
#' @description
#' Extracts the union of drugs ranked within the top `top_n` in one or more
#' selected rank columns.
#'
#' @details
#' `extract_top_ranked_drugs()` is used to collect drugs that appear among the
#' top-ranked candidates in at least one ranking method. A drug is retained if
#' any selected rank column is less than or equal to `top_n`.
#'
#' If multiple rows are present for the same drug, the function returns one row
#' per drug and keeps the minimum rank observed for each selected rank column.
#' Missing rank columns are ignored. If `rank_cols = NULL`, all numeric columns
#' except the identifier column are used.
#'
#' `get_top_union_drugs()` is a convenience wrapper that returns only the unique
#' drug identifiers from `extract_top_ranked_drugs()`.
#'
#' @param df Data frame containing a drug identifier column and one or more
#'   numeric rank columns.
#' @param rank_cols Character vector of rank columns to evaluate. Missing
#'   columns are ignored. If `NULL`, all numeric columns except `id_col` are
#'   used.
#' @param top_n Positive integer number of top-ranked entries to keep per rank
#'   column. Default is `100`.
#' @param id_col Name of the drug identifier column. Default is `"Drug"`.
#'
#' @return
#' A data frame containing one row per selected drug, the identifier column, and
#' the selected rank columns.
#'
#' @examples
#' rank_df <- data.frame(
#'   Drug = c("drug_a", "drug_b", "drug_c", "drug_a"),
#'   Method1 = c(1, 50, 120, 3),
#'   Method2 = c(150, 2, 80, 10)
#' )
#'
#' extract_top_ranked_drugs(
#'   df = rank_df,
#'   rank_cols = c("Method1", "Method2"),
#'   top_n = 100
#' )
#'
#' get_top_union_drugs(
#'   df = rank_df,
#'   rank_cols = c("Method1", "Method2"),
#'   top_n = 100
#' )
#'
#' @importFrom dplyr filter if_any all_of select group_by summarise across arrange distinct
#' @export
extract_top_ranked_drugs <- function(df,
                                     rank_cols = NULL,
                                     top_n = 100,
                                     id_col = "Drug") {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }
  if (!is.character(id_col) || length(id_col) != 1L || is.na(id_col) || !nzchar(id_col)) {
    stop("`id_col` must be a single non-empty character string.", call. = FALSE)
  }
  if (!id_col %in% names(df)) {
    stop("`df` must contain the identifier column `", id_col, "`.", call. = FALSE)
  }
  if (!is.numeric(top_n) || length(top_n) != 1L || is.na(top_n) || top_n < 1) {
    stop("`top_n` must be a single positive integer value.", call. = FALSE)
  }
  top_n <- as.integer(top_n)

  if (is.null(rank_cols)) {
    rank_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    rank_cols <- setdiff(rank_cols, id_col)
  } else {
    if (!is.character(rank_cols)) {
      stop("`rank_cols` must be NULL or a character vector.", call. = FALSE)
    }
    rank_cols <- unique(rank_cols)
  }

  rank_cols <- intersect(rank_cols, names(df))
  if (!length(rank_cols)) {
    return(df[0, id_col, drop = FALSE])
  }

  non_numeric_cols <- rank_cols[!vapply(df[rank_cols], is.numeric, logical(1))]
  if (length(non_numeric_cols)) {
    stop(
      "`rank_cols` must identify numeric rank columns. Non-numeric columns: ",
      paste(non_numeric_cols, collapse = ", "),
      call. = FALSE
    )
  }

  df %>%
    dplyr::filter(
      dplyr::if_any(dplyr::all_of(rank_cols), ~ !is.na(.x) & .x <= top_n)
    ) %>%
    dplyr::select(dplyr::all_of(c(id_col, rank_cols))) %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(rank_cols),
        ~ if (all(is.na(.x))) NA_real_ else min(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data[[id_col]])
}

#' @rdname extract_top_ranked_drugs
#' @return `get_top_union_drugs()` returns a one-column data frame containing
#'   the unique selected drug identifiers.
#' @export
get_top_union_drugs <- function(df,
                                rank_cols,
                                top_n = 100,
                                id_col = "Drug") {
  extract_top_ranked_drugs(
    df = df,
    rank_cols = rank_cols,
    top_n = top_n,
    id_col = id_col
  ) %>%
    dplyr::select(dplyr::all_of(id_col)) %>%
    dplyr::distinct()
}
