# Package-level imports for dependencies used through qualified calls in
# optional rendering branches. Declaring the symbols keeps DESCRIPTION and the
# generated namespace consistent for R CMD check.
#'
#' @importFrom data.table as.data.table copy data.table is.data.table rbindlist
#'   set setcolorder setnames
#' @importFrom ggalluvial geom_alluvium geom_stratum stat_stratum
#' @importFrom ggforce geom_arc_bar geom_circle
#' @importFrom plotly layout plot_ly save_image
#' @importFrom wordcloud wordcloud
NULL
