.report_visualization_items <- function(visualization) {
  if (is.null(visualization) || !is.list(visualization$plots)) {
    return(list())
  }
  visualization$plots
}

.report_has_annotation <- function(object) {
  is.list(object) && !is.null(object$DrugAnnotation)
}

.report_has_visualization <- function(object) {
  is.list(object) && length(.report_visualization_items(object$Visualization)) > 0L
}

.report_visualization_title <- function(key) {
  labels <- c(
    drug_indications = "Drug indications",
    top_k_hits = "Top K hits",
    top_k_overlap = "Top K overlap",
    rank_agreement = "Rank correlation",
    rank_distribution_scatter = "Rank distributions",
    enriched_terms = "Functional Enrichment",
    drug_hierarchy = "Drug hierarchy"
  )
  if (key %in% names(labels)) {
    return(unname(labels[[key]]))
  }
  tools::toTitleCase(gsub("_", " ", key, fixed = TRUE))
}

.report_normalize_pipeline_result <- function(x) {
  if (!.is_drug_searching_pipeline(x)) {
    return(x)
  }

  result <- list(
    DrugSearching = methods::slot(x, "DrugSearching"),
    RankAggregation = methods::slot(x, "RankAggregation"),
    type = methods::slot(x, "type")
  )
  object_slots <- methods::slotNames(x)
  if ("DrugAnnotation" %in% object_slots) {
    result$DrugAnnotation <- methods::slot(x, "DrugAnnotation")
  }
  if ("Visualization" %in% object_slots) {
    result$Visualization <- methods::slot(x, "Visualization")
  }
  for (name in c("metadata", "provenance")) {
    value <- attr(x, name, exact = TRUE)
    if (!is.null(value)) result[[name]] <- value
  }
  result
}

.report_expand_pipeline_result <- function(x) {
  result <- .report_normalize_pipeline_result(x)
  sections <- intersect(
    c("signature", "network", "integrated"),
    names(result$RankAggregation)
  )

  if (length(sections) == 0L) {
    return(list(result))
  }

  lapply(sections, function(section) {
    raw <- result$DrugSearching$Raw[[section]]
    processed <- result$DrugSearching$Processed[[section]]
    expanded <- list(
      DrugSearching = list(
        Raw = if (is.null(raw)) list() else raw,
        Processed = if (is.null(processed)) list() else processed
      ),
      RankAggregation = result$RankAggregation[[section]],
      DrugAnnotation = result$DrugAnnotation[[section]],
      Visualization = result$Visualization[[section]],
      type = if (identical(section, "integrated")) "integration" else section
    )
    for (name in intersect(c("metadata", "provenance", "parameters"), names(result))) {
      expanded[[name]] <- result[[name]]
    }
    expanded
  })
}

.report_result_title <- function(object, index = NULL) {
  object_type <- if (is.list(object)) object$type else NULL
  title <- switch(
    object_type,
    signature = "Signature-based results",
    network = "Network-based results",
    integration = "Integrated results",
    NULL
  )
  if (!is.null(title)) {
    return(title)
  }
  paste("Result object", if (is.null(index)) "" else index)
}


#' @title Write DrugSigNet Analysis Report
#'
#' @description
#' Generates an HTML or PDF report summarizing one or more DrugSigNet pipeline
#' results.
#'
#' @details
#' `write_report()` creates a report from pipeline results returned by
#' DrugSigNet workflows. A combined `drugRepurposingPipeline()` result is shown
#' in separate signature-based, network-based, and integrated report parts.
#'
#' Scientific HTML reports provide sidebar navigation, an executive summary,
#' input and rank-availability statistics, a verified candidate shortlist,
#' ranking agreement, focused annotations/enrichment, interpretation, and
#' compact methods/provenance. Complete Results retains full result tables with
#' client-side search, stable sorting, pagination, expandable cells, and CSV
#' downloads of all matching rows. This report does not require Shiny or DT.
#'
#' Reports can optionally export pipeline results as Excel files using
#' `write_pipeline_results()`. PDF reports require a LaTeX engine such as
#' `pdflatex`, `xelatex`, or `lualatex`. If the `tinytex` R package is
#' installed but no LaTeX engine is available, `write_report()` attempts to
#' install TinyTeX automatically before rendering the PDF.
#'
#' PDF reports use a concise scientific layout, prioritized annotations, compact
#' input and parameter summaries, and a report-time software inventory. Stored
#' ranks are never recalculated. Invalid/nonpositive ranks sort after valid
#' ranks; candidates without any valid rank are excluded. Full results remain
#' in the input object or optional supplementary RDS / Excel exports.
#' Section, orientation, DPI, and table-row controls all use the same concise
#' PDF renderer; explicitly passing a control never selects a legacy layout.
#' PDFs include a linked contents page and stored pipeline figures, placed in
#' the comparison, annotation, or enrichment section. Unavailable Plotly sunbursts
#' may use a labelled summary of stored branch values as a static fallback.
#'
#' HTML figures are generated from every entry in
#' `object@Visualization$plots`; it is not limited to a fixed set of plot names.
#' Missing annotations, enrichment, and input metadata are explicitly identified.
#' Stored Plotly figures remain interactive; compatible ggplots can be converted
#' when `interactive = TRUE`.
#'
#' HTML reports can be generated either as:
#' \itemize{
#'   \item a standalone scientific HTML report (`dashboard = "tabset"`, retained
#'     as the compatibility name), or
#'   \item a Shiny dashboard application.
#' }
#'
#' @param object A DrugSigNet pipeline result object or a list of pipeline
#'   result objects.
#' @param title Optional report title. Defaults to
#'   `"DrugSigNet <current date>"`.
#' @param file Output file name with or without extension. Defaults to
#'   `"drugsignet_<current date>"`.
#' @param write_results Logical indicating whether pipeline results should also
#'   be exported as Excel files. Default is `FALSE`.
#' @param device Output format. One of `"html"` (default) or `"pdf"`.
#' @param interactive Logical; if `TRUE` and `device = "html"`, compatible
#'   `ggplot` objects are rendered as interactive Plotly figures.
#' @param author Optional author name.
#' @param dashboard HTML report layout. One of `"tabset"` (default) or
#'   `"shinydashboard"`.
#' @param template PDF template. `"scientific"` produces the publication-style
#'   layout. This argument does not alter HTML or Shiny reports.
#' @param include_summary,include_methods,include_interpretation Logical
#'   scientific HTML/PDF section controls.
#' @param include_reproducibility Logical scientific HTML/PDF provenance and
#'   rendering-software control.
#' @param include_appendix Logical PDF supplementary-material section control.
#' @param table_orientation PDF table orientation: `"auto"`, `"portrait"`, or
#'   `"landscape"`.
#' @param figure_dpi Resolution used for static HTML/PDF figures.
#' @param table_rows Maximum rows in PDF input, method, and provenance summaries.
#'   Candidate and annotation tables follow `top_n`. Complete results remain
#'   in the pipeline object and optional exports.
#'
#' @param top_n Positive integer number of prioritized candidates in scientific
#'   HTML/PDF reports (default 10).
#'   Selection uses increasing stored CRank, Dowdall, RRA, then identifier;
#'   ties are preserved and the display does not expand ties at the cutoff.
#' @param enrichment_n Positive integer number of enrichment terms across all
#'   libraries in scientific HTML/PDF reports (default 12), ordered by stored
#'   adjusted p-value.
#' @param supplementary Logical; save the complete original input as a companion
#'   `_supplementary.rds` file (default `FALSE`).
#'
#' @return
#' Invisibly returns the path to the generated report.
#'
#' @examples
#' \dontrun{
#' ## Single pipeline result
#' write_report(result)
#'
#' ## HTML report with interactive plots
#' write_report(
#'   object = result,
#'   file = "DrugSigNet_Report",
#'   device = "html",
#'   interactive = TRUE
#' )
#'
#' ## PDF report
#' write_report(
#'   object = result,
#'   device = "pdf",
#'   title = "Drug Repurposing Analysis",
#'   author = "Analysis Team",
#'   top_n = 20,
#'   include_summary = TRUE,
#'   include_methods = TRUE,
#'   include_reproducibility = TRUE,
#'   table_orientation = "auto",
#'   figure_dpi = 300
#' )
#'
#' ## Multiple pipeline results
#' write_report(
#'   object = list(result1, result2),
#'   dashboard = "tabset"
#' )
#' }
#'
#' @export
write_report <- function(object,
                         title = NULL,
                         file = NULL,
                         write_results = FALSE,
                         device = c("html", "pdf"),
                         interactive = FALSE,
                         author = NULL,
                         dashboard = c("tabset", "shinydashboard"),
                         template = "scientific",
                         top_n = 10L,
                         include_summary = TRUE,
                         include_methods = TRUE,
                         include_interpretation = TRUE,
                         include_reproducibility = TRUE,
                         include_appendix = TRUE,
                         table_orientation = c("auto", "portrait", "landscape"),
                         figure_dpi = 300,
                         table_rows = 50L,
                         enrichment_n = 12L,
                         supplementary = FALSE) {
  device <- match.arg(device)
  dashboard <- match.arg(dashboard)
  table_orientation <- match.arg(table_orientation)
  if (!identical(template, "scientific")) stop("`template` must be 'scientific'.", call. = FALSE)
  if (!is.numeric(figure_dpi) || length(figure_dpi) != 1L || !is.finite(figure_dpi) || figure_dpi <= 0) stop("`figure_dpi` must be positive.", call. = FALSE)
  if (!is.numeric(table_rows) || length(table_rows) != 1L || !is.finite(table_rows) || table_rows < 1 || table_rows != floor(table_rows)) stop("`table_rows` must be a positive integer.", call. = FALSE)

  for (nm in c("include_summary", "include_methods", "include_interpretation",
               "include_reproducibility", "include_appendix")) {
    value <- get(nm)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop("`", nm, "` must be TRUE or FALSE.", call. = FALSE)
    }
  }

  for (nm in c("top_n", "enrichment_n")) {
    value <- get(nm)
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value < 1 || value != floor(value) || value > .Machine$integer.max) {
      stop("`", nm, "` must be a positive integer.", call. = FALSE)
    }
  }
  if (!is.logical(supplementary) || length(supplementary) != 1L || is.na(supplementary)) {
    stop("`supplementary` must be TRUE or FALSE.", call. = FALSE)
  }

  if (is.null(object)) {
    stop("`object` cannot be NULL.", call. = FALSE)
  }

  is_pipeline_result <- function(x) {
    x_norm <- .report_normalize_pipeline_result(x)
    is.list(x_norm) && !is.null(x_norm$RankAggregation)
  }

  raw_objects <- if (is_pipeline_result(object)) {
    list(object)
  } else if (is.list(object) && length(object) > 0 && all(vapply(object, is_pipeline_result, logical(1)))) {
    object
  } else {
    stop("`object` must be a pipeline result list or a list of pipeline results.", call. = FALSE)
  }
  objects <- unlist(
    lapply(raw_objects, .report_expand_pipeline_result),
    recursive = FALSE
  )

  base_name <- if (is.null(file)) {
    paste0("drugsignet_", Sys.Date())
  } else {
    tools::file_path_sans_ext(file)
  }
  report_title <- if (is.null(title)) paste0("DrugSigNet ", Sys.Date()) else title

  if (isTRUE(write_results)) {
    for (i in seq_along(objects)) {
      excel_file <- paste0(base_name, "_results_", i, ".xlsx")
      write_pipeline_results(objects[[i]], file_path = excel_file, top_n = 100)
    }
  }

  if (isTRUE(supplementary)) saveRDS(object, paste0(base_name, "_supplementary.rds"))

  rmd_file <- paste0(base_name, ".Rmd")
  output_file <- paste0(base_name, ".", ifelse(device == "html", "html", "pdf"))

  if (device == "pdf") {
    return(.write_pdf_report(
      objects, report_title, base_name, author, top_n, enrichment_n, supplementary,
      options = list(include_summary = include_summary, include_methods = include_methods,
        include_interpretation = include_interpretation,
        include_reproducibility = include_reproducibility, include_appendix = include_appendix,
        table_orientation = table_orientation, figure_dpi = figure_dpi, table_rows = table_rows)
    ))
  }

  if (dashboard == "shinydashboard") {
    if (device != "html") {
      stop("`dashboard = 'shinydashboard'` requires `device = 'html'`.", call. = FALSE)
    }

    app_dir <- paste0(base_name, "_shinydashboard")
    if (!dir.exists(app_dir)) dir.create(app_dir, recursive = TRUE)

    data_path <- file.path(app_dir, "report_objects.rds")
    saveRDS(objects, data_path)

    app_lines <- c(
      "library(shiny)",
      "library(shinydashboard)",
      "library(DT)",
      "library(plotly)",
      "library(ggplot2)",
      "",
      "report_objects <- readRDS('report_objects.rds')",
      "",
      "extract_df <- function(x) {",
      "  if (is.null(x)) return(NULL)",
      "  if (isS4(x) && 'result' %in% methods::slotNames(x)) x <- x@result",
      "  if (is.list(x) && !is.data.frame(x) && !is.null(x$result) && is.data.frame(x$result)) x <- x$result",
      "  if (!is.data.frame(x)) return(NULL)",
      "  x",
      "}",
      "",
      "extract_plot <- function(x) {",
      "  if (is.null(x)) return(NULL)",
      "  if (is.list(x) && !is.null(x$plot)) return(x$plot)",
      "  x",
      "}",
      "",
      "visualization_items <- function(viz) {",
      "  if (is.null(viz) || !is.list(viz[['plots']])) return(list())",
      "  viz[['plots']]",
      "}",
      "plot_title <- function(key) tools::toTitleCase(gsub('_', ' ', key, fixed = TRUE))",
      "result_title <- function(obj, index) {",
      "  switch(obj$type, signature = 'Signature-based results', network = 'Network-based results', integration = 'Integrated results', paste('Result object', index))",
      "}",
      "result_choices <- stats::setNames(",
      "  seq_along(report_objects),",
      "  vapply(seq_along(report_objects), function(i) result_title(report_objects[[i]], i), character(1))",
      ")",
      "",
      "select_harmonized_rank_df <- function(rank_items, obj_type) {",
      "  if (!is.list(rank_items)) return(NULL)",
      "  target <- if (identical(obj_type, 'signature')) {",
      "    'Signature_Harmonized'",
      "  } else if (identical(obj_type, 'network')) {",
      "    'Network_Harmonized'",
      "  } else if (identical(obj_type, 'both')) {",
      "    'Signature_Network_Harmonized'",
      "  } else {",
      "    'Signature_Network_Harmonized'",
      "  }",
      "  df <- extract_df(rank_items[[target]])",
      "  if (!is.null(df)) return(df)",
      "  NULL",
      "}",
      "",
      "safe_names <- function(x) {",
      "  n <- names(x)",
      "  if (is.null(n) || length(n) == 0) return(character(0))",
      "  n[nzchar(n)]",
      "}",
      "",
      "ui <- dashboardPage(",
      "  dashboardHeader(title = 'DrugSigNet Interactive Report'),",
      "  dashboardSidebar(",
      "    selectInput('object_idx', 'Result section', choices = result_choices, selected = 1),",
      "    sidebarMenuOutput('sidebar_menu')",
      "  ),",
      "  dashboardBody(",
      "    tabItems(",
      "      tabItem(tabName = 'drugsearching',",
      "        fluidRow(",
      "          box(width = 12, title = 'DrugSearching - Raw', uiOutput('raw_tabs'))",
      "        ),",
      "        fluidRow(",
      "          box(width = 12, title = 'DrugSearching - Processed', uiOutput('processed_tabs'))",
      "        )",
      "      ),",
      "      tabItem(tabName = 'rankaggregation',",
      "        fluidRow(box(width = 12, title = 'RankAggregation', uiOutput('rank_tabs')))",
      "      ),",
      "      tabItem(tabName = 'drugannotation',",
      "        fluidRow(",
      "          box(width = 12, title = 'DrugAnnotation',",
      "            tabsetPanel(",
      "              tabPanel('top_k drugs', DTOutput('anno_topk')),",
      "              tabPanel('All drugs', DTOutput('anno_all')),",
      "              tabPanel('Functional Enrichment', DTOutput('anno_fe'))",
      "            )",
      "          )",
      "        )",
      "      ),",
      "      tabItem(tabName = 'visualization',",
      "        fluidRow(",
      "          box(width = 12, title = 'Visualization',",
      "            uiOutput('visualization_tabs')",
      "          )",
      "        )",
      "      )",
      "    )",
      "  )",
      ")",
      "",
      "server <- function(input, output, session) {",
      "  current_obj <- reactive({",
      "    report_objects[[as.integer(input$object_idx)]]",
      "  })",
      "",
      "  output$sidebar_menu <- renderMenu({",
      "    obj <- current_obj()",
      "    items <- list(",
      "      menuItem('DrugSearching', tabName = 'drugsearching', icon = icon('search')),",
      "      menuItem('RankAggregation', tabName = 'rankaggregation', icon = icon('list-ol'))",
      "    )",
      "    if (!is.null(obj$DrugAnnotation)) {",
      "      items <- c(items, list(menuItem('DrugAnnotation', tabName = 'drugannotation', icon = icon('tags'))))",
      "    }",
      "    if (length(visualization_items(obj$Visualization)) > 0) {",
      "      items <- c(items, list(menuItem('Visualization', tabName = 'visualization', icon = icon('chart-line'))))",
      "    }",
      "    do.call(sidebarMenu, items)",
      "  })",
      "",
      "  build_table_tabs <- function(items, prefix = 'tbl') {",
      "    nms <- safe_names(items)",
      "    if (length(nms) == 0) return(tags$p('No results available.'))",
      "    tabs <- lapply(nms, function(nm) {",
      "      tabPanel(nm, DTOutput(paste0(prefix, '_', gsub('[^A-Za-z0-9]', '_', nm))))",
      "    })",
      "    do.call(tabsetPanel, c(list(id = NULL, type = 'tabs'), tabs))",
      "  }",
      "",
      "  output$raw_tabs <- renderUI({",
      "    raw_items <- current_obj()$DrugSearching$Raw",
      "    build_table_tabs(raw_items, prefix = 'raw')",
      "  })",
      "",
      "  output$processed_tabs <- renderUI({",
      "    proc_items <- current_obj()$DrugSearching$Processed",
      "    build_table_tabs(proc_items, prefix = 'processed')",
      "  })",
      "",
      "  output$rank_tabs <- renderUI({",
      "    rank_items <- current_obj()$RankAggregation",
      "    nms <- safe_names(rank_items)",
      "    nms <- nms[!(grepl('_Harmonized$', nms) & nms != 'Harmonized')]",
      "    if (length(nms) == 0) return(tags$p('No results available.'))",
      "    tabs <- lapply(nms, function(nm) tabPanel(nm, DTOutput(paste0('rank_', gsub('[^A-Za-z0-9]', '_', nm))))",
      "    )",
      "    do.call(tabsetPanel, c(list(type = 'tabs'), tabs))",
      "  })",
      "",
      "  observe({",
      "    obj <- current_obj()",
      "    raw_items <- obj$DrugSearching$Raw",
      "    for (nm in safe_names(raw_items)) {",
      "      local({",
      "        id <- paste0('raw_', gsub('[^A-Za-z0-9]', '_', nm))",
      "        item <- raw_items[[nm]]",
      "        output[[id]] <- renderDT({",
      "          df <- extract_df(item)",
      "          if (is.null(df)) return(datatable(data.frame(Message = 'No table available')))",
      "          datatable(df, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))",
      "        })",
      "      })",
      "    }",
      "",
      "    proc_items <- obj$DrugSearching$Processed",
      "    for (nm in safe_names(proc_items)) {",
      "      local({",
      "        id <- paste0('processed_', gsub('[^A-Za-z0-9]', '_', nm))",
      "        item <- proc_items[[nm]]",
      "        output[[id]] <- renderDT({",
      "          df <- extract_df(item)",
      "          if (is.null(df)) return(datatable(data.frame(Message = 'No table available')))",
      "          datatable(df, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))",
      "        })",
      "      })",
      "    }",
      "",
      "    rank_items <- obj$RankAggregation",
      "    rank_names <- safe_names(rank_items)",
      "    rank_names <- rank_names[!(grepl('_Harmonized$', rank_names) & rank_names != 'Harmonized')]",
      "    for (nm in rank_names) {",
      "      local({",
      "        id <- paste0('rank_', gsub('[^A-Za-z0-9]', '_', nm))",
      "        item <- rank_items[[nm]]",
      "        output[[id]] <- renderDT({",
      "          obj_type <- obj$type",
      "          if (identical(nm, 'Harmonized')) {",
      "            df <- select_harmonized_rank_df(rank_items, obj_type)",
      "          } else {",
      "            df <- extract_df(item)",
      "            if (is.null(df) && is.list(item)) {",
      "              first_df <- NULL",
      "              for (cand in item) {",
      "                cand_df <- extract_df(cand)",
      "                if (!is.null(cand_df)) {",
      "                  first_df <- cand_df",
      "                  break",
      "                }",
      "              }",
      "              df <- first_df",
      "            }",
      "          }",
      "          if (is.null(df)) return(datatable(data.frame(Message = 'No table available')))",
      "          datatable(df, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))",
      "        })",
      "      })",
      "    }",
      "  })",
      "",
      "  output$anno_topk <- renderDT({",
      "    df <- extract_df(current_obj()$DrugAnnotation[['top_k_union_drugs']])",
      "    if (is.null(df)) return(datatable(data.frame(Message = 'No data available')))",
      "    datatable(df, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))",
      "  })",
      "",
      "  output$anno_all <- renderDT({",
      "    df <- extract_df(current_obj()$DrugAnnotation[['Features']])",
      "    if (is.null(df)) return(datatable(data.frame(Message = 'No data available')))",
      "    datatable(df, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))",
      "  })",
      "",
      "  output$anno_fe <- renderDT({",
      "    fe <- current_obj()$DrugAnnotation[['Functional_Enrichment']]",
      "    if (isS4(fe) && 'result' %in% methods::slotNames(fe)) fe <- fe@result",
      "    df <- extract_df(fe)",
      "    if (is.null(df)) return(datatable(data.frame(Message = 'No data available')))",
      "    datatable(df, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))",
      "  })",
      "",
      "  render_plotly_panel <- function(plot_obj) {",
      "    p <- extract_plot(plot_obj)",
      "    if (is.null(p)) return(plotly_empty())",
      "    if (inherits(p, 'ggplot')) return(ggplotly(p))",
      "    if (inherits(p, 'plotly')) return(p)",
      "    plotly_empty()",
      "  }",
      "",
      "  output$visualization_tabs <- renderUI({",
      "    items <- visualization_items(current_obj()$Visualization)",
      "    if (length(items) == 0) return(tags$p('No plots available.'))",
      "    item_names <- names(items)",
      "    if (is.null(item_names) || length(item_names) != length(items)) item_names <- rep('', length(items))",
      "    tabs <- lapply(seq_along(items), function(i) {",
      "      key <- item_names[[i]]",
      "      if (!nzchar(key)) key <- paste0('plot_', i)",
      "      id <- paste0('viz_', gsub('[^A-Za-z0-9]', '_', key))",
      "      p <- extract_plot(items[[i]])",
      "      panel <- if (inherits(p, 'recordedplot')) plotOutput(id, height = '800px') else plotlyOutput(id, height = '800px')",
      "      tabPanel(plot_title(key), panel)",
      "    })",
      "    do.call(tabsetPanel, c(list(type = 'tabs'), tabs))",
      "  })",
      "  observe({",
      "    items <- visualization_items(current_obj()$Visualization)",
      "    item_names <- names(items)",
      "    if (is.null(item_names) || length(item_names) != length(items)) item_names <- rep('', length(items))",
      "    for (i in seq_along(items)) local({",
      "      key <- item_names[[i]]",
      "      if (!nzchar(key)) key <- paste0('plot_', i)",
      "      id <- paste0('viz_', gsub('[^A-Za-z0-9]', '_', key))",
      "      plot_obj <- items[[i]]",
      "      p <- extract_plot(plot_obj)",
      "      if (inherits(p, 'recordedplot')) {",
      "        output[[id]] <- renderPlot(grDevices::replayPlot(p))",
      "      } else {",
      "        output[[id]] <- renderPlotly(render_plotly_panel(plot_obj))",
      "      }",
      "    })",
      "  })",
      "}",
      "",
      "shinyApp(ui, server)"
    )

    writeLines(app_lines, con = file.path(app_dir, "app.R"))
    return(invisible(normalizePath(app_dir, winslash = "/", mustWork = FALSE)))
  }

  .write_html_report(objects, report_title, base_name, author, top_n, enrichment_n, interactive,
    options = list(include_summary = include_summary, include_methods = include_methods,
      include_interpretation = include_interpretation, include_reproducibility = include_reproducibility,
      include_appendix = include_appendix, table_rows = table_rows, figure_dpi = figure_dpi))
}
