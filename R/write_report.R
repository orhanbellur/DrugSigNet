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


#' @title Write DrugSigNet Analysis Report
#'
#' @description
#' Generates an HTML or PDF report summarizing one or more DrugSigNet pipeline
#' results.
#'
#' @details
#' `write_report()` creates a report from pipeline results returned by
#' DrugSigNet workflows.
#'
#' HTML reports include interactive sections for:
#' \itemize{
#'   \item Drug searching results,
#'   \item Rank aggregation,
#'   \item Drug annotation,
#'   \item Visualization.
#' }
#'
#' Reports can optionally export pipeline results as Excel files using
#' `write_pipeline_results()`. PDF reports require a LaTeX engine such as
#' `pdflatex`, `xelatex`, or `lualatex`. If the `tinytex` R package is
#' installed but no LaTeX engine is available, `write_report()` attempts to
#' install TinyTeX automatically before rendering the PDF.
#'
#' The visualization section is generated from every entry in
#' `object@Visualization$plots`; it is not limited to a fixed set of plot names.
#' Optional DrugAnnotation and Visualization sections are omitted when those
#' components are absent from the pipeline result.
#'
#' HTML reports can be generated either as:
#' \itemize{
#'   \item a tabset-based R Markdown report, or
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
#'   device = "pdf"
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
                         dashboard = c("tabset", "shinydashboard")) {
  device <- match.arg(device)
  dashboard <- match.arg(dashboard)

  if (is.null(object)) {
    stop("`object` cannot be NULL.", call. = FALSE)
  }

  normalize_pipeline_result <- function(x) {
    if (.is_drug_searching_pipeline(x)) {
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
      return(result)
    }
    x
  }

  is_pipeline_result <- function(x) {
    x_norm <- normalize_pipeline_result(x)
    is.list(x_norm) && !is.null(x_norm$RankAggregation)
  }

  raw_objects <- if (is_pipeline_result(object)) {
    list(object)
  } else if (is.list(object) && length(object) > 0 && all(vapply(object, is_pipeline_result, logical(1)))) {
    object
  } else {
    stop("`object` must be a pipeline result list or a list of pipeline results.", call. = FALSE)
  }
  objects <- lapply(raw_objects, normalize_pipeline_result)

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

  rmd_file <- paste0(base_name, ".Rmd")
  output_file <- paste0(base_name, ".", ifelse(device == "html", "html", "pdf"))

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
      "    selectInput('object_idx', 'Result Object', choices = seq_along(report_objects), selected = 1),",
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

  yaml <- c(
    "---",
    paste0("title: \"", report_title, "\""),
    paste0("date: \"", Sys.Date(), "\""),
    if (!is.null(author)) paste0("author: \"", paste(author, collapse = ", "), "\"") else NULL,
    if (device == "html") {
      c(
        "output:",
        "  html_document:",
        "    theme: flatly",
        "    toc: true",
        "    toc_float: true",
        "    code_folding: hide"
      )
    } else {
      "output: pdf_document"
    },
    "---",
    ""
  )

  setup_chunk <- c(
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(",
    "  echo = FALSE, warning = FALSE, message = FALSE, results = 'asis', comment = NA,",
    "  fig.width = 11, fig.height = 8, dpi = 150, fig.align = 'center',",
    "  out.width = if (knitr::is_latex_output()) '\\\\linewidth' else '100%'",
    ")",
    "extract_df <- function(x) {",
    "  if (is.null(x)) return(NULL)",
    "  if (isS4(x) && 'result' %in% methods::slotNames(x)) x <- x@result",
    "  if (is.list(x) && !is.data.frame(x) && !is.null(x$result) && is.data.frame(x$result)) x <- x$result",
    "  if (is.list(x) && !is.data.frame(x)) {",
    "    for (cand in x) {",
    "      cand_df <- extract_df(cand)",
    "      if (!is.null(cand_df)) return(cand_df)",
    "    }",
    "    return(NULL)",
    "  }",
    "  if (!is.data.frame(x)) return(NULL)",
    "  x",
    "}",
    "render_table <- function(x) {",
    "  if (is.null(x)) {",
    "    cat('*No data available.*\\n')",
    "    return(invisible(NULL))",
    "  }",
    "  x <- extract_df(x)",
    "  if (!is.data.frame(x)) {",
    "    cat('*No tabular result available for this section.*\\n')",
    "    return(invisible(NULL))",
    "  }",
    "  if (knitr::is_html_output() && requireNamespace('DT', quietly = TRUE)) {",
    "    return(DT::datatable(",
    "      x,",
    "      rownames = FALSE,",
    "      filter = 'top',",
    "      class = 'cell-border stripe hover compact',",
    "      options = list(",
    "        scrollX = TRUE,",
    "        pageLength = 15,",
    "        lengthMenu = c(10, 15, 25, 50, 100),",
    "        autoWidth = TRUE",
    "      )",
    "    ))",
    "  } else {",
    "    table_format <- if (knitr::is_latex_output()) 'latex' else 'pipe'",
    "    table_rows <- if (knitr::is_latex_output()) 6 else 200",
    "    table_head <- utils::head(x, table_rows)",
    "    tbl <- knitr::kable(table_head, format = table_format, escape = TRUE)",
    "    if (knitr::is_latex_output() && requireNamespace('kableExtra', quietly = TRUE)) {",
    "      tbl <- kableExtra::kable_styling(tbl, latex_options = c('scale_down', 'hold_position'), font_size = 7, full_width = FALSE)",
    "    }",
    "    return(tbl)",
    "  }",
    "  invisible(NULL)",
    "}",
    "emit_knit <- function(x) {",
    "  if (is.null(x)) return(invisible(NULL))",
    "  if (inherits(x, 'knitr_kable')) {",
    "    cat(paste(x, collapse = '\\n'), '\\n')",
    "  } else {",
    "    print(x)",
    "  }",
    "  invisible(NULL)",
    "}",
    "render_plot_panel <- function(p, use_plotly = FALSE) {",
    "  if (is.null(p)) {",
    "    cat('*No plot available.*\\n')",
    "    return(invisible(NULL))",
    "  }",
    "  if (is.list(p) && !is.null(p$plot)) p <- p$plot",
    "  if (is.list(p) && !is.null(p$result) && !is.data.frame(p$result)) p <- p$result",
    "  if (is.list(p) && !is.null(p$plot)) p <- p$plot",
    "  if (use_plotly && inherits(p, 'ggplot') && requireNamespace('plotly', quietly = TRUE)) return(plotly::ggplotly(p))",
    "  if (inherits(p, 'plotly')) {",
    "    if (knitr::is_html_output()) return(p)",
    "    if (requireNamespace('plotly', quietly = TRUE)) {",
    "      img <- tempfile(fileext = '.png')",
    "      saved <- tryCatch({",
    "        plotly::save_image(p, file = img, width = 1400, height = 1000, scale = 2)",
    "        file.exists(img) && file.info(img)$size > 0",
    "      }, error = function(e) {",
    "        message('Could not render Plotly object as a static image for PDF: ', conditionMessage(e))",
    "        FALSE",
    "      })",
    "      if (isTRUE(saved)) return(knitr::include_graphics(img))",
    "    }",
    "    cat('*Plotly visualization omitted from PDF because static image export is unavailable.*\n')",
    "    return(invisible(NULL))",
    "  }",
    "  p",
    "}",
    "collect_tables <- function(x, prefix = NULL) {",
    "  out <- list()",
    "  if (is.list(x) && !is.data.frame(x) && (is.null(x$result) || !is.data.frame(x$result))) {",
    "    nms <- names(x)",
    "    if (is.null(nms) || length(nms) != length(x)) nms <- rep('', length(x))",
    "    nms[!nzchar(nms)] <- paste0('Item_', which(!nzchar(nms)))",
    "    for (i in seq_along(x)) {",
    "      child_prefix <- if (is.null(prefix) || !nzchar(prefix)) nms[[i]] else paste(prefix, nms[[i]], sep = ' / ')",
    "      out <- c(out, collect_tables(x[[i]], child_prefix))",
    "    }",
    "    return(out)",
    "  }",
    "  df <- extract_df(x)",
    "  if (!is.null(df)) {",
    "    nm <- if (is.null(prefix) || !nzchar(prefix)) 'Table' else prefix",
    "    out[[nm]] <- df",
    "  }",
    "  out",
    "}",
    "render_table_collection <- function(x, prefix = NULL) {",
    "  tabs <- collect_tables(x, prefix)",
    "  if (length(tabs) == 0) {",
    "    if (knitr::is_html_output()) return(htmltools::tags$p('No tabular result available for this section.'))",
    "    cat('*No tabular result available for this section.*\\n')",
    "    return(invisible(NULL))",
    "  }",
    "  if (knitr::is_html_output()) {",
    "    tabs_ui <- lapply(names(tabs), function(nm) {",
    "      tbl <- render_table(tabs[[nm]])",
    "      if (is.null(tbl)) tbl <- htmltools::tags$p('No table available')",
    "      shiny::tabPanel(title = nm, tbl)",
    "    })",
    "    return(do.call(shiny::tabsetPanel, c(list(type = 'tabs'), tabs_ui)))",
    "  }",
    "  for (nm in names(tabs)) {",
    "    cat(paste0('##### ', nm, '\\n\\n'))",
    "    tbl <- render_table(tabs[[nm]])",
    "    if (!is.null(tbl)) emit_knit(tbl)",
    "    cat('\\n\\n')",
    "  }",
    "  invisible(NULL)",
    "}",
    "render_rank_collection <- function(rank_agg, obj_type = NULL) {",
    "  if (is.null(rank_agg) || length(rank_agg) == 0) {",
    "    cat('*No results available.*\\n')",
    "    return(invisible(NULL))",
    "  }",
    "  rank_names <- names(rank_agg)",
    "  if (is.null(rank_names) || length(rank_names) != length(rank_agg)) rank_names <- rep('', length(rank_agg))",
    "  rank_names[!nzchar(rank_names)] <- paste0('Item_', which(!nzchar(rank_names)))",
    "  rank_names <- rank_names[!(grepl('_Harmonized$', rank_names) & rank_names != 'Harmonized')]",
    "  if ('Harmonized' %in% rank_names) rank_names <- 'Harmonized'",
    "  if (knitr::is_html_output()) {",
    "    tabs_ui <- list()",
    "    for (nm in rank_names) {",
    "      if (identical(nm, 'Harmonized')) {",
    "        target <- if (identical(obj_type, 'signature')) {",
    "          'Signature_Harmonized'",
    "        } else if (identical(obj_type, 'network')) {",
    "          'Network_Harmonized'",
    "        } else {",
    "          'Signature_Network_Harmonized'",
    "        }",
    "        panel_content <- render_table_collection(rank_agg[[target]], prefix = target)",
    "      } else {",
    "        panel_content <- render_table_collection(rank_agg[[nm]], prefix = nm)",
    "      }",
    "      tabs_ui <- c(tabs_ui, list(shiny::tabPanel(title = nm, panel_content)))",
    "    }",
    "    return(do.call(shiny::tabsetPanel, c(list(type = 'tabs'), tabs_ui)))",
    "  }",
    "  for (nm in rank_names) {",
    "    cat(paste0('#### ', nm, '\\n\\n'))",
    "    if (identical(nm, 'Harmonized')) {",
    "      target <- if (identical(obj_type, 'signature')) {",
    "        'Signature_Harmonized'",
    "      } else if (identical(obj_type, 'network')) {",
    "        'Network_Harmonized'",
    "      } else {",
    "        'Signature_Network_Harmonized'",
    "      }",
    "      render_table_collection(rank_agg[[target]], prefix = target)",
    "    } else {",
    "      render_table_collection(rank_agg[[nm]], prefix = nm)",
    "    }",
    "    cat('\\n\\n')",
    "  }",
    "  invisible(NULL)",
    "}",
    "named_items <- function(x) {",
    "  if (is.null(x) || length(x) == 0) return(list())",
    "  nms <- names(x)",
    "  if (is.null(nms) || length(nms) != length(x)) nms <- rep('', length(x))",
    "  nms[!nzchar(nms)] <- paste0('Item_', which(!nzchar(nms)))",
    "  stats::setNames(as.list(x), nms)",
    "}",
    "render_named_list_tabs <- function(x, heading_level = 5) {",
    "  if (is.null(x) || length(x) == 0) {",
    "    cat('*No results available.*\\n')",
    "    return(invisible(NULL))",
    "  }",
    "  if (!is.list(x) || is.data.frame(x)) {",
    "    tbl <- render_table(x)",
    "    if (!is.null(tbl)) emit_knit(tbl)",
    "    return(invisible(NULL))",
    "  }",
    "  items <- named_items(x)",
    "  for (nm in names(items)) {",
    "    cat(paste0(strrep('#', heading_level), ' ', nm, '\\n\\n'))",
    "    item <- items[[nm]]",
    "    if (is.list(item) && !is.data.frame(item) && is.null(item$result)) {",
    "      render_named_list_tabs(item, heading_level = min(heading_level + 1, 6))",
    "    } else {",
    "      tbl <- render_table(item)",
    "      if (!is.null(tbl)) emit_knit(tbl)",
    "    }",
    "    cat('\\n\\n')",
    "  }",
    "}",
    "pick_harmonized_table <- function(rank_agg, obj_type) {",
    "  if (!is.list(rank_agg)) return(NULL)",
    "  target <- if (identical(obj_type, 'signature')) {",
    "    'Signature_Harmonized'",
    "  } else if (identical(obj_type, 'network')) {",
    "    'Network_Harmonized'",
    "  } else if (identical(obj_type, 'both')) {",
    "    'Signature_Network_Harmonized'",
    "  } else {",
    "    'Signature_Network_Harmonized'",
    "  }",
    "  rank_agg[[target]]",
    "}",
    "render_rank_aggregation_tabs <- function(rank_agg, obj_type = NULL) {",
    "  if (is.null(rank_agg) || length(rank_agg) == 0) {",
    "    cat('*No results available.*\\n')",
    "    return(invisible(NULL))",
    "  }",
    "  rank_names <- names(rank_agg)",
    "  if (is.null(rank_names) || length(rank_names) != length(rank_agg)) {",
    "    rank_names <- paste0('Item_', seq_along(rank_agg))",
    "  }",
    "  rank_names[!nzchar(rank_names)] <- paste0('Item_', which(!nzchar(rank_names)))",
    "  rank_names <- rank_names[!(grepl('_Harmonized$', rank_names) & rank_names != 'Harmonized')]",
    "  for (nm in rank_names) {",
    "    cat(paste0('#### ', nm, '\\n\\n'))",
    "    item <- rank_agg[[which(rank_names == nm)[1]]]",
    "    if (identical(nm, 'Harmonized')) item <- pick_harmonized_table(rank_agg, obj_type)",
    "    tbl <- render_table(item)",
    "    if (!is.null(tbl)) emit_knit(tbl)",
    "    cat('\\n\\n')",
    "  }",
    "}",
    "get_viz_plot <- function(viz, key) {",
    "  if (is.null(viz)) return(NULL)",
    "  if (is.list(viz[['plots']]) && !is.null(viz[['plots']][[key]])) return(viz[['plots']][[key]])",
    "  alias_keys <- switch(",
    "    key,",
    "    drug_hierarchy = c('drug_hierarchy_plot', 'drugHierarchy', 'drug_hierarchy'),",
    "    rank_distribution_scatter = c('rank_distribution_scatter', 'rank_distribution'),",
    "    rank_agreement = c('rank_agreement', 'rank_correlation'),",
    "    top_k_hits = c('top_k_hits'),",
    "    top_k_overlap = c('top_k_overlap'),",
    "    enriched_terms = c('enriched_terms', 'functional_enrichment'),",
    "    c(key)",
    "  )",
    "  if (is.list(viz[['plots']])) {",
    "    for (k in alias_keys) if (!is.null(viz[['plots']][[k]])) return(viz[['plots']][[k]])",
    "  }",
    "  if (!is.null(viz[[key]])) return(viz[[key]])",
    "  for (k in alias_keys) if (!is.null(viz[[k]])) return(viz[[k]])",
    "  NULL",
    "}",
    "```",
    ""
  )

  style_block <- if (device == "html") c(
    "<style>",
    ".tabset > .nav-tabs {",
    "  border-bottom: 2px solid #dce3ec;",
    "  margin-bottom: 14px;",
    "}",
    ".tabset > .nav-tabs > li > a {",
    "  border-radius: 10px 10px 0 0;",
    "  margin-right: 6px;",
    "  background: #f6f9fc;",
    "  border: 1px solid #dce3ec;",
    "  border-bottom: none;",
    "  color: #2c3e50;",
    "  font-weight: 600;",
    "}",
    ".tabset > .nav-tabs > li.active > a,",
    ".tabset > .nav-tabs > li > a:hover {",
    "  background: #2c7fb8;",
    "  color: #ffffff !important;",
    "  border-color: #2c7fb8;",
    "}",
    ".tab-content {",
    "  border: 1px solid #dce3ec;",
    "  border-radius: 0 8px 8px 8px;",
    "  padding: 14px;",
    "  background: #ffffff;",
    "}",
    ".section.level2, .section.level3 {",
    "  margin-top: 18px;",
    "}",
    "table.dataTable thead th {",
    "  background-color: #eef5fb;",
    "}",
    "</style>",
    ""
  ) else character(0)

  .chunk_id_safe <- function(x) gsub("[^A-Za-z0-9]+", "-", x)
  .chunk_id_counter <- 0L
  .next_chunk_id <- function(prefix = "chunk") {
    .chunk_id_counter <<- .chunk_id_counter + 1L
    paste0(.chunk_id_safe(prefix), "-", .chunk_id_counter)
  }

  .build_visualization_blocks <- function(viz, object_index) {
    items <- .report_visualization_items(viz)
    if (!length(items)) {
      return(c("*No plots available.*", ""))
    }
    item_names <- names(items)
    if (is.null(item_names) || length(item_names) != length(items)) {
      item_names <- rep("", length(items))
    }
    item_names[!nzchar(item_names)] <- paste0("plot_", which(!nzchar(item_names)))

    lines <- character()
    for (plot_index in seq_along(items)) {
      key <- item_names[[plot_index]]
      lines <- c(
        lines,
        paste0("#### ", .report_visualization_title(key)),
        paste0("```{r result-", object_index, "-viz-", .chunk_id_safe(key), "}"),
        paste0(
          "render_plot_panel(obj$Visualization[['plots']][[", plot_index,
          "]], use_plotly = ", interactive_flag, ")"
        ),
        "```",
        ""
      )
    }
    lines
  }

  .build_table_blocks <- function(x, expr, heading_level = 5, chunk_prefix = "tbl") {
    lines <- character(0)
    if (is.null(x)) return(lines)
    if (!is.list(x) || is.data.frame(x)) {
      chunk_id <- .next_chunk_id(paste0(chunk_prefix, "-single"))
      return(c(
        paste0("```{r ", chunk_id, "}"),
        paste0("tbl <- render_table(", expr, ")"),
        "if (!is.null(tbl)) emit_knit(tbl)",
        "```",
        ""
      ))
    }

    item_names <- names(x)
    if (is.null(item_names) || length(item_names) != length(x)) item_names <- rep("", length(x))
    item_names[!nzchar(item_names)] <- paste0("Item_", which(!nzchar(item_names)))

    for (idx in seq_along(x)) {
      nm <- item_names[[idx]]
      item <- x[[idx]]
      item_expr <- paste0(expr, "[[", idx, "]]")
      lines <- c(lines, paste0(strrep("#", heading_level), " ", nm), "")

      if (is.list(item) && !is.data.frame(item) && is.null(item$result)) {
        lines <- c(
          lines,
          .build_table_blocks(
            x = item,
            expr = item_expr,
            heading_level = min(heading_level + 1, 6),
            chunk_prefix = paste0(chunk_prefix, "-", .chunk_id_safe(nm))
          )
        )
      } else {
        chunk_id <- .next_chunk_id(paste0(chunk_prefix, "-", idx, "-", nm))
        lines <- c(
          lines,
          paste0("```{r ", chunk_id, "}"),
          paste0("tbl <- render_table(", item_expr, ")"),
          "if (!is.null(tbl)) emit_knit(tbl)",
          "```",
          ""
        )
      }
    }

    lines
  }

  .rank_target_name <- function(obj_type) {
    if (identical(obj_type, "signature")) {
      "Signature_Harmonized"
    } else if (identical(obj_type, "network")) {
      "Network_Harmonized"
    } else {
      "Signature_Network_Harmonized"
    }
  }

  .build_rank_blocks <- function(rank_agg, obj_type, expr = "obj$RankAggregation", chunk_prefix = "rank") {
    lines <- character(0)
    if (is.null(rank_agg) || length(rank_agg) == 0) {
      return(c("*No results available.*", ""))
    }

    nms <- names(rank_agg)
    if (is.null(nms) || length(nms) != length(rank_agg)) nms <- rep("", length(rank_agg))
    nms[!nzchar(nms)] <- paste0("Item_", which(!nzchar(nms)))
    keep <- !(grepl("_Harmonized$", nms) & nms != "Harmonized")
    nms <- nms[keep]
    idxs <- which(keep)

    for (j in seq_along(idxs)) {
      idx <- idxs[[j]]
      nm <- nms[[j]]
      lines <- c(lines, paste0("#### ", nm), "")

      if (identical(nm, "Harmonized")) {
        target <- .rank_target_name(obj_type)
        lines <- c(
          lines,
          paste0("```{r ", .next_chunk_id(paste0(chunk_prefix, "-", nm)), "}"),
          paste0("tbl <- render_table(", expr, "[['", target, "']])"),
          "if (!is.null(tbl)) emit_knit(tbl)",
          "```",
          ""
        )
      } else {
        lines <- c(
          lines,
          .build_table_blocks(
            x = rank_agg[[idx]],
            expr = paste0(expr, "[[", idx, "]]"),
            heading_level = 5,
            chunk_prefix = paste0(chunk_prefix, "-", .chunk_id_safe(nm))
          )
        )
      }
    }
    lines
  }

  body <- c()
  interactive_flag <- if (device == "html") "TRUE" else "FALSE"
  for (i in seq_along(objects)) {
    annotation_body <- if (.report_has_annotation(objects[[i]])) {
      c(
        "### DrugAnnotation {.tabset}",
        "",
        "#### top_k drugs",
        paste0("```{r result-", i, "-anno-topk}"),
        "render_table(obj$DrugAnnotation[['top_k_union_drugs']])",
        "```",
        "",
        "#### All drugs",
        paste0("```{r result-", i, "-anno-all}"),
        "render_table(obj$DrugAnnotation[['Features']])",
        "```",
        "",
        "#### Functional Enrichment",
        paste0("```{r result-", i, "-anno-fe}"),
        "fe_obj <- obj$DrugAnnotation[['Functional_Enrichment']]",
        "if (isS4(fe_obj) && 'result' %in% methods::slotNames(fe_obj)) fe_obj <- fe_obj@result",
        "render_table(fe_obj)",
        "```",
        ""
      )
    } else {
      character()
    }

    visualization_body <- if (.report_has_visualization(objects[[i]])) {
      c(
        "### Visualization {.tabset}",
        "",
        .build_visualization_blocks(objects[[i]]$Visualization, i)
      )
    } else {
      character()
    }

    body <- c(
      body,
      paste0("## Result object ", i),
      "",
      paste0("```{r result-", i, "-obj, include=FALSE}"),
      paste0("obj <- report_objects[[", i, "]]"),
      "```",
      "",
      "### DrugSearching {.tabset}",
      "",
      "#### Raw {.tabset}",
      paste0("```{r result-", i, "-drugsearch-raw}"),
      "render_table_collection(obj$DrugSearching$Raw, prefix = 'Raw')",
      "```",
      "",
      "#### Processed {.tabset}",
      paste0("```{r result-", i, "-drugsearch-processed}"),
      "render_table_collection(obj$DrugSearching$Processed, prefix = 'Processed')",
      "```",
      "",
      "### RankAggregation {.tabset}",
      paste0("```{r result-", i, "-rankagg}"),
      "render_rank_collection(obj$RankAggregation, obj$type)",
      "```",
      "",
      annotation_body,
      visualization_body
    )
  }

  footer <- c(
    "## Session Info",
    "```{r}",
    "print(sessionInfo())",
    "```",
    ""
  )

  writeLines(c(yaml, setup_chunk, style_block, body, footer), con = rmd_file)

  if (device == "pdf") {
    latex_engines <- Sys.which(c("pdflatex", "xelatex", "lualatex"))
    if (!any(nzchar(latex_engines))) {
      if (requireNamespace("tinytex", quietly = TRUE)) {
        message("No LaTeX engine found; installing TinyTeX before rendering the PDF report.")
        install_error <- tryCatch(
          {
            tinytex::install_tinytex()
            NULL
          },
          error = function(e) conditionMessage(e)
        )
        latex_engines <- Sys.which(c("pdflatex", "xelatex", "lualatex"))
        if (!is.null(install_error) || !any(nzchar(latex_engines))) {
          stop(
            "PDF reports require a LaTeX engine, and automatic TinyTeX installation did not make one available. ",
            if (!is.null(install_error)) paste0("TinyTeX install error: ", install_error, " ") else "",
            "Run `tinytex::install_tinytex()` manually or install a system TeX distribution, then rerun `write_report(..., device = 'pdf')`.",
            call. = FALSE
          )
        }
      } else {
        stop(
          "PDF reports require a LaTeX engine, but none of pdflatex, xelatex, or lualatex was found. ",
          "Install TinyTeX with `install.packages('tinytex'); tinytex::install_tinytex()` ",
          "or install a system TeX distribution, then rerun `write_report(..., device = 'pdf')`.",
          call. = FALSE
        )
      }
    }
  }

  render_format <- if (device == "html") "html_document" else "pdf_document"
  rmarkdown::render(
    input = rmd_file,
    output_format = render_format,
    output_file = basename(output_file),
    envir = list2env(list(report_objects = objects), parent = globalenv()),
    quiet = TRUE
  )

  invisible(normalizePath(output_file, winslash = "/", mustWork = FALSE))
}
