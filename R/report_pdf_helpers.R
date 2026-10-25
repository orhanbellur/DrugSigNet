.report_extract_data_frame <- function(x) {
  if (is.null(x)) return(NULL)
  if (isS4(x) && "result" %in% methods::slotNames(x)) x <- methods::slot(x, "result")
  if (is.data.frame(x)) return(x)
  if (!is.list(x)) return(NULL)
  if (is.data.frame(x$result)) return(x$result)
  for (item in x) {
    value <- .report_extract_data_frame(item)
    if (!is.null(value)) return(value)
  }
  NULL
}

.report_harmonized_table <- function(x) {
  ranks <- x$RankAggregation
  if (!is.list(ranks)) return(NULL)
  preferred <- switch(x$type,
    signature = "Signature_Harmonized",
    network = "Network_Harmonized",
    integration = "Signature_Network_Harmonized",
    NULL
  )
  candidates <- unique(c(preferred, "Harmonized", names(ranks)))
  for (name in candidates[!is.na(candidates)]) {
    value <- .report_extract_data_frame(ranks[[name]])
    if (!is.null(value) && nrow(value)) return(value)
  }
  .report_extract_data_frame(ranks)
}

.report_ranking_columns <- function(x) {
  if (!is.data.frame(x)) return(character())
  names(x)[grepl("CRank|Dowdall|(^|_)RRA($|_rank$)", names(x), ignore.case = TRUE)]
}

.report_drug_column <- function(x) {
  candidates <- c("Drug", "drug", "pert", "perturbation", "Name", "name", "ID")
  hit <- intersect(candidates, names(x))
  if (length(hit)) hit[[1]] else NULL
}

.report_top_candidates <- function(x, top_n = 10L) {
  table <- .report_harmonized_table(x)
  if (is.null(table)) return(NULL)
  drug <- .report_drug_column(table)
  ranks <- .report_ranking_columns(table)
  if (is.null(drug) || !length(ranks)) return(NULL)
  out <- table[, unique(c(drug, ranks)), drop = FALSE]
  primary <- ranks[[1]]
  values <- suppressWarnings(as.numeric(out[[primary]]))
  out <- out[order(values, na.last = TRUE), , drop = FALSE]
  utils::head(out, top_n)
}

.report_candidate_names <- function(x) {
  table <- .report_harmonized_table(x)
  if (is.null(table)) return(character())
  drug <- .report_drug_column(table)
  if (is.null(drug)) return(character())
  unique(stats::na.omit(as.character(table[[drug]])))
}

.report_recorded_metadata <- function(x, path = "result") {
  out <- list()
  if (isS4(x)) {
    slots <- methods::slotNames(x)
    if ("parameters" %in% slots) out[[paste0(path, ".parameters")]] <- methods::slot(x, "parameters")
    return(out)
  }
  if (!is.list(x) || is.data.frame(x)) return(out)
  if (!is.null(x$parameters)) out[[paste0(path, ".parameters")]] <- x$parameters
  for (name in setdiff(names(x), c("parameters", "Visualization"))) {
    out <- c(out, .report_recorded_metadata(x[[name]], paste(path, name, sep = ".")))
  }
  out
}

.report_pdf_model <- function(objects, top_n = 10L) {
  types <- vapply(objects, function(x) if (is.null(x$type)) "unspecified" else x$type, character(1))
  candidates <- lapply(objects, .report_candidate_names)
  names(candidates) <- types
  signature <- unique(unlist(candidates[types == "signature"], use.names = FALSE))
  network <- unique(unlist(candidates[types == "network"], use.names = FALSE))
  list(
    objects = objects,
    types = types,
    counts = vapply(candidates, length, integer(1)),
    overlap = if (length(signature) && length(network)) length(intersect(signature, network)) else NA_integer_,
    top = lapply(objects, .report_top_candidates, top_n = top_n),
    metadata = lapply(objects, .report_recorded_metadata)
  )
}

.report_pdf_template <- function(title, author = NULL) {
  esc <- function(x) gsub('"', "'", x, fixed = TRUE)
  c(
    "---", paste0('title: "', esc(title), '"'),
    if (!is.null(author)) paste0('author: "', esc(paste(author, collapse = ", ")), '"') else NULL,
    paste0('date: "', Sys.Date(), '"'),
    "output:", "  pdf_document:", "    toc: true", "    toc_depth: 2",
    "    number_sections: true", "    fig_caption: true", "fontsize: 10pt",
    "geometry: margin=0.85in", "header-includes:", "  - \\usepackage{booktabs}",
    "  - \\usepackage{longtable}", "  - \\usepackage{pdflscape}",
    "  - \\usepackage{fancyhdr}", "  - \\pagestyle{fancy}",
    "  - \\fancyhead[L]{DrugSigNet Scientific Report}", "  - \\fancyhead[R]{\\thepage}",
    "---", ""
  )
}

.report_render_scientific_pdf <- function(objects, title, author, rmd_file,
                                           output_file, top_n, include_summary,
                                           include_methods, include_interpretation,
                                           include_reproducibility, include_appendix,
                                           table_orientation, figure_dpi,
                                           table_rows) {
  model <- .report_pdf_model(objects, top_n)
  body <- c(
    "```{r setup, include=FALSE}",
    paste0("knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE, fig.width=7.2, fig.height=5.2, dpi=", figure_dpi, ", fig.align='center', out.width='\\\\linewidth')"),
    paste0("table_rows <- ", table_rows), paste0("table_orientation <- '", table_orientation, "'"),
    "render_table <- function(x, caption=NULL, limit=table_rows) {",
    " if (is.null(x) || !is.data.frame(x) || !nrow(x)) { cat('*No tabular results were recorded.*\\n'); return(invisible()) }",
    " shown <- utils::head(x, limit)",
    " shown[] <- lapply(shown, function(column) { values <- if (is.list(column)) vapply(column, function(value) paste(as.character(value), collapse='; '), character(1)) else as.character(column); values[is.na(values)] <- ''; ifelse(nchar(values, type='width') > 80L, paste0(substr(values, 1L, 77L), '...'), values) })",
    " id_col <- intersect(c('Drug','drug','pert','perturbation','Name','name','ID'), names(shown)); id_col <- if (length(id_col)) id_col[[1L]] else NULL",
    " groups <- if (ncol(shown) <= 7L) list(names(shown)) else split(setdiff(names(shown), id_col), ceiling(seq_along(setdiff(names(shown), id_col))/6L))",
    " for (g in seq_along(groups)) { cols <- unique(c(id_col, groups[[g]])); part <- shown[, cols, drop=FALSE]; wide <- ncol(part) > 5L; landscape <- table_orientation == 'landscape' || (table_orientation == 'auto' && wide); if (landscape) cat('\\\\begin{landscape}\\n'); part_caption <- if (length(groups) > 1L) paste0(caption %||% 'Table', ' (columns ', g, ' of ', length(groups), ')') else caption; z <- knitr::kable(part, format='latex', booktabs=TRUE, longtable=TRUE, caption=part_caption, escape=TRUE, row.names=FALSE); if (requireNamespace('kableExtra', quietly=TRUE)) z <- kableExtra::kable_styling(z, latex_options='repeat_header', font_size=if(wide) 7 else 8, full_width=FALSE); print(z); if (landscape) cat('\\n\\\\end{landscape}\\n') }",
    " if (nrow(x) > limit) cat(sprintf('\\n*Showing %d of %d rows; the complete table is retained in the pipeline object.*\\n', limit, nrow(x)))",
    "}",
    "render_figure <- function(p, caption) {",
    " if (is.list(p) && !is.null(p$plot)) p <- p$plot",
    " if (inherits(p, 'plotly')) { img <- tempfile(fileext='.png'); ok <- FALSE; if (requireNamespace('plotly', quietly=TRUE)) ok <- tryCatch({ plotly::save_image(p, img, width=1800, height=1200, scale=1); file.exists(img) && file.info(img)$size > 0 }, error=function(e) FALSE); if (ok) { print(knitr::include_graphics(img)); return(invisible()) }; cat(paste0('*', caption, ': interactive Plotly content could not be exported statically; inspect the HTML report or pipeline object.*\\n')); return(invisible()) }",
    " if (inherits(p, c('ggplot','grob','recordedplot'))) { print(p); return(invisible()) }",
    " cat(paste0('*', caption, ': no renderable static figure was recorded.*\\n'))",
    "}", "```", ""
  )
  if (include_summary) body <- c(body, "# Executive Summary", "", "```{r summary, results='asis'}", "cat('This report summarizes the analysis outputs actually present in the supplied DrugSigNet object(s). Computational prioritization ranks candidates; it does not establish clinical efficacy.\\n\\n')", "for (i in seq_along(report_model$objects)) cat(sprintf('- **%s analysis:** %d unique ranked candidate(s).\\n', tools::toTitleCase(report_model$types[[i]]), report_model$counts[[i]]))", "if (!is.na(report_model$overlap)) cat(sprintf('- **Signature/network overlap:** %d candidate(s).\\n', report_model$overlap))", "```", "")
  body <- c(body, "# Top-Ranked Drug Candidates", "", "Lower numeric rank values indicate stronger prioritization. CRank, Dowdall, and RRA are distinct aggregation methods; agreement is supporting computational evidence, not proof of biological activity or clinical benefit.", "", "```{r top-tables, results='asis'}", "for (i in seq_along(report_model$top)) { cat(sprintf('\\n## %s\\n\\n', tools::toTitleCase(report_model$types[[i]]))); render_table(report_model$top[[i]], sprintf('Top-ranked candidates (%s analysis)', report_model$types[[i]]), limit=top_n) }", "```", "")
  labels <- c(signature="Signature-Based Analysis", network="Network-Based Analysis", integration="Integrated Drug Prioritization")
  body <- c(body, "```{r analyses, results='asis'}", "for (i in seq_along(report_model$objects)) { obj <- report_model$objects[[i]]; cat(sprintf('\\n# %s\\n\\n', labels[[obj$type]] %||% 'Analysis Results')); render_table(DrugSigNet:::.report_harmonized_table(obj), 'Harmonized ranking results'); if (!is.null(obj$DrugAnnotation)) { cat('\\n## Drug Annotation\\n\\n'); render_table(DrugSigNet:::.report_extract_data_frame(obj$DrugAnnotation$Features), 'Available drug annotations'); fe <- DrugSigNet:::.report_extract_data_frame(obj$DrugAnnotation$Functional_Enrichment); if (!is.null(fe)) { cat('\\n## Functional Enrichment\\n\\nEnrichment describes over-representation among prioritized candidates and does not demonstrate causality or efficacy.\\n\\n'); render_table(fe, 'Functional enrichment results') } }; plots <- DrugSigNet:::.report_visualization_items(obj$Visualization); if (length(plots)) { cat('\\n## Visualization\\n\\n'); for (j in seq_along(plots)) { cap <- DrugSigNet:::.report_visualization_title(names(plots)[j]); cat(sprintf('\\n### %s\\n\\n', cap)); render_figure(plots[[j]], cap) } } }", "```", "")
  if (include_interpretation) body <- c(body, "# Interpretation", "", "Rank aggregation measures agreement across computational methods. Functional enrichment summarizes associations among selected candidates. Neither result alone is evidence of clinical efficacy; experimental and clinical validation remain necessary.", "")
  if (include_methods) body <- c(body, "# Methods", "", "The report uses recorded DrugSigNet outputs without recalculating priorities. CRank, Dowdall, and RRA columns are displayed when present; unavailable methods and sections are not inferred.", "", "## Recorded Analysis Configuration", "", "```{r recorded-configuration}", "if (!length(unlist(report_model$metadata, recursive=FALSE))) cat('No analysis parameters or reference metadata were recorded in the supplied objects.\\n') else print(report_model$metadata)", "```", "")
  if (include_reproducibility) body <- c(body, "# Reproducibility", "", "```{r reproducibility}", "cat('DrugSigNet version:', as.character(utils::packageVersion('DrugSigNet')), '\\n\\n')", "cat('R version:', R.version.string, '\\n\\n')", "print(sessionInfo())", "```", "")
  if (include_appendix) body <- c(body, "# Appendix", "", paste0("Main result tables are limited to ", table_rows, " rows for readability. Complete, unmodified results remain available in the supplied pipeline object and optional Excel export."), "")
  writeLines(c(.report_pdf_template(title, author), body), rmd_file)
  rmarkdown::render(rmd_file, output_format="pdf_document", output_file=basename(output_file), envir=list2env(list(report_model=model, top_n=top_n, labels=labels, `%||%`=function(x,y) if (is.null(x)) y else x), parent=globalenv()), quiet=TRUE)
  invisible(normalizePath(output_file, winslash="/", mustWork=FALSE))
}
