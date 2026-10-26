# HTML presentation shares verified result extraction with the PDF report.
.report_html_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- "Not available"
  for (pair in list(c("&", "&amp;"), c("<", "&lt;"), c(">", "&gt;"), c('"', "&quot;"), c("'", "&#39;"), c("`", "&#96;"))) {
    x <- gsub(pair[1L], pair[2L], x, fixed = TRUE)
  }
  x
}

.report_html_cell <- function(value, depth = 0L) {
  if (is.null(value) || !length(value)) return("")
  if (is.data.frame(value) || is.matrix(value)) return(.report_value(value))
  if (is.list(value)) {
    if (depth >= 5L) return(paste("list:", length(value), "elements"))
    return(paste(vapply(value, .report_html_cell, character(1), depth = depth + 1L), collapse = " | "))
  }
  if (!is.atomic(value)) return(paste(class(value), collapse = "/"))
  paste(as.character(value[!is.na(value)]), collapse = " | ")
}

.report_html_table <- function(df, id, caption, page_size = 10L) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df) || !ncol(df)) return('<p class="ds-empty">No tabular results were supplied.</p>')
  rows <- lapply(seq_len(nrow(df)), function(i) unname(lapply(df, function(column) {
    value <- if (is.matrix(column)) column[i, ] else if (is.list(column)) column[[i]] else column[i]
    if (is.numeric(value) && length(value) == 1L && is.finite(value)) return(unname(value))
    if (length(value) == 1L && is.atomic(value) && is.na(value)) return(NULL)
    .report_html_cell(value)
  })))
  payload <- jsonlite::toJSON(list(columns = I(names(df)), rows = unname(rows), pageSize = page_size), auto_unbox = TRUE, digits = NA, null = "null")
  # JSON is data, never executable markup, even when a cell contains </script>.
  payload <- paste(strsplit(as.character(payload), "<", fixed = TRUE)[[1L]], collapse = "\\u003c")
  payload <- paste(strsplit(payload, "`", fixed = TRUE)[[1L]], collapse = "\\u0060")
  esc <- .report_html_escape
  paste0('<div class="ds-table" id="', esc(id), '"><div class="ds-table-bar">',
    '<label>Search <input class="ds-search" type="search" placeholder="Search all rows" aria-label="Search ', esc(caption), '"></label>',
    '<label>Rows <select class="ds-page-size" aria-label="Rows per page"><option>10</option><option>20</option><option>50</option><option>100</option></select></label>',
    '<button class="ds-download" type="button" title="Download every matching row across all pages">Download CSV</button></div>',
    '<div class="ds-table-scroll" tabindex="0" role="region" aria-label="', esc(caption), '">',
    '<table><caption>', esc(caption), '</caption><thead><tr>',
    paste0('<th scope="col" aria-sort="none"><button type="button" data-column="', seq_along(df) - 1L, '">', esc(names(df)), '<span aria-hidden="true"> \u2195</span></button></th>', collapse = ""),
    '</tr></thead><tbody></tbody></table></div>',
    '<div class="ds-table-footer"><span class="ds-table-status" role="status" aria-live="polite"></span><div>',
    '<button class="ds-prev" type="button" aria-label="Previous page">Previous</button> ',
    '<button class="ds-next" type="button" aria-label="Next page">Next</button></div></div>',
    '<script type="application/json" class="ds-table-data">', payload, '</script>',
    '<noscript>Enable JavaScript to search, page through, and download this table. Complete results also remain in the original pipeline object.</noscript></div>')
}

.report_html_inventory <- function(object) {
  entries <- .report_method_inventory(object)
  input_names <- c("ppi_network", "drug_target_network", "disease_genes", "query", "input_data")
  inputs <- settings <- results <- list()
  for (entry in entries) {
    if (startsWith(entry$path, "Drug searching / Raw")) results[[length(results) + 1L]] <- data.frame(
      Method = sub("Drug searching / Raw / ", "", entry$path, fixed = TRUE), Rows = nrow(entry$df), Columns = ncol(entry$df))
    for (nm in intersect(input_names, names(entry$parameters))) {
      value <- entry$parameters[[nm]]
      summary <- if (is.atomic(value) && !is.matrix(value)) paste(length(value), "elements") else .report_value(value)
      if (is.list(value) && !is.data.frame(value) && length(value) <= 6L) summary <- paste(
        paste0(if (is.null(names(value))) seq_along(value) else names(value), ": ", lengths(value), " elements"), collapse = "; ")
      inputs[[length(inputs) + 1L]] <- data.frame(Input = nm, Dimensions = summary)
    }
    params <- entry$parameters[setdiff(names(entry$parameters), c(input_names, "output_dir"))]
    if (length(params)) settings[[length(settings) + 1L]] <- data.frame(Method = entry$path,
      Settings = paste(paste0(names(params), " = ", vapply(params, .report_value, character(1))), collapse = "; "))
  }
  bind <- function(x) if (length(x)) unique(do.call(rbind, x)) else data.frame()
  list(inputs = bind(inputs), settings = bind(settings), methods = bind(results))
}

.report_html_metadata <- function(object) {
  out <- list()
  for (nm in intersect(c("metadata", "provenance", "parameters"), names(object))) {
    values <- object[[nm]]
    if (is.list(values) && !is.data.frame(values) && length(values)) {
      keys <- if (is.null(names(values))) seq_along(values) else names(values)
      out[[length(out) + 1L]] <- data.frame(Field = paste(nm, keys, sep = " / "), Value = vapply(values, .report_value, character(1)))
    } else out[[length(out) + 1L]] <- data.frame(Field = nm, Value = .report_value(values))
  }
  if (length(out)) do.call(rbind, out) else data.frame()
}

.report_html_assets <- function(name) {
  path <- system.file("report", name, package = "DrugSigNet")
  if (!nzchar(path)) stop("Missing HTML report asset: ", name, call. = FALSE)
  readLines(path, warn = FALSE)
}

.report_html_plot <- function(item, file, interactive, dpi) {
  p <- .report_unwrap_plot(item)
  if (inherits(p, "plotly")) return(p)
  if (interactive && inherits(p, "ggplot") && requireNamespace("plotly", quietly = TRUE)) {
    widget <- tryCatch(plotly::ggplotly(p), error = function(e) NULL)
    if (!is.null(widget)) return(widget)
  }
  if (.report_static_plot(p, file, dpi = dpi, height = 4.8)) {
    return(knitr::asis_output(paste0('<img class="ds-static-figure" src="', .report_html_escape(normalizePath(file, winslash = "/")), '" alt="Stored pipeline visualization">')))
  }
  knitr::asis_output('<p class="ds-empty">This figure could not be rendered. Its original object remains available in the pipeline result.</p>')
}

.write_html_report <- function(objects, title, base_name, author, top_n, enrichment_n, interactive, options = list()) {
  if (!requireNamespace("rmarkdown", quietly = TRUE) || !requireNamespace("knitr", quietly = TRUE)) stop("HTML reports require rmarkdown and knitr.", call. = FALSE)
  opts <- .report_pdf_options(options)
  esc <- .report_html_escape
  para <- function(x, class = NULL) paste0('<p', if (!is.null(class)) paste0(' class="', class, '"'), '>', esc(x), '</p>')
  section <- function(id, heading) c(paste0('<section class="ds-section" id="', id, '"><h2>', esc(heading), '</h2>'), "")
  close_section <- c('</section>', "")
  models <- lapply(objects, function(obj) list(ranks = .report_rank_model(obj, top_n), enrichment = .report_enrichment(obj, enrichment_n),
    inventory = .report_html_inventory(obj), plots = .report_visualization_items(obj$Visualization)))
  headings <- c(summary = "Executive Summary", overview = "Analysis Overview", candidates = "Top-Ranked Candidates",
    comparison = "Method Comparison", annotation = "Drug Annotation", enrichment = "Functional Enrichment",
    interpretation = "Interpretation and Limitations", methods = "Methods and Provenance", explorer = "Complete Results")
  nav <- character()
  body <- character()
  asset_dir <- paste0(base_name, "_files/figures")
  dir.create(asset_dir, recursive = TRUE, showWarnings = FALSE)
  asset_dir <- normalizePath(asset_dir, winslash = "/", mustWork = TRUE)
  for (i in seq_along(objects)) {
    obj <- objects[[i]]; model <- models[[i]]; rank <- model$ranks; inv <- model$inventory; en <- model$enrichment
    annotation <- .report_annotations(obj, rank$top$Drug)
    annotated <- if (ncol(annotation) > 1L) sum(rowSums(annotation[-1L] != "Not available") > 0L) else 0L
    prefix <- paste0("analysis-", i, "-")
    name <- .report_result_title(obj, i)
    allowed <- headings
    if (!opts$include_summary) allowed <- allowed[names(allowed) != "summary"]
    if (!opts$include_interpretation) allowed <- allowed[names(allowed) != "interpretation"]
    if (!opts$include_methods && !opts$include_reproducibility) allowed <- allowed[names(allowed) != "methods"]
    nav <- c(nav, paste0('<div class="ds-nav-group">', esc(name), '</div><ul>', paste0('<li><a href="#', prefix, names(allowed), '">', esc(allowed), '</a></li>', collapse = ""), '</ul>'))
    body <- c(body, paste0('<header class="ds-analysis"><span class="ds-eyebrow">Analysis ', i, '</span><h1>', esc(name), '</h1></header>'), "")
    add <- function(key, content) {
      if (key %in% names(allowed)) body <<- c(body, section(paste0(prefix, key), headings[[key]]), content, close_section)
    }
    tbl <- function(df, key, caption, size = 10L) .report_html_table(df, paste0(prefix, key), caption, size)
    cards <- c(`Ranked candidates` = nrow(rank$all), `Prioritized candidates` = nrow(rank$top),
      `Annotated shortlist` = paste0(annotated, "/", nrow(rank$top)), `Enrichment records` = en$total)
    summary <- c(paste0('<div class="ds-metrics">', paste0('<div class="ds-metric"><strong>', esc(cards), '</strong><span>', esc(names(cards)), '</span></div>', collapse = ""), '</div>'),
      para(if (nrow(rank$top)) paste("Leading candidates in the report ordering:", paste(vapply(utils::head(rank$top$Drug, 3L), .report_value, character(1), limit = 80L), collapse = ", "), ".") else "No eligible drug rankings were supplied."),
      para("This report summarizes stored results. It does not rerun prioritization or enrichment, and computational rankings do not establish clinical efficacy.", "ds-callout"))
    add("summary", summary)
    coverage <- data.frame(Method = c("CRank", "Dowdall", "RRA"), `Valid ranks` = vapply(rank$all[-1L], function(x) sum(is.finite(x) & x > 0), integer(1)), check.names = FALSE)
    add("overview", c(para("Dimensions describe the inputs retained in method objects and may reflect prior filtering. Equal dimensions do not establish identical content."),
      if (nrow(inv$methods)) tbl(inv$methods, "search-methods", "Stored raw search outputs") else para("Raw search methods were not retained for this analysis."),
      if (nrow(inv$inputs)) tbl(inv$inputs, "inputs", "Retained input dimensions") else para("Input dimensions were not retained."),
      tbl(coverage, "coverage", "Rank availability across eligible candidates"),
      para(sprintf("Relevant annotations are available for %d of %d prioritized candidates. Candidates without an identifier or any valid positive rank are excluded (%d source rows).", annotated, nrow(rank$top), rank$excluded))))
    tie_note <- "CRank ties are preserved. Binned evidence and the configured tie method can produce repeated ranks; ranks alone cannot establish the exact cause of a tie."
    if (nrow(rank$top) && is.finite(utils::tail(rank$top$CRank, 1L)) && utils::tail(rank$top$CRank, 1L) > 0) {
      cutoff <- utils::tail(rank$top$CRank, 1L)
      tie_note <- paste(tie_note, sprintf("At CRank cutoff %s, %d of %d tied candidates are displayed.", cutoff, sum(rank$top$CRank == cutoff, na.rm = TRUE), sum(rank$all$CRank == cutoff, na.rm = TRUE)))
    }
    add("candidates", c(para(sprintf("Selection: up to %d unique drugs ordered by increasing stored CRank, then Dowdall, RRA, and identifier. Missing/nonpositive ranks sort last. Ties do not expand the fixed-size shortlist. Table sorting only reorders these selected candidates.", top_n)),
      tbl(rank$top, "shortlist", "Prioritized candidates and stored aggregation ranks", 20L), para(tie_note, "ds-callout")))
    agreement <- .report_agreement(rank$all)
    plots <- model$plots
    plot_keys <- names(plots)
    if (is.null(plot_keys)) plot_keys <- rep("", length(plots))
    plot_groups <- vapply(plot_keys, function(key) if (key %in% c("drug_indications", "drug_status", "drug_similarity", "drug_hierarchy")) "annotation" else if (key %in% c("enriched_terms", "functional_enrichment")) "enrichment" else "comparison", character(1))
    figure_lines <- function(group) {
      lines <- character()
      for (j in which(plot_groups == group)) {
        label <- if (!is.na(plot_keys[j]) && nzchar(plot_keys[j])) .report_visualization_title(plot_keys[j]) else paste("Stored plot", j)
        lines <- c(lines, paste0('<figure class="ds-figure"><figcaption><strong>', esc(label), '</strong><span>Original pipeline plot; scope and thresholds may differ from the report shortlist.</span></figcaption>'), "",
          sprintf("```{r html-plot-%d-%d}", i, j), sprintf("report_plot(html_models[[%d]]$plots[[%d]], %d, %d)", i, j, i, j), "```", "", '</figure>', "")
      }
      lines
    }
    add("comparison", c(para("Spearman correlation uses candidates with finite positive ranks in both methods and average ranks for ties. Fewer than three shared candidates or constant ranks produce an unavailable coefficient."),
      tbl(transform(agreement, Spearman = round(agreement$Spearman, 3L)), "agreement", "Pairwise aggregation agreement"), figure_lines("comparison"),
      if (!any(plot_groups == "comparison")) para("No comparison plots were stored in this result object.")))
    add("annotation", c(para("Annotations are matched by case-insensitive drug identifier to the shortlist, without synonym expansion. Main-table fields are abridged to 180 characters per cell. Full annotations, including structures and synonyms, are available in Complete Results. Missing annotations do not imply absence of a biological property."),
      tbl(annotation, "annotations", "Relevant annotations for prioritized candidates"), figure_lines("annotation")))
    add("enrichment", c(if (en$total) c(para(sprintf("Showing the %d smallest stored adjusted p-values across all available libraries; %d of %d valid term-library records have adjusted p < 0.05. No additional correction is applied. Overlap is reproduced as supplied. Adjusted p-values use three significant digits for display; downloads retain the stored values.", nrow(en$top), en$significant, en$total)), tbl(en$top, "enrichment-terms", "Leading functional enrichment terms")) else para("No valid enrichment records with stored adjusted p-values were supplied."),
      para("Enrichment reflects overrepresentation in the analyzed gene/target set, not causal or clinical efficacy. Related terms can share genes; adjustment scopes and backgrounds may differ across libraries."), figure_lines("enrichment")))
    rho <- agreement$Spearman[is.finite(agreement$Spearman)]
    add("interpretation", c(para(if (length(rho)) sprintf("Available pairwise rank correlations range from %.2f to %.2f. This measures consistency of computational rankings; methods may share inputs and are not independent validation.", min(rho), max(rho)) else "Ranking agreement cannot be estimated from the available columns; no concordance claim is made."),
      para("Primary CRank ordering can omit candidates favored only by another method. Compare the complete rankings when assessing sensitivity to aggregation choice."),
      para(switch(obj$type, network = "Network results depend on interaction coverage, disease-gene selection and network-method assumptions. Proximity and centrality do not establish the direction of a drug effect or target engagement.", signature = "Signature results depend on reference cell types, doses, exposure times and gene coverage. A transcriptional association does not establish a causal therapeutic mechanism.", "Integrated results combine partially dependent evidence. Identifier matching and available-candidate intersections affect the candidate universe.")),
      para("Use these findings for hypothesis generation. Experimental validation, exposure, toxicity and clinical evidence are needed before therapeutic conclusions.", "ds-callout")))
    meta <- .report_html_metadata(obj)
    add("methods", c(if (opts$include_methods) c(para("CRank combines binned evidence through Bayesian updates; Dowdall sums scaled reciprocal-rank contributions; RRA evaluates unusually high ranks under a random-order null. The report displays retained outputs and does not infer parameters from current defaults."), tbl(inv$settings, "settings", "Recorded methods and settings")),
      if (opts$include_reproducibility) if (nrow(meta)) tbl(meta, "provenance", "Recorded provenance") else para("Source database releases, analysis dates, and additional provenance were not retained. Report-time software versions are listed separately below.")))
    collections <- c(.report_tables(obj$DrugSearching, "Drug searching"), .report_tables(obj$RankAggregation, "Rank aggregation"), .report_tables(obj$DrugAnnotation, "Drug annotation"))
    explorer <- c(para("Full result tables are separate from the scientific shortlist. Search scans all rows; downloads include every matching row across all pages. Expand long cells to read their complete text. Nested cells are flattened to readable text; original R structures remain in the pipeline object or optional supplementary RDS export."),
      '<div class="ds-callout">Use <code>supplementary = TRUE</code> for an unabridged RDS export or <code>write_results = TRUE</code> for Excel exports.</div>')
    for (j in seq_along(collections)) {
      explorer <- c(explorer, paste0('<details class="ds-result"><summary>', esc(names(collections)[j]), '<span>', nrow(collections[[j]]), ' rows</span></summary>'),
        tbl(collections[[j]], paste0("complete-", j), names(collections)[j]), '</details>')
    }
    add("explorer", explorer)
  }
  packages <- c("DrugSigNet", "rmarkdown", "knitr", "ggplot2", "plotly", "RobustRankAggreg")
  if (opts$include_reproducibility) {
    nav <- c(nav, '<ul><li><a href="#report-software">Report Software</a></li></ul>')
    versions <- data.frame(Component = c("R", packages), Version = c(R.version.string,
      vapply(packages, function(x) if (requireNamespace(x, quietly = TRUE)) as.character(utils::packageVersion(x)) else "Not installed", character(1))))
    body <- c(body, section("report-software", "Report Software"), para("Versions describe the rendering environment, which may differ from the original analysis environment."), .report_html_table(versions, "report-versions", "Rendering software versions"), close_section)
  }
  quote_yaml <- function(x) encodeString(paste(x, collapse = ", "), quote = '"')
  yaml <- c("---", paste0("title: ", quote_yaml(esc(title))), "output:", "  html_document:", "    self_contained: true", "    theme: null", "    toc: false", "---", "")
  setup <- c("```{r setup, include=FALSE}", "knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE, results='asis', comment=NA)", "```", "")
  header <- c('<a class="ds-skip" href="#ds-main">Skip to report content</a>', '<header class="ds-header"><span class="ds-eyebrow">DrugSigNet \u00b7 Scientific report</span>',
    paste0('<h1>', esc(title), '</h1>'), para(paste(c(if (!is.null(author)) paste(author, collapse = ", "), as.character(Sys.Date())), collapse = " \u00b7 ")), '</header>',
    '<div class="ds-shell"><aside class="ds-nav" aria-label="Report sections"><div class="ds-nav-title">Contents</div>', nav, '</aside><main id="ds-main" tabindex="-1">')
  rmd <- paste0(base_name, ".Rmd"); output <- paste0(base_name, ".html")
  writeLines(c(yaml, setup, "<style>", .report_html_assets("report.css"), "</style>", header, body, '</main></div>',
    "<script>", .report_html_assets("report.js"), "</script>"), rmd)
  rmarkdown::render(rmd, output_format = rmarkdown::html_document(self_contained = TRUE, theme = NULL, toc = FALSE), output_file = basename(output), quiet = TRUE,
    envir = list2env(list(html_models = models, report_plot = function(item, i, j) .report_html_plot(item, file.path(asset_dir, paste0("plot-", i, "-", j, ".png")), interactive, opts$figure_dpi)), parent = asNamespace("DrugSigNet")))
  invisible(normalizePath(output, winslash = "/", mustWork = FALSE))
}
