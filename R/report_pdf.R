# PDF presentation helpers deliberately operate on copies of stored results.
.report_df <- function(x) {
  if (is.data.frame(x)) return(x)
  if (isS4(x) && "result" %in% methods::slotNames(x)) return(.report_df(methods::slot(x, "result")))
  if (is.list(x) && !is.null(x$result)) return(.report_df(x$result))
  NULL
}

.report_tables <- function(x, path = "", depth = 0L) {
  df <- .report_df(x)
  if (!is.null(df)) return(stats::setNames(list(df), path))
  if (!is.list(x) || depth > 12L) return(list())
  out <- list()
  for (i in seq_along(x)) {
    nm <- names(x)[i]
    if (is.null(nm) || is.na(nm) || !nzchar(nm)) nm <- paste0("Item ", i)
    out <- c(out, .report_tables(x[[i]], paste0(path, if (nzchar(path)) " / ", nm), depth + 1L))
  }
  out
}

.report_rank_model <- function(object, top_n = 20L) {
  prefix <- switch(object$type, signature = "Signature", network = "Network", "Signature_Network")
  agg <- object$RankAggregation
  df <- .report_df(agg[[paste0(prefix, "_Harmonized")]])
  # Legacy lists may contain only the three harmonized aggregation objects.
  if (is.null(df)) {
    df <- .report_df(agg$Harmonized)
    if (is.null(df)) {
      tabs <- .report_tables(agg$Harmonized)
      if (!length(tabs)) tabs <- lapply(agg[intersect(paste0(prefix, "_", c("CRank", "Dowdall", "RRA")), names(agg))], .report_df)
      tabs <- Filter(function(x) !is.null(x) && "Drug" %in% names(x), tabs)
      tabs <- lapply(tabs, function(x) x[c("Drug", grep("(^|_)(CRank|Dowdall|RRA)(_rank)?$", names(x), value = TRUE))])
      if (length(tabs)) df <- Reduce(function(a, b) merge(a, b, by = "Drug", all = TRUE, sort = FALSE), tabs)
    }
  }
  empty <- data.frame(Drug = character(), CRank = numeric(), Dowdall = numeric(), RRA = numeric())
  if (is.null(df) || !"Drug" %in% names(df)) return(list(all = empty, top = empty, source = character(), excluded = 0L, duplicates = 0L))
  out <- data.frame(Drug = as.character(df$Drug), stringsAsFactors = FALSE)
  sources <- character()
  for (method in c("CRank", "Dowdall", "RRA")) {
    choices <- c(paste0(prefix, "_", method), paste0(method, if (method != "CRank") "_rank"), method)
    col <- choices[choices %in% names(df)][1L]
    out[[method]] <- if (is.na(col) || !is.numeric(df[[col]])) rep(NA_real_, nrow(df)) else df[[col]]
    if (!is.na(col)) sources[method] <- col
  }
  valid_id <- !is.na(out$Drug) & nzchar(trimws(out$Drug))
  duplicates <- sum(duplicated(out$Drug[valid_id]))
  # Do not silently resolve conflicting duplicate records into invented ranks.
  out <- unique(out[valid_id, , drop = FALSE])
  if (anyDuplicated(out$Drug)) stop("Conflicting duplicate drug identifiers in report rankings; inspect the harmonized result.", call. = FALSE)
  eligible <- Reduce(`|`, lapply(out[-1], function(x) is.finite(x) & x > 0))
  excluded <- sum(!valid_id) + sum(!eligible)
  out <- out[eligible, , drop = FALSE]
  sort_rank <- function(x) ifelse(is.finite(x) & x > 0, x, Inf)
  out <- out[order(sort_rank(out$CRank), sort_rank(out$Dowdall), sort_rank(out$RRA), out$Drug, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  list(all = out, top = utils::head(out, top_n), source = sources, excluded = excluded, duplicates = duplicates)
}

.report_value <- function(x, limit = 180L) {
  if (is.null(x)) return("not recorded")
  if (is.data.frame(x) || is.matrix(x)) return(sprintf("%s rows x %s columns", nrow(x), ncol(x)))
  if (is.list(x)) return(sprintf("list: %s elements", length(x)))
  if (length(x) > 6L) return(sprintf("%s vector: %s elements", class(x)[1], length(x)))
  if (!is.atomic(x)) return(class(x)[1])
  val <- paste(as.character(x), collapse = ", ")
  val <- gsub("[\r\n\t]+", " ", val)
  if (nchar(val) > limit) paste0(substr(val, 1, limit - 3L), "...") else val
}

.report_method_inventory <- function(object) {
  rows <- list()
  walk <- function(x, path, depth = 0L) {
    if (depth > 12L) return(invisible(NULL))
    df <- .report_df(x)
    params <- if (isS4(x) && "parameters" %in% methods::slotNames(x)) methods::slot(x, "parameters") else if (is.list(x)) x$parameters else NULL
    if (!is.null(df)) {
      rows[[length(rows) + 1L]] <<- list(path = path, df = df, parameters = params)
    } else if (is.list(x)) {
      for (i in seq_along(x)) {
        nm <- names(x)[i]
        if (is.null(nm) || is.na(nm) || !nzchar(nm)) nm <- as.character(i)
        walk(x[[i]], paste(path, nm, sep = " / "), depth + 1L)
      }
    }
  }
  walk(object$DrugSearching, "Drug searching")
  walk(object$RankAggregation, "Rank aggregation")
  rows
}

.report_annotations <- function(object, drugs) {
  features <- .report_df(object$DrugAnnotation$Features)
  if (is.null(features) || !"Drug" %in% names(features) || !length(drugs)) return(data.frame())
  # A small allowlist prevents chemical structures and synonym lists entering PDF.
  fields <- list(
    Status = c("Highest_status", "Status", "status"),
    Mechanism = c("mechanismOfAction", "mechanism_of_action", "Mechanism_of_action", "MOA", "MoA", "moa"),
    Indication = c("indication", "Indication", "Indications", "indications"),
    `ATC class` = c("ATC_level2_description", "ATC_level2"),
    `BBB class` = "BBB_class"
  )
  cols <- unique(unlist(lapply(fields, function(choices) utils::head(intersect(choices, names(features)), 1L)), use.names = FALSE))
  if (!length(cols)) return(data.frame())
  key <- tolower(trimws(as.character(features$Drug)))
  out <- data.frame(Drug = drugs, stringsAsFactors = FALSE)
  for (col in cols) {
    out[[col]] <- vapply(tolower(trimws(drugs)), function(drug) {
      indices <- which(!is.na(key) & key == drug)
      values <- unique(unlist(lapply(indices, function(i) {
        value <- features[[col]][[i]]
        if (is.atomic(value)) as.character(value) else character()
      }), use.names = FALSE))
      values <- values[!is.na(values) & nzchar(trimws(values))]
      if (!length(values)) return("Not available")
      .report_value(paste(values, collapse = "; "), limit = 180L)
    }, character(1))
  }
  out
}

.report_enrichment <- function(object, n = 12L) {
  tabs <- .report_tables(object$DrugAnnotation$Functional_Enrichment)
  out <- list()
  for (i in seq_along(tabs)) {
    df <- tabs[[i]]
    term <- intersect(c("Term", "Description", "term"), names(df))[1]
    pval <- intersect(c("Adjusted.P.value", "p.adjust", "padj", "FDR"), names(df))[1]
    if (is.na(term) || is.na(pval) || !is.numeric(df[[pval]])) next
    ontology <- intersect(c("ont", "Ontology", "Database"), names(df))[1]
    overlap <- intersect(c("Overlap", "GeneRatio", "Count"), names(df))[1]
    out[[length(out) + 1L]] <- data.frame(
      Term = as.character(df[[term]]),
      Library = if (!is.na(ontology)) as.character(df[[ontology]]) else if (nzchar(names(tabs)[i])) names(tabs)[i] else "Not recorded",
      `Adjusted p` = df[[pval]],
      Overlap = if (!is.na(overlap)) as.character(df[[overlap]]) else "Not recorded",
      check.names = FALSE
    )
  }
  if (!length(out)) return(list(top = data.frame(), all = data.frame(), total = 0L, significant = 0L))
  all <- unique(do.call(rbind, out))
  all <- all[is.finite(all[["Adjusted p"]]) & all[["Adjusted p"]] >= 0 & all[["Adjusted p"]] <= 1 & !is.na(all$Term) & nzchar(all$Term), , drop = FALSE]
  all <- all[order(all[["Adjusted p"]], all$Library, all$Term, method = "radix"), , drop = FALSE]
  list(top = utils::head(all, n), all = all, total = nrow(all), significant = sum(all[["Adjusted p"]] < 0.05))
}

# Escape every cell, permitting breaks inside long unspaced identifiers.
.report_tex <- function(x) {
  replacements <- c("\\" = "\\textbackslash{}", "{" = "\\{", "}" = "\\}", "$" = "\\$", "&" = "\\&", "#" = "\\#", "%" = "\\%", "_" = "\\_", "~" = "\\textasciitilde{}", "^" = "\\textasciicircum{}")
  vapply(as.character(x), function(value) {
    if (is.na(value)) return("Not available")
    chars <- strsplit(gsub("[\r\n\t]", " ", value), "", fixed = TRUE)[[1]]
    escaped <- vapply(chars, function(ch) if (ch %in% names(replacements)) replacements[[ch]] else ch, character(1))
    run <- 0L
    breaks <- vapply(chars, function(ch) {
      run <<- if (grepl("[[:space:]]", ch)) 0L else run + 1L
      if (run > 0L && run %% 16L == 0L) "\\allowbreak{}" else ""
    }, character(1))
    paste0(escaped, breaks, collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

.report_pdf_table <- function(df, widths = rep(1 / ncol(df), ncol(df))) {
  if (!nrow(df)) return("No data available.\n")
  values <- lapply(names(df), function(nm) {
    x <- df[[nm]]
    if (identical(nm, "Adjusted p")) return(.report_tex(formatC(x, format = "g", digits = 3L)))
    if (is.numeric(x)) {
      .report_tex(vapply(x, function(v) if (is.na(v)) "Not available" else format(v, digits = 15, trim = TRUE, scientific = abs(v) < 0.001 && v != 0), character(1)))
    } else .report_tex(x)
  })
  spec <- paste0("@{}", paste0(">{\\raggedright\\arraybackslash}p{\\dimexpr", sprintf("%.4f", widths), "\\linewidth-2\\tabcolsep\\relax}", collapse = ""), "@{}")
  header <- paste(paste0("\\textbf{", .report_tex(names(df)), "}"), collapse = " & ")
  c("\\begingroup\\small", paste0("\\begin{longtable}{", spec, "}"), "\\toprule", paste0(header, " \\\\"), "\\midrule", "\\endfirsthead", "\\toprule", paste0(header, " \\\\"), "\\midrule", "\\endhead", "\\midrule", paste0("\\multicolumn{", ncol(df), "}{r}{\\footnotesize Continued on next page} \\\\"), "\\endfoot", "\\bottomrule", "\\endlastfoot", vapply(seq_len(nrow(df)), function(i) paste0(paste(vapply(values, `[`, character(1), i), collapse = " & "), " \\\\[4pt]"), character(1)), "\\end{longtable}", "\\endgroup", "")
}

.report_pdf_heading <- function(text, level = "subsection") {
  c("\\FloatBarrier", "\\Needspace{11\\baselineskip}", paste0("\\", level, "{", .report_tex(text), "}"), "")
}

.report_agreement <- function(ranks) {
  pairs <- utils::combn(c("CRank", "Dowdall", "RRA"), 2L, simplify = FALSE)
  do.call(rbind, lapply(pairs, function(pair) {
    a <- ranks[[pair[1]]]; b <- ranks[[pair[2]]]
    ok <- is.finite(a) & a > 0 & is.finite(b) & b > 0
    rho <- if (sum(ok) >= 3L && length(unique(a[ok])) > 1L && length(unique(b[ok])) > 1L) stats::cor(a[ok], b[ok], method = "spearman") else NA_real_
    data.frame(Comparison = paste(pair, collapse = " vs "), N = sum(ok), Spearman = rho)
  }))
}

.report_unwrap_plot <- function(item) {
  for (i in seq_len(8L)) {
    if (inherits(item, c("ggplot", "plotly", "recordedplot", "grob", "gTree", "gtable"))) return(item)
    if (isS4(item) && "result" %in% methods::slotNames(item)) {
      item <- methods::slot(item, "result")
    } else if (is.list(item) && !is.null(item$plot)) {
      item <- item$plot
    } else if (is.list(item) && !is.null(item$result) && !is.data.frame(item$result)) {
      item <- item$result
    } else break
  }
  item
}

.report_hierarchy_fallback <- function(plot) {
  traces <- plot$x$data
  if (requireNamespace("plotly", quietly = TRUE)) {
    traces <- tryCatch(plotly::plotly_build(plot)$x$data, error = function(e) traces)
  }
  for (trace in traces) {
    if (!identical(trace$type, "sunburst")) next
    labels <- as.character(unlist(trace$labels))
    parents <- as.character(unlist(trace$parents))
    ids <- as.character(unlist(trace$ids))
    values <- suppressWarnings(as.numeric(unlist(trace$values)))
    if (!length(labels) || length(parents) != length(labels) || length(values) != length(labels)) next
    if (length(ids) != length(labels)) ids <- labels
    roots <- which(!is.na(parents) & !nzchar(parents))
    selected <- if (length(roots) == 1L && any(parents %in% ids[roots])) which(parents %in% ids[roots]) else roots
    selected <- selected[is.finite(values[selected]) & values[selected] > 0 & !is.na(labels[selected])]
    if (!length(selected)) next
    selected <- utils::head(selected[order(-values[selected], labels[selected])], 12L)
    df <- data.frame(Label = factor(seq_along(selected), levels = rev(seq_along(selected))), Value = values[selected])
    names_by_id <- stats::setNames(stringr::str_wrap(labels[selected], 40L), seq_along(selected))
    return(ggplot2::ggplot(df, ggplot2::aes(x = .data$Label, y = .data$Value)) +
      ggplot2::geom_col(fill = "#246080", width = 0.65) + ggplot2::coord_flip() +
      ggplot2::scale_x_discrete(labels = names_by_id) +
      ggplot2::labs(x = NULL, y = "Stored branch value", title = "Hierarchy: top-level branches",
        subtitle = "Static summary of up to 12 branches; full interactive hierarchy remains in HTML") +
      ggplot2::theme_minimal(base_size = 11))
  }
  NULL
}

.report_prepare_figure <- function(item, file, dpi = 300, height = 4.6) {
  if (.report_static_plot(item, file, dpi, height = height)) return(list(path = file, note = NULL))
  p <- .report_unwrap_plot(item)
  if (inherits(p, "plotly")) {
    fallback <- .report_hierarchy_fallback(p)
    if (!is.null(fallback) && .report_static_plot(fallback, file, dpi, height = height)) {
      return(list(path = file, note = "Static Plotly export was unavailable. This figure summarizes stored top-level hierarchy branch values; it does not reproduce the interactive sunburst. These values are not inferred candidate counts."))
    }
  }
  list(path = NULL, note = "Static figure export is unavailable for this stored visualization. Inspect its interactive version in the HTML report or original object.")
}

.report_stored_figure_blocks <- function(plots, index) {
  blocks <- list(comparison = character(), annotation = character(), enrichment = character())
  for (i in seq_along(plots)) {
    key <- names(plots)[i]
    if (is.null(key) || is.na(key) || !nzchar(key)) key <- paste("Stored figure", i)
    section <- if (key %in% c("drug_indications", "drug_status", "drug_similarity", "drug_hierarchy")) "annotation" else if (key %in% c("enriched_terms", "functional_enrichment")) "enrichment" else "comparison"
    title <- .report_visualization_title(key)
    height <- if (key %in% c("rank_agreement", "rank_distribution_scatter", "drug_hierarchy", "enriched_terms")) 5.2 else 4.4
    caption <- paste0(title, ". Stored pipeline visualization; candidate scope and thresholds follow the original plot and may differ from the report Top-N.")
    blocks[[section]] <- c(blocks[[section]],
      sprintf("```{r stored-figure-%d-%d, results='asis', fig.height=%s, fig.cap=%s}", index, i, height, encodeString(caption, quote = '"')),
      sprintf("image_file <- paste0(knitr::opts_chunk$get('fig.path'), 'stored-%d-%d.png')", index, i),
      "dir.create(dirname(image_file), recursive = TRUE, showWarnings = FALSE)",
      sprintf("prepared <- prepare_figure(pdf_models[[%d]]$plots[[%d]], image_file, height = %s)", index, i, height),
      "if (!is.null(prepared$path)) knitr::include_graphics(prepared$path)",
      sprintf("if (!is.null(prepared$note)) cat(paste0('\\n\\n', %s, ': ', prepared$note, '\\n\\n'))", encodeString(title, quote = '"')),
      "```", "")
  }
  blocks
}

.report_plotly_export <- function(plot, file, dpi = 300, width = 7, height = 4.6) {
  if (!requireNamespace("plotly", quietly = TRUE)) stop("Plotly is unavailable.")
  suppressWarnings(plotly::save_image(plot, file = file, width = width * dpi, height = height * dpi))
}

.report_static_plot <- function(item, file, dpi = 300, width = 7, height = 4.6) {
  p <- .report_unwrap_plot(item)
  tryCatch({
    if (inherits(p, "plotly")) {
      .report_plotly_export(p, file, dpi, width, height)
    } else if (inherits(p, "ggplot")) {
      p <- p + ggplot2::theme(plot.margin = ggplot2::margin(12, 18, 12, 12), text = ggplot2::element_text(size = 11))
      ggplot2::ggsave(file, p, width = width, height = height, units = "in", limitsize = TRUE, dpi = dpi)
    } else if (inherits(p, "recordedplot")) {
      grDevices::png(file, width = width, height = height, units = "in", res = dpi)
      tryCatch(grDevices::replayPlot(p), finally = grDevices::dev.off())
    } else if (inherits(p, c("grob", "gTree", "gtable"))) {
      ggplot2::ggsave(file, p, width = width, height = height, units = "in", limitsize = TRUE, dpi = dpi)
    } else return(FALSE)
    file.exists(file) && file.info(file)$size > 0
  }, error = function(e) FALSE)
}

.report_install_tinytex <- function() {
  # utils::untar() otherwise honors R's external TAR setting. GNU tar needs an
  # xz executable for current TinyTeX bundles, which may be absent from the host
  # RStudio PATH even when it is installed inside Pixi. R's internal extractor
  # reads .tar.xz directly using its built-in compression support.
  previous_tar <- Sys.getenv("TAR", unset = NA_character_)
  on.exit(if (is.na(previous_tar)) Sys.unsetenv("TAR") else Sys.setenv(TAR = previous_tar), add = TRUE)
  Sys.setenv(TAR = "internal")
  # The report uses the returned user installation directly; do not register
  # executables in the operating system's PATH or request administrative access.
  tinytex::install_tinytex(add_path = FALSE)
}

.report_use_tinytex <- function(root) {
  if (!is.character(root) || length(root) != 1L || is.na(root) || !nzchar(root)) return(invisible(FALSE))
  bins <- list.dirs(file.path(path.expand(root), "bin"), recursive = FALSE, full.names = TRUE)
  engine_files <- c("xelatex", "lualatex", "pdflatex")
  if (.Platform$OS.type == "windows") engine_files <- paste0(engine_files, ".exe")
  bins <- bins[vapply(bins, function(bin) any(file.exists(file.path(bin, engine_files))), logical(1))]
  if (!length(bins)) return(invisible(FALSE))
  Sys.setenv(PATH = paste(c(bins, Sys.getenv("PATH")), collapse = .Platform$path.sep))
  invisible(TRUE)
}

.report_latex_engine <- function() {
  engines <- c("xelatex", "lualatex", "pdflatex")
  available <- engines[nzchar(Sys.which(engines))]
  if (!length(available) && requireNamespace("tinytex", quietly = TRUE)) {
    # An existing user installation can be usable even when its bin directory
    # is absent from the current RStudio session's PATH.
    .report_use_tinytex(tinytex::tinytex_root(error = FALSE))
    available <- engines[nzchar(Sys.which(engines))]
    if (!length(available)) {
      message("No LaTeX engine found; installing TinyTeX before rendering the PDF report.")
      root <- tryCatch(.report_install_tinytex(), error = function(e) stop("PDF reports require a LaTeX engine. TinyTeX install error: ", conditionMessage(e), call. = FALSE))
      .report_use_tinytex(root)
    }
    available <- engines[nzchar(Sys.which(engines))]
  }
  if (!length(available)) stop("PDF reports require a LaTeX engine. Install TinyTeX or a system TeX distribution, then rerun write_report().", call. = FALSE)
  available[1L]
}

.report_display_ids <- function(ids, universe) {
  vapply(ids, function(id) {
    if (nchar(id) <= 80L) return(id)
    paste0("[C", sprintf("%02d", match(id, universe)), "] ", substr(id, 1, 36L), "...", substr(id, nchar(id) - 15L, nchar(id)))
  }, character(1), USE.NAMES = FALSE)
}

.report_pdf_options <- function(options = list()) {
  defaults <- list(include_summary = TRUE, include_methods = TRUE,
    include_interpretation = TRUE, include_reproducibility = TRUE,
    include_appendix = TRUE, figure_dpi = 300, table_rows = 50L,
    table_orientation = "auto")
  utils::modifyList(defaults, options)
}

.report_pdf_filter_sections <- function(lines, options) {
  controls <- c("Executive Summary" = options$include_summary,
    "Methods and Reproducibility" = options$include_methods,
    "Interpretation and Limitations" = options$include_interpretation,
    "Analysis Provenance" = options$include_reproducibility,
    "Optional Supplementary Material" = options$include_appendix)
  keep <- rep(TRUE, length(lines))
  headings <- which(startsWith(lines, "\\subsection{"))
  for (i in seq_along(headings)) {
    title <- substr(lines[headings[i]], nchar("\\subsection{") + 1L, nchar(lines[headings[i]]) - 1L)
    if (title %in% names(controls) && !controls[[title]]) {
      start <- max(1L, headings[i] - 2L)
      end <- if (i < length(headings)) headings[i + 1L] - 3L else length(lines)
      keep[seq.int(start, end)] <- FALSE
    }
  }
  lines[keep]
}

.report_pdf_body <- function(object, index, top_n, enrichment_n, supplementary, options = list()) {
  options <- .report_pdf_options(options)
  rank <- .report_rank_model(object, top_n)
  inventory <- .report_method_inventory(object)
  enrichment <- .report_enrichment(object, enrichment_n)
  top <- rank$top
  all <- rank$all
  annotation <- .report_annotations(object, top$Drug)
  title <- .report_result_title(object, index)
  text <- function(x) c(.report_tex(x), "")
  heading <- .report_pdf_heading
  table <- function(df, widths) {
    shown <- utils::head(df, options$table_rows)
    c(.report_pdf_table(shown, widths), if (nrow(df) > nrow(shown)) text(sprintf(
      "Showing %d of %d summary rows; complete values remain in the input object.", nrow(shown), nrow(df))))
  }
  raw <- Filter(function(x) startsWith(x$path, "Drug searching / Raw"), inventory)
  n_methods <- length(raw)
  methods <- if (n_methods) paste(vapply(raw, function(x) sub("Drug searching / Raw / ", "", x$path, fixed = TRUE), character(1)), collapse = ", ") else "not recorded"
  input_stats <- character()
  for (nm in c("ppi_network", "drug_target_network", "disease_genes", "query")) {
    values <- unique(unlist(lapply(inventory, function(entry) {
      value <- entry$parameters[[nm]]
      if (is.null(value)) return(NULL)
      if (is.atomic(value) && !is.matrix(value)) return(paste(length(value), "elements"))
      .report_value(value)
    }), use.names = FALSE))
    if (length(values)) input_stats <- c(input_stats, paste0(nm, ": ", paste(utils::head(values, 3L), collapse = " / "), if (length(values) > 3L) " (additional input sizes retained)"))
  }
  lines <- c(heading(title, "section"), heading("Executive Summary"), text(sprintf(
    "%s analysis contains %d eligible ranked candidates; %d are prioritized for this report. Stored raw search methods (%d): %s. Leading candidates in the report ordering: %s.",
    switch(object$type, network = "Network-based", signature = "Signature-based", "Integrated"), nrow(all), nrow(top), n_methods, methods,
    if (nrow(top)) paste(.report_display_ids(utils::head(top$Drug, 3L), top$Drug), collapse = ", ") else "none available"
  )), text(if (length(input_stats)) paste("Retained input statistics:", paste(input_stats, collapse = "; ")) else "Original input statistics were not retained in this section."), text(sprintf("Functional enrichment contains %d valid term-library records, including %d with stored adjusted p < 0.05. Report summaries use only supplied results; no searches, ranking algorithms, or enrichment tests are rerun.", enrichment$total, enrichment$significant)), heading("Analysis Overview"))
  if (length(raw)) {
    overview <- do.call(rbind, lapply(raw, function(x) data.frame(Method = sub("Drug searching / Raw / ", "", x$path, fixed = TRUE), `Result rows` = nrow(x$df), `Result columns` = ncol(x$df), check.names = FALSE)))
    lines <- c(lines, table(overview, c(0.6, 0.2, 0.2)))
  } else lines <- c(lines, text("Raw search results were not supplied for this section. Integrated sections may retain only aggregation outputs."))
  # Summarize shared input dimensions once, without traversing gene/network contents.
  input_names <- c("ppi_network", "drug_target_network", "disease_genes", "query", "input_data")
  input_rows <- list()
  for (entry in inventory) {
    for (nm in intersect(input_names, names(entry$parameters))) {
      x <- entry$parameters[[nm]]
      description <- .report_value(x)
      if (is.atomic(x) && !is.matrix(x)) description <- sprintf("%s vector: %s elements", class(x)[1L], length(x))
      if (is.list(x) && !is.data.frame(x) && length(x) <= 6L) {
        description <- paste(paste0(names(x), ": ", vapply(x, function(value) {
          if (is.atomic(value) && !is.matrix(value)) paste(length(value), "elements") else .report_value(value)
        }, character(1))), collapse = "; ")
      }
      input_rows[[length(input_rows) + 1L]] <- data.frame(Input = nm, Dimensions = description)
    }
  }
  if (length(input_rows)) {
    inputs <- unique(do.call(rbind, input_rows))
    lines <- c(lines, table(inputs, c(0.32, 0.68)), text("Identical input labels and dimensions are collapsed above; equal dimensions do not establish identical content. Dimensions refer to stored method inputs, which may already have been filtered."))
  } else lines <- c(lines, text("Input dimensions were not retained in the supplied result objects."))
  lines <- c(lines, heading("Top-Ranked Drug Candidates"), text(sprintf(
    "Selection: at most %d unique candidates, ordered by increasing stored CRank, then Dowdall rank, then RRA rank, then drug identifier (bytewise order). Only finite positive ranks are eligible; missing or nonpositive ranks sort last. No rank values are recalculated. Rows with no eligible rank or no identifier are excluded (%d); exact duplicate ranking rows are collapsed (%d duplicate identifiers in the source).", top_n, rank$excluded, rank$duplicates
  )))
  if (length(rank$source)) lines <- c(lines, text(paste("Rank columns:", paste(paste(names(rank$source), rank$source, sep = " = "), collapse = "; "))))
  display_top <- top
  display_top$Drug <- .report_display_ids(top$Drug, top$Drug)
  lines <- c(lines, .report_pdf_table(display_top, c(0.43, 0.19, 0.19, 0.19)))
  if (any(nchar(top$Drug) > 80L)) lines <- c(lines, text("Identifiers longer than 80 characters are abbreviated with a unique C-number referring to their position in this candidate table. Full identifiers remain in the original objects and optional supplementary exports."))
  tied <- all$CRank[is.finite(all$CRank) & all$CRank > 0]
  tied_groups <- sum(base::table(tied) > 1L)
  lines <- c(lines, text(sprintf("CRank tie audit: %d repeated rank values across %d candidates with valid CRank. CRank uses binned evidence and its configured ties_method; identical evidence can yield tied ranks. With ties_method = 'max', tied candidates receive the largest occupied position, so repeated values such as 42 need not indicate a sorting error. The report preserves stored ties; secondary columns determine display order only. The exact cause of an individual tie cannot be established from ranks alone.", tied_groups, length(tied))))
  if (nrow(top) && is.finite(utils::tail(top$CRank, 1)) && utils::tail(top$CRank, 1) > 0) {
    cutoff <- utils::tail(top$CRank, 1)
    group_total <- sum(all$CRank == cutoff, na.rm = TRUE)
    group_shown <- sum(top$CRank == cutoff, na.rm = TRUE)
    lines <- c(lines, text(sprintf("At the displayed CRank cutoff (%s), %d of %d tied candidates are shown. The fixed Top-N display does not expand ties; omitted tied candidates remain in the complete results.", format(cutoff, trim = TRUE), group_shown, group_total)))
  }
  plots <- .report_visualization_items(object$Visualization)
  stored_figures <- .report_stored_figure_blocks(plots, index)
  agreement <- .report_agreement(all)
  lines <- c(lines, heading("Method Comparison and Visualizations"), text("Pairwise Spearman correlation is computed across candidates with finite positive ranks in both methods, using average ranks for correlation ties. This diagnostic does not alter prioritization. A coefficient is unavailable for fewer than three shared candidates or a constant ranking."), table(transform(agreement, Spearman = round(agreement$Spearman, 3L)), c(0.55, 0.15, 0.3)))
  # The full-rank scatter adds distribution and tie structure, rather than
  # repeating the coefficients or the Top-N candidate table in a heatmap.
  if (!any(names(plots) %in% c("rank_distribution_scatter", "rank_distribution")) && sum(is.finite(all$CRank) & all$CRank > 0 & is.finite(all$Dowdall) & all$Dowdall > 0) >= 3L) {
    lines <- c(lines,
      sprintf("```{r rank-scatter-%d, fig.height=3.6, fig.cap='Stored CRank versus Dowdall ranks across all candidates with valid ranks in both methods. The diagonal marks equal ranks; vertical bands reveal CRank ties. Points may overlap.'}", index),
      sprintf("ranks <- pdf_models[[%d]]$ranks", index),
      "ok <- is.finite(ranks$CRank) & ranks$CRank > 0 & is.finite(ranks$Dowdall) & ranks$Dowdall > 0",
      "ggplot2::ggplot(ranks[ok, ], ggplot2::aes(x = .data$CRank, y = .data$Dowdall)) + ggplot2::geom_abline(slope = 1, intercept = 0, colour = 'grey65', linewidth = 0.4) + ggplot2::geom_point(colour = '#246080', alpha = 0.4, size = 1.2) + ggplot2::labs(x = 'CRank (smaller is better)', y = 'Dowdall rank (smaller is better)') + ggplot2::theme_minimal(base_size = 11)", "```", "")
  }
  lines <- c(lines, stored_figures$comparison)
  if (nrow(annotation)) {
    lines <- c(lines, heading("Drug Annotation"), text("Annotations are matched by case-insensitive drug identifier to the displayed candidates only; no synonym expansion is performed. Fields are abridged to 180 characters per cell. Missing values indicate absent annotations, not absence of a biological property. Status and indication describe source annotations and do not establish efficacy for the analyzed condition."))
    cols <- setdiff(names(annotation), "Drug")
    labels <- c(Highest_status = "Status", mechanismOfAction = "Mechanism", mechanism_of_action = "Mechanism", Mechanism_of_action = "Mechanism", MOA = "Mechanism", moa = "Mechanism", MoA = "Mechanism", ATC_level2_description = "ATC class", ATC_level2 = "ATC class", BBB_class = "BBB class")
    evidence <- vapply(seq_len(nrow(annotation)), function(i) {
      paste(vapply(cols, function(col) paste0(if (col %in% names(labels)) labels[[col]] else gsub("_", " ", col), ": ", annotation[[col]][i]), character(1)), collapse = "; ")
    }, character(1))
    lines <- c(lines, .report_pdf_table(data.frame(Drug = .report_display_ids(annotation$Drug, top$Drug), Annotation = evidence), c(0.3, 0.7)))
  } else lines <- c(lines, heading("Drug Annotation"), text("No supported annotation fields were available for the prioritized candidates. Full annotation payloads, if supplied, remain in the original objects."))
  lines <- c(lines, stored_figures$annotation)
  if (enrichment$total) {
    lines <- c(lines, heading("Functional Enrichment"), text(sprintf("The %d displayed terms are the smallest stored adjusted p-values across all available libraries (not per library); ties are ordered by library and term. Identical term-library-p-overlap records are collapsed. No additional multiple-testing adjustment is applied. Overlap is reproduced as supplied (usually hit count / term-set size); missing denominators are not inferred.", nrow(enrichment$top))), .report_pdf_table(enrichment$top, c(0.48, 0.16, 0.18, 0.18)), text("Enrichment describes overrepresentation in the supplied gene or target set. Related terms may share genes and are not independent mechanisms. Background, library versions, and the original correction scope must be checked in the source analysis; ranking across libraries does not harmonize their statistical tests."))
  }
  if (length(stored_figures$enrichment) && !enrichment$total) lines <- c(lines, heading("Functional Enrichment"))
  lines <- c(lines, stored_figures$enrichment)
  if (!length(stored_figures$enrichment) && enrichment$total && length(unique(enrichment$all$Library)) > 1L) {
    lines <- c(lines, sprintf("```{r enrichment-scope-%d, fig.height=3.3, fig.cap='Full enrichment scope by library, including terms outside the displayed shortlist. Counts use stored adjusted p < 0.05 as a descriptive threshold; correction scopes may differ between libraries.'}", index),
      sprintf("terms <- pdf_models[[%d]]$enrichment", index),
      "terms$Threshold <- ifelse(terms[['Adjusted p']] < 0.05, 'Adjusted p < 0.05', 'Adjusted p >= 0.05')",
      "counts <- as.data.frame(table(terms$Library, terms$Threshold), stringsAsFactors = FALSE)",
      "names(counts) <- c('Library', 'Threshold', 'Count')",
      "ggplot2::ggplot(counts, ggplot2::aes(x = .data$Library, y = .data$Count, fill = .data$Threshold)) + ggplot2::geom_col(width = 0.65) + ggplot2::coord_flip() + ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, 32)) + ggplot2::scale_fill_manual(values = c('#246080', '#cbd5df')) + ggplot2::labs(x = NULL, y = 'Term-library records', fill = NULL) + ggplot2::theme_minimal(base_size = 11) + ggplot2::theme(legend.position = 'bottom')", "```", "")
  }
  lines <- c(lines, heading("Interpretation and Limitations"))
  usable <- agreement$Spearman[is.finite(agreement$Spearman)]
  if (length(usable)) {
    lines <- c(lines, text(sprintf("Available pairwise rank correlations range from %.2f to %.2f. Agreement measures consistency of computational rankings, not independent biological validation; methods may share inputs. Disagreement identifies candidates whose priority depends on the aggregation method.", min(usable), max(usable))))
  } else lines <- c(lines, text("Ranking agreement could not be estimated from the supplied columns; no claim of concordance is made."))
  if (nrow(top)) {
    jointly_high <- sum(Reduce(`&`, lapply(top[-1], function(x) is.finite(x) & x > 0 & x <= top_n)))
    lines <- c(lines, text(sprintf("Among the displayed candidates, %d have a stored rank of at most %d in all three aggregation methods. This rank-threshold diagnostic differs from the fixed-size display selection when ranks are tied. Primary CRank ordering may omit candidates favored only by Dowdall or RRA; inspect complete rankings for sensitivity analysis.", jointly_high, top_n)))
  }
  lines <- c(lines, text(switch(object$type,
    network = "Network-based prioritization depends on interaction coverage, disease-gene selection, and network-method assumptions. Proximity or centrality alone does not establish the direction of a drug effect, target engagement, or disease-specific efficacy.",
    signature = "Signature-based prioritization depends on reference cell types, doses, exposure times, and gene coverage. A transcriptional association does not establish an effective dose or a causal therapeutic mechanism.",
    "Integrated prioritization combines partially dependent signature and network evidence. Identifier matching and the intersection of available candidates can change the candidate universe; evidence agreement is not clinical validation."
  )), text("These results support hypothesis generation. Annotation completeness, ties, missing ranks, and database coverage can affect prioritization. Experimental validation, exposure, toxicity, and clinical evidence are required before therapeutic conclusions."), heading("Methods and Reproducibility"), text("The following inventory records stored method outputs and retained parameters. Large tables, networks, vectors, and nested objects are represented by dimensions or element counts. Repeated identical parameter summaries are grouped; values not retained by the analysis cannot be reconstructed. Parameter values are not inferred from current function defaults."))
  if (index == 1L) lines <- c(lines, text("Aggregation methods: CRank combines binned rank evidence through iterative Bayesian updates; Dowdall sums scaled reciprocal-rank contributions; robust rank aggregation (RRA) evaluates unusually high ranks under a random-order null. Stored harmonized aggregation outputs, rather than intermediate method scores, supply the candidate table."))
  parameter_rows <- list()
  for (entry in inventory) {
    params <- entry$parameters
    params <- params[setdiff(names(params), c(input_names, "output_dir"))]
    detail <- if (length(params)) paste(paste0(names(params), " = ", vapply(params, .report_value, character(1))), collapse = "; ") else "Parameters not recorded"
    # A row may not grow beyond a page even for unusual legacy parameter lists.
    if (nchar(detail) > 650L) detail <- paste0(substr(detail, 1, 647L), "... [abridged; see original object]")
    parameter_rows[[length(parameter_rows) + 1L]] <- data.frame(Method = entry$path, Summary = detail)
  }
  if (length(parameter_rows)) {
    params <- unique(do.call(rbind, parameter_rows))
    # Harmonized wide tables have no parameters and their dimensions already
    # follow from the candidate universe; omit those non-method records.
    params <- params[params$Summary != "Parameters not recorded" | startsWith(params$Method, "Drug searching / Raw"), , drop = FALSE]
    lines <- c(lines, table(params, c(0.35, 0.65)))
  }
  lines <- c(lines, heading("Analysis Provenance"))
  metadata <- object[intersect(c("metadata", "provenance", "parameters"), names(object))]
  if (length(metadata)) {
    meta <- do.call(rbind, lapply(names(metadata), function(nm) {
      value <- metadata[[nm]]
      if (is.list(value) && !is.data.frame(value) && length(value)) {
        keys <- names(value)
        if (is.null(keys)) keys <- as.character(seq_along(value))
        data.frame(Field = paste(nm, keys, sep = " / "), Value = vapply(value, .report_value, character(1)))
      } else data.frame(Field = nm, Value = .report_value(value))
    }))
    lines <- c(lines, table(utils::head(meta, 20L), c(0.35, 0.65)))
    if (nrow(meta) > 20L) lines <- c(lines, text("Provenance abridged to 20 entries; complete metadata remain in the object."))
  } else lines <- c(lines, text("No additional source provenance metadata were retained. Report-time software versions below describe this rendering environment, not necessarily the original analysis environment. Database releases and analysis dates are unknown unless recorded in the method parameters."))
  if (index == 1L) lines <- c(lines, heading("Optional Supplementary Material"), text(if (supplementary) "Complete original input object(s), including annotations, parameters, and visualization objects, were saved to the companion _supplementary.rds file. Existing Excel exports, if requested, provide tabular results. Neither export changes stored rankings." else "Complete results remain accessible in the supplied pipeline objects. Set supplementary = TRUE to save an unabridged RDS companion, or write_results = TRUE for the existing Excel table exports. Synonyms, structures, full enrichment, and large method inputs are intentionally excluded from the main PDF."))
  list(lines = .report_pdf_filter_sections(lines, options), ranks = all, plots = plots, enrichment = enrichment$all)
}

.write_pdf_report <- function(objects, title, base_name, author, top_n, enrichment_n, supplementary, options = list()) {
  previous_path <- Sys.getenv("PATH", unset = NA_character_)
  on.exit(if (is.na(previous_path)) Sys.unsetenv("PATH") else Sys.setenv(PATH = previous_path), add = TRUE)
  options <- .report_pdf_options(options)
  engine <- .report_latex_engine()
  if (!requireNamespace("rmarkdown", quietly = TRUE) || !requireNamespace("knitr", quietly = TRUE)) stop("PDF reports require rmarkdown and knitr.", call. = FALSE)
  models <- lapply(seq_along(objects), function(i) .report_pdf_body(objects[[i]], i, top_n, enrichment_n, supplementary, options))
  # YAML quoted scalars protect titles/authors containing colons, quotes or lines.
  quote_yaml <- function(x) encodeString(paste(x, collapse = ", "), quote = '"')
  yaml <- c("---", paste0("title: ", quote_yaml(title)), paste0("date: ", quote_yaml(Sys.Date())),
    if (!is.null(author)) paste0("author: ", quote_yaml(author)),
    "documentclass: article", "fontsize: 11pt", if (identical(options$table_orientation, "landscape")) "geometry: [margin=22mm, landscape]" else "geometry: margin=22mm", "colorlinks: true",
    "output:", "  pdf_document:", paste0("    latex_engine: ", engine), "    keep_tex: true", "    toc: true", "    toc_depth: 2", "    number_sections: true",
    "header-includes:", "  - \\usepackage{longtable,booktabs,array,needspace,float}",
    "  - \\usepackage[section]{placeins}", "  - \\usepackage{caption}",
    "  - \\captionsetup{font=small,labelfont=bf}", "  - \\setlength{\\parindent}{0pt}",
    "  - \\setlength{\\parskip}{5pt}", "  - \\setlength{\\tabcolsep}{5pt}",
    "  - \\AtBeginDocument{\\hypersetup{bookmarksopen=true,bookmarksnumbered=true}}", "  - \\raggedbottom", "  - \\pagestyle{plain}", "---", "")
  setup <- c("```{r setup, include=FALSE}",
    sprintf("knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE, fig.width=6.7, fig.height=4, dpi=%s, fig.align='center', out.width='100%%', fig.pos='H', dev='png')", format(options$figure_dpi, scientific = FALSE)), "```", "", "\\clearpage", "")
  packages <- c("DrugSigNet", "ggplot2", "rmarkdown", "knitr", "RobustRankAggreg", "reticulate", "plotly")
  versions <- vapply(packages, function(pkg) if (requireNamespace(pkg, quietly = TRUE)) as.character(utils::packageVersion(pkg)) else "Not installed", character(1))
  software <- data.frame(Component = c("R", packages, "LaTeX engine", "Pandoc"), Version = c(R.version.string, versions, engine, as.character(rmarkdown::pandoc_version())))
  footer <- if (options$include_reproducibility) c(.report_pdf_heading("Report Software", "section"), .report_pdf_table(software, c(0.35, 0.65))) else character()
  rmd <- paste0(base_name, ".Rmd")
  output <- paste0(base_name, ".pdf")
  writeLines(c(yaml, setup, unlist(lapply(models, `[[`, "lines"), use.names = FALSE), footer), rmd)
  rmarkdown::render(rmd, output_format = rmarkdown::pdf_document(latex_engine = engine, keep_tex = TRUE, toc = TRUE, toc_depth = 2, number_sections = TRUE), output_file = basename(output),
    envir = list2env(list(pdf_models = models, prepare_figure = function(item, file, height) .report_prepare_figure(item, file, dpi = options$figure_dpi, height = height)), parent = asNamespace("DrugSigNet")), quiet = TRUE)
  invisible(normalizePath(output, winslash = "/", mustWork = FALSE))
}
