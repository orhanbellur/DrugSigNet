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
  # Retain this internal entry point for callers from older code, but delegate
  # to the same bounded layout. No chunk may regenerate exhaustive raw tables.
  .write_pdf_report(objects, title, tools::file_path_sans_ext(output_file), author,
    top_n, enrichment_n = 12L, supplementary = FALSE,
    options = list(include_summary = include_summary, include_methods = include_methods,
      include_interpretation = include_interpretation,
      include_reproducibility = include_reproducibility, include_appendix = include_appendix,
      table_orientation = table_orientation, figure_dpi = figure_dpi, table_rows = table_rows))
}
