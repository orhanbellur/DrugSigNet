test_that("report input expansion preserves standalone pipeline sections", {
  result <- DrugSearchingPipeline(
    RankAggregation = list(Signature_Harmonized = data.frame(Drug = "drug_a")),
    DrugAnnotation = list(Features = data.frame(Drug = "drug_a")),
    Visualization = list(plots = list(example = "plot")),
    type = "signature"
  )

  expanded <- DrugSigNet:::.report_expand_pipeline_result(result)

  expect_length(expanded, 1)
  expect_identical(expanded[[1]]$DrugAnnotation$Features$Drug, "drug_a")
  expect_identical(expanded[[1]]$Visualization$plots$example, "plot")
})

test_that("report input expansion preserves every repurposing section", {
  sections <- c("signature", "network", "integrated")
  result <- DrugSearchingPipeline(
    DrugSearching = list(
      Raw = stats::setNames(lapply(sections, function(x) list()), sections),
      Processed = stats::setNames(lapply(sections, function(x) list()), sections)
    ),
    RankAggregation = stats::setNames(lapply(sections, function(section) {
      list(Harmonized = data.frame(Drug = paste0(section, "_drug")))
    }), sections),
    DrugAnnotation = stats::setNames(lapply(sections, function(section) {
      list(Features = data.frame(Drug = paste0(section, "_drug")))
    }), sections),
    Visualization = stats::setNames(lapply(sections, function(section) {
      list(plots = stats::setNames(list("plot"), section))
    }), sections),
    type = "both"
  )

  expanded <- DrugSigNet:::.report_expand_pipeline_result(result)

  expect_length(expanded, 3)
  expect_identical(vapply(expanded, `[[`, character(1), "type"), c(
    "signature", "network", "integration"
  ))
  expect_identical(
    vapply(expanded, DrugSigNet:::.report_result_title, character(1)),
    c("Signature-based results", "Network-based results", "Integrated results")
  )
  expect_identical(
    vapply(expanded, function(x) x$DrugAnnotation$Features$Drug, character(1)),
    paste0(sections, "_drug")
  )
  expect_true(all(vapply(
    expanded,
    DrugSigNet:::.report_has_visualization,
    logical(1)
  )))
})

test_that("scientific report model summarizes partial and integrated outputs", {
  make_result <- function(type, prefix) list(
    DrugSearching = list(Raw = list(), Processed = list()),
    RankAggregation = stats::setNames(list(data.frame(
      Drug = c("shared", paste0(prefix, "_drug")),
      CRank = c(2, 1), Dowdall_rank = c(1, 2), RRA_rank = c(2, 1)
    )), switch(type, signature = "Signature_Harmonized", network = "Network_Harmonized", "Signature_Network_Harmonized")),
    type = type
  )
  objects <- list(make_result("signature", "sig"), make_result("network", "net"), make_result("integration", "int"))
  model <- DrugSigNet:::.report_pdf_model(objects, top_n = 10)
  expect_identical(unname(model$counts), c(2L, 2L, 2L))
  expect_identical(model$overlap, 1L)
  expect_named(model$top[[1]], c("Drug", "CRank", "Dowdall_rank", "RRA_rank"))
  expect_identical(model$top[[1]]$Drug[[1]], "sig_drug")
})

test_that("scientific report helpers omit unavailable components gracefully", {
  partial <- list(DrugSearching = list(), RankAggregation = list(), type = "network")
  model <- DrugSigNet:::.report_pdf_model(list(partial), top_n = 20)
  expect_identical(model$counts, c(network = 0L))
  expect_null(model$top[[1]])
  expect_false(DrugSigNet:::.report_has_visualization(partial))
  expect_false(DrugSigNet:::.report_has_annotation(partial))
})

test_that("scientific PDF template includes publication report features", {
  yaml <- paste(DrugSigNet:::.report_pdf_template("Study", "Analyst"), collapse = "\n")
  expect_match(yaml, "toc: true", fixed = TRUE)
  expect_match(yaml, "number_sections: true", fixed = TRUE)
  expect_match(yaml, "fig_caption: true", fixed = TRUE)
  expect_match(yaml, "fancyhdr", fixed = TRUE)
})

test_that("scientific PDF renders when LaTeX is available", {
  skip_if_not_installed("rmarkdown")
  skip_if(!any(nzchar(Sys.which(c("pdflatex", "xelatex", "lualatex")))))
  result <- DrugSearchingPipeline(
    RankAggregation = list(Signature_Harmonized = data.frame(
      Drug = c("drug_a", "drug_b"), Signature_CRank = 1:2,
      Signature_Dowdall = 2:1, Signature_RRA = 1:2
    )), type = "signature"
  )
  target <- tempfile("drugsignet-report-")
  pdf <- write_report(result, file = target, device = "pdf", include_appendix = FALSE)
  expect_true(file.exists(pdf))
  expect_gt(file.info(pdf)$size, 0)
})

report_fixture <- function(type = "network") {
  prefix <- switch(type, signature = "Signature", network = "Network", "Signature_Network")
  ranks <- data.frame(
    Drug = c("zeta", "beta", "alpha", "delta", "unranked"),
    CRank = c(42, 42, 42, 3, NA_real_),
    Dowdall = c(2, 1, 1, 4, NA_real_), RRA = c(1, 2, 1, 3, NA_real_)
  )
  names(ranks)[-1] <- paste0(prefix, c("_CRank", "_Dowdall", "_RRA"))
  list(type = type, DrugSearching = list(Raw = list(), Processed = list()),
       RankAggregation = stats::setNames(list(ranks), paste0(prefix, "_Harmonized")))
}

test_that("PDF selection sorts stored ranks with deterministic ties and no mutation", {
  for (type in c("network", "signature", "integration")) {
    obj <- report_fixture(type)
    before <- serialize(obj, NULL)
    model <- DrugSigNet:::.report_rank_model(obj, 3L)
    expect_identical(model$top$Drug, c("delta", "alpha", "beta"))
    expect_identical(model$top$CRank, c(3, 42, 42))
    expect_equal(model$excluded, 1L)
    expect_identical(serialize(obj, NULL), before)
    shuffled <- obj
    shuffled$RankAggregation[[1]] <- obj$RankAggregation[[1]][c(5, 3, 1, 4, 2), ]
    expect_identical(DrugSigNet:::.report_rank_model(shuffled, 3L)$top, model$top)
    body <- DrugSigNet:::.report_pdf_body(obj, 1, 3, 12, FALSE)$lines
    expect_true(any(grepl("2 of 3 tied candidates", body, fixed = TRUE)))
  }
})

test_that("invalid ranks sort last and conflicting identifiers are diagnosed", {
  obj <- report_fixture()
  obj$RankAggregation[[1]]$Network_CRank <- c(NA, 0, -1, 3, Inf)
  m <- DrugSigNet:::.report_rank_model(obj, 10)
  expect_identical(m$top$Drug, c("delta", "alpha", "beta", "zeta"))
  obj$RankAggregation[[1]]$Drug[2] <- "zeta"
  expect_error(DrugSigNet:::.report_rank_model(obj), "Conflicting duplicate")
})

test_that("large inputs and nested parameters are dimension summaries, never dumps", {
  obj <- report_fixture()
  network <- data.frame(source = rep("RAW_NETWORK_SENTINEL", 10000), target = "g")
  raw <- NetworkBased(result = data.frame(Drug = "alpha"), ppi_network = network,
                      drug_target_network = network, disease_genes = data.frame(g = letters), method = "TrustRank")
  raw@parameters$nested <- list(deep = list(secret_data = network))
  raw@parameters$gene_vector <- rep("RAW_GENE_SENTINEL", 10000)
  obj$DrugSearching$Raw$TrustRank <- raw
  body <- paste(DrugSigNet:::.report_pdf_body(obj, 1, 3, 12, FALSE)$lines, collapse = "\n")
  expect_match(body, "10000 rows x 2 columns", fixed = TRUE)
  expect_match(body, "character vector: 10000 elements", fixed = TRUE)
  expect_false(grepl("RAW_NETWORK_SENTINEL|RAW_GENE_SENTINEL|secret_data", body))
  expect_lt(nchar(body), 17000)
})

test_that("prioritized annotations omit unrelated drugs and exhaustive metadata", {
  obj <- report_fixture()
  expect_equal(nrow(DrugSigNet:::.report_annotations(obj, "alpha")), 0)
  obj$DrugAnnotation <- list(Features = data.frame(
    Drug = c("ALPHA", "other", "ALPHA"), Highest_status = c("Research", "Other", "Research"),
    mechanismOfAction = c("Mechanism A", "Unrelated", "Mechanism B"),
    SMILES = "SMILES_SENTINEL", Synonyms = "SYNONYM_SENTINEL"
  ))
  ann <- DrugSigNet:::.report_annotations(obj, c("alpha", "missing"))
  expect_identical(names(ann), c("Drug", "Highest_status", "mechanismOfAction"))
  expect_identical(ann$Drug, c("alpha", "missing"))
  expect_identical(ann$mechanismOfAction, c("Mechanism A; Mechanism B", "Not available"))
  expect_false(any(grepl("Unrelated|SENTINEL", unlist(ann))))
})

test_that("enrichment selects adjusted p-values globally and preserves overlap", {
  obj <- report_fixture()
  obj$DrugAnnotation <- list(Functional_Enrichment = list(
    BP = data.frame(Term = paste0("BP", 1:20), Adjusted.P.value = seq_len(20)/100, Overlap = "2/80"),
    MF = data.frame(Term = c("MF1", "bad", "NA"), Adjusted.P.value = c(0.005, -1, NA), Overlap = "3/60")
  ))
  en <- DrugSigNet:::.report_enrichment(obj, 12)
  expect_equal(en$total, 21L)
  expect_equal(nrow(en$top), 12L)
  expect_identical(en$top$Term[1], "MF1")
  expect_identical(en$top$Overlap[1], "3/60")
  expect_true(all(diff(en$top[["Adjusted p"]]) >= 0))
  obj$DrugAnnotation$Functional_Enrichment <- data.frame(Term = "no adjustment", P.value = 0.001)
  expect_equal(DrugSigNet:::.report_enrichment(obj)$total, 0)
})

test_that("PDF table escapes special characters and wraps long identifiers", {
  escaped <- DrugSigNet:::.report_tex("drug_%&#{test}\\id")
  expect_match(escaped, "\\_", fixed = TRUE)
  expect_match(escaped, "\\%", fixed = TRUE)
  expect_match(escaped, "\\textbackslash{}", fixed = TRUE)
  long <- paste(rep("identifier", 40), collapse = "")
  latex <- paste(DrugSigNet:::.report_pdf_table(data.frame(Drug = long, CRank = 123456)), collapse = "\n")
  expect_match(latex, "\\allowbreak{}", fixed = TRUE)
  expect_match(latex, "\\endfirsthead", fixed = TRUE)
  expect_match(latex, "\\endhead", fixed = TRUE)
  expect_match(latex, "123456", fixed = TRUE)
  expect_false(grepl("scale_down", latex, fixed = TRUE))
})

test_that("ranking diagnostics handle missing or constant methods", {
  x <- data.frame(CRank = c(42,42,42), Dowdall = 1:3, RRA = c(NA,NA,NA))
  agreement <- DrugSigNet:::.report_agreement(x)
  expect_true(all(is.na(agreement$Spearman)))
  expect_identical(agreement$N, c(3L, 0L, 0L))
  expect_false(DrugSigNet:::.report_static_plot(list(unavailable = TRUE), tempfile()))
})

test_that("PDF renders long tables, large parameters and optional complete exports", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available())
  skip_if_not(any(nzchar(Sys.which(c("xelatex", "lualatex", "pdflatex")))))
  obj <- report_fixture()
  obj$RankAggregation[[1]] <- data.frame(
    Drug = c(paste(rep("Long_ID_%&", 12), collapse = ""), paste0("compound", 2:45)),
    Network_CRank = rep(c(3,42,45), c(3,39,3)),
    Network_Dowdall = seq_len(45), Network_RRA = rev(seq_len(45))
  )
  obj$DrugSearching$Raw$TrustRank <- NetworkBased(
    result = data.frame(Drug = "compound2"), method = "TrustRank",
    ppi_network = data.frame(a = rep("NO_RAW_DUMP", 10000), b = "gene")
  )
  tmp <- tempfile("pdf-regression-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  path <- file.path(tmp, "report")
  before <- serialize(obj, NULL)
  output <- write_report(obj, title = 'Scientific report: "quoted" & validated',
                         file = path, device = "pdf", top_n = 40L, figure_dpi = 300, supplementary = TRUE)
  expect_true(file.exists(output))
  expect_gt(file.info(output)$size, 10000)
  expect_identical(readRDS(paste0(path, "_supplementary.rds")), obj)
  expect_identical(serialize(obj, NULL), before)
  tex <- paste(readLines(paste0(path, ".tex")), collapse = "\n")
  expect_false(grepl("NO_RAW_DUMP|Session Info|SMILES|tab:analyses", tex))
  expect_match(paste(readLines(paste0(path, ".Rmd")), collapse = "\n"), "dpi=300", fixed = TRUE)
  expect_match(tex, "\\pagestyle{plain}", fixed = TRUE)
  expect_match(tex, "\\endhead", fixed = TRUE)
  expect_match(tex, "Executive Summary", fixed = TRUE)
  expect_match(tex, "Interpretation and Limitations", fixed = TRUE)
  # Compile the retained TeX once more so layout regressions have an observable
  # log (rmarkdown normally cleans successful compilation logs).
  previous_dir <- setwd(tmp)
  compilation <- tryCatch(system2(DrugSigNet:::.report_latex_engine(),
    c("-interaction=nonstopmode", "-halt-on-error", "report.tex"),
    stdout = TRUE, stderr = TRUE), finally = setwd(previous_dir))
  expect_null(attr(compilation, "status"))
  expect_false(any(grepl("Overfull|multiply[ -]defined", compilation)),
               info = paste(compilation[grepl("Overfull|multiply[ -]defined", compilation)], collapse = "\n"))
})

test_that("new report options validate without invoking a renderer", {
  expect_error(write_report(report_fixture(), top_n = NA_real_), "positive integer")
  expect_error(write_report(report_fixture(), enrichment_n = 1.5), "positive integer")
  expect_error(write_report(report_fixture(), supplementary = NA), "TRUE or FALSE")
})

test_that("legacy harmonized aggregation objects supply final ranks only", {
  obj <- report_fixture()
  obj$RankAggregation <- list(Harmonized = list(
    CRank = RankAggregation(result = data.frame(Drug = c("b", "a"), CRank = c(42, 42))),
    Dowdall = RankAggregation(result = data.frame(Drug = c("a", "b"), input_score = c(9, 8), Dowdall_score = c(0.9, 0.8), Dowdall_rank = c(1, 2))),
    RRA = RankAggregation(result = data.frame(Drug = c("a", "b"), RRA_pval = c(0.001, 0.01), RRA_rank = c(2, 1)))
  ))
  m <- DrugSigNet:::.report_rank_model(obj)
  expect_identical(m$top$Drug, c("a", "b"))
  expect_identical(m$top$Dowdall, c(1,2))
  expect_identical(names(m$top), c("Drug", "CRank", "Dowdall", "RRA"))
})

test_that("failed Plotly static exports do not abort a PDF", {
  testthat::local_mocked_bindings(.report_plotly_export = function(...) stop("export unavailable"), .package = "DrugSigNet")
  expect_false(DrugSigNet:::.report_static_plot(structure(list(), class = "plotly"), tempfile(fileext = ".png")))
})

test_that("existing HTML and Shiny report paths retain detailed views", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("shiny")
  skip_if_not_installed("htmltools")
  skip_if_not(rmarkdown::pandoc_available())
  obj <- report_fixture()
  obj$DrugSearching$Raw$example <- data.frame(Drug = "HTML_RAW_RESULT")
  tmp <- tempfile("html-report-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  html <- write_report(obj, file = file.path(tmp, "report"))
  expect_true(file.exists(html))
  content <- paste(readLines(html, warn = FALSE), collapse = "\n")
  expect_match(content, "HTML_RAW_RESULT", fixed = TRUE)
  expect_match(content, "Network_Harmonized", fixed = TRUE)
})

test_that("Shiny app generation retains existing data and code without optional runtime packages", {
  obj <- report_fixture()
  tmp <- tempfile("shiny-report-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  app <- write_report(obj, file = file.path(tmp, "shiny"), dashboard = "shinydashboard", supplementary = TRUE)
  expect_true(file.exists(file.path(app, "report_objects.rds")))
  expect_identical(readRDS(file.path(tmp, "shiny_supplementary.rds")), obj)
  expect_silent(parse(file.path(app, "app.R")))
})


test_that("abbreviated long identifiers remain unique without changing source IDs", {
  ids <- c(paste0(strrep("a", 100), "x", strrep("b", 100)), paste0(strrep("a", 100), "y", strrep("b", 100)))
  labels <- DrugSigNet:::.report_display_ids(ids, ids)
  expect_equal(length(unique(labels)), 2)
  expect_true(all(nchar(labels) < 80))
  expect_match(labels[1], "[C01]", fixed = TRUE)
  expect_match(labels[2], "[C02]", fixed = TRUE)
})

test_that("combined analyses retain provenance while adapting section content", {
  obj <- list(type = "both", DrugSearching = list(),
    RankAggregation = list(signature = report_fixture("signature")$RankAggregation,
                           network = report_fixture("network")$RankAggregation,
                           integrated = report_fixture("integration")$RankAggregation),
    provenance = list(database_version = "fixture-v1"))
  parts <- DrugSigNet:::.report_expand_pipeline_result(obj)
  expect_identical(vapply(parts, function(x) x$provenance$database_version, character(1)), rep("fixture-v1", 3))
  bodies <- lapply(seq_along(parts), function(i) DrugSigNet:::.report_pdf_body(parts[[i]], i, 3, 12, FALSE)$lines)
  expect_true(any(grepl("Network-based prioritization", bodies[[2]], fixed = TRUE)))
  expect_true(any(grepl("Signature-based prioritization", bodies[[1]], fixed = TRUE)))
  expect_true(any(grepl("Integrated prioritization", bodies[[3]], fixed = TRUE)))
  expect_equal(sum(grepl("Optional Supplementary Material", unlist(bodies), fixed = TRUE)), 1)
})

test_that("empty and unnamed provenance metadata remain renderable", {
  obj <- report_fixture()
  obj$metadata <- list()
  obj$provenance <- list("source A", "source B")
  expect_silent(DrugSigNet:::.report_pdf_body(obj, 1, 3, 12, FALSE))
})

test_that("explicit layout controls and figure DPI always use the concise renderer", {
  received <- NULL
  testthat::local_mocked_bindings(
    .report_render_scientific_pdf = function(...) stop("Legacy renderer must not run"),
    .write_pdf_report = function(..., options) { received <<- options; "concise-pdf" },
    .package = "DrugSigNet"
  )
  obj <- report_fixture()
  expect_identical(write_report(obj, device = "pdf", top_n = 20, figure_dpi = 300), "concise-pdf")
  expect_identical(received$figure_dpi, 300)
  expect_identical(write_report(obj, device = "pdf", include_methods = FALSE,
    include_reproducibility = FALSE, table_orientation = "landscape", table_rows = 5), "concise-pdf")
  expect_false(received$include_methods)
  expect_false(received$include_reproducibility)
  expect_identical(received$table_orientation, "landscape")
  expect_equal(received$table_rows, 5)
})

test_that("section controls remove only their own scientific content", {
  opts <- list(include_summary = FALSE, include_methods = FALSE,
    include_interpretation = FALSE, include_reproducibility = FALSE,
    include_appendix = FALSE)
  text <- paste(DrugSigNet:::.report_pdf_body(report_fixture(), 1, 20, 12, FALSE, opts)$lines, collapse = "\n")
  expect_false(grepl("Executive Summary|Methods and Reproducibility|Interpretation and Limitations|Analysis Provenance|Optional Supplementary Material", text))
  expect_match(text, "Analysis Overview", fixed = TRUE)
  expect_match(text, "Top-Ranked Drug Candidates", fixed = TRUE)
  expect_match(text, "Method Comparison and Visualizations", fixed = TRUE)
})

test_that("DPI applies to stored static figures", {
  skip_if_not_installed("ggplot2")
  path <- tempfile(fileext = ".png")
  on.exit(unlink(path), add = TRUE)
  plot <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x = x, y = y)) + ggplot2::geom_point()
  expect_true(DrugSigNet:::.report_static_plot(plot, path, dpi = 100))
  # The PNG IHDR stores big-endian width and height immediately after its tag.
  header <- readBin(path, "raw", n = 24)
  decode <- function(bytes) sum(as.integer(bytes) * 256^(3:0))
  expect_equal(decode(header[17:20]), 700)
  # Raster devices may truncate a fractional pixel from 4.6 inches.
  expect_lte(abs(decode(header[21:24]) - 460), 1)
})
