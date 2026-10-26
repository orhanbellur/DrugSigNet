html_table_payload <- function(html) {
  value <- strsplit(html, '<script type="application/json" class="ds-table-data">', fixed = TRUE)[[1L]][2L]
  value <- strsplit(value, '</script>', fixed = TRUE)[[1L]][1L]
  jsonlite::fromJSON(value, simplifyVector = FALSE)
}

test_that("HTML tables retain all rows and original values beyond the old preview limit", {
  df <- data.frame(Drug = paste0("compound_", 1:260), Rank = 260:1)
  df$Drug[260] <- "last-row-260"
  before <- serialize(df, NULL)
  html <- DrugSigNet:::.report_html_table(df, "test-table", "Complete ranking")
  data <- html_table_payload(html)
  expect_length(data$rows, 260)
  expect_identical(data$columns, list("Drug", "Rank"))
  expect_identical(data$rows[[260]][[1]], "last-row-260")
  expect_equal(data$rows[[260]][[2]], 1)
  expect_identical(serialize(df, NULL), before)
  expect_match(html, "Download CSV", fixed = TRUE)
  expect_match(html, 'aria-live="polite"', fixed = TRUE)
})

test_that("HTML data and metadata are escaped without corrupting values", {
  payload <- '</script><img src=x onerror="window.bad=true"> `r stop("unsafe")`'
  df <- data.frame(Drug = payload, Value = NA_real_)
  html <- DrugSigNet:::.report_html_table(df, "safe-table", payload)
  expect_false(grepl('<img src=x', html, fixed = TRUE))
  expect_false(grepl('`r stop', html, fixed = TRUE))
  expect_identical(html_table_payload(html)$rows[[1]][[1]], payload)
  expect_null(html_table_payload(html)$rows[[1]][[2]])
  expect_match(DrugSigNet:::.report_html_escape('<b>"&`'), '&lt;b&gt;&quot;&amp;&#96;', fixed = TRUE)
})

test_that("HTML tables handle matrices and list cells without truncating their contents", {
  df <- data.frame(Drug = c("a", "b"))
  df$Matrix <- I(matrix(1:4, nrow = 2))
  df$Targets <- list(c("X", "Y"), "Z")
  data <- html_table_payload(DrugSigNet:::.report_html_table(df, "complex", "Complex cells"))
  expect_identical(data$rows[[1]][[2]], "1 | 3")
  expect_identical(data$rows[[1]][[3]], "X | Y")
  single <- html_table_payload(DrugSigNet:::.report_html_table(data.frame(Drug = "only"), "single", "Single column"))
  expect_identical(single$columns, list("Drug"))
  expect_identical(single$rows[[1]], list("only"))
})

test_that("HTML method summaries retain dimensions without exposing raw input vectors", {
  raw <- NetworkBased(result = data.frame(Drug = "a"), method = "TrustRank",
    ppi_network = data.frame(a = rep("NETWORK_SENTINEL", 5000), b = "g"))
  inv <- DrugSigNet:::.report_html_inventory(list(DrugSearching = list(Raw = list(TrustRank = raw))))
  expect_match(paste(inv$inputs$Dimensions, collapse = " "), "5000 rows x 2 columns", fixed = TRUE)
  expect_false(any(grepl("NETWORK_SENTINEL", unlist(inv), fixed = TRUE)))
})

test_that("HTML rendering adds distinct scientific sections without Shiny or DT", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available())
  obj <- list(type = "network", DrugSearching = list(Raw = list(), Processed = list()),
    RankAggregation = list(Network_Harmonized = data.frame(Drug = c("z", "a"), Network_CRank = c(42, 42), Network_Dowdall = c(2, 1), Network_RRA = c(1, 2))))
  tmp <- tempfile("scientific-html-"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  report <- write_report(obj, device = "html", top_n = 2, file = file.path(tmp, "report"), title = 'Study "A" & B')
  content <- paste(readLines(report, warn = FALSE), collapse = "\n")
  for (section in c("Executive Summary", "Analysis Overview", "Top-Ranked Candidates", "Method Comparison", "Drug Annotation", "Functional Enrichment", "Interpretation and Limitations", "Methods and Provenance", "Complete Results", "Report Software")) expect_match(content, section, fixed = TRUE)
  expect_match(content, "Study &quot;A&quot; &amp; B", fixed = TRUE)
  expect_false(grepl("Session Info", content, fixed = TRUE))
  expect_match(content, "ds-table-data", fixed = TRUE)
})

test_that("native Plotly widgets retain their interactive object", {
  p <- structure(list(x = list(data = list())), class = "plotly")
  expect_identical(DrugSigNet:::.report_html_plot(p, tempfile(), interactive = FALSE, dpi = 100), p)
  expect_identical(DrugSigNet:::.report_html_plot(p, tempfile(), interactive = TRUE, dpi = 100), p)
})
