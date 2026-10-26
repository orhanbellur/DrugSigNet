test_that("vignette browser validates names", {
  expect_error(
    browse_drugsignet_vignette("../getting-started", launch = FALSE),
    "basename"
  )
  expect_error(
    browse_drugsignet_vignette(NA_character_, launch = FALSE),
    "one non-missing"
  )
})

test_that("installed vignette can be copied to a session directory", {
  installed <- system.file("doc", "getting-started.html", package = "DrugSigNet")
  skip_if(!nzchar(installed) || !file.exists(installed), "built vignettes are unavailable")

  copied <- browse_drugsignet_vignette("getting-started", launch = FALSE)
  expect_true(file.exists(copied))
  expect_true(file.access(copied, 4L) == 0L)
  # Every installed HTML vignette must keep its local figures, scripts and
  # stylesheets when copied for RStudio Server viewing.
  pages <- list.files(dirname(copied), pattern = "[.]html$", full.names = TRUE)
  pages <- pages[basename(pages) != "index.html"]
  for (page in pages) {
    html <- paste(readLines(page, warn = FALSE), collapse = "\n")
    attrs <- regmatches(html, gregexpr('(src|href)="[^"]+"', html))[[1L]]
    urls <- sub('^[^=]+="(.*)"$', "\\1", attrs)
    local <- urls[!grepl("^(#|/|[A-Za-z][A-Za-z0-9+.-]*:)", urls)]
    assets <- local[grepl("[.](css|js|png|jpe?g|svg)([?#].*)?$", local)]
    assets <- sub("[?#].*$", "", assets)
    expect_true(all(file.exists(file.path(dirname(page), assets))), info = basename(page))
    expect_false(any(grepl("^(/workspace/|[A-Za-z]:/)", urls)), info = basename(page))
  }
})
