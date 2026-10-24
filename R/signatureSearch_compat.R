# Run signatureSearch code with its package attached to the search path.
#
# Some signatureSearch result-annotation methods load their bundled data with
# `data()` without specifying `package = "signatureSearch"`.  Such lookups only
# find the data when signatureSearch is attached; merely importing its namespace
# (the normal state while DrugSigNet is running) is not sufficient.  Keep this
# compatibility workaround local to the operation and restore the caller's
# search path afterwards.
.drugsignet_require_signature_search <- function() {
  if (!requireNamespace("signatureSearch", quietly = TRUE)) {
    stop(
      "Package 'signatureSearch' is required only for signature-based methods. ",
      "Install it with BiocManager::install('signatureSearch'), then retry. ",
      "Network-based DrugSigNet methods do not require this package.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.with_signature_search_attached <- function(code) {
  .drugsignet_require_signature_search()
  was_attached <- "package:signatureSearch" %in% search()

  if (!was_attached) {
    suppressPackageStartupMessages(
      base::attachNamespace("signatureSearch")
    )
    on.exit(
      detach("package:signatureSearch", unload = FALSE, character.only = TRUE),
      add = TRUE
    )
  }

  force(code)
}
