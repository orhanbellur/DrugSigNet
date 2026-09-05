# Run signatureSearch code with its package attached to the search path.
#
# Some signatureSearch result-annotation methods load their bundled data with
# `data()` without specifying `package = "signatureSearch"`.  Such lookups only
# find the data when signatureSearch is attached; merely importing its namespace
# (the normal state while DrugSigNet is running) is not sufficient.  Keep this
# compatibility workaround local to the operation and restore the caller's
# search path afterwards.
.with_signature_search_attached <- function(code) {
  was_attached <- "package:signatureSearch" %in% search()

  if (!was_attached) {
    suppressPackageStartupMessages(
      library("signatureSearch", character.only = TRUE)
    )
    on.exit(
      detach("package:signatureSearch", unload = FALSE, character.only = TRUE),
      add = TRUE
    )
  }

  force(code)
}
