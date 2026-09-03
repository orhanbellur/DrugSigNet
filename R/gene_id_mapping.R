# Internal helper: convert gene IDs with AnnotationDbi/org.Hs.eg.db
.map_gene_ids_orgdb <- function(ids, from, to = "SYMBOL") {
  if (!requireNamespace("AnnotationDbi", quietly = TRUE)) {
    stop(
      "Package 'AnnotationDbi' is required for gene ID conversion. Install it with: ",
      "BiocManager::install('AnnotationDbi')",
      call. = FALSE
    )
  }
  if (!requireNamespace("org.Hs.eg.db", quietly = TRUE)) {
    stop(
      "Package 'org.Hs.eg.db' is required for human gene ID conversion. Install it with: ",
      "BiocManager::install('org.Hs.eg.db')",
      call. = FALSE
    )
  }

  if (missing(ids) || length(ids) == 0) {
    stop("`ids` must be a non-empty character vector.", call. = FALSE)
  }
  if (missing(from) || length(from) != 1 || is.na(from) || !nzchar(trimws(from))) {
    stop("`from` must be a single non-empty AnnotationDbi keytype or supported alias.", call. = FALSE)
  }
  if (missing(to) || length(to) == 0) {
    stop("`to` must contain at least one AnnotationDbi column or supported alias.", call. = FALSE)
  }

  ids <- unique(trimws(as.character(ids)))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  if (length(ids) == 0) {
    stop("`ids` contains no valid non-missing identifiers.", call. = FALSE)
  }

  keytype <- .normalize_orgdb_keytype(from)
  columns <- unique(vapply(to, .normalize_orgdb_keytype, character(1)))
  if (identical(keytype, "ENSEMBL")) {
    ids <- sub("\\.[0-9]+$", "", ids)
  }

  AnnotationDbi::select(
    org.Hs.eg.db::org.Hs.eg.db,
    keys = ids,
    keytype = keytype,
    columns = columns
  )
}

.normalize_orgdb_keytype <- function(x) {
  x <- trimws(as.character(x))
  if (length(x) != 1 || is.na(x) || !nzchar(x)) {
    stop("AnnotationDbi keytypes/columns must be non-empty character scalars.", call. = FALSE)
  }

  key <- toupper(x)
  key <- gsub("[.-]", "_", key)

  switch(
    key,
    ENSEMBL_GENE_ID = "ENSEMBL",
    ENSEMBL = "ENSEMBL",
    HGNC_SYMBOL = "SYMBOL",
    GENE_SYMBOL = "SYMBOL",
    SYMBOL = "SYMBOL",
    ENTREZGENE_ID = "ENTREZID",
    ENTREZGENE = "ENTREZID",
    ENTREZ_ID = "ENTREZID",
    ENTREZ = "ENTREZID",
    ENTREZID = "ENTREZID",
    UNIPROTSWISSPROT = "UNIPROT",
    UNIPROT_SWISSPROT = "UNIPROT",
    UNIPROT = "UNIPROT",
    ENSEMBL_PROTEIN_ID = "ENSEMBLPROT",
    ENSEMBLPROT = "ENSEMBLPROT",
    REFSEQ_MRNA = "REFSEQ",
    REFSEQ = "REFSEQ",
    key
  )
}


.looks_like_ensembl_gene_ids <- function(ids) {
  ids <- unique(trimws(as.character(ids)))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  if (length(ids) == 0) {
    return(FALSE)
  }
  mean(grepl("^ENSG[0-9]+(\\.[0-9]+)?$", ids)) >= 0.8
}


############################################################
# Internal helper: map Entrez IDs to HGNC gene symbols
############################################################

.map_entrez_to_gene_symbols <- function(entrez_ids) {

  if (missing(entrez_ids))
    stop("`entrez_ids` must be provided.", call. = FALSE)

  entrez_ids <- trimws(as.character(entrez_ids))

  if (!length(entrez_ids))
    return(character(0))

  valid_ids <- entrez_ids[!is.na(entrez_ids) & nzchar(entrez_ids)]

  if (!length(valid_ids))
    return(entrez_ids)

  mapping <- .map_gene_ids_orgdb(
    ids = valid_ids,
    from = "ENTREZID",
    to = "SYMBOL"
  )

  required_cols <- c("ENTREZID", "SYMBOL")
  if (!all(required_cols %in% colnames(mapping))) {
    warning(
      "AnnotationDbi mapping did not return ENTREZID and SYMBOL columns; returning original IDs.",
      call. = FALSE
    )
    return(entrez_ids)
  }

  mapping$ENTREZID <- trimws(as.character(mapping$ENTREZID))
  mapping$SYMBOL <- trimws(as.character(mapping$SYMBOL))

  keep <- !is.na(mapping$ENTREZID) & nzchar(mapping$ENTREZID) &
    !is.na(mapping$SYMBOL) & nzchar(mapping$SYMBOL)
  mapping <- mapping[keep, required_cols, drop = FALSE]
  mapping <- mapping[!duplicated(mapping$ENTREZID), , drop = FALSE]

  idx <- match(entrez_ids, mapping$ENTREZID)

  gene_symbols <- entrez_ids
  has_symbol <- !is.na(idx)
  gene_symbols[has_symbol] <- mapping$SYMBOL[idx[has_symbol]]

  gene_symbols
}
