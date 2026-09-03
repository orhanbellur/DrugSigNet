#' @title Extract Blood-Brain Barrier Permeability Information
#'
#' @description
#' Retrieves blood-brain barrier permeability annotations for one or more drugs
#' from curated DrugSigNet BBB annotation data.
#'
#' @details
#' `get_drug_bbb()` extracts blood-brain barrier permeability classifications
#' for the supplied drugs. The function accepts a character vector or a
#' one-column data frame and returns a `DrugAnnotation` object with BBB
#' information stored in `object@result`.
#'
#' BBB annotations are derived from the Brain-Blood Barrier Database (B3DB), a
#' curated molecular database compiled from experimentally measured
#' blood-brain barrier permeability data reported in published resources.
#' DrugSigNet processes these records into a drug-level annotation resource by
#' collapsing multiple records for the same drug and retaining BBB
#' classifications together with supporting metadata.
#'
#' Drug matching is performed by drug name and is case-insensitive through the
#' shared annotation-matching helpers. Annotation resources are loaded from the
#' local cache or Synapse using `force` and `auth_token`.
#'
#' The returned annotation may include `BBB_class` together with available
#' supporting metadata such as compound identifiers, PubChem CID, SMILES,
#' logBB values, decision thresholds, reference identifiers, data group, and
#' comments.
#'
#' Multiple BBB annotations may be associated with a single drug; these are
#' collapsed with `|` by the shared annotation helpers.
#'
#' @inheritParams annotate_drugs
#'
#' @return
#' A `DrugAnnotation` object containing a data frame of BBB permeability
#' annotations in `object@result`.
#'
#' @examples
#' \dontrun{
#' res <- get_drug_bbb(
#'   drugs = c("Cetirizine", "Pentazocine"),
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#'
#' drug_df <- data.frame(
#'   drug = c("Cetirizine", "Pentazocine")
#' )
#'
#' res_df <- get_drug_bbb(
#'   drugs = drug_df,
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#'
#' res_pipe <- get_drug_bbb(
#'   object = drug_df,
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#' }
#'
#' @references
#' Meng F, Xi Y, Huang J, Ayers PW.
#' A curated diverse molecular database of blood-brain barrier permeability
#' with chemical descriptors.
#' \emph{Scientific Data}. 2021;8:310.
#' \doi{10.1038/s41597-021-01069-5}
#'
#' @importFrom dplyr distinct
#' @importFrom tibble tibble
#' @export
setGeneric(
  "get_drug_bbb",
  function(object = NULL,
           drugs = NULL,
           force = FALSE,
           auth_token = NULL) {

    if (is.null(drugs)) {
      if (is.null(object)) {
        stop("`drugs` is missing. Supply a character vector or one-column data frame.")
      }
      if (methods::is(object, "DrugAnnotation")) {
        stop("`drugs` is missing. When supplying a DrugAnnotation object, also provide `drugs`.")
      }
      drugs <- object
      object <- NULL
    }

    if (is.character(drugs)) {
      input_vec <- drugs
    } else if (is.data.frame(drugs)) {
      if (ncol(drugs) != 1) {
        stop("`drugs` data frame must contain exactly one column.")
      }
      input_vec <- as.character(drugs[[1]])
    } else {
      stop("`drugs` must be a character vector or a one-column data frame.")
    }

    input_vec <- unique(trimws(stats::na.omit(input_vec)))
    input_vec <- input_vec[nzchar(input_vec)]
    if (length(input_vec) == 0) {
      stop("No valid drug names supplied.")
    }

    if (is.null(object)) {
      object <- new(
        "DrugAnnotation",
        result = data.frame(),
        parameters = list(
          input_data = input_vec,
          force = force,
          auth_token = auth_token
        )
      )
    }

    standardGeneric("get_drug_bbb")
  }
)

#' @rdname get_drug_bbb
setMethod(
  "get_drug_bbb",
  signature = "DrugAnnotation",
  function(object) {

    params <- object@parameters
    force <- isTRUE(params$force)
    auth_token <- params$auth_token

    input_tbl <- tibble::tibble(Drug = params$input_data)

    bbb_tbl <- load_annotation_section(
      "bbb",
      aliases = c("BBB", "B3DB", "B3DB_classification"),
      force = force,
      auth_token = auth_token
    )

    if (is.null(bbb_tbl)) {
      bbb_tbl <- load_drugsignet_data_compat("B3DB_classification", envir = environment(), force = force, auth_token = auth_token)
    }

    drug_col <- resolve_annotation_col(bbb_tbl, c("drug_name", "name", "Drug", "drug"), "drug")
    bbb_col <- resolve_annotation_col(bbb_tbl, c("BBB_class", "bbb_class", "classification"), "BBB_class")

    optional <- c(
      compound_ids = resolve_annotation_col(bbb_tbl, c("compound_ids"), "compound_ids", required = FALSE),
      pubchem_cid = resolve_annotation_col(bbb_tbl, c("pubchem_cid", "cid"), "pubchem_cid", required = FALSE),
      smiles = resolve_annotation_col(bbb_tbl, c("smiles", "canonical_smiles"), "smiles", required = FALSE),
      log_bb = resolve_annotation_col(bbb_tbl, c("log_bb", "logBB"), "log_bb", required = FALSE),
      log_bb_min = resolve_annotation_col(bbb_tbl, c("log_bb_min"), "log_bb_min", required = FALSE),
      log_bb_max = resolve_annotation_col(bbb_tbl, c("log_bb_max"), "log_bb_max", required = FALSE),
      decision_threshold = resolve_annotation_col(bbb_tbl, c("decision_threshold"), "decision_threshold", required = FALSE),
      reference_id = resolve_annotation_col(bbb_tbl, c("reference_id"), "reference_id", required = FALSE),
      data_group = resolve_annotation_col(bbb_tbl, c("data_group"), "data_group", required = FALSE),
      comment = resolve_annotation_col(bbb_tbl, c("comment"), "comment", required = FALSE)
    )
    selected <- c(Drug = drug_col, BBB_class = bbb_col, optional)
    selected <- selected[!is.na(selected)]

    ref_tbl <- bbb_tbl[, unname(selected), drop = FALSE]
    names(ref_tbl) <- names(selected)
    ref_tbl <- ref_tbl %>%
      dplyr::distinct() %>%
      collapse_annotations_by_drug()

    object@result <- left_join_annotations_by_drug(
      input_tbl,
      ref_tbl,
      empty_cols = setdiff(names(selected), "Drug")
    )

    object
  }
)
