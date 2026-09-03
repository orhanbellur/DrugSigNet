#' Disease Gene-Expression Signature
#'
#' An example disease gene-expression signature for demonstrating
#' signature-based drug repurposing with DrugSigNet.
#'
#' The signature is derived from peripheral blood leukocyte transcriptomic
#' profiles of COVID-19 convalescent donors (CCD) compared with healthy donors
#' (HD), as reported by Gedda et al. (2022). Genes from the 120--149 day
#' post-symptom-onset comparison were retained when the false discovery rate
#' (FDR) was less than or equal to 0.05 and a log2 fold-change value was
#' available. Gene identifiers were mapped to NCBI Entrez Gene identifiers.
#'
#' @format A data frame with two columns:
#' \describe{
#'   \item{Entrez}{NCBI Entrez Gene identifier.}
#'   \item{FC}{Log2 fold-change for COVID-19 convalescent donors relative to
#'   healthy donors. Positive and negative values indicate higher and lower
#'   expression in convalescent donors, respectively.}
#' }
#'
#' @source
#' Gedda MR, Danaher P, Shao L, et al. (2022).
#' Longitudinal transcriptional analysis of peripheral blood leukocytes in
#' COVID-19 convalescent donors.
#' \emph{Journal of Translational Medicine}, 20, 587.
#' \doi{10.1186/s12967-022-03751-7}
#'
#' @examples
#' data(disease_signature)
#' head(disease_signature)
#'
"disease_signature"


#' Disease Seed Genes
#'
#' An example set of disease-associated seed genes for demonstrating
#' network-based drug repurposing with DrugSigNet.
#'
#' The seed genes are derived from the SARS-CoV-2 host protein targets used by
#' Gysi et al. (2021) to define the COVID-19 disease module for network-based
#' drug repurposing. The original set comprised 332 experimentally identified
#' human proteins targeted by SARS-CoV-2 proteins. Gene symbols were mapped to
#' Ensembl gene identifiers for use with the DrugSigNet network-based workflow.
#'
#' @format A data frame with one column:
#' \describe{
#'   \item{gene}{Ensembl gene identifier of a SARS-CoV-2 host protein target.}
#' }
#'
#' @source
#' Gysi DM, do Valle I, Zitnik M, et al. (2021).
#' Network medicine framework for identifying drug-repurposing opportunities
#' for COVID-19.
#' \emph{Proceedings of the National Academy of Sciences}, 118(19),
#' e2025581118.
#' \doi{10.1073/pnas.2025581118}
#'
#' @examples
#' data(disease_seeds)
#' head(disease_seeds)
#'
"disease_seeds"

#' Cell-Line Metadata
#'
#' Metadata for cell lines represented in the bundled connectivity-map
#' reference data.
#'
#' @format A data frame with one row per cell line and columns describing the
#'   cell-line identifier and biological attributes.
"cell_info"

#' Extended Cell-Line Metadata
#'
#' An extended cell-line metadata table used with LINCS signatures.
#'
#' @format A data frame with one row per cell line and columns describing the
#'   cell-line identifier and biological attributes.
#' @note This object is distributed in its original binary data file.
"cell_info2"

#' ChEMBL Mechanism-of-Action Annotations
#'
#' Drug and mechanism-of-action annotations derived from ChEMBL.
#'
#' @format A data frame containing drug identifiers and their mechanism or
#'   target annotations.
"chembl_moa_list"

#' CLUE Mechanism-of-Action Annotations
#'
#' Drug and mechanism-of-action annotations used by the CLUE connectivity-map
#' resources.
#'
#' @format A data frame containing perturbagen names and mechanism-of-action
#'   annotations.
"clue_moa_list"

#' Example Drug Names
#'
#' Ten example perturbagen names for DrugSigNet demonstrations.
#'
#' @format A character vector of length 10.
"drugs10"

#' LINCS Expression-Instance Metadata
#'
#' Metadata for expression instances in the bundled LINCS reference data.
#'
#' @format A data frame with one row per expression instance and columns for
#'   signature, perturbagen, cell, dose, and treatment metadata where
#'   available.
"lincs_expr_inst_info"

#' LINCS Perturbagen Metadata
#'
#' Perturbagen annotations for the bundled LINCS reference data.
#'
#' @format A data frame with one row per perturbagen and identifier and
#'   annotation columns.
"lincs_pert_info"

#' Curated LINCS Perturbagen Metadata
#'
#' A curated subset of LINCS perturbagen annotations used to map perturbagen
#' identifiers to names.
#'
#' @format A data frame with one row per perturbagen and columns including
#'   perturbagen identifiers and names.
"lincs_pert_info2"

#' LINCS Signature Metadata
#'
#' Signature-level annotations for the bundled LINCS reference data.
#'
#' @format A data frame with one row per signature and columns describing its
#'   perturbagen and experimental context.
"lincs_sig_info"

#' Example Drug-Target Genes
#'
#' An example set of target genes for target-set enrichment analysis.
#'
#' @format A character vector of gene identifiers.
"targetList"
