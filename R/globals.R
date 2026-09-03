# Non-standard evaluation -------------------------------------------------
#
# DrugSigNet uses dplyr/ggplot2 data masks throughout its analysis and plotting
# code.  These names are columns created or validated immediately before use;
# declaring them here tells R's static checker about that deliberate NSE.  Real
# functions are namespace-qualified at their call sites, and package/external
# datasets are listed separately below rather than being mistaken for columns.

#' @importFrom rlang .data
NULL

utils::globalVariables(c(
  # Magrittr/tidy-evaluation syntax used inside data-masked expressions.
  ".", ":=",

  # Input and result-table columns.
  ".drug_key", ".intervention_norm", ".score_tmp",
  "Adjusted.P.value", "AlluviumID", "Axis",
  "Clinical_trial_conditions", "Clinical_trial_phase", "Conditions",
  "Dowdall_rank", "Drug", "Drug_confidence", "Drug_statue", "Ensembl",
  "Entrez", "FC", "Freq", "GeneRatio", "Gene_symbol", "Group", "Groups",
  "ID", "Method_x", "Method_y", "NCT Number", "NCT_Number", "Name",
  "Overlap_count", "Phases", "RRA_rank", "Rank", "Score", "Status",
  "Stratum", "Study", "Target", "Target_Group_ID", "Target_Set",
  "Target_confidence", "Term", "Var1", "Var2", "WTCS", "WTCS_Pval", "Z",
  "canonical_smiles", "category", "cell", "condition_key", "cor",
  "condition_status", "confidence", "cor_score", "direction",
  "disease_seed_gene", "drug_graph_id", "edge_class", "edge_key",
  "edge_role", "edge_type", "effect", "frac", "from", "g1_graph_id",
  "g2_graph_id", "gene", "gene1", "gene2", "gene_graph_id", "graphId",
  "highest_phase_num", "highest_status", "hits", "indication", "is_drug",
  "is_drug_target", "label", "label_full", "label_small", "logfc",
  "match_source", "match_text", "match_text_with_phase", "matched_drug",
  "method", "method_1", "method_2", "n_overlap", "name",
  "name_synonym_list", "nearest_target",
  "node", "node_size", "ont", "outline_width", "overlap_all",
  "overlap_label", "padj", "pert", "pert_iname", "pert_lc", "perturbation",
  "phase_num", "pval", "r", "r0", "rank_score", "rank_score_1",
  "rank_score_2", "rank_x", "rank_y", "raw_score", "regulation",
  "scaled_score", "sig_level", "similarity", "symbol", "target_status",
  "theta_end", "theta_start", "to", "top_k", "toxicity_class", "trend",
  "type", "warning_type", "x", "x0", "x_anchor", "x_elbow", "x_lab",
  "x_text", "y", "y0", "y_anchor", "y_lab", "y_text",

  # Object loaded explicitly from signatureSearch data at runtime.
  "lincs_pert_info2"
))
