#' @title Plot Drug Signature Overlay
#'
#' @description
#' Visualizes a drug-centered subgraph with drug targets, disease seed genes,
#' and drug-signature genes overlaid.
#'
#' @details
#' `plot_drug_signature_overlay()` plots an extracted drug-centered PPI subgraph
#' returned by `extract_drug_subgraph()`. Drug-target edges are retained to show
#' the target context, while disease seed and drug-signature evidence are
#' highlighted on the graph.
#'
#' Disease seed genes can be supplied as a character vector or one-column data
#' frame. Drug-signature genes can be supplied as a character vector,
#' one-column data frame, or a result from `get_drug_signature()`.
#'
#' When direct target evidence is unavailable, the function can use one-step
#' disease seed neighbors or shortest paths between drug targets and
#' drug-signature genes.
#'
#' @inheritParams plot_top_k_overlap
#' @param Drug,refdb,res Optional workflow-style inputs. When `res` is supplied,
#'   `subgraph_result` is taken from `res[[Drug]][[refdb]]`.
#' @param disease_genes Optional disease-gene table used as
#'   `disease_seed_genes` when `res` is supplied.
#' @param gene_map Optional gene ID-to-symbol mapping table used when
#'   `symbol_map` is not supplied.
#' @param subgraph_result Result returned by `extract_drug_subgraph()`.
#' @param disease_seed_genes Disease seed genes as a character vector or
#'   one-column data frame.
#' @param drug_signature_genes Drug-signature genes as a character vector,
#'   one-column data frame, or result from `get_drug_signature()`.
#' @param drug_signature_use Component of a `get_drug_signature()` result to
#'   use. One of `"top"`, `"up"`, `"down"`, or `"signature"`.
#' @param symbol_map Optional two-column data frame with graph gene IDs and gene
#'   symbols.
#' @param max_signature_path_distance Maximum shortest-path distance from drug
#'   targets to disease or signature genes. Default is `Inf`.
#' @param include_all_drug_targets Logical; if `TRUE`, include all direct drug
#'   targets in the visualization. Default is `FALSE`.
#' @param node_alpha Node transparency. Default is `0.85`.
#' @param seed Random seed used for graph layout. Default is `1`.
#' @param layout Graph layout passed to `ggraph::create_layout()`. Default is
#'   `"fr"`.
#'
#' @return
#' A list containing the plot, graph, node table, edge table, disease seed
#' evidence, drug-signature evidence, shortest-path information, and evidence
#' mode summaries.
#'
#' @examples
#' \dontrun{
#' subg <- extract_drug_subgraph(
#'   disease_genes_df = disease_genes,
#'   drug = "miglitol",
#'   k_hops = 2
#' )
#'
#' drug_sig <- get_drug_signature(
#'   drug = "miglitol",
#'   refdb = "lincs2",
#'   n_up = 100,
#'   n_down = 100
#' )
#'
#' seed_df <- data.frame(
#'   gene = c("ENSG00000141510", "ENSG00000171862")
#' )
#'
#' overlay <- plot_drug_signature_overlay(
#'   subgraph_result = subg,
#'   disease_seed_genes = seed_df,
#'   drug_signature_genes = drug_sig,
#'   drug_signature_use = "top"
#' )
#'
#' overlay$plot
#' }
#'
#' @importFrom dplyr mutate filter distinct select bind_rows left_join transmute case_when group_by summarise first pull rename
#' @importFrom tibble tibble
#' @importFrom ggplot2 aes geom_point geom_text scale_fill_manual scale_shape_manual scale_size_manual scale_color_manual guides guide_legend theme_void theme element_text ggsave
#' @importFrom ggraph ggraph create_layout geom_edge_link geom_node_point geom_node_text scale_edge_color_manual
#' @importFrom igraph graph_from_data_frame V E as_data_frame delete_vertices distances shortest_paths induced_subgraph
#' @importFrom grid unit
#' @export
setGeneric(
  "plot_drug_signature_overlay",
  function(object = NULL,
           Drug = NULL,
           refdb = NULL,
           res = NULL,
           disease_genes = NULL,
           gene_map = NULL,
           subgraph_result = NULL,
           disease_seed_genes = NULL,
           drug_signature_genes = NULL,
           drug_signature_use = c("top", "up", "down", "signature"),
           symbol_map = NULL,
           max_signature_path_distance = Inf,
           include_all_drug_targets = FALSE,
           node_alpha = 0.85,
           seed = 1,
           layout = "fr",
           file_type = "pdf",
           file_name = NULL,
           width = 10,
           height = 10,
           units = "in") {
    if (!is.null(res)) {
      if (is.null(Drug) || is.null(refdb)) {
        stop("`Drug` and `refdb` must be provided when `res` is supplied.", call. = FALSE)
      }
      subgraph_result <- res[[Drug]][[refdb]]
      if (is.null(disease_seed_genes)) {
        disease_seed_genes <- disease_genes
      }
      if (is.null(symbol_map)) {
        symbol_map <- gene_map
      }
    }
    if (is.null(symbol_map) && !is.null(gene_map)) {
      symbol_map <- gene_map
    }

    if (!is.null(object) && !methods::is(object, "PlotObject")) {
      if (!is.null(subgraph_result)) {
        disease_seed_genes <- subgraph_result
      }
      subgraph_result <- object
      object <- NULL
    }

    if (is.null(object)) {
      if (is.null(subgraph_result)) {
        stop("`subgraph_result` must be provided unless `object` is a PlotObject.", call. = FALSE)
      }
      if (is.null(disease_seed_genes)) {
        stop("`disease_seed_genes` must be provided unless `object` is a PlotObject.", call. = FALSE)
      }
      auto_file_name <- is.null(file_name)
      if (auto_file_name) {
        file_name <- tempfile("plot_drug_signature_overlay_")
      }

      object <- PlotObject(
        input_data = data.frame(plot_type = "drug_signature_overlay"),
        file_type = file_type,
        file_name = file_name,
        width = width,
        height = height,
        units = units
      )
      object@parameters$subgraph_result <- subgraph_result
      object@parameters$Drug <- Drug
      object@parameters$refdb <- refdb
      object@parameters$disease_seed_genes <- disease_seed_genes
      object@parameters$drug_signature_genes <- drug_signature_genes
      object@parameters$drug_signature_use <- match.arg(drug_signature_use)
      object@parameters$symbol_map <- symbol_map
      object@parameters$max_signature_path_distance <- max_signature_path_distance
      object@parameters$include_all_drug_targets <- include_all_drug_targets
      object@parameters$node_alpha <- node_alpha
      object@parameters$seed <- seed
      object@parameters$layout <- layout
      object@parameters$auto_file_name <- auto_file_name
    }

    standardGeneric("plot_drug_signature_overlay")
  }
)

#' @rdname plot_drug_signature_overlay
#' @export
setMethod(
  "plot_drug_signature_overlay",
  signature = "PlotObject",
  function(object,
           Drug = NULL,
           refdb = NULL,
           res = NULL,
           disease_genes = NULL,
           gene_map = NULL,
           subgraph_result = NULL,
           disease_seed_genes = NULL,
           drug_signature_genes = NULL,
           drug_signature_use = c("top", "up", "down", "signature"),
           symbol_map = NULL,
           max_signature_path_distance = Inf,
           include_all_drug_targets = FALSE,
           node_alpha = 0.85,
           seed = 1,
           layout = "fr",
           file_type = "pdf",
           file_name = NULL,
           width = 10,
           height = 10,
           units = "in") {
    params <- object@parameters

    result <- .plot_drug_signature_overlay_impl(
      subgraph_result = .overlay_param(params, "subgraph_result", if (!is.null(res) && !is.null(Drug) && !is.null(refdb)) res[[Drug]][[refdb]] else subgraph_result),
      drug = .overlay_param(params, "Drug", Drug),
      disease_seed_genes = .overlay_param(params, "disease_seed_genes", if (!is.null(disease_genes)) disease_genes else disease_seed_genes),
      drug_signature = .overlay_param(params, "drug_signature_genes", drug_signature_genes),
      drug_signature_use = .overlay_param(params, "drug_signature_use", match.arg(drug_signature_use)),
      symbol_map = .overlay_param(
        params,
        "symbol_map",
        if (!is.null(symbol_map)) {
          symbol_map
        } else if (!is.null(gene_map)) {
          gene_map
        } else {
          NULL
        }
      ),
      max_signature_path_distance = .overlay_param(params, "max_signature_path_distance", max_signature_path_distance),
      include_all_drug_targets = .overlay_param(params, "include_all_drug_targets", include_all_drug_targets),
      node_alpha = .overlay_param(params, "node_alpha", node_alpha),
      seed = .overlay_param(params, "seed", seed),
      layout = .overlay_param(params, "layout", layout)
    )
    if (is.null(result)) {
      return(NULL)
    }
    .overlay_save_plot(result$plot, params)
    result
  }
)

.overlay_param <- function(params, name, default = NULL) {
  if (name %in% names(params)) {
    return(params[[name]])
  }
  default
}

.overlay_save_plot <- function(plot, params) {
  if (!isTRUE(params$auto_file_name) && !is.null(params$file_name) && nzchar(params$file_name)) {
    ggplot2::ggsave(
      filename = paste0(params$file_name, ".", params$file_type),
      plot = plot,
      width = params$width,
      height = params$height,
      units = params$units
    )
  }
  invisible(TRUE)
}

.plot_drug_signature_overlay_impl <- function(subgraph_result,
                                              drug = NULL,
                                              disease_seed_genes,
                                              drug_signature = NULL,
                                              drug_signature_use = c("top", "up", "down", "signature"),
                                              symbol_map = NULL,
                                              max_signature_path_distance = Inf,
                                              include_all_drug_targets = FALSE,
                                              node_alpha = 0.85,
                                              seed = 1,
                                              layout = "fr") {
  drug_signature_use <- match.arg(drug_signature_use)
  .overlay_require_packages()

  if (!is.list(subgraph_result) || is.null(subgraph_result$subgraph)) {
    stop("`subgraph_result` must be a list containing a `subgraph` edge data frame.", call. = FALSE)
  }
  if (!is.data.frame(subgraph_result$subgraph) || !nrow(subgraph_result$subgraph)) {
    return(NULL)
  }
  if (!is.numeric(max_signature_path_distance) || length(max_signature_path_distance) != 1 ||
      is.na(max_signature_path_distance) || max_signature_path_distance < 0) {
    stop("`max_signature_path_distance` must be a non-negative number.", call. = FALSE)
  }

  clean_id <- .overlay_clean_ids
  make_edge_key <- .overlay_edge_key
  normalize_edge_type <- function(x) {
    dplyr::case_when(
      x %in% c("DT", "DTI", "Drug-target", "drug_target") ~ "DT",
      grepl("Co-expression", x) ~ "Co-expression",
      grepl("Co-abundance", x) ~ "Co-abundance",
      grepl("PPI", x) ~ "PPI",
      TRUE ~ as.character(x)
    )
  }

  drug <- .overlay_resolve_drug(subgraph_result, requested_drug = drug)
  subgraph_edges <- subgraph_result$subgraph %>%
    dplyr::mutate(
      from = clean_id(from),
      to = clean_id(to),
      type = if ("type" %in% names(.)) normalize_edge_type(type) else "PPI",
      edge_key = make_edge_key(from, to)
    ) %>%
    dplyr::filter(!is.na(from), !is.na(to), nzchar(from), nzchar(to), from != to) %>%
    dplyr::distinct()

  if (!nrow(subgraph_edges)) {
    return(NULL)
  }

  g_full <- igraph::graph_from_data_frame(
    subgraph_edges %>% dplyr::select(from, to, type),
    directed = FALSE
  )

  if (is.null(drug) || !(drug %in% igraph::V(g_full)$name)) {
    dt_drugs <- unique(c(subgraph_edges$from[subgraph_edges$type == "DT"], subgraph_edges$to[subgraph_edges$type == "DT"]))
    drug <- dt_drugs[dt_drugs %in% igraph::V(g_full)$name][1]
  }
  if (is.na(drug) || is.null(drug) || !nzchar(drug)) {
    stop("Could not infer the drug node from `subgraph_result`.", call. = FALSE)
  }

  drug_targets <- unique(c(
    subgraph_edges$from[subgraph_edges$type == "DT"],
    subgraph_edges$to[subgraph_edges$type == "DT"]
  ))
  drug_targets <- setdiff(drug_targets, drug)
  drug_targets <- intersect(drug_targets, igraph::V(g_full)$name)

  graph_nodes <- igraph::V(g_full)$name
  symbol_df <- .overlay_prepare_symbol_map(symbol_map)
  if (is.null(symbol_map)) {
    symbol_df <- .overlay_add_orgdb_symbols(graph_nodes, symbol_df)
  }
  get_symbol <- function(x) .overlay_get_symbol(x, symbol_df)

  disease_seed_nodes <- .overlay_direct_ensembl_nodes(
    disease_seed_genes,
    graph_nodes = graph_nodes,
    symbol_df = symbol_df,
    source_filter = c("AD Atlas-seeds", "piTracer-seeds")
  )
  if (is.null(disease_seed_nodes)) {
    disease_seed_df <- .overlay_prepare_disease_seeds(disease_seed_genes)
    disease_seed_nodes <- intersect(.overlay_match_gene_set_to_graph(disease_seed_df, graph_nodes, symbol_df)$node, graph_nodes)
  }

  drug_signature_source <- drug_signature
  if (is.null(drug_signature_source) && !is.null(subgraph_result$metadata$drug_signature)) {
    drug_signature_source <- subgraph_result$metadata$drug_signature
  }
  direct_drug_signature_df <- if (is.null(drug_signature)) {
    .overlay_direct_ensembl_gene_set(
      drug_signature_source,
      graph_nodes = graph_nodes,
      symbol_df = symbol_df,
      set_name = "drug_signature"
    )
  } else {
    NULL
  }
  if (!is.null(direct_drug_signature_df)) {
    drug_signature_df <- direct_drug_signature_df
  } else {
    drug_signature_df <- .overlay_prepare_drug_signature_input(
      drug_signature_source,
      gene_col = NULL,
      direction_col = "direction",
      use = drug_signature_use
    )
    drug_signature_df <- .overlay_match_gene_set_to_graph(drug_signature_df, graph_nodes, symbol_df)
  }
  drug_sig_nodes <- intersect(drug_signature_df$node, graph_nodes)

  disease_seed_direct_targets <- intersect(disease_seed_nodes, drug_targets)
  gene_gene_edges <- subgraph_edges %>% dplyr::filter(type != "DT")
  disease_seed_one_step_edges <- gene_gene_edges %>%
    dplyr::filter(
      (from %in% drug_targets & to %in% disease_seed_nodes) |
        (to %in% drug_targets & from %in% disease_seed_nodes)
    ) %>%
    dplyr::mutate(edge_role = "Disease seed one-step")

  disease_seed_one_step_nodes <- unique(c(
    disease_seed_one_step_edges$from[disease_seed_one_step_edges$from %in% disease_seed_nodes],
    disease_seed_one_step_edges$to[disease_seed_one_step_edges$to %in% disease_seed_nodes]
  ))
  disease_seed_one_step_targets <- unique(c(
    disease_seed_one_step_edges$from[disease_seed_one_step_edges$from %in% drug_targets],
    disease_seed_one_step_edges$to[disease_seed_one_step_edges$to %in% drug_targets]
  ))
  disease_seed_focus_nodes <- union(disease_seed_direct_targets, disease_seed_one_step_nodes)

  disease_mode <- dplyr::case_when(
    length(disease_seed_direct_targets) > 0 && length(disease_seed_one_step_nodes) > 0 ~ "direct_target_and_one_step",
    length(disease_seed_direct_targets) > 0 ~ "direct_target",
    length(disease_seed_one_step_nodes) > 0 ~ "one_step_neighbor",
    TRUE ~ "none"
  )

  drug_sig_direct_targets <- intersect(drug_sig_nodes, drug_targets)
  g_path <- g_full
  if (drug %in% igraph::V(g_path)$name) {
    g_path <- igraph::delete_vertices(g_path, drug)
  }
  drug_targets_path <- intersect(drug_targets, igraph::V(g_path)$name)
  drug_sig_path_candidates <- setdiff(intersect(drug_sig_nodes, igraph::V(g_path)$name), drug_sig_direct_targets)
  drug_sig_paths <- .overlay_extract_shortest_paths(
    graph = g_path,
    sources = drug_targets_path,
    targets = drug_sig_path_candidates,
    max_distance = max_signature_path_distance,
    get_symbol = get_symbol,
    edge_role = "Drug signature shortest path"
  )
  drug_sig_path_edges <- drug_sig_paths$edges %>%
    dplyr::rename(type = edge_type) %>%
    dplyr::select(from, to, type, edge_role)
  drug_sig_path_nodes <- drug_sig_paths$target_nodes
  drug_sig_path_targets <- drug_sig_paths$source_nodes
  pathway_intermediate_nodes <- setdiff(drug_sig_paths$intermediate_nodes, c(drug_targets, drug))
  drug_sig_path_info <- drug_sig_paths$path_info
  drug_sig_focus_nodes <- union(drug_sig_direct_targets, drug_sig_path_nodes)

  signature_mode <- dplyr::case_when(
    length(drug_sig_direct_targets) > 0 && length(drug_sig_path_nodes) > 0 ~ "direct_target_and_shortest_path",
    length(drug_sig_direct_targets) > 0 ~ "direct_target",
    length(drug_sig_path_nodes) > 0 ~ "shortest_path",
    TRUE ~ "none"
  )

  relevant_targets <- unique(c(
    disease_seed_direct_targets,
    disease_seed_one_step_targets,
    drug_sig_direct_targets,
    drug_sig_path_targets
  ))
  if (isTRUE(include_all_drug_targets)) {
    relevant_targets <- drug_targets
  }

  dt_edges <- subgraph_edges %>%
    dplyr::filter(type == "DT") %>%
    dplyr::filter(from %in% c(drug, relevant_targets) | to %in% c(drug, relevant_targets)) %>%
    dplyr::mutate(edge_role = "Drug-target")

  display_edges_raw <- dplyr::bind_rows(
    dt_edges %>% dplyr::select(from, to, type, edge_role),
    disease_seed_one_step_edges %>% dplyr::select(from, to, type, edge_role),
    drug_sig_path_edges
  ) %>%
    dplyr::mutate(
      from = clean_id(from),
      to = clean_id(to),
      type = normalize_edge_type(type),
      edge_key = make_edge_key(from, to)
    ) %>%
    dplyr::filter(!is.na(from), !is.na(to), nzchar(from), nzchar(to), from != to)

  if (!nrow(display_edges_raw)) {
    return(NULL)
  }

  display_edges <- display_edges_raw %>%
    dplyr::group_by(edge_key) %>%
    dplyr::summarise(
      from = dplyr::first(from),
      to = dplyr::first(to),
      type = dplyr::first(type),
      has_dt = any(edge_role == "Drug-target"),
      has_seed = any(edge_role == "Disease seed one-step"),
      has_sig = any(edge_role == "Drug signature shortest path"),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      edge_role = dplyr::case_when(
        has_seed ~ "Disease seed one-step",
        has_sig ~ "Drug signature shortest path",
        has_dt ~ "Drug-target",
        TRUE ~ "Drug-target"
      ),
      edge_role = factor(edge_role, levels = .overlay_edge_role_levels())
    ) %>%
    dplyr::select(from, to, type, edge_role)

  g_plot <- igraph::graph_from_data_frame(display_edges, directed = FALSE)

  disease_map <- .overlay_prepare_disease_map(disease_seed_genes) %>%
    dplyr::mutate(Ensembl = .overlay_map_ids_to_graph(Ensembl, graph_nodes, symbol_df)) %>%
    dplyr::filter(!is.na(Ensembl), nzchar(Ensembl)) %>%
    dplyr::distinct(Ensembl, .keep_all = TRUE)
  drug_sig_map <- drug_signature_df %>%
    dplyr::transmute(Ensembl = node, direction = direction) %>%
    dplyr::distinct(Ensembl, .keep_all = TRUE)

  node_df <- tibble::tibble(name = igraph::V(g_plot)$name) %>%
    dplyr::mutate(
      is_drug = name == drug,
      is_drug_target = name %in% drug_targets,
      is_disease_seed = name %in% disease_seed_nodes,
      is_disease_seed_direct_target = name %in% disease_seed_direct_targets,
      is_disease_seed_one_step = name %in% disease_seed_one_step_nodes,
      is_drug_signature = name %in% drug_sig_nodes,
      is_drug_signature_direct_target = name %in% drug_sig_direct_targets,
      is_drug_signature_path = name %in% drug_sig_path_nodes,
      is_pathway_intermediate = name %in% pathway_intermediate_nodes
    ) %>%
    dplyr::left_join(disease_map, by = c("name" = "Ensembl")) %>%
    dplyr::left_join(drug_sig_map, by = c("name" = "Ensembl"), suffix = c(".disease", ".drugsig")) %>%
    dplyr::mutate(
      label = get_symbol(name),
      category = dplyr::case_when(
        is_disease_seed ~ "DiseaseSeed",
        is_drug_signature ~ "DrugSignature",
        is_drug_target ~ "DrugTarget",
        TRUE ~ "Other"
      ),
      category = factor(category, levels = .overlay_node_category_levels()),
      regulation = factor(
        dplyr::case_when(
          direction.disease %in% c("up", "UP", "Up", "upregulated", "Upregulated") ~ "Upregulated",
          direction.disease %in% c("down", "DOWN", "Down", "downregulated", "Downregulated") ~ "Downregulated",
          TRUE ~ "None"
        ),
        levels = .overlay_disease_regulation_levels()
      ),
      node_size = factor(dplyr::if_else(is_drug, "Drug", "Gene"), levels = .overlay_node_type_levels()),
      node_type = node_size,
      target_status = factor(
        dplyr::if_else(is_drug_target, "Direct drug target", "Not direct target"),
        levels = .overlay_target_status_levels()
      ),
      outline_width = dplyr::if_else(is_drug_target, 3.2, 0.6)
    )

  igraph::V(g_plot)$label <- node_df$label
  igraph::V(g_plot)$category <- node_df$category
  igraph::V(g_plot)$regulation <- node_df$regulation
  igraph::V(g_plot)$node_size <- node_df$node_size
  igraph::V(g_plot)$node_type <- node_df$node_type
  igraph::V(g_plot)$target_status <- node_df$target_status
  igraph::V(g_plot)$outline_width <- node_df$outline_width

  disease_seed_evidence <- tibble::tibble(
    gene = disease_seed_focus_nodes,
    gene_symbol = get_symbol(disease_seed_focus_nodes),
    evidence = dplyr::case_when(
      disease_seed_focus_nodes %in% disease_seed_direct_targets ~ "Disease seed is direct drug target",
      disease_seed_focus_nodes %in% disease_seed_one_step_nodes ~ "Disease seed is one-step neighbor of drug target",
      TRUE ~ NA_character_
    ),
    mode = disease_mode,
    distance_to_drug_target = dplyr::case_when(
      disease_seed_focus_nodes %in% disease_seed_direct_targets ~ 0,
      disease_seed_focus_nodes %in% disease_seed_one_step_nodes ~ 1,
      TRUE ~ NA_real_
    )
  )

  if (nrow(disease_seed_evidence) > 0) {
    direct_target_map <- tibble::tibble(
      gene = disease_seed_direct_targets,
      nearest_target = disease_seed_direct_targets,
      nearest_target_symbol = get_symbol(disease_seed_direct_targets)
    )
    one_step_target_map <- disease_seed_one_step_edges %>%
      dplyr::mutate(
        disease_seed_gene = dplyr::case_when(from %in% disease_seed_nodes ~ from, to %in% disease_seed_nodes ~ to, TRUE ~ NA_character_),
        nearest_target = dplyr::case_when(from %in% drug_targets ~ from, to %in% drug_targets ~ to, TRUE ~ NA_character_)
      ) %>%
      dplyr::filter(!is.na(disease_seed_gene), !is.na(nearest_target)) %>%
      dplyr::transmute(gene = disease_seed_gene, nearest_target = nearest_target, nearest_target_symbol = get_symbol(nearest_target)) %>%
      dplyr::distinct(gene, .keep_all = TRUE)
    disease_seed_evidence <- disease_seed_evidence %>%
      dplyr::left_join(dplyr::bind_rows(direct_target_map, one_step_target_map) %>% dplyr::distinct(gene, .keep_all = TRUE), by = "gene")
  }

  drug_signature_evidence <- tibble::tibble(
    gene = drug_sig_focus_nodes,
    gene_symbol = get_symbol(drug_sig_focus_nodes),
    evidence = dplyr::case_when(
      drug_sig_focus_nodes %in% drug_sig_direct_targets ~ "Drug-signature gene is direct drug target",
      drug_sig_focus_nodes %in% drug_sig_path_nodes ~ "Drug-signature gene connected by shortest path",
      TRUE ~ NA_character_
    ),
    mode = signature_mode
  )
  if (nrow(drug_signature_evidence) > 0) {
    direct_sig_map <- tibble::tibble(
      gene = drug_sig_direct_targets,
      gene_symbol = get_symbol(drug_sig_direct_targets),
      target = drug_sig_direct_targets,
      target_symbol = get_symbol(drug_sig_direct_targets),
      distance = rep(0, length(drug_sig_direct_targets)),
      path = drug_sig_direct_targets,
      path_symbols = get_symbol(drug_sig_direct_targets),
      path_edges = rep("", length(drug_sig_direct_targets))
    )
    drug_signature_evidence <- drug_signature_evidence %>%
      dplyr::left_join(dplyr::bind_rows(direct_sig_map, drug_sig_path_info) %>% dplyr::distinct(gene, .keep_all = TRUE), by = c("gene", "gene_symbol"))
  }

  list(
    plot = .overlay_plot_graph(g_plot, node_alpha, seed, layout),
    edges = display_edges,
    nodes = node_df,
    graph = g_plot,
    disease_seed_evidence = disease_seed_evidence,
    drug_signature_evidence = drug_signature_evidence,
    drug_sig_path_info = drug_sig_path_info,
    disease_mode = disease_mode,
    signature_mode = signature_mode
  )
}

.overlay_require_packages <- function() {
  for (pkg in c("igraph", "ggraph", "ggplot2", "dplyr", "tibble", "grid")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

.overlay_resolve_drug <- function(subgraph_result, requested_drug = NULL) {
  candidates <- c(
    requested_drug,
    if (!is.null(subgraph_result$drug)) subgraph_result$drug else NULL,
    if (!is.null(subgraph_result$metadata$drug_network)) subgraph_result$metadata$drug_network else NULL,
    if (!is.null(subgraph_result$metadata$drug)) subgraph_result$metadata$drug else NULL
  )
  candidates <- trimws(as.character(candidates))
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  if (length(candidates)) candidates[1] else NULL
}

.overlay_direct_ensembl_gene_set <- function(x,
                                             graph_nodes,
                                             symbol_df = NULL,
                                             gene_col = NULL,
                                             direction_col = "direction",
                                             set_name = "gene_set",
                                             source_filter = NULL) {
  if (!is.data.frame(x) || !is.null(gene_col) || !("Ensembl" %in% names(x))) {
    return(NULL)
  }
  if (!is.null(source_filter)) {
    if (!("source" %in% names(x))) {
      return(NULL)
    }
    x <- x %>%
      dplyr::mutate(source = as.character(source)) %>%
      dplyr::filter(source %in% source_filter)
  }

  direction_col <- .overlay_resolve_direction_col(x, direction_col)
  direction <- if (!is.null(direction_col)) {
    as.character(x[[direction_col]])
  } else {
    rep(NA_character_, nrow(x))
  }

  tibble::tibble(
    gene = .overlay_clean_ids(x$Ensembl),
    direction = direction,
    set = set_name,
    node = .overlay_map_ids_to_graph(.overlay_clean_ids(x$Ensembl), graph_nodes, symbol_df)
  ) %>%
    dplyr::filter(!is.na(node), nzchar(node), node %in% graph_nodes) %>%
    dplyr::distinct(node, .keep_all = TRUE)
}

.overlay_direct_ensembl_nodes <- function(x,
                                          graph_nodes,
                                          symbol_df = NULL,
                                          gene_col = NULL,
                                          source_filter = NULL) {
  direct_df <- .overlay_direct_ensembl_gene_set(
    x,
    graph_nodes = graph_nodes,
    symbol_df = symbol_df,
    gene_col = gene_col,
    direction_col = NULL,
    set_name = "direct_ensembl",
    source_filter = source_filter
  )
  if (is.null(direct_df)) {
    return(NULL)
  }
  unique(direct_df$node)
}

.overlay_prepare_disease_seeds <- function(x, gene_col = NULL) {
  if (is.null(x)) {
    return(tibble::tibble(gene = character(), direction = character(), set = character()))
  }
  if (is.data.frame(x) && "source" %in% names(x)) {
    x <- x %>%
      dplyr::mutate(source = as.character(source)) %>%
      dplyr::filter(source %in% c("AD Atlas-seeds", "piTracer-seeds"))
  }
  .overlay_prepare_gene_set(x, gene_col = gene_col, direction_col = NULL, set_name = "disease_seed")
}

.overlay_prepare_disease_map <- function(x, gene_col = NULL) {
  if (is.null(x)) {
    return(tibble::tibble(Ensembl = character(), direction.disease = character()))
  }
  if (!is.data.frame(x)) {
    return(tibble::tibble(Ensembl = .overlay_clean_ids(x), direction.disease = NA_character_))
  }
  gene_col <- .overlay_resolve_gene_col(x, gene_col)
  direction_col <- .overlay_resolve_direction_col(x, "direction")
  direction <- if (!is.null(direction_col)) as.character(x[[direction_col]]) else rep(NA_character_, nrow(x))
  tibble::tibble(
    Ensembl = .overlay_clean_ids(x[[gene_col]]),
    direction.disease = direction
  ) %>%
    dplyr::filter(!is.na(Ensembl), nzchar(Ensembl)) %>%
    dplyr::distinct(Ensembl, .keep_all = TRUE)
}
.overlay_clean_ids <- function(x) {
  x <- trimws(as.character(x))
  sub("\\.[0-9]+$", "", x)
}

.overlay_edge_key <- function(a, b) {
  paste(pmin(a, b), pmax(a, b), sep = "__")
}
.overlay_standardize_edges <- function(edges) {
  if (!is.data.frame(edges)) {
    stop("`subgraph_result$edges` must be a data frame.", call. = FALSE)
  }
  if (!all(c("from", "to") %in% colnames(edges))) {
    stop("`subgraph_result$edges` must contain `from` and `to` columns.", call. = FALSE)
  }

  edge_type <- if ("edge_type" %in% colnames(edges)) {
    as.character(edges$edge_type)
  } else if ("type" %in% colnames(edges)) {
    as.character(edges$type)
  } else {
    rep("PPI", nrow(edges))
  }
  edge_class <- if ("edge_class" %in% colnames(edges)) {
    as.character(edges$edge_class)
  } else {
    dplyr::case_when(
      edge_type %in% c("DT", "DTI", "Drug-target", "drug_target") ~ "DTI",
      TRUE ~ "PPI"
    )
  }

  data.frame(
    from = .overlay_clean_ids(edges$from),
    to = .overlay_clean_ids(edges$to),
    edge_type = edge_type,
    edge_class = edge_class,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::filter(!is.na(from), !is.na(to), nzchar(from), nzchar(to), from != to) %>%
    dplyr::distinct()
}

.overlay_prepare_gene_set <- function(x, gene_col = "gene", direction_col = NULL, set_name = "gene_set") {
  if (is.null(x)) {
    return(tibble::tibble(gene = character(), direction = character(), set = character()))
  }
  if (is.vector(x) && !is.list(x)) {
    return(tibble::tibble(gene = unique(.overlay_clean_ids(x)), direction = NA_character_, set = set_name) %>%
             dplyr::filter(!is.na(gene), nzchar(gene)))
  }
  if (!is.data.frame(x)) {
    stop("Gene sets must be character vectors or data frames.", call. = FALSE)
  }

  gene_col <- .overlay_resolve_gene_col(x, gene_col)
  direction_col <- .overlay_resolve_direction_col(x, direction_col)
  direction <- if (!is.null(direction_col)) {
    as.character(x[[direction_col]])
  } else {
    rep(NA_character_, nrow(x))
  }

  tibble::tibble(
    gene = .overlay_clean_ids(x[[gene_col]]),
    direction = direction,
    set = set_name
  ) %>%
    dplyr::filter(!is.na(gene), nzchar(gene)) %>%
    dplyr::distinct(gene, .keep_all = TRUE)
}

.overlay_resolve_gene_col <- function(x, gene_col = NULL) {
  if (!is.null(gene_col) && gene_col %in% colnames(x)) {
    return(gene_col)
  }
  if (ncol(x) == 1) {
    return(colnames(x)[1])
  }
  candidates <- c("gene", "Gene", "Ensembl", "ENSEMBL", "Gene_symbol", "SYMBOL", "Symbol", "Entrez", "ENTREZID")
  hit <- candidates[candidates %in% colnames(x)]
  if (length(hit)) {
    return(hit[1])
  }
  stop("Could not infer a gene column. Please provide the appropriate `*_gene_col` argument.", call. = FALSE)
}

.overlay_resolve_direction_col <- function(x, direction_col = NULL) {
  if (is.null(direction_col)) {
    return(NULL)
  }
  if (direction_col %in% colnames(x)) {
    return(direction_col)
  }
  candidates <- c("direction", "Direction", "regulation", "Regulation", "trend", "Trend")
  hit <- candidates[candidates %in% colnames(x)]
  if (length(hit)) {
    return(hit[1])
  }
  NULL
}

.overlay_prepare_drug_signature_input <- function(x,
                                                  gene_col = NULL,
                                                  direction_col = "direction",
                                                  use = c("top", "up", "down", "signature")) {
  use <- match.arg(use)
  if (is.null(x)) {
    return(tibble::tibble(gene = character(), direction = character(), set = character()))
  }
  if (!(is.list(x) && !is.data.frame(x) && "signature" %in% names(x))) {
    return(.overlay_prepare_gene_set(x, gene_col, direction_col, "drug_signature"))
  }

  if (use %in% c("up", "down")) {
    element <- paste0(use, "_genes")
    if (is.null(x[[element]])) {
      stop("`drug_signature_use = '", use, "'` requires `drug_signature$", element, "`.", call. = FALSE)
    }
    out <- .overlay_prepare_gene_set(x[[element]], .overlay_resolve_gene_col(x[[element]], gene_col), NULL, "drug_signature")
    out$direction <- use
    return(out)
  }
  if (use == "top") {
    up <- if (!is.null(x$up_genes)) .overlay_prepare_drug_signature_input(x, gene_col, direction_col, "up") else NULL
    down <- if (!is.null(x$down_genes)) .overlay_prepare_drug_signature_input(x, gene_col, direction_col, "down") else NULL
    if (is.null(up) && is.null(down)) {
      stop("`drug_signature_use = 'top'` requires `up_genes` and/or `down_genes`.", call. = FALSE)
    }
    return(dplyr::bind_rows(up, down) %>% dplyr::distinct(gene, .keep_all = TRUE))
  }

  .overlay_prepare_gene_set(x$signature, .overlay_resolve_gene_col(x$signature, gene_col), direction_col, "drug_signature")
}

.overlay_prepare_symbol_map <- function(symbol_map, gene_col = NULL, symbol_col = NULL) {
  if (is.null(symbol_map)) {
    return(tibble::tibble(gene = character(), symbol = character()))
  }
  if (!is.data.frame(symbol_map)) {
    stop("`symbol_map` must be a data frame or NULL.", call. = FALSE)
  }
  if (ncol(symbol_map) < 2) {
    stop("`symbol_map` must contain at least gene ID and symbol columns.", call. = FALSE)
  }

  gene_col <- .overlay_resolve_symbol_map_col(
    symbol_map,
    requested_col = gene_col,
    candidates = c("gene", "Gene", "Ensembl", "ENSEMBL", "ENTREZID", "Entrez"),
    fallback_index = 1
  )
  symbol_col <- .overlay_resolve_symbol_map_col(
    symbol_map,
    requested_col = symbol_col,
    candidates = c("symbol", "Symbol", "SYMBOL", "Gene_symbol"),
    fallback_index = 2
  )

  tibble::tibble(
    gene = .overlay_clean_ids(symbol_map[[gene_col]]),
    symbol = trimws(as.character(symbol_map[[symbol_col]]))
  ) %>%
    dplyr::filter(!is.na(gene), nzchar(gene), !is.na(symbol), nzchar(symbol)) %>%
    dplyr::distinct(gene, .keep_all = TRUE)
}

.overlay_resolve_symbol_map_col <- function(symbol_map, requested_col, candidates, fallback_index) {
  if (!is.null(requested_col) && requested_col %in% colnames(symbol_map)) {
    return(requested_col)
  }
  hit <- candidates[candidates %in% colnames(symbol_map)]
  if (length(hit)) {
    return(hit[1])
  }
  if (ncol(symbol_map) == 2) {
    return(colnames(symbol_map)[fallback_index])
  }
  stop("Could not infer columns in `symbol_map`; provide a two-column gene-to-symbol map.", call. = FALSE)
}

.overlay_add_orgdb_symbols <- function(graph_nodes, symbol_df) {
  graph_nodes <- unique(.overlay_clean_ids(graph_nodes))
  graph_nodes <- graph_nodes[!is.na(graph_nodes) & nzchar(graph_nodes)]
  keytype <- .overlay_infer_orgdb_keytype(graph_nodes)

  if (is.null(keytype)) {
    return(symbol_df)
  }

  mapping <- tryCatch(
    .map_gene_ids_orgdb(graph_nodes, from = keytype, to = "SYMBOL"),
    error = function(e) NULL
  )
  if (is.null(mapping) || !(keytype %in% colnames(mapping)) || !("SYMBOL" %in% colnames(mapping))) {
    return(symbol_df)
  }

  orgdb_symbols <- tibble::tibble(
    gene = .overlay_clean_ids(mapping[[keytype]]),
    symbol = trimws(as.character(mapping$SYMBOL))
  ) %>%
    dplyr::filter(!is.na(gene), nzchar(gene), !is.na(symbol), nzchar(symbol)) %>%
    dplyr::distinct(gene, .keep_all = TRUE)

  dplyr::bind_rows(symbol_df, orgdb_symbols) %>%
    dplyr::distinct(gene, .keep_all = TRUE)
}

.overlay_infer_orgdb_keytype <- function(ids) {
  ids <- unique(.overlay_clean_ids(ids))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  if (!length(ids)) {
    return(NULL)
  }
  if (.looks_like_ensembl_gene_ids(ids)) {
    return("ENSEMBL")
  }
  if (mean(grepl("^[0-9]+$", ids)) >= 0.8) {
    return("ENTREZID")
  }
  NULL
}

.overlay_match_gene_set_to_graph <- function(gene_df, graph_nodes, symbol_df) {
  if (!nrow(gene_df)) {
    gene_df$node <- character()
    return(gene_df)
  }
  gene_df$node <- .overlay_map_ids_to_graph(gene_df$gene, graph_nodes, symbol_df)
  gene_df %>%
    dplyr::filter(!is.na(node), node %in% graph_nodes) %>%
    dplyr::distinct(node, .keep_all = TRUE)
}

.overlay_map_ids_to_graph <- function(ids, graph_nodes, symbol_df = NULL) {
  ids <- .overlay_clean_ids(ids)
  mapped <- rep(NA_character_, length(ids))

  direct <- match(ids, graph_nodes)
  mapped[!is.na(direct)] <- graph_nodes[direct[!is.na(direct)]]

  if (!is.null(symbol_df) && nrow(symbol_df)) {
    needs_map <- is.na(mapped)

    symbol_to_gene <- match(ids[needs_map], symbol_df$symbol)
    gene_candidates <- symbol_df$gene[symbol_to_gene]
    use_gene <- !is.na(symbol_to_gene) & gene_candidates %in% graph_nodes
    idx <- which(needs_map)
    mapped[idx[use_gene]] <- gene_candidates[use_gene]

    needs_map <- is.na(mapped)
    gene_to_symbol <- match(ids[needs_map], symbol_df$gene)
    symbol_candidates <- symbol_df$symbol[gene_to_symbol]
    use_symbol <- !is.na(gene_to_symbol) & symbol_candidates %in% graph_nodes
    idx <- which(needs_map)
    mapped[idx[use_symbol]] <- symbol_candidates[use_symbol]
  }

  mapped
}

.overlay_get_symbol <- function(x, symbol_df) {
  x <- .overlay_clean_ids(x)
  if (is.null(symbol_df) || !nrow(symbol_df)) {
    return(x)
  }
  out <- symbol_df$symbol[match(x, symbol_df$gene)]
  out[is.na(out) | !nzchar(out)] <- x[is.na(out) | !nzchar(out)]
  out
}
.overlay_empty_path_result <- function() {
  list(
    edges = tibble::tibble(from = character(), to = character(), edge_type = character(), edge_class = character(), edge_role = character()),
    path_info = tibble::tibble(gene = character(), gene_symbol = character(), target = character(), target_symbol = character(), distance = numeric(), path = character(), path_symbols = character(), path_edges = character()),
    target_nodes = character(),
    source_nodes = character(),
    intermediate_nodes = character()
  )
}
.overlay_extract_shortest_paths <- function(
    graph,
    sources,
    targets,
    max_distance,
    get_symbol,
    edge_role
) {

  out <- .overlay_empty_path_result()

  sources <- intersect(
    unique(.overlay_clean_ids(sources)),
    igraph::V(graph)$name
  )

  targets <- intersect(
    unique(.overlay_clean_ids(targets)),
    igraph::V(graph)$name
  )

  if (!length(sources) || !length(targets)) {
    return(out)
  }

  ## IMPORTANT:
  ## Preserve the original igraph edge order.
  raw_edge_df <- igraph::as_data_frame(
    graph,
    what = "edges"
  )

  dist_mat <- igraph::distances(
    graph,
    v = sources,
    to = targets,
    mode = "all"
  )

  edge_list <- list()
  info_list <- list()

  for (target_gene in targets) {

    dvec <- dist_mat[, target_gene]

    if (
      !any(is.finite(dvec)) ||
      min(dvec[is.finite(dvec)]) > max_distance
    ) {
      next
    }

    best_source <- names(dvec)[which.min(dvec)]
    best_distance <- as.numeric(dvec[best_source])

    sp <- igraph::shortest_paths(
      graph,
      from = best_source,
      to = target_gene,
      mode = "all",
      output = "both"
    )

    edge_ids <- as.integer(sp$epath[[1]])

    if (!length(edge_ids)) {
      next
    }

    vpath <- igraph::as_ids(sp$vpath[[1]])

    ## Subset using igraph edge IDs before distinct()/standardization
    this_edges <- raw_edge_df[
      edge_ids,
      ,
      drop = FALSE
    ]

    this_edges <- .overlay_standardize_edges(
      this_edges
    )

    this_edges$edge_role <- edge_role

    edge_list[[length(edge_list) + 1L]] <- this_edges

    info_list[[length(info_list) + 1L]] <- tibble::tibble(
      gene = target_gene,
      gene_symbol = get_symbol(target_gene),
      target = best_source,
      target_symbol = get_symbol(best_source),
      distance = best_distance,
      path = paste(
        vpath,
        collapse = " -> "
      ),
      path_symbols = paste(
        get_symbol(vpath),
        collapse = " -> "
      ),
      path_edges = paste(
        this_edges$edge_type,
        collapse = " -> "
      )
    )

    out$target_nodes <- union(
      out$target_nodes,
      target_gene
    )

    out$source_nodes <- union(
      out$source_nodes,
      best_source
    )

    out$intermediate_nodes <- union(
      out$intermediate_nodes,
      setdiff(
        vpath,
        c(best_source, target_gene)
      )
    )
  }

  if (length(edge_list)) {
    out$edges <- dplyr::bind_rows(
      edge_list
    ) %>%
      dplyr::distinct()

    out$path_info <- dplyr::bind_rows(
      info_list
    )
  }

  out
}

.overlay_edge_role_levels <- function() {
  c("Drug-target", "Disease seed one-step", "Drug signature shortest path")
}

.overlay_node_category_levels <- function() {
  c("DiseaseSeed", "DrugSignature", "DrugTarget", "Other")
}

.overlay_disease_regulation_levels <- function() {
  c("None", "Upregulated", "Downregulated")
}

.overlay_node_type_levels <- function() {
  c("Drug", "Gene")
}

.overlay_target_status_levels <- function() {
  c("Direct drug target", "Not direct target")
}

.overlay_plot_graph <- function(graph, node_alpha, seed, layout) {
  set.seed(seed)
  layout_tbl <- ggraph::create_layout(graph, layout = layout)
  y_range <- diff(range(layout_tbl$y))
  if (!is.finite(y_range) || y_range == 0) {
    y_range <- 1
  }
  drug_label_df <- layout_tbl %>%
    dplyr::filter(node_size == "Drug") %>%
    dplyr::mutate(x_lab = x, y_lab = y + 0.05 * y_range)

  ggraph::ggraph(layout_tbl) +
    ggraph::geom_edge_link(
      ggplot2::aes(color = edge_role),
      alpha = 0.55,
      linewidth = 1.0
    ) +
    ggraph::geom_node_point(
      ggplot2::aes(
        fill = category,
        shape = regulation,
        size = node_size,
        color = target_status,
        stroke = outline_width
      ),
      alpha = node_alpha
    ) +
    ggplot2::geom_point(
      data = drug_label_df,
      ggplot2::aes(x = x, y = y),
      inherit.aes = FALSE,
      shape = 21,
      size = 11,
      fill = "white",
      color = "white",
      stroke = 0,
      alpha = 0.95,
      show.legend = FALSE
    ) +
    ggplot2::geom_point(
      data = drug_label_df,
      ggplot2::aes(x = x, y = y),
      inherit.aes = FALSE,
      shape = 21,
      size = 8.5,
      fill = "#FFD166",
      color = "black",
      stroke = 1.5,
      alpha = 1,
      show.legend = FALSE
    ) +
    ggraph::geom_node_text(
      ggplot2::aes(filter = node_size == "Gene", label = label),
      size = 3.2,
      repel = TRUE,
      seed = seed,
      point.padding = grid::unit(0.18, "lines"),
      box.padding = grid::unit(0.22, "lines"),
      max.overlaps = Inf,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = drug_label_df,
      ggplot2::aes(x = x_lab, y = y_lab, label = label),
      inherit.aes = FALSE,
      color = "black",
      fontface = "bold",
      size = 5.5,
      show.legend = FALSE
    ) +
    ggraph::scale_edge_color_manual(
      name = "Edge type",
      breaks = .overlay_edge_role_levels(),
      limits = .overlay_edge_role_levels(),
      values = c(
        `Drug-target` = "grey45",
        `Disease seed one-step` = "dodgerblue4",
        `Drug signature shortest path` = "firebrick4"
      ),
      drop = TRUE
    ) +
    ggplot2::scale_fill_manual(
      name = "Node category",
      breaks = .overlay_node_category_levels(),
      limits = .overlay_node_category_levels(),
      values = c(
        DiseaseSeed = "dodgerblue",
        DrugSignature = "salmon",
        DrugTarget = "goldenrod",
        Other = "grey85"
      ),
      drop = TRUE
    ) +
    ggplot2::scale_shape_manual(
      name = "Disease regulation",
      breaks = .overlay_disease_regulation_levels(),
      limits = .overlay_disease_regulation_levels(),
      values = c(None = 21, Upregulated = 24, Downregulated = 25),
      drop = TRUE
    ) +
    ggplot2::scale_size_manual(
      name = "Node type",
      breaks = .overlay_node_type_levels(),
      limits = .overlay_node_type_levels(),
      values = c(Drug = 8, Gene = 4),
      drop = TRUE
    ) +
    ggplot2::scale_color_manual(
      name = "Direct target status",
      breaks = .overlay_target_status_levels(),
      limits = .overlay_target_status_levels(),
      values = c(`Direct drug target` = "black", `Not direct target` = "grey70"),
      drop = TRUE
    ) +
    ggplot2::guides(
      size = ggplot2::guide_legend(order = 1),
      fill = ggplot2::guide_legend(
        order = 2,
        override.aes = list(shape = 21, size = 4, alpha = 1, color = "black", stroke = 0.7)
      ),
      shape = ggplot2::guide_legend(
        order = 3,
        override.aes = list(size = 4, fill = "white", color = "black", alpha = 1)
      ),
      color = ggplot2::guide_legend(
        order = 4,
        override.aes = list(shape = 21, size = 5, alpha = 1, fill = "white", stroke = c(3.2, 0.6))
      ),
      edge_color = ggplot2::guide_legend(
        order = 5,
        override.aes = list(linewidth = 1.3, alpha = 1)
      )
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold")
    )
}
