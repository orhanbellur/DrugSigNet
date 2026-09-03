#' @title Diagnose Network Inputs for Network-Based Drug Prioritization
#'
#' @description
#' Diagnoses structural, mapping, target-set, and computational properties of
#' network inputs before applying shortest-path network proximity or
#' diffusion-based drug-prioritization methods.
#'
#' @details
#' `diagnoseNetworkInput()` is an input-screening utility for network-based drug
#' prioritization. It constructs an undirected, simplified protein-protein
#' interaction (PPI) graph and performs diagnostics on its largest connected
#' component (LCC).
#'
#' The function reports network connectivity and degree heterogeneity, disease
#' seed mapping and degree profiles, and a baseline nearest-seed topology
#' obtained by repeatedly sampling nodes uniformly from the LCC.
#'
#' The baseline nearest-seed analysis describes how readily arbitrary network
#' nodes reach the disease-seed set. It is a topology diagnostic only and is not
#' the degree- and size-matched null distribution used by network-proximity
#' algorithms to calculate z-scores. Short baseline distances therefore should
#' not by themselves be interpreted as evidence that network proximity is
#' invalid or unsuitable.
#'
#' When `drug_targets` is provided, the function additionally reports target
#' mapping, the fraction of scorable drugs, mapped target-set sizes,
#' seed-target overlap, and target degree profiles. Small drug-target sets are
#' reported because they may affect randomization-based proximity statistics,
#' but no universal minimum target-set size is assumed.
#'
#' For diffusion-based methods, the function estimates the memory required for
#' dense float64 N x N matrices used by the current implementation. The
#' working-memory estimate is approximate and implementation-specific; actual
#' peak memory may be higher because of temporary matrices and parallel-process
#' copies.
#'
#' The returned diagnostics are intended to identify potential limitations of
#' the network input. They are descriptive screening outputs and are not
#' universal PASS/FAIL validity criteria.
#'
#' @param ppi Protein-protein interaction network as a data frame or object
#'   coercible to a data frame.
#' @param seeds Disease-associated seed genes, supplied either as a character
#'   vector or as a data frame containing the column specified by `seed_col`.
#' @param drug_targets Optional drug-target interaction table containing the
#'   columns specified by `drug_col` and `target_col`.
#' @param ppi_cols Character vector of length two specifying the PPI endpoint
#'   columns. Default is `c("gene1", "gene2")`.
#' @param seed_col Column containing seed-gene identifiers when `seeds` is a
#'   data frame. Default is `"gene"`.
#' @param drug_col Column containing drug identifiers in `drug_targets`.
#'   Default is `"ID"`.
#' @param target_col Column containing target identifiers in `drug_targets`.
#'   Default is `"Target"`.
#' @param n_random Positive integer giving the maximum number of LCC nodes
#'   sampled in each nearest-seed topology replicate. Default is `1000`.
#' @param n_repeats Positive integer giving the number of independent
#'   nearest-seed sampling replicates. Repetition is used to assess Monte Carlo
#'   stability; `20` is a computational default rather than a literature-based
#'   requirement.
#' @param random_seed Integer seed used for reproducible node sampling.
#'   Default is `1`.
#' @param memory_limit_gb Optional positive numeric value specifying an
#'   available memory budget in GB. If not `NULL`, the approximate dense
#'   diffusion working-memory requirement is compared with this value.
#'   Default is `8`.
#'
#' @return
#' A named list containing:
#' \describe{
#'   \item{network}{Network size, connected-component coverage, and global
#'   degree-distribution summaries.}
#'   \item{seeds}{Seed mapping, network coverage, and seed-degree summaries.}
#'   \item{baseline_nearest_seed}{Repeated uniform random-node nearest-seed
#'   topology summaries and replicate-level results.}
#'   \item{drugs}{Drug-target mapping, scorable-drug, target-set-size,
#'   degree-profile, and seed-overlap summaries.}
#'   \item{target_mapping_by_drug}{Per-drug target mapping and direct
#'   seed-overlap information.}
#'   \item{diffusion_memory}{Approximate dense-matrix storage and
#'   working-memory requirements.}
#'   \item{diagnostics}{Compact method-specific summaries for network proximity
#'   and diffusion-based methods.}
#' }
#'
#' @examples
#' ppi <- data.frame(
#'   gene1 = c("A", "B", "C", "D", "E"),
#'   gene2 = c("B", "C", "D", "E", "A")
#' )
#'
#' seeds <- c("A", "C")
#'
#' drug_targets <- data.frame(
#'   ID = c("D1", "D1", "D2"),
#'   Target = c("A", "B", "E")
#' )
#'
#' result <- diagnoseNetworkInput(
#'   ppi = ppi,
#'   seeds = seeds,
#'   drug_targets = drug_targets,
#'   n_random = 5,
#'   n_repeats = 2
#' )
#'
#' result$diagnostics$network_proximity
#' result$diagnostics$diffusion
#'
#' @seealso
#' `Network_proximity()`, `Diffusion()`, `drugNetworkPipeline()`
#'
#' @export
diagnoseNetworkInput <- function(
    ppi,
    seeds,
    drug_targets = NULL,
    ppi_cols = c("gene1", "gene2"),
    seed_col = "gene",
    drug_col = "ID",
    target_col = "Target",
    n_random = 1000,
    n_repeats = 20,
    random_seed = 1,
    memory_limit_gb = 8
) {

  # ------------------------------------------------------------------
  # Helpers
  # ------------------------------------------------------------------

  clean_ids <- function(x) {
    x <- trimws(as.character(x))
    unique(x[!is.na(x) & nzchar(x)])
  }

  check_columns <- function(x, cols, arg) {
    missing_cols <- setdiff(cols, names(x))

    if (length(missing_cols)) {
      stop(
        sprintf(
          "`%s` is missing required column%s: %s.",
          arg,
          if (length(missing_cols) == 1L) "" else "s",
          paste(missing_cols, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  clean_pairs <- function(x, cols, new_names, arg) {
    x <- as.data.frame(x)
    check_columns(x, cols, arg)

    out <- x[, cols, drop = FALSE]
    names(out) <- new_names

    out[] <- lapply(
      out,
      function(z) trimws(as.character(z))
    )

    keep <- !is.na(out[[1]]) &
      !is.na(out[[2]]) &
      nzchar(out[[1]]) &
      nzchar(out[[2]])

    unique(out[keep, , drop = FALSE])
  }

  validate_positive_integer <- function(x, arg) {
    if (!is.numeric(x) ||
        length(x) != 1L ||
        is.na(x) ||
        !is.finite(x) ||
        x < 1 ||
        x != as.integer(x)) {
      stop(
        sprintf("`%s` must be a positive integer.", arg),
        call. = FALSE
      )
    }

    as.integer(x)
  }

  valid_col_name <- function(x) {
    is.character(x) &&
      length(x) == 1L &&
      !is.na(x) &&
      nzchar(x)
  }

  numeric_summary <- function(x) {
    x <- x[is.finite(x)]

    if (!length(x)) {
      return(
        stats::setNames(
          rep(NA_real_, 6),
          c("min", "q1", "median", "mean", "q3", "max")
        )
      )
    }

    c(
      min = min(x),
      q1 = unname(stats::quantile(x, 0.25)),
      median = stats::median(x),
      mean = mean(x),
      q3 = unname(stats::quantile(x, 0.75)),
      max = max(x)
    )
  }

  degree_summary <- function(x) {
    x <- x[is.finite(x)]

    if (!length(x)) {
      return(
        stats::setNames(
          rep(NA_real_, 6),
          c("mean", "median", "cv", "p95", "p99", "max")
        )
      )
    }

    mu <- mean(x)

    c(
      mean = mu,
      median = stats::median(x),
      cv = if (mu == 0 || length(x) < 2L) {
        NA_real_
      } else {
        stats::sd(x) / mu
      },
      p95 = unname(stats::quantile(x, 0.95)),
      p99 = unname(stats::quantile(x, 0.99)),
      max = max(x)
    )
  }

  value_or_na <- function(x, fun) {
    if (length(x)) fun(x) else NA_real_
  }

  pct <- function(x) 100 * x

  # ------------------------------------------------------------------
  # Validate arguments
  # ------------------------------------------------------------------

  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required.", call. = FALSE)
  }

  if (!is.character(ppi_cols) ||
      length(ppi_cols) != 2L ||
      any(is.na(ppi_cols)) ||
      any(!nzchar(ppi_cols))) {
    stop(
      "`ppi_cols` must contain exactly two non-empty column names.",
      call. = FALSE
    )
  }

  if (!valid_col_name(seed_col)) {
    stop(
      "`seed_col` must be a non-empty column name.",
      call. = FALSE
    )
  }

  if (!valid_col_name(drug_col) ||
      !valid_col_name(target_col)) {
    stop(
      "`drug_col` and `target_col` must each be non-empty column names.",
      call. = FALSE
    )
  }

  n_random <- validate_positive_integer(
    n_random,
    "n_random"
  )

  n_repeats <- validate_positive_integer(
    n_repeats,
    "n_repeats"
  )

  if (!is.numeric(random_seed) ||
      length(random_seed) != 1L ||
      is.na(random_seed) ||
      !is.finite(random_seed) ||
      random_seed != floor(random_seed) ||
      abs(random_seed) > .Machine$integer.max) {
    stop(
      "`random_seed` must be a single integer-valued seed.",
      call. = FALSE
    )
  }

  set.seed(as.integer(random_seed))

  if (!is.null(memory_limit_gb)) {
    if (!is.numeric(memory_limit_gb) ||
        length(memory_limit_gb) != 1L ||
        is.na(memory_limit_gb) ||
        !is.finite(memory_limit_gb) ||
        memory_limit_gb <= 0) {
      stop(
        "`memory_limit_gb` must be a positive numeric value or `NULL`.",
        call. = FALSE
      )
    }

    memory_limit_gb <- as.numeric(
      memory_limit_gb
    )
  }

  # ------------------------------------------------------------------
  # 1. Build and clean network
  # ------------------------------------------------------------------

  edges <- clean_pairs(
    ppi,
    ppi_cols,
    c("from", "to"),
    "ppi"
  )

  if (!nrow(edges)) {
    stop(
      "No valid PPI edges remain after cleaning.",
      call. = FALSE
    )
  }

  g_full <- igraph::graph_from_data_frame(
    edges,
    directed = FALSE
  )

  g_full <- igraph::simplify(
    g_full,
    remove.multiple = TRUE,
    remove.loops = TRUE
  )

  if (!igraph::ecount(g_full)) {
    stop(
      "No PPI edges remain after removing loops and duplicates.",
      call. = FALSE
    )
  }

  # ------------------------------------------------------------------
  # 2. Largest connected component
  # ------------------------------------------------------------------

  comp <- igraph::components(g_full)
  lcc_id <- which.max(comp$csize)

  lcc_nodes <- names(comp$membership)[
    comp$membership == lcc_id
  ]

  g <- igraph::induced_subgraph(
    g_full,
    vids = lcc_nodes
  )

  nodes <- igraph::V(g)$name
  deg <- igraph::degree(g)

  n_full <- igraph::vcount(g_full)
  n_lcc <- igraph::vcount(g)

  lcc_fraction <- n_lcc / n_full

  network_degree <- degree_summary(
    deg
  )

  hub_ratio <- if (
    is.na(network_degree["median"]) ||
    network_degree["median"] == 0
  ) {
    NA_real_
  } else {
    unname(
      network_degree["max"] /
        network_degree["median"]
    )
  }

  # ------------------------------------------------------------------
  # 3. Disease-seed diagnostics
  # ------------------------------------------------------------------

  if (is.data.frame(seeds)) {
    check_columns(
      seeds,
      seed_col,
      "seeds"
    )

    seed_input <- clean_ids(
      seeds[[seed_col]]
    )
  } else {
    seed_input <- clean_ids(
      seeds
    )
  }

  if (!length(seed_input)) {
    stop(
      "No valid seed genes remain after cleaning.",
      call. = FALSE
    )
  }

  seed_in_lcc <- intersect(
    seed_input,
    nodes
  )

  if (!length(seed_in_lcc)) {
    stop(
      "No seed genes map to the largest connected component.",
      call. = FALSE
    )
  }

  seed_mapping <-
    length(seed_in_lcc) /
    length(seed_input)

  seed_coverage <-
    length(seed_in_lcc) /
    n_lcc

  seed_degree <- degree_summary(
    deg[seed_in_lcc]
  )

  seed_degree_enrichment <- unname(
    seed_degree["mean"] /
      network_degree["mean"]
  )

  # ------------------------------------------------------------------
  # 4. Uniform random-node nearest-seed topology baseline
  #
  # Descriptive only.
  # This is NOT the degree-matched proximity null.
  # ------------------------------------------------------------------

  sample_size <- min(
    n_random,
    length(nodes)
  )

  effective_repeats <- if (
    sample_size == length(nodes)
  ) {
    1L
  } else {
    n_repeats
  }

  baseline_runs <- vector(
    "list",
    effective_repeats
  )

  all_random_distances <- vector(
    "list",
    effective_repeats
  )

  for (i in seq_len(effective_repeats)) {

    random_nodes <- sample(
      nodes,
      size = sample_size,
      replace = FALSE
    )

    d <- igraph::distances(
      g,
      v = random_nodes,
      to = seed_in_lcc,
      mode = "all"
    )

    d_min <- apply(
      d,
      1,
      min,
      na.rm = TRUE
    )

    all_random_distances[[i]] <- d_min

    baseline_runs[[i]] <- data.frame(
      replicate = i,
      mean_distance = mean(d_min),
      median_distance = stats::median(d_min),
      fraction_d0 = mean(d_min == 0),
      fraction_d1 = mean(d_min == 1),
      fraction_d2 = mean(d_min == 2),
      fraction_le1 = mean(d_min <= 1),
      fraction_le2 = mean(d_min <= 2),
      fraction_ge3 = mean(d_min >= 3),
      stringsAsFactors = FALSE
    )
  }

  baseline_runs <- do.call(
    rbind,
    baseline_runs
  )

  baseline_distance <- numeric_summary(
    unlist(
      all_random_distances,
      use.names = FALSE
    )
  )

  baseline_summary <- data.frame(
    n_random_per_replicate = sample_size,
    n_repeats_requested = n_repeats,
    n_repeats_used = effective_repeats,

    mean_distance =
      mean(baseline_runs$mean_distance),

    mean_distance_sd =
      if (effective_repeats > 1L) {
        stats::sd(
          baseline_runs$mean_distance
        )
      } else {
        0
      },

    median_distance =
      stats::median(
        baseline_runs$median_distance
      ),

    fraction_d0 =
      mean(baseline_runs$fraction_d0),

    fraction_d1 =
      mean(baseline_runs$fraction_d1),

    fraction_d2 =
      mean(baseline_runs$fraction_d2),

    fraction_le1 =
      mean(baseline_runs$fraction_le1),

    fraction_le2 =
      mean(baseline_runs$fraction_le2),

    fraction_ge3 =
      mean(baseline_runs$fraction_ge3),

    stringsAsFactors = FALSE
  )

  # ------------------------------------------------------------------
  # 5. Drug-target diagnostics
  # ------------------------------------------------------------------

  drug_summary <- NULL
  target_mapping_by_drug <- NULL

  if (!is.null(drug_targets)) {

    drug_df <- clean_pairs(
      drug_targets,
      c(drug_col, target_col),
      c("Drug", "Target"),
      "drug_targets"
    )

    if (!nrow(drug_df)) {
      stop(
        "No valid drug-target pairs remain after cleaning.",
        call. = FALSE
      )
    }

    drug2targets <- lapply(
      split(
        drug_df$Target,
        drug_df$Drug
      ),
      unique
    )

    mapped_targets <- lapply(
      drug2targets,
      intersect,
      nodes
    )

    input_sizes <- lengths(
      drug2targets
    )

    mapped_sizes <- lengths(
      mapped_targets
    )

    scorable <- mapped_sizes > 0
    scorable_sizes <- mapped_sizes[
      scorable
    ]

    all_input_targets <- unique(
      unlist(
        drug2targets,
        use.names = FALSE
      )
    )

    all_mapped_targets <- unique(
      unlist(
        mapped_targets,
        use.names = FALSE
      )
    )

    target_mapping_fraction <-
      length(all_mapped_targets) /
      length(all_input_targets)

    target_degree <- degree_summary(
      deg[all_mapped_targets]
    )

    target_degree_enrichment <- unname(
      target_degree["mean"] /
        network_degree["mean"]
    )

    seed_target_genes <- intersect(
      all_mapped_targets,
      seed_in_lcc
    )

    drug_seed_overlap <- vapply(
      mapped_targets,
      function(x) {
        any(x %in% seed_in_lcc)
      },
      logical(1)
    )

    target_mapping_by_drug <- data.frame(
      Drug = names(drug2targets),
      input_targets = input_sizes,
      mapped_targets = mapped_sizes,
      mapping_fraction =
        mapped_sizes / input_sizes,
      scorable = scorable,
      direct_seed_overlap =
        drug_seed_overlap,
      stringsAsFactors = FALSE
    )

    drug_summary <- data.frame(
      n_drugs =
        length(mapped_targets),

      n_scorable_drugs =
        sum(scorable),

      scorable_drug_fraction =
        mean(scorable),

      unique_input_targets =
        length(all_input_targets),

      unique_mapped_targets =
        length(all_mapped_targets),

      target_mapping_fraction =
        target_mapping_fraction,

      median_input_targets =
        stats::median(input_sizes),

      median_mapped_targets_all =
        stats::median(mapped_sizes),

      median_mapped_targets_scorable =
        value_or_na(
          scorable_sizes,
          stats::median
        ),

      mean_mapped_targets_scorable =
        value_or_na(
          scorable_sizes,
          mean
        ),

      zero_target_drug_fraction =
        mean(mapped_sizes == 0),

      single_target_fraction_all =
        mean(mapped_sizes == 1),

      single_target_fraction_scorable =
        value_or_na(
          scorable_sizes == 1,
          mean
        ),

      multi_target_fraction_scorable =
        value_or_na(
          scorable_sizes >= 2,
          mean
        ),

      target_degree_enrichment =
        target_degree_enrichment,

      target_degree_cv =
        unname(
          target_degree["cv"]
        ),

      target_degree_p95 =
        unname(
          target_degree["p95"]
        ),

      target_degree_p99 =
        unname(
          target_degree["p99"]
        ),

      seed_target_gene_overlap =
        length(seed_target_genes),

      seed_target_gene_overlap_fraction =
        length(seed_target_genes) /
        length(seed_in_lcc),

      drugs_with_seed_overlap =
        sum(drug_seed_overlap),

      drugs_with_seed_overlap_fraction =
        mean(drug_seed_overlap),

      stringsAsFactors = FALSE
    )
  }

  # ------------------------------------------------------------------
  # 6. Diffusion implementation feasibility
  # ------------------------------------------------------------------

  gb_per_matrix <-
    (as.double(n_lcc)^2 * 8) /
    1024^3

  four_matrix_storage_gb <-
    4 * gb_per_matrix

  approximate_working_memory_gb <-
    5 * gb_per_matrix

  diffusion_memory <- data.frame(
    nodes = n_lcc,

    gb_per_float64_matrix =
      gb_per_matrix,

    four_matrix_storage_gb =
      four_matrix_storage_gb,

    approximate_working_memory_gb =
      approximate_working_memory_gb,

    memory_limit_gb =
      if (is.null(memory_limit_gb)) {
        NA_real_
      } else {
        memory_limit_gb
      },

    exceeds_memory_limit =
      if (is.null(memory_limit_gb)) {
        NA
      } else {
        approximate_working_memory_gb >
          memory_limit_gb
      },

    estimate_type =
      "Approximate; implementation-specific",

    stringsAsFactors = FALSE
  )

  # ------------------------------------------------------------------
  # 7. Compact method-specific diagnostic summaries
  # ------------------------------------------------------------------

  has_drugs <- !is.null(
    drug_summary
  )

  not_provided <- "Not provided"

  proximity_drug_value <- if (!has_drugs) {
    not_provided
  } else {
    sprintf(
      paste0(
        "mapped %.1f%%; scorable %.1f%%; ",
        "median %.1f targets; %.1f%% single-target ",
        "among scorable drugs"
      ),
      pct(
        drug_summary$target_mapping_fraction
      ),
      pct(
        drug_summary$scorable_drug_fraction
      ),
      drug_summary$median_mapped_targets_scorable,
      pct(
        drug_summary$single_target_fraction_scorable
      )
    )
  }

  overlap_value <- if (!has_drugs) {
    not_provided
  } else {
    sprintf(
      "%d seed-target genes; %.1f%% drugs with direct overlap",
      drug_summary$seed_target_gene_overlap,
      pct(
        drug_summary$drugs_with_seed_overlap_fraction
      )
    )
  }

  diffusion_mapping_value <- if (!has_drugs) {
    not_provided
  } else {
    sprintf(
      "target %.1f%%; scorable drugs %.1f%%",
      pct(
        drug_summary$target_mapping_fraction
      ),
      pct(
        drug_summary$scorable_drug_fraction
      )
    )
  }

  diffusion_degree_value <- if (!has_drugs) {
    sprintf(
      "seed enrichment %.2fx; seed CV %.2f",
      seed_degree_enrichment,
      seed_degree["cv"]
    )
  } else {
    sprintf(
      "seed %.2fx/CV %.2f; target %.2fx/CV %.2f",
      seed_degree_enrichment,
      seed_degree["cv"],
      drug_summary$target_degree_enrichment,
      drug_summary$target_degree_cv
    )
  }

  diffusion_size_value <- if (!has_drugs) {
    not_provided
  } else {
    sprintf(
      "median %.1f; %.1f%% single-target among scorable drugs",
      drug_summary$median_mapped_targets_scorable,
      pct(
        drug_summary$single_target_fraction_scorable
      )
    )
  }

  proximity_checks <- data.frame(
    diagnostic = c(
      "Largest connected component",
      "Seed mapping",
      "Baseline nearest-seed topology",
      "Network degree heterogeneity",
      "Seed degree profile",
      "Drug-target mapping/size",
      "Seed-target overlap"
    ),

    value = c(
      sprintf(
        "%.1f%%",
        pct(lcc_fraction)
      ),

      sprintf(
        "%.1f%%",
        pct(seed_mapping)
      ),

      sprintf(
        paste0(
          "mean %.2f; median %.2f; ",
          "<=1 hop %.1f%%; <=2 hops %.1f%%"
        ),
        baseline_summary$mean_distance,
        baseline_summary$median_distance,
        pct(
          baseline_summary$fraction_le1
        ),
        pct(
          baseline_summary$fraction_le2
        )
      ),

      sprintf(
        "CV %.2f; P95 %.0f; P99 %.0f; max/median %.1f",
        network_degree["cv"],
        network_degree["p95"],
        network_degree["p99"],
        hub_ratio
      ),

      sprintf(
        "enrichment %.2fx; CV %.2f; P99 %.0f",
        seed_degree_enrichment,
        seed_degree["cv"],
        seed_degree["p99"]
      ),

      proximity_drug_value,

      overlap_value
    ),

    stringsAsFactors = FALSE
  )

  diffusion_checks <- data.frame(
    diagnostic = c(
      "Largest connected component",
      "Seed mapping",
      "Drug-target mapping",
      "Network degree heterogeneity",
      "Seed/target degree profiles",
      "Drug-target size",
      "Seed-target overlap",
      "Dense matrix requirement"
    ),

    value = c(
      sprintf(
        "%.1f%%",
        pct(lcc_fraction)
      ),

      sprintf(
        "%.1f%%",
        pct(seed_mapping)
      ),

      diffusion_mapping_value,

      sprintf(
        "CV %.2f; P99 %.0f; max/median %.1f",
        network_degree["cv"],
        network_degree["p99"],
        hub_ratio
      ),

      diffusion_degree_value,

      diffusion_size_value,

      overlap_value,

      sprintf(
        "%.2f GB/matrix; ~%.2f GB approximate working memory",
        gb_per_matrix,
        approximate_working_memory_gb
      )
    ),

    stringsAsFactors = FALSE
  )

  # ------------------------------------------------------------------
  # Return
  # ------------------------------------------------------------------

  list(

    network = data.frame(
      nodes_full = n_full,
      nodes_lcc = n_lcc,
      edges_lcc =
        igraph::ecount(g),

      components =
        comp$no,

      lcc_fraction =
        lcc_fraction,

      mean_degree =
        unname(
          network_degree["mean"]
        ),

      median_degree =
        unname(
          network_degree["median"]
        ),

      degree_cv =
        unname(
          network_degree["cv"]
        ),

      degree_p95 =
        unname(
          network_degree["p95"]
        ),

      degree_p99 =
        unname(
          network_degree["p99"]
        ),

      max_degree =
        unname(
          network_degree["max"]
        ),

      hub_ratio =
        hub_ratio,

      stringsAsFactors = FALSE
    ),

    seeds = data.frame(
      input_seeds =
        length(seed_input),

      seeds_in_lcc =
        length(seed_in_lcc),

      mapping_fraction =
        seed_mapping,

      network_coverage =
        seed_coverage,

      degree_enrichment =
        seed_degree_enrichment,

      degree_mean =
        unname(
          seed_degree["mean"]
        ),

      degree_median =
        unname(
          seed_degree["median"]
        ),

      degree_cv =
        unname(
          seed_degree["cv"]
        ),

      degree_p95 =
        unname(
          seed_degree["p95"]
        ),

      degree_p99 =
        unname(
          seed_degree["p99"]
        ),

      stringsAsFactors = FALSE
    ),

    baseline_nearest_seed = list(
      summary =
        baseline_summary,

      pooled_distance_summary =
        as.data.frame(
          t(baseline_distance)
        ),

      replicate_results =
        baseline_runs,

      note = paste(
        "Uniform random-node topology screen;",
        "not the degree-matched proximity null."
      )
    ),

    drugs =
      drug_summary,

    target_mapping_by_drug =
      target_mapping_by_drug,

    diffusion_memory =
      diffusion_memory,

    diagnostics = list(
      network_proximity =
        proximity_checks,

      diffusion =
        diffusion_checks
    )
  )
}
