# Internal registry helpers for network pipeline method configuration.
# These centralize method lists and method-specific settings used by
# drugNetworkPipeline() so the pipeline body stays focused on orchestration.

.network_centrality_registry <- function(trust_hub_penalty,
                                         harmonic_hub_penalty,
                                         damping_factor) {
  list(
    TrustRank = list(
      method_name = "TrustRank",
      hub_penalty_value = trust_hub_penalty,
      damping_factor_value = damping_factor
    ),
    Harmonic_centrality = list(
      method_name = "Harmonic_centrality",
      hub_penalty_value = harmonic_hub_penalty,
      damping_factor_value = NULL
    ),
    Degree_centrality = list(
      method_name = "Degree_centrality",
      hub_penalty_value = NULL,
      damping_factor_value = NULL
    )
  )
}

.network_aggregation_registry <- function() {
  list(
    CRank = list(fun = CRank, reverse = TRUE),
    Dowdall = list(fun = Dowdall, reverse = FALSE),
    RRA = list(fun = RRA, reverse = FALSE)
  )
}

.network_diffusion_rank_cols <- function() {
  c(
    "DSD-min-Rank",
    "KL-med-Rank",
    "KL-min-Rank",
    "JS-med-Rank",
    "JS-min-Rank"
  )
}
