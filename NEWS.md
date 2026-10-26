# DrugSigNet news

## DrugSigNet 1.0.0

### Initial release

- Added signature-based, network-based, and integrated drug-repurposing
  workflows.

### Documentation

- Reworked `README.md` to prioritize onboarding and quick-start usage.
- Added ten package vignettes covering setup, analysis selection, methods,
  annotation, visualization, end-to-end workflows, and an integrated case
  study.

### Installation

- Declared optional `synapser` support and its repository in `DESCRIPTION`.
- Made configure scripts side-effect free so staged installation and vignette
  builds never alter packages in the user's libraries.
- Added `setup_synapser()` as an explicit, verified retry path when the optional
  Synapse repository was unavailable during the main package installation.
- Install Synapser on first use rather than through `dependencies = TRUE`, and
  install its compatible `rjson 0.2.21` archive first. This avoids CRAN selecting
  incompatible `rjson 0.2.23` and avoids rebuilding dependency vignettes.
- Treat `enrichR` as a runtime-checked optional dependency so it does not keep
  an incompatible `rjson` namespace loaded when Synapser setup replaces it.

## Scientific PDF reports

* `write_report(device = "pdf")` now presents a concise scientific report with
  verified candidate counts, explicit deterministic Top-N selection and CRank
  tie auditing, ranking diagnostics, prioritized annotations, globally selected
  enrichment terms, and compact method/input/software summaries.
* Stored ranks and prioritization algorithms are unchanged. HTML/Shiny retain
  their existing detailed views. PDF tables wrap long identifiers and repeat
  headers across pages; figures stay with their sections and unavailable static
  exports receive an informative fallback.
* New optional `top_n` (10), `enrichment_n` (12), and `supplementary` (FALSE)
  arguments control PDF detail and complete original-object RDS exports.
