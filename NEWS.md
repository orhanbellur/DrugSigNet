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

## Signature ranking identity correction

* Signature pipelines now normalize LINCS2 identifiers using the effective
  reference mode. Parallel default-mode runs resolve frozen references and must
  apply frozen identifier normalization before filtering and rank scoring.
* Unmapped frozen perturbation IDs remain distinct instead of becoming a shared
  missing identifier. Method score formulas, significance thresholds, tie rules,
  and aggregation directions are unchanged from main.
