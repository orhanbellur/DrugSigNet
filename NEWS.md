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
- Declared `synapser` as an official GitHub remote so dependency-aware installs
  no longer rely on the unavailable legacy Synapse R package index.
