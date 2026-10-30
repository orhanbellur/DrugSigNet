# DrugSigNet news

## DrugSigNet 1.0.0

### Fixes

- Initialize ExperimentHub metadata and signature-search target annotations before
  dispatching workers, with a cross-process initialization lock. Repair duplicate
  `experimenthub.sqlite3` registrations with metadata backups while preserving
  downloaded resources. Frozen references still require these annotations.

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

### Scientific PDF reports

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

* Explicit `figure_dpi` and section/layout controls now use the concise PDF
  renderer, fixing the legacy routing regression that produced raw-data tables
  and repeated `tab:analyses` labels. Resolution and section controls are honored
  without changing candidate selection or stored ranks.

* Restored a linked contents page and PDF outline. All stored pipeline figures
  are included once under the relevant scientific section, including wrapped
  plot objects. Unavailable Plotly sunbursts get a labelled static branch-summary
  fallback; export notes no longer render as console output.

* Scientific HTML reports now use a responsive sidebar layout with verified
  summary cards, input/rank/annotation coverage, a candidate shortlist, ranking
  agreement, focused enrichment, interpretation, and compact provenance.
* Full results have searchable, sortable, paginated native HTML tables with
  expandable cells and CSV downloads across all matching rows. The previous
  200-row preview limit no longer hides later rows from table search/export.
* Standalone HTML no longer requires Shiny or DT; the existing Shiny dashboard
  remains available. Native Plotly figures are retained and `interactive` is
  honored for ggplot conversion. Scientific summaries share PDF extraction and
  tie handling without modifying rankings.
### Signature ranking identity correction

* Signature pipelines now normalize LINCS2 identifiers using the effective
  reference mode. Parallel default-mode runs resolve frozen references and must
  apply frozen identifier normalization before filtering and rank scoring.
* Unmapped frozen perturbation IDs remain distinct instead of becoming a shared
  missing identifier. Method score formulas, significance thresholds, tie rules,
  and aggregation directions are unchanged from main.

### Python 3.12 installation default

* New Conda, Pixi/manifest and Docker environments now select Python 3.12;
  Windows CI uses the same version. Remove legacy NumPy/pandas pins that cannot
  support Python 3.12 from the local and Docker installers.
* Existing complete runtimes remain reusable and are not upgraded in place.
  Select a separate Python 3.12 interpreter when migrating an existing project.
* Cross-runtime checks confirm deterministic CRank/diffusion agreement on
  controlled fixtures. Randomized proximity backgrounds can differ across
  Python versions despite a fixed seed because collection iteration order can
  change; retain runtime versions when reproducing those statistics.

### Isolated network Python runtime

* Run graph construction, centrality, proximity and diffusion through the
  configured Python in separate processes. This fixes graph-tool GLIBCXX
  import failures when host RStudio has already loaded an older system C++
  runtime, despite successful Pixi installation validation. Existing Python
  algorithms and rank aggregation are unchanged.
* Default worker hash seeding to zero (unless explicitly configured) so a
  fixed simulation seed remains reproducible across fresh worker processes.
* Preserve typed inputs, leading-zero identifiers and complete result tables;
  diffusion target sets now contain serializable R vectors. Worker failures
  report the selected interpreter and the native error instead of partial data.
* Clarify that activating a Pixi package library does not replace the running
  R executable; document clean session restart after reinstallation.
