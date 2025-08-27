
# Hominid Dates – Spatio‑Temporal Overlap of African Hominins

A fully reproducible **targets + renv** compendium. It ingests a cleaned fossil database (`data/hominid.csv`), collapses duplicate fragments into single occurrences, estimates sampling intensity, performs spatio‑temporal clustering, propagates date uncertainty, and exports summaries and figures. A Quarto report (docs/report.qmd → docs/report.pdf) ties the results together.

> Scope (v0.1)
> Point‑estimate summaries, Monte‑Carlo uncertainty, clustering, lead–lag, and site re‑occupancy diagnostics. Modelling extensions (e.g., OLE, occupancy) follow.

---

## Directory

```
hominid-dates/
├── _targets.R                  # pipeline (deterministic seed, file targets)
├── R/                          # helpers (explicit imports, one per file)
├── inst/extdata/hominid_sample.csv  # tiny sample for tests/examples
├── docs/
│   ├── CHANGELOG.md            # Keep a Changelog
│   ├── PIPELINE.md             # DAG overview and notes
│   ├── DEVELOPMENT.qmd         # setup + dev notes
│   └── report.qmd              # Quarto source → docs/report.pdf
├── metadata/data_manifest.csv  # input schema documentation
├── data/hominid.csv            # cleaned hominid dates database
├── outputs/                    # generated CSVs/PNGs (git‑ignored)
├── tests/testthat/             # unit tests (compendium‑style)
├── dependencies.R              # project_dependencies() for pak
└── README.md
```

---

## Setup

RStudio (recommended):

Open the `hominid-dates.Rproj` file, then in the Console run:

```r
renv::restore(prompt = FALSE)
source("dependencies.R")
pak::pkg_install(project_dependencies())
```

Shell alternative:

```bash
R -e 'renv::restore(prompt=FALSE); source("dependencies.R"); pak::pkg_install(project_dependencies())'
```

Note: Do NOT run `bootstrap_env.R` for a cloned repo with an existing `renv.lock`.
That script is for bootstrapping a brand‑new project (no lockfile) and is kept for
historical/one‑off use only.

---

## Pipeline

Build and inspect:

```r
targets::tar_make()                 # full build
targets::tar_visnetwork()           # interactive DAG
targets::tar_read(genus_species_freq)
```

Selected targets (high‑level):

- raw_csv (file) → raw_tbl → occ_tbl (deduplicated, with sampling_intensity)
- st_clusters → mixing_index_by_cluster (+ CSV/PNG)
- occ_tbl_no_indet → genus_species_freq, temporal_span, overlap_matrix
- mc_draws → spans_mc, overlap_mc, bin_counts_mc (with sensitivity + convergence)
- range_metrics → per‑species CSVs + centroid track PNGs
- leadlag_country → rankings + choropleths
- site_lambda → reocc_pvals → likely_reoccupancy_events
- beta_turnover_tbl → beta_turnover_plot_file (temporal β diversity at key localities)
- lazarus_tbl → lazarus_plot_rank_file (within‑range gaps vs effort)
- bin_sense_results → bin_sense_series_file + bin_sense_stability_file (width stability)
- pdf_report (docs/report.pdf)

Validate:

```r
targets::tar_validate()
```

---

## Data

- Input: `data/hominid.csv` (user‑provided). See `metadata/data_manifest.csv` for fields and examples.
- Paths: resolved via `here::here()`; no `setwd()`.
- Example: `inst/extdata/hominid_sample.csv` is a tiny synthetic dataset for tests.

---

## Tests

Run all unit tests (fast, compendium‑style):

```r
testthat::test_dir("tests/testthat", reporter = "summary")
```

---

## Report & Docs

- Quarto source: `docs/report.qmd` → output: `docs/report.pdf`.
- Render docs:

```bash
quarto render docs/
```

- Minimal usage vignette: see `docs/USAGE.qmd` for a quick tour (setup, build, and how to read targets and artefacts).
 - The pipeline also builds `docs/USAGE.pdf` via the `usage_pdf` target.

---

## Outputs

Generated under `outputs/` by file targets (examples):

- Tables: `genus_freq.csv`, `species_freq.csv`, `temporal_span.csv`, `overlap_matrix.csv`,
  `spans_mc_ci.csv`, `overlap_mc_ci.csv`, `bin_counts_mc_ci.csv`,
  `st_clusters.csv`, `mixing_index_by_cluster.csv`, `site_lambda.csv`,
  `reocc_pvals.csv`, `likely_reoccupancy_events.csv`, `range_metrics.csv`,
  `range_metrics_<species>.csv` (per‑species), `beta_turnover.csv`,
  `leadlag_country.csv`, `leadlag_ranks.csv`,
  `lazarus_gaps.csv`, `bin_sense_summary.csv`.
- Plots: `genus_freq.png`, `species_freq.png`, `temporal_span.png`,
  `temporal_span_mc.png`, `overlap_heatmap.png`, `overlap_heatmap_mc.png`,
  `bin_counts_mc.png`, `mixing_index_by_cluster.png`, `beta_turnover.png`,
  `centroid_track_<species>.png`, `leadlag_delta_fad_<species>.png`,
  `lazarus_rank.png`, `bin_sense_series.png`, `bin_sense_stability.png`.
- Report: `docs/report.pdf`.

All file targets declare `format = "file"` to ensure correct change tracking.

---

## Reproducibility

- Environment: `renv` lockfile with optional `pak` acceleration.
- Pipeline: `{targets}` with deterministic seed (`tar_option_set(seed = 1L)`).
- Imports: explicit via `import::from()` or fully qualified calls.
- Style: `{lintr}` + `{styler}`; see `.lintr`.

Helpful checks:

```r
lintr::lint_package()
targets::tar_validate()
```

---

## Roadmap

1. Solow OLE per species (continental).
2. Site‑level occupancy state‑space models using `sampling_intensity`.
3. Interactive explorer for spatio‑temporal dynamics.

Contributions welcome – feel free to open an issue or PR.
