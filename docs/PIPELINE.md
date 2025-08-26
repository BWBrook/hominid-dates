# Pipeline Overview

This project uses the `{targets}` package to define a fully reproducible workflow.

Key points:
- Deterministic seed via `targets::tar_option_set(seed = 1L)`.
- All helpers live under `R/` and are sourced with `targets::tar_source("R")`.
- Portable paths via `here::here()` and file targets with `format = "file"`.

## DAG Summary (high level)

- 1. Data ingest
  - `raw_csv` (file) → `raw_tbl`
- 2. Occurrence normalisation + clustering
  - `occ_tbl` → `st_clusters` → `mixing_index_by_cluster` → plots/CSVs
  - `occ_tbl` → `occ_tbl_no_indet`
- 3. Summaries (point estimates)
  - `genus_species_freq`, `temporal_span`, `overlap_matrix`
- 3b. Uncertainty via Monte Carlo
  - `mc_draws` → `spans_mc`, `overlap_mc`, `bin_counts_mc` (+ sensitivity, convergence)
- 4. Artefacts
  - Tables and plots written under `outputs/` via `write_output()` / `write_plot()`
  - Range dynamics: `range_metrics` + per-species CSVs and centroid maps
- 4c. Lead–lag analysis
  - `leadlag_country` → rankings/maps
- 5. Site re-occupancy analysis
  - `country_intensity_bins` → `site_lambda` → `reocc_pvals` → `likely_reoccupancy_events`
- 6. Report & Vignette
  - `pdf_report` renders the Quarto report to `docs/report.pdf`
  - `usage_pdf` renders the Usage vignette to `docs/USAGE.pdf`

For a visual DAG, run:

```r
# In RStudio Console
targets::tar_visnetwork()
```
