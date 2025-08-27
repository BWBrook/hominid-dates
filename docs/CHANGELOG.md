# Changelog

All notable changes to this project will be documented in this file.

The format is based on Keep a Changelog, and this project adheres to Semantic Versioning.

## [Unreleased]
### Added
- Targets pipeline refactor: use `targets::tar_source("R")`, fully qualified calls, and deterministic `seed = 1L`.
- `dependencies.R` bootstrap with `project_dependencies()`.
- Documentation scaffolding: `docs/PIPELINE.md`, `docs/AGENT_NOTES.md`, `docs/DEVELOPMENT.qmd`.
- Linting config `.lintr` and `.Rbuildignore` entries.
- Tests skeleton under `tests/testthat/` with sample data in `inst/extdata/`.
- Report rendering helper `R/report.R` (renders to `docs/report.pdf`).
 - Temporal beta diversity analysis: `build_incidence_by_cluster()`, `beta_turnover_by_cluster()` with plots and targets.
 - Usage vignette `docs/USAGE.qmd` and pipeline target `usage_pdf` (renders to `docs/USAGE.pdf`).
 - Lazarus gaps module: `effort_time_series()`, `lazarus_all()` with CSV export `outputs/lazarus_gaps.csv` and rank plot `outputs/lazarus_rank.png`; report section added.
 - Usage vignette `docs/USAGE.qmd` and pipeline target `usage_pdf` (renders to `docs/USAGE.pdf`).

### Changed
- `.gitignore` now tracks `docs/` content (ignores `docs/_site/` and `.quarto/`).
- Error/warning handling uses `rlang::abort()` and `cli::cli_warn()` in key helpers.
- Default `{targets}` `callr` strategy set to `never` via `_targets.yaml` (stability on Windows during Quarto renders).
- `bootstrap_env.R` now aborts early if `renv.lock` exists to prevent misuse on cloned repos.
 - Report formatting: wrap long interpretation text, deduplicate symmetric overlap bullets, vectorize bullet rendering, suppress empty highlight sections, and round `vel_km_per_Ma` in range table.

### Fixed
- None yet.
