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
 - Usage vignette `docs/USAGE.qmd` and pipeline target `usage_pdf` (renders to `docs/USAGE.pdf`).

### Changed
- `.gitignore` now tracks `docs/` content (ignores `docs/_site/` and `.quarto/`).
- Error/warning handling uses `rlang::abort()` and `cli::cli_warn()` in key helpers.
- Default `{targets}` `callr` strategy set to `never` via `_targets.yaml` (stability on Windows during Quarto renders).

### Fixed
- None yet.
