# _targets.R ----------------------------------------------------------------------
# Declarative pipeline: flat list of tar_target() calls.

import::from("targets", tar_option_set, tar_target)
import::from("here", here)

# helper functions ---------------------------------------------------------------
import::here(read_hominid,       .from = "R/read_hominid.R")
import::here(collapse_fragments, .from = "R/collapse_fragments.R")
import::here(summarise_freq,     .from = "R/summarise_freq.R")
import::here(summarise_spans,    .from = "R/summarise_spans.R")
import::here(calc_overlap,       .from = "R/calc_overlap.R")
import::here(write_output,       .from = "R/write_outputs.R")

tar_option_set(
  packages = c("dplyr", "tidyr", "here", "readr", "tibble", "purrr"),
  format   = "rds"                     # default for non‑file objects
)

list(
  # 1 raw data ----
  tar_target(
    raw_csv,
    here("data", "hominid.csv"),
    format = "file"
  ),

  tar_target(
    raw_tbl,
    read_hominid(raw_csv)
  ),

  # 2 deduplicate fragments ----
  tar_target(
    occ_tbl,
    collapse_fragments(raw_tbl)
  ),

  # 3 summaries ----
  tar_target(
    genus_species_freq,
    summarise_freq(occ_tbl)
  ),

  tar_target(
    temporal_span,
    summarise_spans(occ_tbl)
  ),

  tar_target(
    overlap_matrix,
    calc_overlap(temporal_span)
  ),

  # 4 write artefacts to /outputs ----
  tar_target(
    genus_species_freq_csv,
    write_output(genus_species_freq, "genus_species_freq.csv"),
    format = "file"
  ),

  tar_target(
    temporal_span_csv,
    write_output(temporal_span, "temporal_span.csv"),
    format = "file"
  ),

  tar_target(
    overlap_matrix_csv,
    write_output(overlap_matrix, "overlap_matrix.csv"),
    format = "file"
  )
)
