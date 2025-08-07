# _targets.R -----------------------------------------------------------------------
# Declarative pipeline: flat list of tar_target() calls.
import::from("targets", tar_option_set, tar_target)
import::from("quarto", quarto_render)
import::from("here", here)

# helper functions -----------------------------------------------------------------
import::here(read_hominid,        .from = "R/read_hominid.R")
import::here(collapse_fragments,  .from = "R/collapse_fragments.R")
import::here(summarise_freq,      .from = "R/summarise_freq.R")
import::here(summarise_spans,     .from = "R/summarise_spans.R")
import::here(calc_overlap,        .from = "R/calc_overlap.R")
import::here(write_output,        .from = "R/write_outputs.R")
import::here(filter_indet,        .from = "R/filter_indet.R")

import::here(plot_genus_freq, 
             plot_species_freq,    .from = "R/plot_genus_species_freq.R")
import::here(plot_temporal_span,   .from = "R/plot_temporal_span.R")
import::here(plot_overlap_heatmap, .from = "R/plot_overlap_heatmap.R")
import::here(write_plot,           .from = "R/write_plot.R")

tar_option_set(
  packages = c("dplyr", "tidyr", "here", "readr", "tibble", "purrr", 
               "ggplot2", "forcats", "scales", "kableExtra"),
  format   = "rds"
)

list(
  # 1 raw data ---------------------------------------------------------------------
  tar_target(
    raw_csv,
    here("data", "hominid.csv"),
    format = "file"
  ),
  tar_target(
    raw_tbl,
    read_hominid(raw_csv)
  ),
  # 2 deduplicate fragments --------------------------------------------------------
  tar_target(
    occ_tbl,
    collapse_fragments(raw_tbl)
  ),
  # 2b drop indeterminates ----
  tar_target(
    occ_tbl_no_indet,
    filter_indet(occ_tbl)
  ),
  # 3 summaries --------------------------------------------------------------------
  tar_target(
    genus_species_freq,
    summarise_freq(occ_tbl_no_indet)
  ),
  tar_target(
    temporal_span,
    summarise_spans(occ_tbl_no_indet)
  ),
  tar_target(
    overlap_matrix,
    calc_overlap(temporal_span)
  ),
  # 4 write artefacts to /outputs --------------------------------------------------
  tar_target(
    genus_freq_csv,
    write_output(genus_species_freq$genus_freq, "genus_freq.csv"),
    format = "file"
  ),
  tar_target(
    species_freq_csv,
    write_output(genus_species_freq$species_freq, "species_freq.csv"),
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
  ),
  # ---- plotting targets ----------------------------------------------------------
  tar_target(
    genus_plot,
    plot_genus_freq(genus_species_freq$genus_freq)
  ),
  tar_target(
    genus_plot_file,
    write_plot(genus_plot, "genus_freq.png"),
    format = "file"
  ),
  tar_target(
    species_plot,
    plot_species_freq(genus_species_freq$species_freq)
  ),
  tar_target(
    species_plot_file,
    write_plot(species_plot, "species_freq.png"),
    format = "file"
  ),
  tar_target(
    span_plot,
    plot_temporal_span(temporal_span)
  ),
  tar_target(
    span_plot_file,
    write_plot(span_plot, "temporal_span.png"),
    format = "file"
  ),
  tar_target(
    overlap_plot,
    plot_overlap_heatmap(overlap_matrix)
  ),
  tar_target(
    overlap_plot_file,
    write_plot(overlap_plot, "overlap_heatmap.png"),
    format = "file"
  ),
  # ---- Quarto report -------------------------------------------------------------
  tar_target(
    pdf_report,
    {
      quarto_render(
        input  = "report.qmd",
        output_format = "pdf",
        execute_params = list()
      )
      "report.pdf"
    },
    format = "file",
    cue = targets::tar_cue(mode = "always")   # re-render every run
  )

)
