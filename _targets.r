# _targets.R -----------------------------------------------------------------------
# Declarative pipeline: flat list of tar_target() calls.
import::from("targets", tar_option_set, tar_target)
import::from("quarto", quarto_render)
import::from("here", here)

# helper functions -----------------------------------------------------------------
import::here(read_hominid,          .from = "R/read_hominid.R")
import::here(collapse_fragments,    .from = "R/collapse_fragments.R")
import::here(summarise_freq,        .from = "R/summarise_freq.R")
import::here(summarise_spans,       .from = "R/summarise_spans.R")
import::here(calc_overlap,          .from = "R/calc_overlap.R")
import::here(write_output,          .from = "R/write_outputs.R")
import::here(filter_indet,          .from = "R/filter_indet.R")
import::here(mc_dates,              .from = "R/mc_dates.R")
import::here(spans_from_draws,      .from = "R/spans_from_draws.R")
import::here(overlap_from_draws,    .from = "R/overlap_from_draws.R")
import::here(bin_counts_from_draws, 
             bin_sensitivity,       .from = "R/bin_counts_mc.R")
import::here(mc_ci_convergence,     .from = "R/mc_checks.R")

import::here(plot_genus_freq, 
             plot_species_freq,     .from = "R/plot_genus_species_freq.R")
import::here(plot_temporal_span,    .from = "R/plot_temporal_span.R")
import::here(plot_overlap_heatmap,  .from = "R/plot_overlap_heatmap.R")
import::here(write_plot,            .from = "R/write_plot.R")
import::here(plot_spans_ci, plot_overlap_heatmap_mc, 
             plot_bin_counts_mc,    .from = "R/plot_mc.R")
import::here(country_intensity, estimate_site_lambda, compute_reocc_pvals,
             likely_reoccupancy, plot_reocc_pvals, .from = "R/reoccupancy.R")
import::here(st_cluster, mixing_index_from_st, plot_mixing_index,
             .from = "R/st_cluster.R")

tar_option_set(
  packages = c("dplyr", "tidyr", "here", "readr", "tibble", "purrr", 
               "ggplot2", "forcats", "scales", "kableExtra", "dbscan"),
  format   = "rds"
)

list(
  # -----------------------------------------------------------------------------
  # 1. RAW DATA INGEST
  # Purpose: establish a single typed source of truth from CSV.
  # Why first: all downstream steps derive from the canonical input table.
  # -----------------------------------------------------------------------------
  # Path to the cleaned input CSV file.
  tar_target(
    raw_csv,
    here("data", "hominid.csv"),
    format = "file"
  ),
  # Read the hominid CSV into a typed tibble.
  tar_target(
    raw_tbl,
    read_hominid(raw_csv)
  ),
  # -----------------------------------------------------------------------------
  # 2. OCCURRENCE NORMALISATION
  # Purpose: collapse duplicate fragments into unique occurrences while
  #          recording sampling_intensity as an effort covariate.
  # Why here: creates the base occurrence table used by both summaries and
  #           clustering; keeps spatial and temporal fields intact.
  # -----------------------------------------------------------------------------
  # Collapse duplicate fragments to unique occurrences; add sampling_intensity.
  tar_target(
    occ_tbl,
    collapse_fragments(raw_tbl)
  ),
  # 2b SPATIO‑TEMPORAL CLUSTERING (site definition)
  # Purpose: define robust site clusters in joint (lon, lat, date) space
  #          using HDBSCAN; label noise as NA cluster_id.
  # Why now: site clusters are consumed by downstream re‑occupancy analysis;
  #          summaries still operate on species‑filtered occurrences.
  # Tuning: minPts controls minimum cluster size; date_weight downweights time.
  # -----------------------------------------------------------------------------
  # Cluster occurrences in (lon, lat, date) space; add cluster_id and sort.
  tar_target(
    st_clusters,
    st_cluster(occ_tbl, minPts = 4, date_weight = 0.5)
  ),
  # CSV export: per-observation clusters (sorted; NA cluster_id last).
  tar_target(
    st_clusters_csv,
    write_output(st_clusters, "st_clusters.csv"),
    format = "file"
  ),
  # Derived: cluster-wise mixing index (temporal span).
  tar_target(
    mixing_index_by_cluster,
    mixing_index_from_st(st_clusters)
  ),
  # CSV export: mixing index per cluster.
  tar_target(
    mixing_index_by_cluster_csv,
    write_output(mixing_index_by_cluster, "mixing_index_by_cluster.csv"),
    format = "file"
  ),
  # Plot: mixing index lollipop and PNG export.
  tar_target(
    mixing_index_plot,
    plot_mixing_index(mixing_index_by_cluster)
  ),
  tar_target(
    mixing_index_plot_file,
    write_plot(mixing_index_plot, "mixing_index_by_cluster.png"),
    format = "file"
  ),
  # 2c DROP INDETERMINATES
  # Purpose: remove species == "indet" for taxonomic summaries and MC steps.
  # Note: re‑occupancy uses clusters + filter_indet() at call sites below.
  # -----------------------------------------------------------------------------
  tar_target(
    occ_tbl_no_indet,
    filter_indet(occ_tbl)
  ),
  # -----------------------------------------------------------------------------
  # 3. SUMMARIES (point estimates)
  # Purpose: count specimens and derive first/last appearances by species.
  # Why after filtering: taxonomic summaries exclude indeterminate rows.
  # Dependencies: occ_tbl_no_indet → temporal_span → overlap_matrix.
  # -----------------------------------------------------------------------------
  # Specimen counts by genus and species (weighting by sampling_intensity).
  tar_target(
    genus_species_freq,
    summarise_freq(occ_tbl_no_indet)
  ),
  # First and last appearance bounds per species (Ma).
  tar_target(
    temporal_span,
    summarise_spans(occ_tbl_no_indet)
  ),
  # Pairwise temporal overlap (Ma) from the point-estimated spans.
  tar_target(
    overlap_matrix,
    calc_overlap(temporal_span)
  ),
  # -----------------------------------------------------------------------------
  # 3b. UNCERTAINTY VIA MONTE CARLO
  # Purpose: propagate date interval uncertainty into species spans, overlaps,
  #          and bin richness. Draws are Uniform over [lower, upper].
  # Dependencies: occ_tbl_no_indet → mc_draws → spans_mc/overlap_mc/bin_counts_mc.
  # -----------------------------------------------------------------------------
  # Monte Carlo draws of latent dates per occurrence (Uniform over [lower, upper]).
  tar_target(
    mc_draws,
    mc_dates(occ_tbl_no_indet, B = 2000, dist = "uniform")
  ),
  # Species span medians and 90% CIs aggregated across MC draws.
  tar_target(
    spans_mc,
    spans_from_draws(mc_draws)
  ),
  # Pairwise overlap medians and 90% CIs aggregated across MC draws.
  tar_target(
    overlap_mc,
    overlap_from_draws(mc_draws)
  ),
  # Species richness per time bin (median and 90% CI over draws).
  tar_target(
    bin_counts_mc,
    bin_counts_from_draws(mc_draws, bin_width = 0.1)
  ),
  # Optional: sensitivity of bin counts to sampling distribution.
  tar_target(
    bin_sense,
    bin_sensitivity(occ_tbl_no_indet, B = 1000, bin_width = 0.1)
  ),
  # Convergence check: CI width vs number of MC draws B.
  tar_target(
    mc_conv,
    mc_ci_convergence(occ_tbl_no_indet, B_grid = c(200, 500, 1000, 2000), dist = "uniform")
  ),
  # -----------------------------------------------------------------------------
  # 4. WRITE TABLE ARTEFACTS TO /outputs
  # Purpose: materialise CSVs required by the report and external consumers.
  # -----------------------------------------------------------------------------
  # Write genus frequency table to CSV.
  tar_target(
    genus_freq_csv,
    write_output(genus_species_freq$genus_freq, "genus_freq.csv"),
    format = "file"
  ),
  # Write species frequency table to CSV.
  tar_target(
    species_freq_csv,
    write_output(genus_species_freq$species_freq, "species_freq.csv"),
    format = "file"
  ),
  # Write species span bounds (point estimates) to CSV.
  tar_target(
    temporal_span_csv,
    write_output(temporal_span, "temporal_span.csv"),
    format = "file"
  ),
  # Write pairwise overlap matrix (point estimates) to CSV.
  tar_target(
    overlap_matrix_csv,
    write_output(overlap_matrix, "overlap_matrix.csv"),
    format = "file"
  ),
  # Write MC span medians and 90% CIs to CSV.
  tar_target(
    spans_mc_csv,
    write_output(spans_mc, "spans_mc_ci.csv"),
    format = "file"
  ),
  # Write MC overlap medians and 90% CIs to CSV.
  tar_target(
    overlap_mc_csv,
    write_output(overlap_mc, "overlap_mc_ci.csv"),
    format = "file"
  ),
  # Write MC species richness per bin (median and 90% CI) to CSV.
  tar_target(
    bin_counts_mc_csv,
    write_output(bin_counts_mc, "bin_counts_mc_ci.csv"),
    format = "file"
  ),
  # -----------------------------------------------------------------------------
  # 4b. PLOTTING TARGETS (PNG to /outputs)
  # Purpose: visual summaries used in the report.
  # -----------------------------------------------------------------------------
  # Bar plot of specimen counts per genus.
  tar_target(
    genus_plot,
    plot_genus_freq(genus_species_freq$genus_freq)
  ),
  # Save genus counts plot to PNG.
  tar_target(
    genus_plot_file,
    write_plot(genus_plot, "genus_freq.png"),
    format = "file"
  ),
  # Bar plot of specimen counts per species.
  tar_target(
    species_plot,
    plot_species_freq(genus_species_freq$species_freq)
  ),
  # Save species counts plot to PNG.
  tar_target(
    species_plot_file,
    write_plot(species_plot, "species_freq.png"),
    format = "file"
  ),
  # Horizontal range plot of point-estimated first–last appearances.
  tar_target(
    span_plot,
    plot_temporal_span(temporal_span)
  ),
  # Save point-estimated span plot to PNG.
  tar_target(
    span_plot_file,
    write_plot(span_plot, "temporal_span.png"),
    format = "file"
  ),
  # Range plot with MC medians and 90% CIs per species.
  tar_target(
    span_mc_plot,
    plot_spans_ci(spans_mc)
  ),
  # Save MC span plot to PNG.
  tar_target(
    span_mc_plot_file,
    write_plot(span_mc_plot, "temporal_span_mc.png"),
    format = "file"
  ),
  # Heat‑map of pairwise overlap from point-estimated spans.
  tar_target(
    overlap_plot,
    plot_overlap_heatmap(overlap_matrix)
  ),
  # Save point-estimated overlap heat‑map to PNG.
  tar_target(
    overlap_plot_file,
    write_plot(overlap_plot, "overlap_heatmap.png"),
    format = "file"
  ),
  # Heat‑map of median pairwise overlap across MC draws.
  tar_target(
    overlap_mc_plot,
    plot_overlap_heatmap_mc(overlap_mc)
  ),
  # Save MC overlap heat‑map to PNG.
  tar_target(
    overlap_mc_plot_file,
    write_plot(overlap_mc_plot, "overlap_heatmap_mc.png"),
    format = "file"
  ),
  # Line + ribbon plot of species richness over time (median + 90% CI).
  tar_target(
    bin_counts_plot,
    plot_bin_counts_mc(bin_counts_mc)
  ),
  # Save species richness plot to PNG.
  tar_target(
    bin_counts_plot_file,
    write_plot(bin_counts_plot, "bin_counts_mc.png", width = 7, height = 4),
    format = "file"
  ),
  # -----------------------------------------------------------------------------
  # 5. SITE OCCUPANCY GAPS (inhomogeneous Poisson)
  # Purpose: quantify re‑occupancy plausibility using site‑specific lambda(t)
  #          scaled from country‑level intensity. Sites = clusters; rows with
  #          NA cluster_id are excluded by design from this analysis.
  # Dependencies: st_clusters + filter_indet() → country_intensity → site_lambda
  #               → reocc_pvals → likely_reoccupancy_events.
  # -----------------------------------------------------------------------------
  # Country-level sampling intensity per 0.1 Ma bin (proxy for effort).
  tar_target(
    country_intensity_bins,
    country_intensity(st_clusters |> filter_indet(), width = 0.1)
  ),
  # Site-specific lambda(t) scaled to observed site counts.
  tar_target(
    site_lambda,
    estimate_site_lambda(st_clusters |> filter_indet(), width = 0.1)
  ),
  # CSV export: site_lambda per bin (retain NA + comment column).
  tar_target(
    site_lambda_csv,
    write_output(site_lambda, "site_lambda.csv"),
    format = "file"
  ),
  # Gap-wise P(no finds) per site under inhomogeneous Poisson.
  tar_target(
    reocc_pvals,
    compute_reocc_pvals(st_clusters |> filter_indet(), site_lambda)
  ),
  # CSV export: gap p-values ordered by ascending p-value.
  tar_target(
    reocc_pvals_csv,
    write_output(dplyr::arrange(reocc_pvals, p_value), "reocc_pvals.csv"),
    format = "file"
  ),
  # Subset of likely re-occupancy events after FDR correction.
  tar_target(
    likely_reoccupancy_events,
    likely_reoccupancy(reocc_pvals)
  ),
  # CSV export: likely re-occupancy events (FDR < 0.05).
  tar_target(
    likely_reoccupancy_events_csv,
    write_output(likely_reoccupancy_events, "likely_reoccupancy_events.csv"),
    format = "file"
  ),
  # Scatter plot of gap duration vs P(no finds), highlighting FDR-significant gaps.
  tar_target(
    reocc_pvals_plot,
    plot_reocc_pvals(reocc_pvals)
  ),
  # -----------------------------------------------------------------------------
  # 6. REPORT RENDER
  # Purpose: render the Quarto report after its dependencies are available.
  # Note: explicit dependency list ensures {targets} schedules upstream builds.
  # -----------------------------------------------------------------------------
  tar_target(
    pdf_report,
    {
      # explicit dependencies: list them so {targets} builds them first
      deps <- list(
        genus_freq_csv, species_freq_csv, temporal_span_csv,
        overlap_matrix_csv, spans_mc_csv, overlap_mc_csv,
        bin_counts_mc_csv, st_clusters_csv, mixing_index_by_cluster_csv,
        mixing_index_plot_file, genus_plot_file, species_plot_file,
        span_plot_file, span_mc_plot_file, overlap_plot_file,
        overlap_mc_plot_file, bin_counts_plot_file, site_lambda,
        site_lambda_csv, reocc_pvals_csv, country_intensity_bins,
        likely_reoccupancy_events_csv, reocc_pvals_plot
      )
      quarto::quarto_render("report.qmd", output_format = "pdf")
      "report.pdf"
    },
    format = "file"
  )
)
