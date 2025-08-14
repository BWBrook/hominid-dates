# R/plot_mc.R ---------------------------------------------------------------
#' Plot MC span medians with 90% CI
#' @export
plot_spans_ci <- function(spans_mc_tbl) {

  import::from("ggplot2", ggplot, aes, geom_linerange, geom_point, scale_x_reverse,
               labs, theme_minimal)
  import::from("dplyr", mutate)

  spans_mc_tbl |>
    mutate(species = forcats::fct_reorder(species, first_med)) |>
    ggplot(aes(y = species)) +
    # 90% CI envelope as thin line
    geom_linerange(aes(xmin = last_lo, xmax = first_hi), linewidth = 1, alpha = 0.4) +
    # median span as thick line
    geom_linerange(aes(xmin = last_med, xmax = first_med), linewidth = 2) +
    geom_point(aes(x = first_med), size = 2) +
    geom_point(aes(x = last_med),  size = 2) +
    scale_x_reverse(name = "Million years ago (Ma)") +
    labs(y = NULL, title = "Temporal spans per species (median with 90% CI)") +
    theme_minimal()
}

#' Plot MC median overlap heatmap
#' @export
plot_overlap_heatmap_mc <- function(overlap_mc_tbl) {

  import::from("tidyr", pivot_wider)

  wide <- overlap_mc_tbl |>
    dplyr::select(species_i, species_j, ov_med) |>
    pivot_wider(names_from = species_j, values_from = ov_med)

  plot_overlap_heatmap(wide)
}

#' Plot MC bin counts (species richness) with 90% CI ribbon
#' @export
plot_bin_counts_mc <- function(bin_counts_tbl) {

  import::from("ggplot2", ggplot, aes, geom_ribbon, geom_line, scale_x_reverse, labs, theme_minimal)

  ggplot(bin_counts_tbl, aes(x = bin_mid)) +
    geom_ribbon(aes(ymin = n_lo, ymax = n_hi), fill = "grey80") +
    geom_line(aes(y = n_med)) +
    scale_x_reverse(name = "Million years ago (Ma)") +
    labs(y = "Species richness",
         title = "Species richness over time (median with 90% CI)") +
    theme_minimal()
}

