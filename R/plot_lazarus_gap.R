# R/plot_lazarus_gap.R ---------------------------------------------------------

#' Rank plot of Lazarus gaps by -log10 adjusted p-value
#' @param lazarus_tbl Output of `lazarus_all()`.
#' @return ggplot object.
#' @export
plot_lazarus_rank <- function(lazarus_tbl) {

  import::from("dplyr", filter, arrange, mutate)
  import::from("ggplot2", ggplot, aes, geom_col, coord_flip, labs, theme_minimal, scale_fill_brewer)

  if (nrow(lazarus_tbl) == 0) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "No Lazarus gaps computed"))
  }

  dat <- lazarus_tbl |>
    filter(!is.na(p_adj)) |>
    mutate(score = -log10(p_adj)) |>
    arrange(dplyr::desc(score)) |>
    mutate(label = paste0(species, " (", sprintf("%0.2f", gap_Ma), " Ma)"))

  # keep top 25 for readability
  if (nrow(dat) > 25) dat <- dat[seq_len(25), ]

  ggplot(dat, aes(x = reorder(label, score), y = score, fill = effort_stratum)) +
    geom_col() +
    coord_flip() +
    labs(x = NULL, y = expression(-log[10](p[adj])), fill = "Stratum",
         title = "Lazarus gaps ranked by improbability (BH-adjusted)") +
    theme_minimal()
}

#' Timeline of one species with Lazarus gaps highlighted
#' @param occ_tbl Occurrence tibble for all species (or one species).
#' @param lazarus_tbl Output of `lazarus_all()`.
#' @param species Species name to plot.
#' @return ggplot object.
#' @export
plot_lazarus_timeline <- function(occ_tbl, lazarus_tbl, species) {

  import::from("dplyr", filter)
  import::from("ggplot2", ggplot, aes, geom_point, geom_segment, scale_x_reverse, labs, theme_minimal)

  occ_sp <- occ_tbl |>
    filter(species == !!species)

  gaps_sp <- lazarus_tbl |>
    filter(species == !!species)

  ggplot() +
    geom_point(data = occ_sp, aes(x = date, y = 0), alpha = 0.8) +
    {
      if (nrow(gaps_sp) > 0) {
        geom_segment(data = gaps_sp,
                     aes(x = gap_start_Ma, xend = gap_end_Ma, y = 0, yend = 0,
                         color = flag_improbable), linewidth = 2, inherit.aes = FALSE)
      } else NULL
    } +
    scale_x_reverse() +
    labs(x = "Time (Ma)", y = NULL, color = "FDR < 0.05",
         title = paste0("Timeline with Lazarus gaps: ", species)) +
    theme_minimal()
}

