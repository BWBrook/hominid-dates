# R/plot_bin_sensitivity.R -----------------------------------------------------

#' Plot per-bin series across widths
#' @param bin_sense_tbl Output table from bin_sensitivity()$bin_sense_tbl
#' @return ggplot object
#' @export
plot_bin_series <- function(bin_sense_tbl) {
  import::from("dplyr", mutate, select)
  import::from("tidyr", pivot_longer)
  import::from("ggplot2", ggplot, aes, geom_line, facet_grid, labs, theme_minimal, scale_x_reverse)

  if (nrow(bin_sense_tbl) == 0) return(ggplot2::ggplot() + ggplot2::labs(title = "No series to plot"))

  dat <- bin_sense_tbl |>
    select(width, time_bin_lo, N, S, lambda_z) |>
    mutate(width = factor(width, levels = sort(unique(width)))) |>
    pivot_longer(cols = c(N, S, lambda_z), names_to = "metric", values_to = "value")

  ggplot(dat, aes(x = time_bin_lo, y = value)) +
    geom_line() +
    scale_x_reverse() +
    facet_grid(metric ~ width, scales = "free_y") +
    labs(x = "Time (Ma)", y = NULL, title = "Per-bin metrics across widths (faceted by metric × width)") +
    theme_minimal()
}

#' Plot stability summary across widths
#' @param bin_sense_summary Output table from bin_sensitivity()$bin_sense_summary
#' @return ggplot object
#' @export
plot_bin_stability <- function(bin_sense_summary) {
  import::from("dplyr", mutate)
  import::from("tidyr", pivot_longer)
  import::from("ggplot2", ggplot, aes, geom_col, facet_grid, labs, theme_minimal)

  if (nrow(bin_sense_summary) == 0) return(ggplot2::ggplot() + ggplot2::labs(title = "No stability results"))

  dat <- bin_sense_summary |>
    mutate(width_pair = paste0(width_comp, " vs ", width_ref)) |>
    pivot_longer(cols = c(spearman_rho, rmsd_z), names_to = "stat", values_to = "value")

  ggplot(dat, aes(x = width_pair, y = value, fill = metric)) +
    geom_col(position = "dodge") +
    facet_grid(stat ~ ., scales = "free_y") +
    labs(x = "Width comparison", y = NULL, fill = "Metric",
         title = "Stability across widths: Spearman rho and RMSD (z-scores)") +
    theme_minimal()
}

