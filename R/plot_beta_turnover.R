# R/plot_beta_turnover.R --------------------------------------------------------
#' Faceted beta diversity components per locality
#'
#' Plots stacked bars of turnover vs nestedness for consecutive bin pairs.
#' Overlays thin grey lines (scaled) for richness (mean of S1,S2) and effort.
#'
#' @param beta_tbl Output of `beta_turnover_by_cluster()`.
#' @param nmax Maximum number of clusters to facet (default 6).
#' @return ggplot object
#' @export
plot_beta_locality <- function(beta_tbl, nmax = 6) {

  import::from("dplyr", mutate, group_by, summarise, ungroup, arrange, slice_head, select, left_join, semi_join)
  import::from("tidyr", pivot_longer)
  import::from("ggplot2", ggplot, aes, geom_col, geom_line, scale_x_reverse, scale_y_continuous,
               theme_minimal, labs, facet_wrap)

  if (nrow(beta_tbl) == 0) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "No eligible clusters"))
  }

  dat <- beta_tbl |>
    mutate(
      step_mid = (bin_lo + bin_next_hi) / 2,
      richness = (S1 + S2) / 2,
      effort   = (effort1 + effort2) / 2
    )

  # choose top nmax clusters by number of steps
  top_clusters <- dat |>
    group_by(cluster_id) |>
    summarise(n_steps = dplyr::n(), .groups = "drop") |>
    arrange(dplyr::desc(n_steps)) |>
    slice_head(n = nmax)

  dat2 <- dat |> dplyr::semi_join(top_clusters, by = "cluster_id")

  # scale richness and effort to [0,1] per cluster for overlay
  scales <- dat2 |>
    group_by(cluster_id) |>
    summarise(
      rmin = min(richness, na.rm = TRUE), rmax = max(richness, na.rm = TRUE),
      emin = min(effort,   na.rm = TRUE), emax = max(effort,   na.rm = TRUE),
      .groups = "drop"
    )

  dat3 <- dat2 |>
    dplyr::left_join(scales, by = "cluster_id") |>
    mutate(
      richness_scaled = ifelse(is.finite(rmax - rmin) & (rmax - rmin) > 0, (richness - rmin)/(rmax - rmin), 0),
      effort_scaled   = ifelse(is.finite(emax - emin) & (emax - emin) > 0, (effort   - emin)/(emax - emin), 0)
    ) |>
    select(-rmin, -rmax, -emin, -emax)

  comp <- dat3 |>
    dplyr::select(cluster_id, step_mid, beta_turnover, beta_nestedness) |>
    pivot_longer(cols = c(beta_turnover, beta_nestedness), names_to = "component", values_to = "value")

  ggplot(comp, aes(x = step_mid, y = value, fill = component)) +
    geom_col(position = "stack", width = 0.22) +
    geom_line(data = dat3, aes(x = step_mid, y = richness_scaled, group = cluster_id),
              color = "grey40", linewidth = 0.4, inherit.aes = FALSE) +
    geom_line(data = dat3, aes(x = step_mid, y = effort_scaled,   group = cluster_id),
              color = "grey70", linewidth = 0.3, inherit.aes = FALSE) +
    scale_x_reverse(name = "Ma (older -> right)") +
    scale_y_continuous(name = "Beta components", sec.axis = ggplot2::sec_axis(~ ., name = "Relative richness / effort")) +
    facet_wrap(~ cluster_id, scales = "free_x") +
    theme_minimal() + labs(fill = NULL, title = "Temporal beta diversity components by locality")
}
