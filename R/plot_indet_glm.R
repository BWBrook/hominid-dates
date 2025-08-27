# R/plot_indet_glm.R -----------------------------------------------------------

#' Plot Pr(indet) over time by country
#' @param indet_effects Output of predict_indet().
#' @return ggplot object
#' @export
plot_indet_over_time <- function(indet_effects) {
  import::from("ggplot2", ggplot, aes, geom_ribbon, geom_line, facet_wrap, labs, theme_minimal, scale_x_reverse)
  import::from("dplyr", arrange)

  if (nrow(indet_effects) == 0) return(ggplot2::ggplot() + ggplot2::labs(title = "No effects to plot"))

  indet_effects <- arrange(indet_effects, country, dplyr::desc(time_mid))
  ggplot(indet_effects, aes(x = time_mid, y = p_hat)) +
    geom_ribbon(aes(ymin = p_lo, ymax = p_hi), fill = "grey85", color = NA) +
    geom_line(color = "steelblue") +
    scale_x_reverse() +
    facet_wrap(~ country, scales = "free_x") +
    labs(x = "Time (Ma)", y = "Pr(indeterminate)", title = "Probability of indeterminate identification over time") +
    theme_minimal()
}

#' Plot observed Pr(indet) vs effort (diagnostic)
#' @param indet_data Output of build_indet_frame().
#' @return ggplot object
#' @export
plot_indet_vs_effort <- function(indet_data) {
  import::from("ggplot2", ggplot, aes, geom_point, geom_smooth, labs, theme_minimal, scale_x_continuous)
  import::from("dplyr", filter, mutate)

  if (nrow(indet_data) == 0) return(ggplot2::ggplot() + ggplot2::labs(title = "No data"))

  dat <- indet_data |>
    filter(n_total > 0) |>
    mutate(p_obs = pmin(1, pmax(0, n_indet / n_total)))

  ggplot(dat, aes(x = log1p(effort), y = p_obs, color = country, weight = n_total)) +
    geom_point(alpha = 0.6) +
    geom_smooth(
      method = "loess",
      formula = y ~ x,
      se = FALSE,
      linewidth = 0.5,
      span = 0.9,
      method.args = list(
        degree = 1,
        family = "gaussian",
        control = stats::loess.control(surface = "direct")
      )
    ) +
    labs(x = "log1p(effort)", y = "Observed Pr(indet)", title = "Observed indeterminate rate vs effort (by country)") +
    theme_minimal()
}
