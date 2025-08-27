# R/plot_outliers.R -----------------------------------------------------------

#' Simple scatter map of outliers vs all occurrences
#' @param outlier_tbl Output of outliers_all().
#' @param occ_tbl Occurrence tibble (non-indet).
#' @return ggplot object
#' @export
plot_outlier_map <- function(outlier_tbl, occ_tbl) {
  import::from("ggplot2", ggplot, aes, geom_point, facet_wrap, scale_color_brewer, labs, theme_minimal)
  if (nrow(occ_tbl) == 0) return(ggplot2::ggplot())
  ggplot() +
    geom_point(data = occ_tbl, aes(x = lon, y = lat), color = "grey80", alpha = 0.5, size = 0.8) +
    {
      if (nrow(outlier_tbl) > 0) geom_point(data = outlier_tbl, aes(x = lon, y = lat, color = type), size = 1.8)
      else NULL
    } +
    facet_wrap(~ species, scales = "free") +
    scale_color_brewer(palette = "Set1") +
    labs(x = "Longitude", y = "Latitude", color = "Outlier type", title = "Outlier audit: spatial view") +
    theme_minimal()
}

#' Timeline plot highlighting temporal outliers for one species
#' @param occ_tbl Occurrence tibble (non-indet)
#' @param spans_mc Tibble from spans_from_draws()
#' @param outlier_tbl Output from outliers_all()
#' @param species Species name
#' @return ggplot object
#' @export
plot_outlier_timeline <- function(occ_tbl, spans_mc, outlier_tbl, species) {
  import::from("ggplot2", ggplot, aes, geom_point, geom_vline, scale_x_reverse, labs, theme_minimal)
  import::from("dplyr", filter)
  occ_sp <- dplyr::filter(occ_tbl, species == !!species)
  env <- dplyr::filter(spans_mc, species == !!species)
  out_sp <- dplyr::filter(outlier_tbl, species == !!species & type %in% c("FAD_ext","LAD_ext"))
  p <- ggplot(occ_sp, aes(x = date, y = 0)) +
    geom_point(alpha = 0.7, color = "grey40", position = ggplot2::position_jitter(height = 0.02)) +
    scale_x_reverse() + labs(x = "Time (Ma)", y = NULL, title = paste0("Temporal tails: ", species)) + theme_minimal()
  if (nrow(env) == 1) {
    p <- p + geom_vline(xintercept = env$first_hi, linetype = "dashed", color = "firebrick") +
      geom_vline(xintercept = env$last_lo, linetype = "dashed", color = "steelblue")
  }
  if (nrow(out_sp) > 0) {
    p <- p + geom_point(data = out_sp, aes(x = date, y = 0, color = type), size = 2, inherit.aes = FALSE)
  }
  p
}

