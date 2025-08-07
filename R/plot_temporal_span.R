#' Horizontal range plot of first–last appearances (Ma)
#' @export
plot_temporal_span <- function(span_tbl) {

  import::from("ggplot2", ggplot, aes, geom_linerange, geom_point, scale_x_reverse,
               labs, theme_minimal)
  import::from("dplyr", mutate)

  span_tbl |>
    mutate(species = forcats::fct_reorder(species, first_occ)) |>
    ggplot(aes(y = species)) +
    geom_linerange(aes(xmin = last_occ, xmax = first_occ), linewidth = 2) +
    geom_point(aes(x = first_occ), size = 2) +
    geom_point(aes(x = last_occ), size = 2) +
    scale_x_reverse(name = "Million years ago (Ma)") +
    labs(y = NULL, title = "Temporal spans per species") +
    theme_minimal()
}
