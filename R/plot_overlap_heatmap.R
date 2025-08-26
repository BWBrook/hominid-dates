#' Heat‑map of pairwise temporal overlap (Ma)
#' @export
plot_overlap_heatmap <- function(overlap_tbl) {

  import::from("ggplot2", ggplot, aes, geom_tile, scale_fill_viridis_c,
               labs, theme_minimal, theme, element_text)
  import::from("tidyr", pivot_longer)
  import::from("dplyr", mutate)

  overlap_tbl |>
    pivot_longer(
      -species_i,
      names_to = "species_j",
      values_to = "overlap_Ma"
    ) |>
    mutate(
      species_i = forcats::fct_reorder(species_i, overlap_Ma, .fun = max),
      species_j = forcats::fct_reorder(species_j, overlap_Ma, .fun = max)
    ) |>
    ggplot(aes(x = species_i, y = species_j, fill = overlap_Ma)) +
    geom_tile() +
    scale_fill_viridis_c(option = "D", na.value = "grey90") +
    labs(x = NULL, y = NULL, fill = "Overlap (Ma)",
         title = "Pairwise temporal overlap") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
