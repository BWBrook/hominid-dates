#' Bar plots for genus & species counts (specimen‑weighted)
#' @export
plot_genus_freq <- function(genus_tbl) {

  import::from("ggplot2", ggplot, aes, geom_col, coord_flip, scale_y_continuous,
               labs, theme_minimal)
  import::from("dplyr", mutate)

  genus_tbl |>
    mutate(genus = forcats::fct_reorder(genus, n_specimens)) |>
    ggplot(aes(x = genus, y = n_specimens)) +
    geom_col() +
    coord_flip() +
    scale_y_continuous(expand = c(0, 0)) +
    labs(y = "Specimens", x = NULL,
         title = "Specimen counts per genus") +
    theme_minimal()
}

#' Species frequency (top 15 only) to avoid an unreadable chart
#' @export
plot_species_freq <- function(species_tbl, top_n = 15) {

  import::from("ggplot2", ggplot, aes, geom_col, coord_flip, scale_y_continuous,
               labs, theme_minimal)
  import::from("dplyr", slice_head, mutate)

  species_tbl |>
    slice_head(n = top_n) |>
    mutate(species = forcats::fct_reorder(species, n_specimens)) |>
    ggplot(aes(x = species, y = n_specimens)) +
    geom_col() +
    coord_flip() +
    scale_y_continuous(expand = c(0, 0)) +
    labs(y = "Specimens", x = NULL,
         title = paste("Top", top_n, "species by specimen count")) +
    theme_minimal()
}
