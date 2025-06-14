# R/calc_overlap.R --------------------------------------------------------------
#' Pairwise temporal overlap matrix (Ma)
#'
#' @param span_tbl Output of `summarise_spans()`.
#' @return Wide tibble: species × species with numeric overlap.
#' @export
calc_overlap <- function(span_tbl) {

  import::from("dplyr", select)
  import::from("purrr", map_dbl)
  import::from("tibble", tibble)
  import::from("tidyr", pivot_wider)

  species      <- span_tbl$species
  spans        <- span_tbl |> select(first_occ, last_occ)

  overlap_fun <- function(i, j) {
    max(0,
        min(spans$first_occ[i], spans$first_occ[j]) -
          max(spans$last_occ[i],  spans$last_occ[j])
    )
  }

  # pairwise grid ---------------------------------------------------
  idx <- seq_along(species)
  grid <- expand.grid(i = idx, j = idx)

  ov_vals <- map_dbl(seq_len(nrow(grid)), \(k) overlap_fun(grid$i[k], grid$j[k]))

  tibble(
    species_i = species[grid$i],
    species_j = species[grid$j],
    overlap_Ma = ov_vals
  ) |>
    pivot_wider(
      names_from  = species_j,
      values_from = overlap_Ma
    )
}
