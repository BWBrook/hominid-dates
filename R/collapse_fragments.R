# R/collapse_fragments.R  (final patch) -----------------------------------------
#' Collapse duplicate fragments into single occurrences
#'
#' Adds `sampling_intensity`, keeps scalar metadata columns.
#' No list-columns are produced.
#'
#' @export
collapse_fragments <- function(tbl) {

  import::from("dplyr", group_by, summarise, ungroup, n, first, mutate, if_else)

  tbl |>
    group_by(lat, lon, species, date) |>
    summarise(
      sampling_intensity = n(),
      id      = first(id),
      genus   = first(genus),
      country = first(country),
      upper   = max(upper, na.rm = TRUE),
      lower   = min(lower, na.rm = TRUE),
      .groups = "drop"
    ) |>
    # ---- new: add genus prefix unless species == "indet" ----
    mutate(
      species = if_else(
        species == "indet",
        species,
        paste0(substr(genus, 1, 1), "_", species)
      )
    ) |>
    ungroup()
}
