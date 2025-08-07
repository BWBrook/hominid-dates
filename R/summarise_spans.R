# R/summarise_spans.R ------------------------------------------------------------
#' First and last appearance per species
#'
#' @param tbl Deduplicated occurrence tibble.
#' @return Tibble with species, first_occ, last_occ, duration (Ma).
#' @export
summarise_spans <- function(tbl) {

  import::from("dplyr", group_by, summarise, mutate, arrange, desc)

  tbl |>
    group_by(species) |>
    summarise(
      first_occ = max(upper, na.rm = TRUE),
      last_occ  = min(lower, na.rm = TRUE),
      .groups   = "drop"
    ) |>
    mutate(duration = round(first_occ - last_occ, 3)) |>
    arrange(desc(first_occ))
}
