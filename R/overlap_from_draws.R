# R/overlap_from_draws.R ----------------------------------------------------
#' Summarise pairwise overlap across MC draws
#'
#' @param mc_draws List of tibbles output by `mc_dates()`.
#' @return Tibble with species_i, species_j and median/90% CI of overlap (Ma).
#' @export
overlap_from_draws <- function(mc_draws) {

  import::from("dplyr", group_by, summarise, ungroup, mutate, select, rename, left_join, bind_rows)
  import::from("purrr", imap, map_dfr)
  import::from("tidyr", crossing)
  import::from("tibble", tibble)

  pairwise_per_draw <- function(df) {
    span <- df |>
      group_by(species) |>
      summarise(
        first_occ = max(date_mc, na.rm = TRUE),
        last_occ  = min(date_mc, na.rm = TRUE),
        .groups = "drop"
      )

    sp <- span$species
    pairs <- crossing(species_i = sp, species_j = sp)

    # Join spans for i and j
    pairs |>
      left_join(span |> rename(first_i = first_occ, last_i = last_occ, species_i = species), by = "species_i") |>
      left_join(span |> rename(first_j = first_occ, last_j = last_occ, species_j = species), by = "species_j") |>
      mutate(
        overlap = pmax(0, pmin(first_i, first_j) - pmax(last_i, last_j))
      ) |>
      select(species_i, species_j, overlap)
  }

  long <- map_dfr(mc_draws, pairwise_per_draw, .id = "draw")

  long |>
    group_by(species_i, species_j) |>
    summarise(
      ov_med = stats::median(overlap, na.rm = TRUE),
      ov_lo  = stats::quantile(overlap, probs = 0.05, na.rm = TRUE, names = FALSE),
      ov_hi  = stats::quantile(overlap, probs = 0.95, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    )
}

