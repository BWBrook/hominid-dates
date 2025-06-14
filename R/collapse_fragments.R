# R/collapse_fragments.R ---------------------------------------------------------
#' Collapse duplicate fragments into single occurrences
#'
#' Identical lat, lon, species, and date are treated as one record.
#'
#' @param tbl Tibble from `read_hominid()`.
#' @return Deduplicated tibble.
#' @export
collapse_fragments <- function(tbl) {

  import::from("dplyr", group_by, summarise, pick, ungroup, n)

  tbl |>
    group_by(lat, lon, species, date) |>
    summarise(
      # retain one exemplar id; retain widest bounds
      id    = pick(id)[1],
      upper = max(upper, na.rm = TRUE),
      lower = min(lower, na.rm = TRUE),
      .groups = "drop"
    ) |>
    ungroup()
}
