# R/collapse_fragments.R ---------------------------------------------------------
#' Collapse duplicate fragments into single occurrences
#'
#' * `sampling_intensity` = number of specimens (rows) represented by the
#'   collapsed occurrence; this count includes indeterminates.
#'
#' @param tbl Tibble from `read_hominid()`.
#' @return Deduplicated tibble with a new `sampling_intensity` column.
#' @export
collapse_fragments <- function(tbl) {

  import::from("dplyr", group_by, summarise, pick, ungroup, n)

  tbl |>
    group_by(lat, lon, species, date) |>
    summarise(
      sampling_intensity = n(),            # how many fragments merged
      id                 = pick(id)[1],    # exemplar
      upper              = max(upper, na.rm = TRUE),
      lower              = min(lower, na.rm = TRUE),
      .groups            = "drop"
    ) |>
    ungroup()
}
