# R/read_hominid.R ---------------------------------------------------------------
#' Read cleaned hominid CSV
#'
#' @param path Path to the CSV (character). Usually provided by a file target.
#' @return Tibble with correct column types.
#' @export
read_hominid <- function(path) {

  import::from("readr",  read_csv)
  import::from("dplyr",  mutate, across)
  import::from("tibble", as_tibble)

  read_csv(path, show_col_types = FALSE) |>
    mutate(
      across(
        .cols = c(lat, lon, date, upper, lower),
        .fns  = as.numeric
      )
    ) |>
    as_tibble()
}
