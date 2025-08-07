# R/filter_indet.R ---------------------------------------------------------------
#' Remove indeterminate species rows
#'
#' Keeps `sampling_intensity` column for effort covariate.
#'
#' @param tbl Occurrence tibble after `collapse_fragments()`.
#' @return Tibble without `species == "indet"`.
#' @export
filter_indet <- function(tbl) {

  import::from("dplyr", filter)
  filter(tbl, species != "indet")
}
