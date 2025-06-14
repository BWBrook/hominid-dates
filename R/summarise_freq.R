# R/summarise_freq.R -------------------------------------------------------------
#' Genus and species frequency tables
#'
#' @param tbl Deduplicated occurrence tibble.
#' @return List with two tibbles: genus_freq, species_freq.
#' @export
summarise_freq <- function(tbl) {

  import::from("dplyr", count, arrange, desc)
  import::from("tibble", tibble)

  genus_freq   <- count(tbl, genus,  name = "n_specimens")   |> arrange(desc(n_specimens))
  species_freq <- count(tbl, species, name = "n_specimens") |> arrange(desc(n_specimens))

  list(genus_freq = genus_freq, species_freq = species_freq)
}
