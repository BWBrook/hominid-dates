# R/summarise_freq.R -------------------------------------------------------------
#' Genus and species frequency tables (indet already filtered)
#'
#' @param tbl Occurrence tibble without indeterminates.
#' @return List with two tibbles: genus_freq, species_freq.
#' @export
summarise_freq <- function(tbl) {

  import::from("dplyr", count, arrange, desc)

  genus_freq   <- tbl |> count(genus,   wt = sampling_intensity,
                               name = "n_specimens") |> arrange(desc(n_specimens))

  species_freq <- tbl |> count(species, wt = sampling_intensity,
                               name = "n_specimens") |> arrange(desc(n_specimens))

  list(genus_freq = genus_freq, species_freq = species_freq)
}
