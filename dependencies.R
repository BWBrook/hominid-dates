# dependencies.R ---------------------------------------------------------------
# Bootstrap helper for installing project dependencies in a compendium project.

#' Return the vector of CRAN package dependencies for this project
#' @return character vector of package names
project_dependencies <- function() {
  c(
    # workflow
    "targets", "import", "here",
    # wrangling / tibble / iterate
    "readr", "dplyr", "tidyr", "tibble", "purrr",
    # plotting
    "ggplot2", "forcats", "scales", "kableExtra",
    # methods
    "dbscan", "countrycode", "rnaturalearth", "sf",
    # reporting
    "quarto",
    # messaging / errors / env
    "rlang", "cli", "withr"
  )
}
