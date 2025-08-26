# Ensure project root is correctly set for here()
try(here::i_am("tests/testthat/helper.R"), silent = TRUE)

# Load all functions in R/ for testing (compendium-style project)
r_files <- list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE)
if (requireNamespace("targets", quietly = TRUE)) {
  if (length(r_files)) targets::tar_source(r_files)
} else if (length(r_files)) {
  invisible(lapply(r_files, source, local = TRUE))
}
