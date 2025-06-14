# R/write_outputs.R -------------------------------------------------------------
#' Write a data frame to /outputs and return the path
#'
#' @param df  Data frame or tibble.
#' @param fname File name (character) to create inside /outputs.
#' @return Full path (character) – for use as `format = "file"` target.
#' @export
write_output <- function(df, fname) {

  import::from("here",  here)
  import::from("readr", write_csv)

  out_path <- here("outputs", fname)
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  write_csv(df, out_path)
  out_path
}
