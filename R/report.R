# R/report.R -----------------------------------------------------------------
#' Render the Quarto report and return its file path
#'
#' This helper is invoked by the `pdf_report` target to keep _targets.R tidy.
#'
#' @return Character path to the rendered PDF under docs/ (for format = "file").
#' @export
render_report <- function() {

  import::from("here", here)
  import::from("quarto", quarto_render)
  import::from("withr", with_envvar)

  # Ensure targets finds the data store regardless of working dir during render
  store_path <- here("_targets")
  with_envvar(c(TAR_TARGETS_STORE = store_path), {
    quarto_render("docs/report.qmd", output_format = "pdf")
  })
  here("docs", "report.pdf")
}

#' Render the Usage vignette and return its file path
#' @return Character path to the rendered PDF under docs/
#' @export
render_usage <- function() {

  import::from("here", here)
  import::from("quarto", quarto_render)
  import::from("withr", with_envvar)

  store_path <- here("_targets")
  with_envvar(c(TAR_TARGETS_STORE = store_path), {
    quarto_render("docs/USAGE.qmd", output_format = "pdf")
  })
  here("docs", "USAGE.pdf")
}
