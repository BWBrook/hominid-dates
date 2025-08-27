# R/stubs_next.R ---------------------------------------------------------------
# Stubs for "Next steps" modules (non-functional placeholders)

#' Placeholder: Solow OLE extinction per species (stub)
#' @return Tibble with a note; replace with real implementation later.
#' @export
ole_extinction_stub <- function() {
  import::from("tibble", tibble)
  tibble(module = "OLE", status = "stub", note = "Replace with Solow OLE implementation")
}

#' Placeholder: Site-level occupancy models (stub)
#' @return Tibble with a note; replace with real implementation later.
#' @export
site_occupancy_stub <- function() {
  import::from("tibble", tibble)
  tibble(module = "site_occupancy", status = "stub", note = "Replace with occupancy model implementation")
}

#' Placeholder: Animated spatio-temporal maps (stub)
#' @return ggplot object with placeholder text.
#' @export
animated_maps_stub <- function() {
  import::from("ggplot2", ggplot, annotate, theme_void)
  ggplot() + annotate("text", x = 0, y = 0, label = "Animated maps stub\n(to be implemented)") + theme_void()
}

