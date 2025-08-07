#' Write ggplot object to /outputs and return the path
#' @export
write_plot <- function(plot_obj, fname, width = 7, height = 5) {

  import::from("here", here)
  import::from("ggplot2", ggsave)

  out_path <- here("outputs", fname)
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  ggsave(out_path, plot = plot_obj, width = width, height = height,
         dpi = 300, units = "in")
  out_path
}
