# bootstrap_env.R
# -----------------------------------------------------------------------------
# NOTICE:
# This script is for bootstrapping a BRAND-NEW project (no renv.lock yet).
# If you cloned this repository and see an existing renv.lock, DO NOT run this.
# Instead, open the .Rproj and run in the Console:
#   renv::restore(prompt = FALSE)
#   source("dependencies.R")
#   pak::pkg_install(project_dependencies())
# -----------------------------------------------------------------------------

if (file.exists("renv.lock")) {
  stop(
    paste(
      "bootstrap_env.R: Detected renv.lock. This script is only for new projects without",
      "a lockfile. For cloned repos, use renv::restore() and dependencies.R as per README.",
      sep = "\n"
    ),
    call. = FALSE
  )
}

if (!requireNamespace("renv", quietly = TRUE)) install.packages(c("renv","imports"))

import::from("renv", init, install, snapshot, status)

required_pkgs <- c(
  "targets", "import", "here", "readr", "dplyr",
  "tidyr", "tibble", "purrr", "ggplot2", "scales",
  "betapart", "broom"
)

# Bare‑bones renv project (no automatic snapshot) -------------------------------
init(bare = TRUE) |> invisible()

missing <- setdiff(required_pkgs, status()$library$Package)
if (length(missing) > 0) install(missing)

snapshot()
