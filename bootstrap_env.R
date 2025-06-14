# bootstrap_env.R
# Initialise renv, pin package versions, and install any missing packages ----------

if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

import::from("renv", init, install, snapshot, status)

required_pkgs <- c(
  "targets", "import", "here", "readr", "dplyr",
  "tidyr", "tibble", "purrr"
)

# Bare‑bones renv project (no automatic snapshot) -------------------------------
init(bare = TRUE) |> invisible()

missing <- setdiff(required_pkgs, status()$library$Package)
if (length(missing) > 0) install(missing)

snapshot()
