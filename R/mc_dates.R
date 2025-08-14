# R/mc_dates.R ---------------------------------------------------------------
#' Monte Carlo draws of latent dates from interval-censored occurrences
#'
#' Treat each row as interval-censored on [lower, upper]. For each draw b=1..B,
#' sample a latent date according to `dist`:
#' - "uniform": U(lower, upper)
#' - "triangular": triangular(lower, mode = date, upper); falls back to uniform
#'                  if `date` is NA or outside [lower, upper].
#' - "beta": scaled Beta on [lower, upper] with mode near `date` when present.
#'           Shape parameters set as alpha = 1 + k*s, beta = 1 + k*(1-s), where
#'           s = (date - lower)/(upper - lower) truncated to (0,1) and k controls
#'           concentration (default k=4). Falls back to alpha=beta=1 when `date`
#'           is NA or outside bounds.
#'
#' @param tbl Occurrence tibble (after collapse and optional filtering).
#' @param B   Number of Monte Carlo draws.
#' @param dist One of "uniform", "triangular", or "beta".
#' @param beta_k Concentration for beta mode anchoring (ignored unless dist=="beta").
#' @return List of length B; each element is a tibble with columns:
#'         species, lat, lon, lower, upper, date (original), sampling_intensity,
#'         and `date_mc` (sampled latent date for that draw).
#' @export
mc_dates <- function(tbl, B = 2000, dist = c("uniform", "triangular", "beta"), beta_k = 4) {

  import::from("dplyr", select, mutate)
  import::from("purrr", map)

  dist <- match.arg(dist)

  # helpers -----------------------------------------------------------------
  rtri <- function(n, a, m, b) {
    # Triangular distribution sampler with support [a, b] and mode m.
    # Vectorised over a, m, b (recycling n if length 1).
    u <- stats::runif(n)
    Fm <- (m - a) / (b - a)
    left <- u < Fm
    out <- numeric(n)
    out[left]  <- a[left] + sqrt(u[left] * (b[left] - a[left]) * (m[left] - a[left]))
    out[!left] <- b[!left] - sqrt((1 - u[!left]) * (b[!left] - a[!left]) * (b[!left] - m[!left]))
    out
  }

  rbeta_scaled <- function(n, a, b, shape1, shape2) {
    a + stats::rbeta(n, shape1 = shape1, shape2 = shape2) * (b - a)
  }

  draw_once <- function(df) {
    l <- df$lower
    u <- df$upper
    d <- df$date

    # Guard against degenerate or missing intervals
    # If lower>upper, swap. If equal, return the point.
    swap <- !is.na(l) & !is.na(u) & (l > u)
    l2 <- ifelse(swap, u, l)
    u2 <- ifelse(swap, l, u)

    same <- !is.na(l2) & !is.na(u2) & (abs(u2 - l2) < .Machine$double.eps^0.5)

    out <- numeric(length(l2))
    # Default uniform (vectorised)
    uu <- stats::runif(sum(!same))
    out[!same] <- l2[!same] + uu * (u2[!same] - l2[!same])
    out[same]  <- l2[same]

    if (dist == "triangular") {
      ok <- !is.na(d) & (d >= l2) & (d <= u2) & !same
      if (any(ok)) {
        out[ok] <- rtri(sum(ok), a = l2[ok], m = d[ok], b = u2[ok])
      }
    } else if (dist == "beta") {
      ok <- !is.na(d) & (d > l2) & (d < u2) & !same
      if (any(ok)) {
        s <- (d[ok] - l2[ok]) / (u2[ok] - l2[ok])
        # ensure s in (0,1)
        s <- pmin(pmax(s, 1e-6), 1 - 1e-6)
        alpha <- 1 + beta_k * s
        beta  <- 1 + beta_k * (1 - s)
        out[ok] <- rbeta_scaled(sum(ok), a = l2[ok], b = u2[ok], shape1 = alpha, shape2 = beta)
      }
    }

    df |>
      mutate(date_mc = out) |>
      select(species, lat, lon, country, id, sampling_intensity, lower, upper, date, date_mc)
  }

  # Return a list of B draws
  map(seq_len(B), ~ draw_once(tbl))
}
