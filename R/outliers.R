# R/outliers.R ---------------------------------------------------------------
# Outlier audit: temporal and spatial flags per species

#' Great-circle distance using Haversine formula (km)
#' @export
haversine_km <- function(lat1, lon1, lat2, lon2) {
  to_rad <- function(x) x * pi / 180
  R <- 6371.0088 # mean Earth radius (km)
  φ1 <- to_rad(lat1); φ2 <- to_rad(lat2)
  dφ <- to_rad(lat2 - lat1)
  dλ <- to_rad(lon2 - lon1)
  a <- sin(dφ/2)^2 + cos(φ1) * cos(φ2) * sin(dλ/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R * c
}

#' Temporal outliers relative to MC envelope
#'
#' Flags occurrences outside the 90% envelope of FAD/LAD by >= delta (Ma).
#'
#' @param occ_tbl Occurrence tibble (indeterminates removed).
#' @param spans_mc Tibble from spans_from_draws() with first_hi/last_lo columns.
#' @param delta Minimum extension beyond the envelope (Ma).
#' @return Tibble: species, id, date, lat, lon, type (FAD_ext/LAD_ext), delta, cluster_id, note.
#' @export
temporal_outliers <- function(occ_tbl, spans_mc, delta = 0.05) {
  import::from("dplyr", left_join, mutate, filter, select, coalesce)

  keep <- c("species","id","date","lat","lon","cluster_id")
  base <- occ_tbl[, intersect(keep, names(occ_tbl))]

  env <- spans_mc |>
    dplyr::select(species, first_hi, last_lo)

  df <- dplyr::left_join(base, env, by = "species")
  if (!all(c("first_hi","last_lo") %in% names(df))) return(df[0, ])

  fad_out <- df |>
    filter(!is.na(first_hi) & !is.na(date) & (date > (first_hi + delta))) |>
    mutate(type = "FAD_ext", delta = date - first_hi, note = NA_character_)

  lad_out <- df |>
    filter(!is.na(last_lo) & !is.na(date) & (date < (last_lo - delta))) |>
    mutate(type = "LAD_ext", delta = last_lo - date, note = NA_character_)

  out <- dplyr::bind_rows(fad_out, lad_out) |>
    select(species, id, date, lat, lon, type, delta, cluster_id, note)
  out
}

#' Spatial outliers by robust distance per species
#'
#' Flags occurrences with distance > median(d) + 3 * MAD(d) from median centre.
#' If cluster_id present, annotates notes for species×cluster singletons.
#'
#' @param occ_tbl Occurrence tibble (indeterminates removed).
#' @return Tibble: species, id, date, lat, lon, type = "spatial", delta (km over threshold), cluster_id, note.
#' @export
spatial_outliers <- function(occ_tbl) {
  import::from("dplyr", group_by, summarise, ungroup, mutate, left_join, select, arrange)

  if (!all(c("lat","lon") %in% names(occ_tbl))) return(occ_tbl[0, ])

  # center per species
  centers <- occ_tbl |>
    group_by(species) |>
    summarise(lat0 = stats::median(lat, na.rm = TRUE), lon0 = stats::median(lon, na.rm = TRUE), .groups = "drop")

  df <- dplyr::left_join(occ_tbl, centers, by = "species") |>
    mutate(dist_km = haversine_km(lat0, lon0, lat, lon))

  # robust threshold per species
  thr <- df |>
    group_by(species) |>
    summarise(d_med = stats::median(dist_km, na.rm = TRUE), d_mad = stats::mad(dist_km, constant = 1, na.rm = TRUE), .groups = "drop") |>
    mutate(threshold = d_med + 3 * d_mad)

  df2 <- dplyr::left_join(df, thr, by = "species")

  out <- df2 |>
    mutate(over_by = dist_km - threshold) |>
    filter(is.finite(over_by) & over_by > 0) |>
    mutate(type = "spatial", delta = over_by, note = NA_character_) |>
    select(species, id, date, lat, lon, type, delta, cluster_id, note)

  # annotate cluster singletons if cluster_id present
  if ("cluster_id" %in% names(occ_tbl)) {
    cln <- occ_tbl |>
      filter(!is.na(cluster_id)) |>
      group_by(species, cluster_id) |>
      summarise(n_cluster = dplyr::n(), .groups = "drop")
    out <- dplyr::left_join(out, cln, by = c("species","cluster_id")) |>
      mutate(note = ifelse(!is.na(n_cluster) & n_cluster == 1, "cluster_singleton", note)) |>
      select(-n_cluster)
  }
  out
}

#' Combine temporal and spatial outliers
#' @export
outliers_all <- function(occ_tbl, spans_mc, delta = 0.05) {
  import::from("dplyr", bind_rows, arrange)
  t_out <- temporal_outliers(occ_tbl, spans_mc, delta = delta)
  s_out <- spatial_outliers(occ_tbl)
  bind_rows(t_out, s_out) |>
    arrange(species, dplyr::desc(delta))
}

