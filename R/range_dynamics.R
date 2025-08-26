# R/range_dynamics.R -----------------------------------------------------------
#' Range dynamics: centroid, extents, hull area, and velocity per bin
#'
#' For each species × time bin (with >= 3 occurrences), compute:
#'  - centroid latitude/longitude (mean of occurrence coordinates)
#'  - latitude/longitude spans (max - min)
#'  - convex hull area (km^2) using an Africa-focused equal-area projection
#'  - centroid velocity (km/Ma) between sequential bins
#'
#' Binning is on the `date` column (Ma). Africa plate motion is ignored.
#'
#' @param occ_tbl Occurrence tibble (preferably after `filter_indet()`).
#' @param bin_width Bin width in Ma (default 0.1).
#' @return Tibble with columns: species, bin, centroid_lat, centroid_long,
#'         lat_span, long_span, hull_area_km2, velocity_km_per_Ma. Rows sorted
#'         by species asc, then bin in chronological order (older → younger).
#' @export
range_dynamics <- function(occ_tbl, bin_width = 0.1) {

  import::from("dplyr", filter, mutate, group_by, summarise, ungroup, arrange,
               select, n, lag, if_else, across, rename)
  import::from("tibble", tibble)
  import::from("rlang", abort)

  # Guard: required columns
  req <- c("species", "lat", "lon", "date")
  missing <- setdiff(req, names(occ_tbl))
  if (length(missing) > 0) {
    abort(sprintf("range_dynamics: missing required columns: %s", paste(missing, collapse = ", ")),
          class = "range_dynamics_error")
  }

  df <- occ_tbl |>
    filter(!is.na(species), is.finite(lat), is.finite(lon), is.finite(date))

  if (nrow(df) == 0) {
    return(tibble(
      species = character(), bin = numeric(), centroid_lat = numeric(),
      centroid_long = numeric(), lat_span = numeric(), long_span = numeric(),
      hull_area_km2 = numeric(), velocity_km_per_Ma = numeric()
    ))
  }

  # Build bin grid and assign a bin midpoint to each row --------------------
  t_min <- floor(min(df$date, na.rm = TRUE))
  t_max <- ceiling(max(df$date, na.rm = TRUE))
  breaks <- seq(t_min, t_max, by = bin_width)
  if (tail(breaks, 1) < t_max) breaks <- c(breaks, t_max)

  df <- df |>
    mutate(
      .bin_lo = pmin( floor((date - t_min) / bin_width) * bin_width + t_min,
                       max(head(breaks, -1)) ),
      .bin_hi = .bin_lo + bin_width,
      bin     = (.bin_lo + .bin_hi) / 2
    )

  # Per species × bin summaries; keep groups with >= 3 occurrences ----------
  core <- df |>
    group_by(species, bin) |>
    summarise(
      n_occ = dplyr::n(),
      centroid_lat  = mean(lat, na.rm = TRUE),
      centroid_long = mean(lon, na.rm = TRUE),
      lat_span  = diff(range(lat, na.rm = TRUE)),
      long_span = diff(range(lon, na.rm = TRUE)),
      hull_area_km2 = convex_hull_area_km2(lat, lon),
      .groups = "drop"
    ) |>
    filter(n_occ >= 3) |>
    select(-n_occ)

  if (nrow(core) == 0) {
    out <- core |>
      mutate(velocity_km_per_Ma = NA_real_) |>
      arrange(species, dplyr::desc(bin))
    return(validate_range_metrics(out))
  }

  # Compute centroid velocity between sequential bins per species -----------
  core <- core |>
    arrange(species, dplyr::desc(bin)) |>
    group_by(species) |>
    mutate(
      prev_lat  = lag(centroid_lat),
      prev_long = lag(centroid_long),
      prev_bin  = lag(bin),
      dist_km   = haversine_km(prev_lat, prev_long, centroid_lat, centroid_long),
      dt        = abs(bin - prev_bin),
      velocity_km_per_Ma = if_else(is.na(dist_km) | is.na(dt) | dt <= 0,
                                   NA_real_, dist_km / dt)
    ) |>
    ungroup() |>
    select(-prev_lat, -prev_long, -prev_bin, -dist_km, -dt)

  validate_range_metrics(core)
}

#' Validate structure and types for range metrics
#' @keywords internal
validate_range_metrics <- function(x) {
  import::from("dplyr", arrange)
  import::from("rlang", abort)
  cols <- c("species", "bin", "centroid_lat", "centroid_long", "lat_span",
            "long_span", "hull_area_km2", "velocity_km_per_Ma")
  missing <- setdiff(cols, names(x))
  if (length(missing) > 0) {
    abort(sprintf("validate_range_metrics: missing columns: %s", paste(missing, collapse = ", ")),
          class = "range_metrics_validation_error")
  }
  # enforce ordering: species asc, then bin chronological (older → younger = desc by Ma)
  x <- arrange(x, species, dplyr::desc(bin))
  x
}

#' Great-circle distance (Haversine) in km
#' @keywords internal
haversine_km <- function(lat1, lon1, lat2, lon2, R = 6371.0088) {
  # Vectorised; returns NA where any input is NA
  to_rad <- function(d) d * pi / 180
  phi1 <- to_rad(lat1); phi2 <- to_rad(lat2)
  dphi <- to_rad(lat2 - lat1)
  dlmb <- to_rad(lon2 - lon1)
  a <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlmb / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R * c
}

#' Convex hull area (km^2) for lat/lon points
#' @keywords internal
convex_hull_area_km2 <- function(lat, lon) {
  import::from("sf", st_as_sf, st_union, st_convex_hull, st_transform, st_area,
               st_crs, st_set_crs)

  ok <- is.finite(lat) & is.finite(lon)
  if (sum(ok) < 3) return(0)

  d <- data.frame(lon = lon[ok], lat = lat[ok])
  d <- unique(d)
  if (nrow(d) < 3) return(0)

  # Points in WGS84
  pts <- st_as_sf(d, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  hull <- st_convex_hull(st_union(pts))
  # Some sf versions may drop CRS; ensure it's set before transform
  if (is.na(st_crs(hull))) hull <- st_set_crs(hull, st_crs(pts))

  # Africa-focused equal-area Albers Conic (units: meters)
  crs_aea_africa <- "+proj=aea +lat_1=-18 +lat_2=18 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  hull_m <- tryCatch(suppressWarnings(st_transform(hull, crs = crs_aea_africa)),
                     error = function(e) NULL)
  if (is.null(hull_m)) return(0)
  as.numeric(st_area(hull_m)) / 1e6
}

#' Plot centroid track for a single species
#' @param range_tbl Output of `range_dynamics()`.
#' @param species Single species name.
#' @return ggplot object.
#' @export
plot_centroid_track <- function(range_tbl, species) {
  import::from("dplyr", filter, arrange)
  import::from("ggplot2", ggplot, aes, geom_path, geom_point, geom_sf, labs, theme_minimal)
  import::from("rnaturalearth", ne_countries)
  import::from("sf", st_as_sf)

  dat <- range_tbl |>
    filter(species == !!species) |>
    arrange(dplyr::desc(bin))

  world <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")

  p <- ggplot() +
    geom_sf(data = world, fill = "grey95", color = "grey70", linewidth = 0.2)

  if (nrow(dat) > 1) {
    p <- p + geom_path(data = dat, aes(x = centroid_long, y = centroid_lat), color = "steelblue")
  }

  p +
    geom_point(data = dat, aes(x = centroid_long, y = centroid_lat), size = 2, color = "steelblue") +
    labs(x = "Longitude", y = "Latitude",
         title = paste("Centroid track:", species),
         subtitle = "Lines connect sequential bins (older → younger)") +
    theme_minimal()
}

#' Write centroid track PNGs per species
#' @param range_tbl Output of `range_dynamics()`.
#' @return Tibble: species, path (PNG file path).
#' @export
write_centroid_maps <- function(range_tbl) {
  import::from("dplyr", distinct, pull, tibble)
  import::from("purrr", map)

  sp <- range_tbl |>
    distinct(species) |>
    pull(species)

  out <- map(sp, function(s) {
    p <- plot_centroid_track(range_tbl, s)
    fname <- paste0("centroid_track_", slugify_species(s), ".png")
    path <- write_plot(p, fname, width = 8, height = 6)
    tibble(species = s, path = path)
  })
  dplyr::bind_rows(out)
}

#' Write per-species range metrics CSVs
#' @param range_tbl Output of `range_dynamics()`.
#' @return Tibble: species, path (CSV file path).
#' @export
write_range_metrics_species <- function(range_tbl) {
  import::from("dplyr", group_by, group_split, group_keys, bind_rows)
  import::from("tibble", tibble)
  import::from("purrr", map2)

  if (nrow(range_tbl) == 0) return(tibble(species = character(), path = character()))

  grps <- range_tbl |>
    group_by(species)

  dfs   <- group_split(grps)
  keys  <- group_keys(grps)$species

  res <- map2(keys, dfs, function(s, df) {
    fname <- paste0("range_metrics_", slugify_species(s), ".csv")
    tibble(species = s, path = write_output(df, fname))
  })
  bind_rows(res)
}

#' Slugify species name for filenames
#' @keywords internal
slugify_species <- function(s) {
  s <- gsub("[^A-Za-z0-9]+", "_", s)
  s <- gsub("_+", "_", s)
  s <- gsub("^_|_$", "", s)
  tolower(s)
}
