# R/st_cluster.R ---------------------------------------------------------------
#' Spatio-temporal clustering with HDBSCAN in (lon, lat, date)
#'
#' Scales each axis and clusters with `dbscan::hdbscan`. Noise points (0)
#' receive `NA` cluster_id. Returns a tibble with required column order and
#' sorting: cluster_id asc (NA last), then date, then id.
#'
#' @param occ_tbl Occurrence tibble with columns `id`, `lon`, `lat`, `date`.
#' @param minPts HDBSCAN `minPts` (default 5).
#' @param date_weight Multiplier applied to the scaled date axis (default 1).
#' @param scale_axes Logical; z-score each axis before clustering (default TRUE).
#' @return Tibble with columns: id, lon, lat, date, cluster_id, and all others.
#' @export
st_cluster <- function(occ_tbl, minPts = 5, date_weight = 1, scale_axes = TRUE) {

  import::from("dplyr", mutate, arrange, select, relocate, bind_cols)
  import::from("dbscan", hdbscan)

  # Ensure required columns exist
  req <- c("id", "lon", "lat", "date")
  missing <- setdiff(req, names(occ_tbl))
  if (length(missing) > 0) {
    stop(sprintf("st_cluster: missing required columns: %s", paste(missing, collapse = ", ")))
  }

  x <- occ_tbl
  valid <- is.finite(x$lon) & is.finite(x$lat) & is.finite(x$date)

  cluster_id <- rep(NA_integer_, nrow(x))

  if (sum(valid) >= max(2L, minPts)) {
    m <- cbind(x$lon[valid], x$lat[valid], x$date[valid])
    if (scale_axes) {
      # z-score each axis; weight time if requested
      m[, 1] <- as.numeric(scale(m[, 1]))
      m[, 2] <- as.numeric(scale(m[, 2]))
      m[, 3] <- as.numeric(scale(m[, 3])) * date_weight
    } else if (!identical(date_weight, 1)) {
      m[, 3] <- m[, 3] * date_weight
    }

    fit <- tryCatch(hdbscan(m, minPts = minPts), error = function(e) e)
    if (inherits(fit, "error") || is.null(fit$cluster)) {
      warning("st_cluster: HDBSCAN failed; assigning NA cluster_id to all rows")
    } else {
      # Map 0 (noise) -> NA
      cl <- as.integer(fit$cluster)
      cl[cl == 0L] <- NA_integer_
      cluster_id[valid] <- cl
      if (all(is.na(cluster_id[valid]))) {
        warning("st_cluster: HDBSCAN produced only noise; all cluster_id = NA")
      } else if (any(is.na(cluster_id[valid]))) {
        warning(sprintf(
          "st_cluster: %d/%d points unassigned (noise)",
          sum(is.na(cluster_id[valid])), sum(valid)
        ))
      }
    }
  } else {
    warning("st_cluster: insufficient valid rows for clustering; all cluster_id = NA")
  }

  out <- x |>
    mutate(cluster_id = cluster_id) |>
    # enforce column order: id, lon, lat, date, cluster_id, others
    relocate(id, lon, lat, date, cluster_id) |>
    # sort: cluster_id asc (NA last), then date, then id
    arrange(is.na(cluster_id), cluster_id, date, id)

  out
}

#' Mixing index per cluster (temporal span)
#'
#' @param st_tbl Output of `st_cluster()`.
#' @return Tibble: cluster_id, n_points, mixing_index, date_min, date_max.
#' @export
mixing_index_from_st <- function(st_tbl) {
  import::from("dplyr", filter, group_by, summarise, arrange)

  st_tbl |>
    filter(!is.na(cluster_id)) |>
    group_by(cluster_id) |>
    summarise(
      n_points     = dplyr::n(),
      date_min     = min(date, na.rm = TRUE),
      date_max     = max(date, na.rm = TRUE),
      mixing_index = date_max - date_min,
      .groups = "drop"
    ) |>
    arrange(cluster_id)
}

#' Plot mixing index by cluster (lollipop)
#' @export
plot_mixing_index <- function(mix_tbl) {
  import::from("ggplot2", ggplot, aes, geom_segment, geom_point, labs, theme_minimal)

  ggplot(mix_tbl, aes(x = cluster_id, y = mixing_index)) +
    geom_segment(aes(xend = cluster_id, y = 0, yend = mixing_index), alpha = 0.6) +
    geom_point(size = 2) +
    labs(x = "cluster_id", y = "Mixing index (Delta date, Ma)",
         title = "Within-cluster temporal mixing index") +
    theme_minimal()
}
