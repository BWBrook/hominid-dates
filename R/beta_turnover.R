# R/beta_turnover.R -------------------------------------------------------------
#' Build species × time-bin incidence per cluster and select key localities
#'
#' @param tbl Tibble like `occ_tbl_no_indet` with at least columns
#'   cluster_id, species, date (Ma), sampling_intensity.
#' @param width Bin width in Ma (default 0.25).
#' @param top_k Number of clusters to auto-select (default 6).
#' @param clusters Optional integer/numeric vector of cluster_id to force include.
#' @return A list with two tibbles:
#'   - incidence: cluster_id, bin_lo, bin_hi, species_list, S, effort
#'   - key_localities: cluster_id plus selection metrics
#' @export
build_incidence_by_cluster <- function(tbl, width = 0.25, top_k = 6, clusters = NULL) {

  import::from("dplyr", filter, mutate, group_by, summarise, ungroup, arrange, n_distinct,
               select, distinct, across)
  import::from("tibble", tibble)
  import::from("tidyr", nest, unnest)
  import::from("rlang", abort)

  req <- c("cluster_id", "species", "date")
  miss <- setdiff(req, names(tbl))
  if (length(miss) > 0) {
    abort(sprintf("beta_turnover: missing required columns: %s", paste(miss, collapse = ", ")), class = "beta_turnover_missing_cols")
  }

  # anchor bins globally on the Ma axis
  df <- tbl |>
    filter(!is.na(cluster_id), !is.na(species), is.finite(date)) |>
    mutate(
      bin_lo = floor(date / width) * width,
      bin_hi = bin_lo + width
    )

  # per cluster × bin: species present, richness, effort proxy
  by_bin <- df |>
    group_by(cluster_id, bin_lo, bin_hi) |>
    summarise(
      species_list = list(sort(unique(species))),
      S       = n_distinct(species),
      effort  = if ("sampling_intensity" %in% names(df)) sum(sampling_intensity, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    ) |>
    arrange(cluster_id, dplyr::desc(bin_lo))

  # cluster-level metrics
  metrics <- by_bin |>
    group_by(cluster_id) |>
    summarise(
      n_bins_nonempty = dplyr::n(),
      n_species_total = length(unique(unlist(species_list))),
      .groups = "drop"
    )

  # selection: criteria + forced include
  eligible <- metrics |>
    filter(n_bins_nonempty >= 3, n_species_total >= 3) |>
    arrange(dplyr::desc(n_bins_nonempty), dplyr::desc(n_species_total))

  forced_ids <- unique(clusters)
  forced <- if (is.null(forced_ids)) {
    tibble(cluster_id = integer(0))
  } else {
    tibble(cluster_id = forced_ids) |> filter(!is.na(cluster_id))
  }
  sel <- dplyr::bind_rows(
    forced,
    eligible |> select(cluster_id)
  ) |>
    distinct() |>
    head(top_k)

  key_localities <- sel |>
    dplyr::left_join(metrics, by = "cluster_id") |>
    arrange(dplyr::desc(n_bins_nonempty), dplyr::desc(n_species_total))

  list(incidence = by_bin, key_localities = key_localities)
}

#' Compute Baselga's beta diversity between successive bins per cluster
#'
#' @param incidence_tbl Output incidence tibble from `build_incidence_by_cluster()`$incidence
#' @return Tidy tibble with beta_total, beta_turnover, beta_nestedness and side metrics.
#' @export
beta_turnover_by_cluster <- function(incidence_tbl) {

  import::from("dplyr", arrange, group_by, summarise, ungroup, mutate, select, bind_rows)
  import::from("tibble", tibble)

  # helper to compute Baselga components directly from sets (Sørensen family)
  beta_pair <- function(sp1, sp2) {
    a <- length(intersect(sp1, sp2))
    b <- length(setdiff(sp1, sp2))
    c <- length(setdiff(sp2, sp1))
    den_sor <- 2 * a + b + c
    beta_sor <- if (den_sor == 0) 0 else (b + c) / den_sor
    min_bc <- min(b, c)
    den_sim <- a + min_bc
    beta_sim <- if (den_sim == 0) 0 else min_bc / den_sim
    beta_sne <- beta_sor - beta_sim
    list(
      beta_total      = beta_sor,
      beta_turnover   = beta_sim,
      beta_nestedness = beta_sne,
      n_shared        = a
    )
  }

  # per cluster: sort bins (older → younger) and compute consecutive pairs
  res <- incidence_tbl |>
    arrange(cluster_id, dplyr::desc(bin_lo)) |>
    group_by(cluster_id) |>
    summarise(
      rows = list(dplyr::pick(dplyr::everything())),
      .groups = "drop"
    ) |>
    mutate(
      pairs = lapply(rows, function(df) {
        # keep non-empty bins
        df2 <- df[df$S > 0, , drop = FALSE]
        if (nrow(df2) < 2) return(tibble())
        # consecutive pairs
        out <- vector("list", nrow(df2) - 1)
        for (i in seq_len(nrow(df2) - 1)) {
          sp1 <- df2$species_list[[i]]
          sp2 <- df2$species_list[[i + 1]]
          bp  <- beta_pair(sp1, sp2)
          out[[i]] <- tibble(
            bin_lo      = df2$bin_lo[i],
            bin_hi      = df2$bin_hi[i],
            bin_next_lo = df2$bin_lo[i + 1],
            bin_next_hi = df2$bin_hi[i + 1],
            beta_total      = bp$beta_total,
            beta_turnover   = bp$beta_turnover,
            beta_nestedness = bp$beta_nestedness,
            S1 = df2$S[i], S2 = df2$S[i + 1],
            effort1 = df2$effort[i], effort2 = df2$effort[i + 1],
            n_shared = bp$n_shared
          )
        }
        bind_rows(out)
      })
    ) |>
    select(cluster_id, pairs)

  out <- bind_rows(lapply(seq_len(nrow(res)), function(i) {
    if (nrow(res$pairs[[i]]) == 0) return(NULL)
    cbind(cluster_id = res$cluster_id[[i]], res$pairs[[i]])
  }))

  if (is.null(out)) {
    return(tibble(cluster_id = integer(), bin_lo = numeric(), bin_hi = numeric(),
                  bin_next_lo = numeric(), bin_next_hi = numeric(), beta_total = numeric(),
                  beta_turnover = numeric(), beta_nestedness = numeric(), S1 = integer(),
                  S2 = integer(), effort1 = numeric(), effort2 = numeric(), n_shared = integer()))
  }
  out
}
