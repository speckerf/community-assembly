validate_affinities <- function(affinities, config) {
  validate_grid(affinities, config)
  counts <- setNames(as.numeric(table(factor(affinities, levels = config$species_ids))), config$species_ids)
  if (!is.null(config$affinity_counts) && !identical(as.numeric(counts), as.numeric(config$affinity_counts))) stop("Affinity counts do not match configuration")
  invisible(affinities)
}

initialize_affinities <- function(config) {
  if (config$niche_layout != "pairs") stop("Supply an affinity matrix for niche_layout = 'explicit'")
  nr <- config$n_rows; nc <- config$n_cols
  counts <- resolve_species_counts(nr * nc, config$species_ids, config$affinity_counts, unit = 2L)
  # A rectangular grid with even area always has an even side.
  tiling <- matrix(0L, nr, nc)
  horizontal <- if (nc %% 2L != 0L) FALSE else if (nr %% 2L != 0L) TRUE else sample.int(2L, 1L) == 1L
  id <- 0L
  if (horizontal) {
    for (r in seq_len(nr)) for (c in seq.int(1L, nc, by = 2L)) {
      id <- id + 1L; tiling[r, c + 0:1] <- id
    }
  } else {
    for (r in seq.int(1L, nr, by = 2L)) for (c in seq_len(nc)) {
      id <- id + 1L; tiling[r + 0:1, c] <- id
    }
  }
  pool <- rep(config$species_ids, counts / 2L)
  labels <- pool[sample.int(length(pool))]
  list(affinities = matrix(labels[tiling], nr, nc), tiling = tiling)
}
