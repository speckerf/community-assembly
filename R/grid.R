neighbor_cells <- function(cell, dimensions) {
  candidates <- rbind(cell + c(-1L, 0L), cell + c(0L, -1L),
                      cell + c(0L, 1L), cell + c(1L, 0L))
  candidates[candidates[, 1] >= 1 & candidates[, 1] <= dimensions[1] &
               candidates[, 2] >= 1 & candidates[, 2] <= dimensions[2], , drop = FALSE]
}

neighbor_species <- function(grid, cell) {
  values <- grid[neighbor_cells(cell, dim(grid))]
  values[!is.na(values)]
}

initialize_community <- function(config) {
  counts <- resolve_species_counts(config$n_rows * config$n_cols, config$species_ids, config$initial_counts)
  pool <- rep(config$species_ids, counts)
  matrix(pool[sample.int(length(pool))], config$n_rows, config$n_cols, byrow = TRUE)
}

validate_grid <- function(grid, config, allow_empty = FALSE) {
  if (!is.matrix(grid) || !is.character(grid) ||
      !identical(dim(grid), as.integer(c(config$n_rows, config$n_cols))) ||
      any(!is.na(grid) & !grid %in% config$species_ids) || (!allow_empty && anyNA(grid))) {
    stop("Grid must be a character matrix of configured dimensions with valid species labels")
  }
  invisible(grid)
}
