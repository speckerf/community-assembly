# Configuration contains inputs; random count remainders are resolved inside a run.
simulation_config <- function(n_rows = 6L, n_cols = 6L, n_species = 6L,
                              species_ids = NULL, initial_counts = NULL,
                              n_rounds = 5L, deaths_per_round = 9L,
                              immigrant_weights = NULL, immigrant_ticket_weight = 1,
                              affinity_counts = NULL, niche_layout = "pairs",
                              seed = 1L, n_replicates = 1L, allow_extinction = FALSE) {
  if (is.null(species_ids)) {
    species_ids <- if (n_species <= 26) LETTERS[seq_len(n_species)] else paste0("species_", seq_len(n_species))
  } else if (missing(n_species)) n_species <- length(species_ids)
  config <- list(n_rows = n_rows, n_cols = n_cols, n_species = n_species,
                 species_ids = species_ids, initial_counts = initial_counts,
                 n_rounds = n_rounds, deaths_per_round = deaths_per_round,
                 immigrant_weights = immigrant_weights,
                 immigrant_ticket_weight = immigrant_ticket_weight,
                 affinity_counts = affinity_counts, niche_layout = niche_layout,
                 seed = seed, n_replicates = n_replicates, allow_extinction = allow_extinction,
                 neighborhood = "orthogonal", birth_update = "sequential",
                 birth_order = "row_major")
  if (is.null(config$immigrant_weights)) config$immigrant_weights <- rep(1, n_species)
  validate_config(config)
}

species_vector <- function(x, ids, label) {
  if (!is.numeric(x) || length(x) != length(ids) || anyNA(x) ||
      any(!is.finite(x)) || any(x < 0)) stop(label, " must contain nonnegative finite values for each species")
  if (!is.null(names(x))) {
    if (anyDuplicated(names(x)) || !setequal(names(x), ids)) stop(label, " names must match species_ids")
    x <- x[ids]
  }
  setNames(x, ids)
}

validate_config <- function(config) {
  if (is.null(config$allow_extinction)) config$allow_extinction <- FALSE
  if (!is.logical(config$allow_extinction) || length(config$allow_extinction) != 1L ||
      is.na(config$allow_extinction)) stop("allow_extinction must be TRUE or FALSE")
  for (key in c("n_rows", "n_cols", "n_species", "n_rounds", "deaths_per_round", "seed", "n_replicates")) {
    x <- config[[key]]
    lower <- if (key %in% c("n_rounds", "deaths_per_round", "seed")) 0 else 1
    if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
        x != floor(x) || x < lower || x > .Machine$integer.max) stop("Invalid ", key)
  }
  ids <- config$species_ids
  if (!is.character(ids) || length(ids) != config$n_species || anyNA(ids) ||
      any(!nzchar(trimws(ids))) || anyDuplicated(ids)) stop("species_ids must be unique nonempty character labels")
  size <- config$n_rows * config$n_cols
  if (config$deaths_per_round > size) stop("deaths_per_round exceeds grid size")
  for (key in c("initial_counts", "affinity_counts")) {
    if (!is.null(config[[key]])) {
      config[[key]] <- species_vector(config[[key]], ids, key)
      if (any(config[[key]] != floor(config[[key]])) || sum(config[[key]]) != size) stop(key, " must be integer counts summing to grid size")
    }
  }
  config$immigrant_weights <- species_vector(config$immigrant_weights, ids, "immigrant_weights")
  if (sum(config$immigrant_weights) <= 0) stop("At least one immigration weight must be positive")
  w <- config$immigrant_ticket_weight
  if (!is.numeric(w) || length(w) != 1L || is.na(w) || !is.finite(w) || w <= 0) stop("immigrant_ticket_weight must be positive")
  if (!config$niche_layout %in% c("pairs", "explicit")) stop("Unknown niche_layout")
  if (!identical(config$birth_order, "row_major") || !identical(config$birth_update, "sequential") ||
      !identical(config$neighborhood, "orthogonal")) stop("Unsupported birth or neighborhood rule")
  config
}

resolve_species_counts <- function(total, species_ids, counts = NULL, unit = 1L) {
  if (total %% unit != 0) stop("Grid size must be divisible by ", unit)
  if (!is.null(counts)) {
    counts <- species_vector(counts, species_ids, "counts")
    if (sum(counts) != total || any(counts %% unit != 0)) stop("Counts must sum to grid size and be multiples of ", unit)
    return(counts)
  }
  units <- total / unit
  counts <- rep(units %/% length(species_ids), length(species_ids))
  remainder <- units %% length(species_ids)
  if (remainder > 0) {
    selected <- sample.int(length(species_ids), remainder)
    counts[selected] <- counts[selected] + 1L
  }
  setNames(counts * unit, species_ids)
}
