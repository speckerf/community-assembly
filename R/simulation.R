disturb <- function(grid, config) {
  occupied <- which(!is.na(grid))
  if (config$deaths_per_round > length(occupied) && !isTRUE(config$allow_extinction)) stop("Not enough occupied cells for disturbance")
  count <- min(config$deaths_per_round, length(occupied))
  indices <- occupied[sample.int(length(occupied), count)]
  cells <- arrayInd(indices, dim(grid))
  deaths <- data.frame(row = cells[, 1], col = cells[, 2], species_id = grid[indices])
  grid[indices] <- NA_character_
  list(grid = grid, vacancies = cells, deaths = deaths)
}

establish <- function(grid, vacancies, config, recruitment_fn, affinities = NULL) {
  vacancies <- vacancies[order(vacancies[, 1], vacancies[, 2]), , drop = FALSE]
  events <- vector("list", nrow(vacancies))
  for (i in seq_len(nrow(vacancies))) {
    cell <- vacancies[i, ]
    neighbors <- neighbor_species(grid, cell)
    preferred <- if (is.null(affinities)) NULL else affinities[cell[1], cell[2]]
    recruitment <- recruitment_fn(neighbors, config, preferred)
    grid[cell[1], cell[2]] <- recruitment$species
    events[[i]] <- c(list(birth_order = i, row = cell[1], col = cell[2], neighbors = neighbors), recruitment)
  }
  list(grid = grid, births = events)
}

step_community <- function(state, config, recruitment_fn) {
  death <- disturb(state$grid, config)
  recruitment_config <- config
  if (isTRUE(config$allow_extinction)) {
    alive <- unique(as.vector(death$grid[!is.na(death$grid)]))
    recruitment_config$immigrant_weights[!config$species_ids %in% alive] <- 0
    recruitment_config$eligible_species <- config$species_ids[config$species_ids %in% alive]
  }
  # Also retry vacancies that could not recruit in an earlier round.
  vacancies <- which(is.na(death$grid), arr.ind = TRUE)
  birth <- establish(death$grid, vacancies, recruitment_config, recruitment_fn, state$affinities)
  list(grid = birth$grid, affinities = state$affinities,
       events = list(deaths = death$deaths, births = birth$births))
}

run_simulation <- function(config = simulation_config(), model = c("neutral", "niche", "fully_random"),
                           initial_grid = NULL, affinities = NULL) {
  model <- match.arg(model)
  config <- validate_config(config)
  with_simulation_seed(config$seed, function() {
    size <- config$n_rows * config$n_cols
    if (is.null(initial_grid)) {
      config$initial_counts <- resolve_species_counts(size, config$species_ids, config$initial_counts)
      initial_grid <- initialize_community(config)
    } else {
      validate_grid(initial_grid, config)
      actual <- setNames(species_frequencies(initial_grid, config$species_ids)$abundance, config$species_ids)
      if (!is.null(config$initial_counts) && any(actual != config$initial_counts)) stop("Initial grid counts do not match configuration")
      config$initial_counts <- actual
    }
    tiling <- NULL
    if (model == "niche") {
      if (is.null(affinities)) {
        config$affinity_counts <- resolve_species_counts(size, config$species_ids, config$affinity_counts, unit = 2L)
        layout <- initialize_affinities(config)
        affinities <- layout$affinities; tiling <- layout$tiling
      } else config$niche_layout <- "explicit"
      validate_affinities(affinities, config)
      config$affinity_counts <- setNames(species_frequencies(affinities, config$species_ids)$abundance, config$species_ids)
    } else if (!is.null(affinities)) stop("Affinities are only used by the niche model")
    recruitment_fn <- recruitment_function(model)
    state <- list(grid = initial_grid, affinities = affinities)
    snapshots <- vector("list", config$n_rounds + 1L)
    snapshots[[1]] <- initial_grid
    events <- vector("list", config$n_rounds)
    for (generation in seq_len(config$n_rounds)) {
      state <- step_community(state, config, recruitment_fn)
      snapshots[[generation + 1L]] <- state$grid
      events[[generation]] <- c(list(generation = generation), state$events)
    }
    species <- do.call(rbind, lapply(seq_along(snapshots), function(i) {
      cbind(replicate = 1L, generation = i - 1L, species_frequencies(snapshots[[i]], config$species_ids))
    }))
    community <- do.call(rbind, lapply(seq_along(snapshots), function(i) {
      cbind(replicate = 1L, generation = i - 1L, community_metrics(snapshots[[i]], config$species_ids))
    }))
    list(config = config, model = model, species_ids = config$species_ids,
         metadata = list(seed = config$seed, rng_kind = RNGkind(),
                         rng_state = get(".Random.seed", envir = .GlobalEnv),
                         r_version = R.version.string),
         initial_grid = initial_grid, affinities = affinities, tiling = tiling,
         snapshots = snapshots, species = species, community = community, events = events)
  })
}

run_replicates <- function(config, model, seeds) {
  config <- validate_config(config)
  if (!is.numeric(seeds) || length(seeds) != config$n_replicates || anyNA(seeds) ||
      any(!is.finite(seeds)) || any(seeds < 0 | seeds > .Machine$integer.max | seeds != floor(seeds)) ||
      anyDuplicated(seeds)) stop("Provide one distinct valid integer seed per replicate")
  runs <- lapply(seq_along(seeds), function(i) {
    current <- config; current$seed <- seeds[i]
    result <- run_simulation(current, model)
    result$species$replicate <- i; result$community$replicate <- i
    result
  })
  list(seeds = seeds, runs = runs,
       species = do.call(rbind, lapply(runs, `[[`, "species")),
       community = do.call(rbind, lapply(runs, `[[`, "community")))
}
