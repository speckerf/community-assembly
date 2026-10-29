# Base-R behavioral checks: Rscript tests/test_simulation.R
for (file in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(file)
expect_error <- function(expr) stopifnot(inherits(tryCatch({force(expr); NULL}, error = identity), "error"))
cfg <- simulation_config()
set.seed(99)
old_seed <- .Random.seed
neutral <- run_simulation(cfg, "neutral")
niche <- run_simulation(cfg, "niche")
stopifnot(identical(old_seed, .Random.seed), identical(neutral, run_simulation(cfg, "neutral")))
expect_error(with_simulation_seed(3, function() stop("test")))
stopifnot(identical(old_seed, .Random.seed))
for (result in list(neutral, niche)) {
  stopifnot(length(result$snapshots) == 6, all(table(result$initial_grid) == 6),
            all(result$community$mean_abundance == 6))
  for (grid in result$snapshots) stopifnot(!anyNA(grid), all(dim(grid) == c(6, 6)))
  for (event in result$events) {
    stopifnot(nrow(event$deaths) == 9, !anyDuplicated(event$deaths[c("row", "col")]))
    positions <- vapply(event$births, function(b) (b$row - 1) * 6 + b$col, numeric(1))
    stopifnot(!is.unsorted(positions))
  }
}
stopifnot(all(table(niche$affinities) == 6))
for (id in unique(as.vector(niche$tiling))) {
  cells <- which(niche$tiling == id, arr.ind = TRUE)
  stopifnot(nrow(cells) == 2, sum(abs(cells[1, ] - cells[2, ])) == 1,
            length(unique(niche$affinities[cells])) == 1)
}
stopifnot(nrow(neighbor_cells(c(1, 1), c(6, 6))) == 2,
          nrow(neighbor_cells(c(1, 2), c(6, 6))) == 3,
          nrow(neighbor_cells(c(2, 2), c(6, 6))) == 4)
p <- neutral_probabilities(c("A", "A"), cfg)
stopifnot(abs(p['A'] - 13/18) < 1e-12, abs(sum(p) - 1) < 1e-12,
          all(neutral_probabilities(character(), cfg) == 1/6))
p <- niche_probabilities(c("A", "A"), "B", "C", cfg)
stopifnot(p['A'] == 2/3, p['B'] == 1/3,
          niche_probabilities(c("A", "A"), "B", "B", cfg)['B'] == 1)
# A newly established cell supplies the next birth.
grid <- matrix(NA_character_, 1, 2)
fixed <- function(neighbors, config, preferred) list(species = "A")
birth <- establish(grid, rbind(c(1, 2), c(1, 1)), cfg, fixed)
stopifnot(identical(birth$births[[2]]$neighbors, "A"))
small <- simulation_config(n_rows = 2, n_cols = 3, species_ids = c("oak", "beech"), deaths_per_round = 6)
s <- run_simulation(small, "niche")
stopifnot(all(s$species$species_id %in% c("oak", "beech")), !anyNA(s$snapshots[[6]]))
stopifnot(species_frequencies(matrix("oak", 1, 1), c("oak", "beech"))$abundance[2] == 0)
zero <- run_simulation(simulation_config(n_rounds = 0, deaths_per_round = 0))
stopifnot(length(zero$snapshots) == 1, length(zero$events) == 0)
run_simulation(simulation_config(n_rounds = 1, deaths_per_round = 0)) |> invisible()
one <- simulation_config(n_rows = 1, n_cols = 1, species_ids = "oak", deaths_per_round = 1)
stopifnot(run_simulation(one)$snapshots[[6]][1, 1] == "oak")
expect_error(run_simulation(one, "niche"))
explicit <- run_simulation(one, "niche", affinities = matrix("oak", 1, 1))
stopifnot(explicit$config$niche_layout == "explicit")
expect_error(simulation_config(species_ids = c("oak", "oak")))
expect_error(simulation_config(deaths_per_round = 37))
expect_error(simulation_config(initial_counts = rep(1, 6)))
reps <- run_replicates(simulation_config(n_replicates = 2), "neutral", c(10, 11))
stopifnot(nrow(reps$community) == 12, identical(reps$seeds, c(10, 11)))
# Also restore an originally absent seed.
rm(.Random.seed, envir = .GlobalEnv)
invisible(run_simulation(cfg))
stopifnot(!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
cat("All simulation checks passed.\n")

# Extinction is checked after disturbance, before immigration, in both models.
for (model in c("neutral", "niche")) {
  cfg <- simulation_config(n_rows = 1, n_cols = 2, species_ids = c("oak", "beech"),
                           deaths_per_round = 1, n_rounds = 8, allow_extinction = TRUE)
  result <- run_simulation(cfg, model)
  lost <- result$events[[1]]$deaths$species_id
  stopifnot(all(vapply(result$snapshots[-1], function(g) !lost %in% g, logical(1))),
            all(result$species$abundance[result$species$species_id == lost & result$species$generation > 0] == 0))
  cfg$deaths_per_round <- 2
  empty <- run_simulation(cfg, model)
  stopifnot(all(vapply(empty$snapshots[-1], function(g) all(is.na(g)), logical(1))),
            all(empty$community$richness[-1] == 0))
}
# A surviving species with zero immigration weight can recruit only locally.
cfg <- simulation_config(species_ids = c("oak", "beech"))
cfg$immigrant_weights[] <- 0
stopifnot(recruit_neutral("oak", cfg)$species == "oak",
          recruit_niche("oak", cfg, "beech")$species == "oak",
          is.na(recruit_neutral(character(), cfg)$species),
          all(neutral_probabilities(character(), cfg) == 0))
expect_error(simulation_config(allow_extinction = NA))
cat("All extinction checks passed.\n")
