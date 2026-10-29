# Run from project root: Rscript ui/test_ui.R
library(shiny)
for (file in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(file)
source("ui/helpers.R")
source("ui/server_functions.R")
# Incremental playback must be identical to a batch run, including RNG state.
set.seed(72)
before <- .Random.seed
for (model in c("neutral", "niche", "fully_random")) {
  config <- simulation_config(seed = 2026)
  interactive_run <- start_interactive_run(config, model)
  for (i in seq_len(config$n_rounds)) interactive_run <- advance_interactive_run(interactive_run)
  stopifnot(identical(interactive_run, run_simulation(config, model)))
}
stopifnot(identical(before, .Random.seed))

testServer(simulation_server, {
  session$setInputs(rows = 6, cols = 6, species_count = 6, species_labels = "",
                    deaths = 9, seed = 2026, iterations = 3, seconds = 0.1)
  stopifnot(is.null(run()))
  session$setInputs(restart_neutral = 1)
  stopifnot(run()$model == "neutral", length(run()$snapshots) == 1)
  session$setInputs(step = 1)
  stopifnot(phase() == "death", sum(is.na(displayed_grid())) == 9,
            length(run()$snapshots) == 1, running())
  stopifnot(grepl("vacant", as.character(community_grid(displayed_grid(), species_colors(run()$species_ids)))))
  session$elapse(49)
  stopifnot(phase() == "death", length(run()$snapshots) == 1)
  session$elapse(1)
  stopifnot(phase() == "birth", !anyNA(displayed_grid()), length(run()$snapshots) == 2)
  # Completing the birth interval must not invalidate the board a second time.
  board_updates <- new.env(); board_updates$count <- 0L
  board_watch <- observe({
    run(); phase(); animate_cells(); displayed_grid()
    board_updates$count <- board_updates$count + 1L
  })
  session$flushReact()
  before_finish <- board_updates$count
  session$elapse(50)
  stopifnot(!running(), phase() == "birth", board_updates$count == before_finish)
  board_watch$destroy()
  session$setInputs(go = 1)
  stopifnot(phase() == "death", length(run()$snapshots) == 2, remaining() == 3)
  session$elapse(50)
  stopifnot(phase() == "birth", length(run()$snapshots) == 3, remaining() == 2)
  session$elapse(50)
  stopifnot(phase() == "death", sum(is.na(displayed_grid())) == 9)
  session$elapse(200)
  stopifnot(length(run()$snapshots) == 5, !running(), !anyNA(displayed_grid()))
  reference <- run_simulation(simulation_config(n_rounds = 4, seed = 2026), "neutral")
  stopifnot(isTRUE(all.equal(run(), reference)))
  session$setInputs(go = 2)
  stopifnot(phase() == "death")
  session$setInputs(stop = 1)
  stopped <- run()
  session$elapse(500)
  stopifnot(identical(run(), stopped), !running(), !anyNA(displayed_grid()), is.null(pending()))
  session$setInputs(go = 3)
  session$setInputs(restart_niche = 1)
  session$elapse(500)
  stopifnot(run()$model == "niche", length(run()$snapshots) == 1, !running())
  session$setInputs(step = 2)
  stopifnot(sum(is.na(displayed_grid())) == 9)
  session$elapse(100)
  stopifnot(isTRUE(all.equal(run(), run_simulation(simulation_config(n_rounds = 1, seed = 2026), "niche"))))
  # Fast-forward extends the current run, ignoring animation duration.
  session$setInputs(iterations = 3, seconds = -1, go_skip = 1)
  stopifnot(!running(), !animate_cells(), length(run()$snapshots) == 5,
            isTRUE(all.equal(run(), run_simulation(simulation_config(n_rounds = 4, seed = 2026), "niche"))))
  skipped <- run()
  session$elapse(500)
  stopifnot(identical(run(), skipped))
  session$setInputs(iterations = 0, go_skip = 2)
  stopifnot(identical(run(), skipped))
  session$setInputs(allow_extinction = TRUE, rows = 1, cols = 2, species_count = 2,
                    deaths = 2, restart_neutral = 2, iterations = 2)
  stopifnot(isTRUE(run()$config$allow_extinction))
  session$setInputs(go_skip = 3)
  stopifnot(all(is.na(displayed_grid())), all(tail(run()$species$abundance, 2) == 0))
  session$setInputs(allow_extinction = FALSE, rows = 6, cols = 6, species_count = 6,
                    deaths = 9, restart_niche = 4)
  session$setInputs(restart_random = 1, iterations = 3, seconds = .1)
  stopifnot(run()$model == "fully_random", is.null(run()$affinities))
  session$setInputs(go_skip = 4)
  stopifnot(isTRUE(all.equal(run(), run_simulation(simulation_config(n_rounds = 3, seed = 2026), "fully_random"))))
  session$setInputs(step = 3)
  session$elapse(100)
  stopifnot(isTRUE(all.equal(run(), run_simulation(simulation_config(n_rounds = 4, seed = 2026), "fully_random"))))
  session$setInputs(restart_niche = 5)
  # Invalid restart preserves the existing run, and stops playback.
  session$setInputs(rows = 3, cols = 3, restart_niche = 2)
  stopifnot(run()$config$n_rows == 6, run()$model == "niche")
  session$setInputs(rows = 2, cols = 4, species_count = 2,
                    species_labels = "oak, beech", deaths = 2, restart_niche = 3)
  stopifnot(identical(run()$species_ids, c("oak", "beech")), nrow(run()$affinities) == 2)
  session$setInputs(seconds = -1, go = 4)
  stopifnot(!running())
})
# Plot both the initial point and a history containing an extinct species.
plot_file <- tempfile(fileext = ".pdf")
grDevices::pdf(plot_file, width = 7, height = 3.5)
plot_species_frequencies(data.frame(generation = 0, species_id = "oak", frequency = 1), "oak")
plot_species_frequencies(data.frame(generation = rep(0:2, each = 2),
                                    species_id = rep(c("oak", "beech"), 3),
                                    frequency = c(.5, .5, .75, .25, 1, 0)), c("oak", "beech"))
invisible(grDevices::dev.off())
stopifnot(file.info(plot_file)$size > 0)
unlink(plot_file)
cat("All UI playback and frequency plot checks passed.\n")
