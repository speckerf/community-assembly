# UI adapters: ecological rules remain in R/.
start_interactive_run <- function(config, model) {
  config$n_rounds <- 0L
  run_simulation(config, model)
}

advance_interactive_run <- function(result) {
  with_simulation_seed(result$config$seed, function() {
    assign(".Random.seed", result$metadata$rng_state, envir = .GlobalEnv)
    recruitment <- if (result$model == "neutral") recruit_neutral else recruit_niche
    state <- step_community(list(grid = tail(result$snapshots, 1)[[1]], affinities = result$affinities),
                            result$config, recruitment)
    generation <- length(result$snapshots)
    result$snapshots[[generation + 1L]] <- state$grid
    result$events[[generation]] <- c(list(generation = generation), state$events)
    result$species <- rbind(result$species, cbind(replicate = 1L, generation = generation,
                                                 species_frequencies(state$grid, result$species_ids)))
    result$community <- rbind(result$community, cbind(replicate = 1L, generation = generation,
                                                     community_metrics(state$grid, result$species_ids)))
    result$config$n_rounds <- generation
    result$metadata$rng_state <- get(".Random.seed", envir = .GlobalEnv)
    result
  })
}

ui_number <- function(value, label, min, max, integer = TRUE) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) || !is.finite(value) ||
      value < min || value > max || (integer && value != floor(value))) {
    stop(label, " must be ", if (integer) "a whole number " else "a number ", "between ", min, " and ", max, ".")
  }
  value
}

species_colors <- function(ids) setNames(grDevices::hcl.colors(length(ids), "Dark 3"), ids)

community_grid <- function(grid, colors, deaths = NULL, label = "Community") {
  cells <- lapply(seq_len(nrow(grid)), function(r) {
    lapply(seq_len(ncol(grid)), function(c) {
      species <- grid[r, c]
      empty <- is.na(species)
      replaced <- !empty && !is.null(deaths) && any(deaths$row == r & deaths$col == c)
      shiny::tags$div(class = paste("grid-cell", if (empty) "vacant" else if (replaced) "replaced" else ""),
                      style = paste0("--species-color:", if (empty) "#b8c7b7" else colors[[species]], ";"),
                      role = "img", title = paste0("Row ", r, ", column ", c, ": ", if (empty) "Empty (death phase)" else species,
                                                   if (replaced) " (recruited this generation)" else ""),
                      shiny::tags$span(if (empty) "" else species))
    })
  })
  shiny::tags$div(class = "community-grid", role = "group", `aria-label` = label,
                  style = sprintf("grid-template-columns:repeat(%d,minmax(0,1fr));", ncol(grid)), cells)
}

# Fixed frequency limits allow comparison while the time axis grows with the run.
plot_species_frequencies <- function(species, species_ids) {
  colors <- species_colors(species_ids)
  last_generation <- max(species$generation)
  old <- par(mar = c(4, 4.5, 1, 1), bg = "#f3f4ed", col.axis = "#65736a",
             col.lab = "#243c32", las = 1)
  on.exit(par(old))
  plot(NA, xlim = c(0, max(1, last_generation)), ylim = c(0, 1),
       xlab = "Generation", ylab = "Relative frequency", axes = FALSE,
       xaxs = "i", yaxs = "i")
  ticks <- if (last_generation == 0) 0 else pretty(c(0, last_generation), n = 6)
  ticks <- ticks[ticks >= 0 & ticks <= last_generation & ticks == floor(ticks)]
  abline(h = seq(0, 1, 0.2), col = "#dce2d8", lwd = 1)
  axis(1, at = ticks, col = "#b8c7b7")
  axis(2, at = seq(0, 1, 0.2), labels = paste0(seq(0, 100, 20), "%"), col = "#b8c7b7")
  for (id in species_ids) {
    observations <- species[species$species_id == id, ]
    observations <- observations[order(observations$generation), ]
    lines(observations$generation, observations$frequency, type = "o",
          pch = 16, cex = 0.75, lwd = 2, col = colors[[id]])
  }
  invisible(NULL)
}
