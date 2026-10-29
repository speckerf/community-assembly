neutral_probabilities <- function(neighbors, config) {
  counts <- tabulate(match(neighbors, config$species_ids), nbins = config$n_species)
  available <- sum(config$immigrant_weights) > 0
  w <- if (available) config$immigrant_ticket_weight else 0
  immigrant_prob <- if (available) config$immigrant_weights / sum(config$immigrant_weights) else rep(0, config$n_species)
  total <- length(neighbors) + w
  setNames(if (total > 0) (counts + w * immigrant_prob) / total else rep(0, config$n_species), config$species_ids)
}

niche_probabilities <- function(neighbors, immigrant, preferred, config) {
  available <- !is.na(immigrant)
  tickets <- c(neighbors, if (available) immigrant else character())
  weights <- c(rep(1, length(neighbors)), if (available) config$immigrant_ticket_weight else numeric())
  probabilities <- setNames(rep(0, config$n_species), config$species_ids)
  if (preferred %in% tickets) probabilities[preferred] <- 1 else {
    for (i in seq_along(tickets)) probabilities[tickets[i]] <- probabilities[tickets[i]] + weights[i]
    if (sum(probabilities) > 0) probabilities <- probabilities / sum(probabilities)
  }
  probabilities
}

recruit_neutral <- function(neighbors, config, preferred = NULL) {
  available <- sum(config$immigrant_weights) > 0
  if (!available && !length(neighbors)) return(empty_recruitment(config))
  weights <- c(rep(1, length(neighbors)), if (available) config$immigrant_ticket_weight else numeric())
  ticket <- sample.int(length(weights), 1L, prob = weights)
  immigration <- available && ticket == length(weights)
  immigrant <- if (immigration) draw_species(config) else NA_character_
  list(species = if (immigration) immigrant else neighbors[ticket],
       candidates = c(neighbors, if (available) NA_character_ else character()), ticket_probabilities = weights / sum(weights),
       species_probabilities = neutral_probabilities(neighbors, config),
       immigrant = immigrant, selected_ticket = ticket, preferred = NA_character_,
       preference_applied = FALSE)
}

recruit_niche <- function(neighbors, config, preferred) {
  available <- sum(config$immigrant_weights) > 0
  if (!available && !length(neighbors)) return(empty_recruitment(config, preferred))
  immigrant <- if (available) draw_species(config) else NA_character_
  candidates <- c(neighbors, if (available) immigrant else character())
  weights <- c(rep(1, length(neighbors)), if (available) config$immigrant_ticket_weight else numeric())
  preference <- preferred %in% candidates
  probabilities <- if (preference) as.numeric(candidates == preferred) * weights else weights
  probabilities <- probabilities / sum(probabilities)
  ticket <- sample.int(length(candidates), 1L, prob = probabilities)
  list(species = candidates[ticket], candidates = candidates,
       ticket_probabilities = probabilities,
       species_probabilities = niche_probabilities(neighbors, immigrant, preferred, config),
       immigrant = immigrant, selected_ticket = ticket, preferred = preferred,
       preference_applied = preference)
}

# No viable local or immigrant source: leave the cell empty.
empty_recruitment <- function(config, preferred = NA_character_) {
  list(species = NA_character_, candidates = character(), ticket_probabilities = numeric(),
       species_probabilities = setNames(rep(0, config$n_species), config$species_ids),
       immigrant = NA_character_, selected_ticket = NA_integer_, preferred = preferred,
       preference_applied = FALSE)
}
