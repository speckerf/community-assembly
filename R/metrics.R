species_frequencies <- function(grid, species_ids) {
  abundance <- tabulate(match(as.vector(grid), species_ids), nbins = length(species_ids))
  data.frame(species_id = species_ids, abundance = abundance, frequency = abundance / length(grid))
}

community_metrics <- function(grid, species_ids) {
  species <- species_frequencies(grid, species_ids)
  present <- species$abundance > 0
  data.frame(richness = sum(present), extinct_species = sum(!present),
             mean_abundance = mean(species$abundance), min_abundance = min(species$abundance),
             max_abundance = max(species$abundance), mean_frequency = mean(species$frequency),
             min_frequency = min(species$frequency), max_frequency = max(species$frequency),
             mean_abundance_survivors = if (any(present)) mean(species$abundance[present]) else NA_real_,
             mean_frequency_survivors = if (any(present)) mean(species$frequency[present]) else NA_real_)
}
