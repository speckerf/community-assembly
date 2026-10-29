# Interactive community assembly

From the project root, start the Shiny app:

```r
shiny::runApp("ui")
```

The only additional dependency is `shiny` (`install.packages("shiny")` if needed).

1. Set dimensions, species count, optional comma-separated character labels,
   deaths per generation, and seed.
2. Choose **Initialize / restart neutral** or **Initialize / restart niche-based**.
   Both start at generation zero. Restarts apply current settings and cancel playback.
3. Press **Next generation** for one full disturbance-and-establishment round.
4. Set **Iterations** and **Seconds / generation**, then **Go**. Each generation first shows empty cells for deaths for half the selected
   duration, then new recruits for the other half. The same animation applies
   to **Next generation**. Metrics and downloads reflect completed generations.
5. **Stop** cancels remaining iterations and keeps the last completed community.
   If stopped during the death preview, that unfinished generation is discarded. A new Go
   adds the requested number of iterations. Timing changes apply on the next Go.

The affinity map is fixed for each niche run and can be hidden. Colors and text
identify species; hover over a cell to read its coordinates and full species label.
Grid dimensions are limited to 30 per side in the UI and species count to 100.
Niche initialization requires an even grid area. Invalid settings show an error
and retain the existing community. Changing the seed produces a new realization;
restarting with the same settings reproduces the same run.

**Save full run** downloads an RDS containing configuration, all snapshots,
metrics, events, affinity data, and RNG state. UI sessions are independent.
Animation displays separate death and birth phases; birth events inside a generation still
follow the core top-left-to-bottom-right rule.

All UI code, styling, and UI tests live in this folder. The simulation rules are
loaded from `../R`. The core result metadata includes the final RNG state so
incremental playback is identical to running the same generations in a batch.
No ecological rules are duplicated in the UI.

Run verification from the project root:

```sh
Rscript ui/test_ui.R
Rscript tests/test_simulation.R
```

Expand **Relative frequency over time** to see one colored line per species,
with a point at generation zero and after each completed birth phase. The time
axis grows as the simulation runs, while the frequency axis stays at 0–100%.
Extinct species remain plotted at zero. Restarting also resets the graph.

**Go — skip animations** adds the selected number of iterations immediately,
without timed death/birth frames. The grid updates once at the end; the frequency
graph, history, and download retain every intermediate generation. The seconds
setting is ignored. Stop animated playback before using this button.

**Allow extinction** (off by default) excludes species absent after the death
phase from the immigrant pool before any births occur. This includes species
absent at initialization. Remaining immigration weights are renormalized; the
original species list is retained in legends, history, and plots with zero
abundances for extinct species. The setting applies on initialization/restart.
If all species die, recruitment has no source and the community remains empty.
With fewer occupied cells than the requested death count, all occupied cells die.
