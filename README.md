# Community assembly

An R simulation and interactive Shiny app for exploring neutral and niche-based
community assembly. The default community contains six species on a 6 × 6 grid.
Each round removes nine individuals, then recruits replacements sequentially
from top left to bottom right.

In the neutral model, recruitment draws from neighboring individuals and an
external immigrant pool. In the niche model, each cell has a preferred species;
that species wins whenever it is among the recruitment candidates.

## Acknowledgment

**Katharina Runge developed the original game and simulation idea.** This project
implements that idea as an R simulation with an interactive interface.

## Clone and set up

```sh
git clone https://github.com/speckerf/community-assembly.git
cd community-assembly
Rscript -e 'install.packages("shiny", repos = "https://cloud.r-project.org")'
```

## Start the interactive app

Run this command in a terminal:

```sh
Rscript -e 'shiny::runApp("ui", host = "127.0.0.1", port = 3838, launch.browser = FALSE)'
```

Keep that terminal running and open [http://127.0.0.1:3838](http://127.0.0.1:3838)
in your browser. Press **Ctrl+C** in the terminal to stop the app process. The

Alternatively, open `community-assembly.Rproj` in RStudio and run:

```r
shiny::runApp("ui")
```

### Use the simulation

1. Choose the grid size, species count, optional species labels, death count,
   and random seed.
2. Optionally check **Allow extinction**. Species with zero survivors after a
   death phase will then be excluded from immigration. This setting applies
   when you initialize or restart.
3. Click **Initialize / restart neutral** or **Initialize / restart niche-based**.
4. Click **Next generation** to advance one round, or set **Iterations** and
   click **Go** to animate several rounds. Each round displays deaths for half
   of **Seconds / generation**, then new recruits for the other half.
5. Use **Go — skip animations** to compute the selected iterations and show
   the final grid immediately, retaining all intermediate results.
6. Expand **Relative frequency over time** or **Generation history** to inspect
   the results. **Save full run (.rds)** downloads the complete run.

Restarting resets the simulation and applies current settings. Using the same
settings and seed reproduces the same run. Niche layouts require an even number
of grid cells because habitat preferences are assigned in adjacent pairs.

See [the UI guide](ui/README.md) for additional details.

<img width="1138" height="835" alt="image" src="https://github.com/user-attachments/assets/64cd4baa-0b54-4868-8a7a-f4c48ddc7af8" />

