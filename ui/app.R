# Start from the project root with shiny::runApp("ui").
library(shiny)
project_root <- if (dir.exists("../R")) normalizePath("..") else normalizePath(".")
for (file in list.files(file.path(project_root, "R"), pattern = "\\.R$", full.names = TRUE)) source(file, local = TRUE)
source(file.path(project_root, "ui", "helpers.R"), local = TRUE)
source(file.path(project_root, "ui", "server_functions.R"), local = TRUE)

ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
            tags$script(src = "controls.js")),
  div(class = "app-shell",
    tags$header(class = "page-header",
      div(tags$p(class = "eyebrow", "ECOLOGY / COMMUNITY ASSEMBLY"),
          h1("A forest, one generation at a time."),
          p(class = "subtitle", "Explore how chance and habitat preference shape a community.")),
      div(class = "header-mark", "6 × 6", tags$small("START SMALL. WATCH IT CHANGE."))
    ),
    div(class = "workspace",
      tags$aside(class = "control-panel",
        h2("Start a community"),
        p(class = "hint", "Choose a scenario to initialize or restart with these settings."),
        div(class = "input-pair", numericInput("rows", "Rows", 6, min = 1, max = 30),
            numericInput("cols", "Columns", 6, min = 1, max = 30)),
        numericInput("species_count", "Number of species", 6, min = 1, max = 100),
        textInput("species_labels", "Species labels (optional)", "", placeholder = "oak, beech, pine, …"),
        p(class = "hint", "Comma-separated, one unique label per species. Leave blank for A, B, C…"),
        div(class = "input-pair", numericInput("deaths", "Deaths / generation", 9, min = 0),
            numericInput("seed", "Random seed", 42, min = 0)),
        checkboxInput("allow_extinction", "Allow extinction", FALSE),
        p(class = "hint", "Exclude species with no survivors after deaths from immigration. Applies on restart."),
        div(class = "scenario-controls",
            actionButton("restart_random", "Initialize: neutral - fully random", class = "btn-model"),
            actionButton("info_random", label = NULL, icon = icon("circle-info"),
                         class = "scenario-info", title = "Birth rules: neutral - fully random",
                         `aria-label` = "Birth rules: neutral - fully random")),
        div(class = "scenario-controls",
            actionButton("restart_neutral", "Initialize: neutral - favor adjacent", class = "btn-model"),
            actionButton("info_neutral", label = NULL, icon = icon("circle-info"),
                         class = "scenario-info", title = "Birth rules: neutral - favor adjacent",
                         `aria-label` = "Birth rules: neutral - favor adjacent")),
        div(class = "scenario-controls",
            actionButton("restart_niche", "Initialize: niche-based", class = "btn-model niche-button"),
            actionButton("info_niche", label = NULL, icon = icon("circle-info"),
                         class = "scenario-info", title = "Birth rules: niche-based",
                         `aria-label` = "Birth rules: niche-based")),
        tags$hr(), h2("Move through time"),
        actionButton("step", "Next generation →", class = "btn-step"),
        div(class = "input-pair", numericInput("iterations", "Iterations", 5, min = 1, max = 10000),
            numericInput("seconds", "Seconds / generation", 1, min = 0.1, max = 30, step = 0.1)),
        div(class = "play-controls", actionButton("go", "Go ▶", class = "btn-go"),
            actionButton("stop", "Stop", class = "btn-stop")),
        actionButton("go_skip", "Go — skip animations", class = "btn-step")
      ),
      tags$main(class = "simulation-panel",
        uiOutput("status"),
        div(class = "grid-card", uiOutput("board"),
            p(class = "board-caption", "Empty cells show deaths; outlined cells show new recruits. Metrics reflect completed generations. Births proceed from top left to bottom right.")),
        uiOutput("legend"),
        uiOutput("metrics"),
        conditionalPanel(condition = "input.show_affinity == true", uiOutput("affinity")),
        checkboxInput("show_affinity", "Show niche preferences", TRUE),
        tags$details(class = "history frequency-history",
          tags$summary("Relative frequency over time"),
          p(class = "hint", "Each point is a completed generation. Colors match the species in the grid; lines connect successive observations."),
          plotOutput("frequency_plot", height = "320px"), uiOutput("frequency_legend")),
        tags$details(class = "history", tags$summary("Generation history"), tableOutput("history"))
      )
    )
  )
)
shinyApp(ui, simulation_server)
