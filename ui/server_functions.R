simulation_server <- function(input, output, session) {
  run <- reactiveVal(NULL)
  running <- reactiveVal(FALSE)
  remaining <- reactiveVal(0L)
  interval <- reactiveVal(1000)
  phase <- reactiveVal("ready")
  pending <- reactiveVal(NULL)
  animate_cells <- reactiveVal(TRUE)

  cancel_playback <- function() {
    running(FALSE); remaining(0L); pending(NULL); phase("ready")
  }
  displayed_grid <- reactive({
    req(run())
    grid <- tail(run()$snapshots, 1)[[1]]
    if (phase() == "death" && !is.null(pending())) {
      deaths <- tail(pending()$events, 1)[[1]]$deaths
      grid[as.matrix(deaths[c("row", "col")])] <- NA_character_
    }
    grid
  })

  report_error <- function(error) showNotification(conditionMessage(error), type = "error", duration = 8)
  initialize <- function(model) {
    cancel_playback()
    tryCatch({
      n <- ui_number(input$species_count, "Species count", 1, 100)
      labels <- trimws(input$species_labels)
      ids <- if (nzchar(labels)) trimws(strsplit(labels, ",", fixed = TRUE)[[1]]) else NULL
      if (nzchar(labels) && grepl(",$", labels)) stop("Species labels cannot end with a comma.")
      config <- simulation_config(
        n_rows = ui_number(input$rows, "Rows", 1, 30),
        n_cols = ui_number(input$cols, "Columns", 1, 30), n_species = n, species_ids = ids,
        deaths_per_round = ui_number(input$deaths, "Deaths", 0, 900),
        seed = ui_number(input$seed, "Seed", 0, .Machine$integer.max),
        allow_extinction = isTRUE(input$allow_extinction))
      animate_cells(TRUE)
      run(start_interactive_run(config, model))
    }, error = report_error)
  }
  observeEvent(input$restart_neutral, initialize("neutral"))
  observeEvent(input$restart_niche, initialize("niche"))

  start_playback <- function(count) {
    if (is.null(run())) {
      showNotification("Initialize a neutral or niche-based community first.", type = "message")
      return()
    }
    if (running()) return()
    tryCatch({
      count <- ui_number(count, "Iterations", 1, 10000)
      seconds <- ui_number(input$seconds, "Seconds per generation", 0.1, 30, integer = FALSE)
      animate_cells(TRUE)
      remaining(count); interval(seconds * 1000); phase("ready"); running(TRUE)
    }, error = report_error)
  }
  observeEvent(input$step, start_playback(1L))
  observeEvent(input$go, start_playback(input$iterations))
  observeEvent(input$go_skip, {
    if (is.null(run())) {
      showNotification("Initialize a neutral or niche-based community first.", type = "message")
      return()
    }
    if (running()) return()
    tryCatch({
      count <- ui_number(input$iterations, "Iterations", 1, 10000)
      result <- run()
      # Work locally and publish once; retain every intermediate snapshot and metric.
      for (i in seq_len(count)) result <- advance_interactive_run(result)
      animate_cells(FALSE)
      run(result)
    }, error = report_error)
  })
  # An interrupted death preview is discarded; completed generations are retained.
  observeEvent(input$stop, cancel_playback())

  # Each generation has a death frame and a birth frame, each lasting seconds / 2.
  # The pending result uses the core engine once; visualization consumes no RNG.
  observe({
    if (!running()) return()
    invalidateLater(isolate(interval()) / 2, session)
    isolate({
      tryCatch({
        if (phase() == "death") {
          run(pending()); pending(NULL)
          remaining(remaining() - 1L); phase("birth")
        } else if (remaining() > 0L) {
          pending(advance_interactive_run(run())); phase("death")
        } else cancel_playback()
      }, error = function(error) { cancel_playback(); report_error(error) })
    })
  })
  observe({
    session$sendCustomMessage("playback", list(running = running(), initialized = !is.null(run())))
  })

  output$status <- renderUI({
    result <- run()
    if (is.null(result)) return(div(class = "status-row", h2("Your community starts here"), span(class = "badge", "Not initialized")))
    div(class = "status-row", div(h2(if (result$model == "neutral") "Neutral assembly" else "Niche-based assembly"),
         p(class = "hint", if (running()) paste(if (phase() == "death") "Death phase · vacancies" else "Birth phase · established", "·", remaining(), "generations remaining") else "Ready to explore")),
        span(class = "badge", paste("Generation", length(result$snapshots) - 1L)))
  })
  output$board <- renderUI({
    result <- run()
    if (is.null(result)) return(div(class = "empty-state", span("↗"), h3("Choose a process to begin"),
                                  p("Initialize a community using the controls on the left.")))
    deaths <- if (phase() == "death") NULL else if (length(result$events)) tail(result$events, 1)[[1]]$deaths else NULL
    div(class = if (!animate_cells()) "no-animation" else NULL,
        community_grid(displayed_grid(), species_colors(result$species_ids), deaths))
  })
  output$legend <- renderUI({
    req(run())
    result <- run(); colors <- species_colors(result$species_ids)
    current <- tail(result$species, length(result$species_ids))
    div(class = "legend", lapply(seq_len(nrow(current)), function(i) {
      div(class = "legend-item", span(class = "swatch", style = paste0("background:", colors[[current$species_id[i]]])),
          span(current$species_id[i]), strong(current$abundance[i]))
    }))
  })
  output$metrics <- renderUI({
    req(run()); last <- tail(run()$community, 1)
    div(class = "metric-row",
        div(class = "metric", span("Species present"), strong(last$richness)),
        div(class = "metric", span("Lowest abundance"), strong(last$min_abundance)),
        div(class = "metric", span("Highest abundance"), strong(last$max_abundance)),
        div(class = "metric", span("Mean abundance"), strong(round(last$mean_abundance, 2))))
  })
  output$affinity <- renderUI({
    req(run()); result <- run()
    if (is.null(result$affinities)) return(p(class = "hint", "Neutral assembly has no habitat preference."))
    div(class = "affinity-card", h3("Habitat preferences"),
        p(class = "hint", "Fixed adjacent pairs. A preferred species always wins when present among recruitment candidates."),
        community_grid(result$affinities, species_colors(result$species_ids), label = "Niche preferences"))
  })
  output$frequency_plot <- renderPlot({
    req(run())
    plot_species_frequencies(run()$species, run()$species_ids)
  }, res = 110, alt = "Species relative frequency by generation. Each colored line connects completed-generation observations; the vertical axis ranges from zero to 100 percent.")
  output$frequency_legend <- renderUI({
    req(run())
    ids <- run()$species_ids; colors <- species_colors(ids)
    div(class = "legend", lapply(ids, function(id) {
      div(class = "legend-item", span(class = "swatch", style = paste0("background:", colors[[id]])), span(id))
    }))
  })
  output$history <- renderTable({ req(run()); run()$community[, c("generation", "richness", "min_abundance", "max_abundance", "mean_frequency")] }, digits = 3)
  output$download <- downloadHandler(
    filename = function() paste0("community-", run()$model, "-generation-", length(run()$snapshots) - 1L, ".rds"),
    content = function(file) { req(run()); saveRDS(run(), file) }
  )
}
