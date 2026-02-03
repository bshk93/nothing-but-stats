# Trade Machine Module ----
# Dynamic team panels, asset management, and trade validation

TM_MAX_ASSETS <- 15L

# Reactive state for asset counts and validation results
tm_asset_counts <- reactiveValues()
tm_validation_result <- reactiveValues(results = NULL)

# Helper to build a single asset row
tm_build_asset_row <- function(team_idx, asset_idx, dest_choices) {
  div(
    id = paste0("tm_asset_row_", team_idx, "_", asset_idx),
    fluidRow(
      column(4, textInput(
        paste0("tm_team_", team_idx, "_asset_", asset_idx, "_desc"),
        NULL,
        placeholder = "Player or pick"
      )),
      column(3, numericInput(
        paste0("tm_team_", team_idx, "_asset_", asset_idx, "_sal"),
        NULL,
        value = NA,
        min = 0,
        step = 100000
      )),
      column(4, selectInput(
        paste0("tm_team_", team_idx, "_asset_", asset_idx, "_dest"),
        NULL,
        choices = dest_choices,
        selected = if (length(dest_choices) == 1L) dest_choices else NULL
      ))
    )
  )
}

# Helper to get destination choices for a team
tm_get_dest_choices <- function(team_idx, n_teams) {
  team_labels <- paste("Team", seq_len(n_teams))
  other_choices <- setNames(as.character(seq_len(n_teams)), team_labels)
  other_choices[-team_idx]
}

# When number of teams changes, rebuild the panels (this resets everything)
observeEvent(input$tm_num_teams, {
  n <- as.integer(req(input$tm_num_teams))
  # Reset asset counts for all teams

  for (i in 1:4) {
    tm_asset_counts[[paste0("team_", i)]] <- 1L
  }
}, ignoreNULL = TRUE)

# Add Asset button handlers (teams 1-4)
lapply(1:4, function(i) {
  observeEvent(input[[paste0("tm_add_asset_", i)]], {
    n <- as.integer(req(input$tm_num_teams))
    if (i > n) return()

    key <- paste0("team_", i)
    current <- tm_asset_counts[[key]] %||% 1L
    if (current >= TM_MAX_ASSETS) return()

    new_idx <- current + 1L
    tm_asset_counts[[key]] <- new_idx

    dest_choices <- tm_get_dest_choices(i, n)
    insertUI(

      selector = paste0("#tm_assets_container_", i),
      where = "beforeEnd",
      ui = tm_build_asset_row(i, new_idx, dest_choices)
    )
  })
})

# Remove Asset button handlers (teams 1-4) - removes last asset
lapply(1:4, function(i) {
  observeEvent(input[[paste0("tm_remove_asset_", i)]], {
    key <- paste0("team_", i)
    current <- tm_asset_counts[[key]] %||% 1L
    if (current <= 0L) return()

    removeUI(selector = paste0("#tm_asset_row_", i, "_", current))
    tm_asset_counts[[key]] <- current - 1L
  })
})

# Dynamic team panels - only rebuilds when number of teams changes
output$tm_team_panels <- renderUI({
  n <- as.integer(req(input$tm_num_teams))
  req(n >= 2L, n <= 4L)

  panels <- lapply(seq_len(n), function(i) {
    dest_choices <- tm_get_dest_choices(i, n)
    # Start with one asset row
    initial_asset <- tm_build_asset_row(i, 1L, dest_choices)

    column(
      width = 12 / n,
      box(
        title = paste("Team", i),
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        numericInput(
          paste0("tm_team_", i, "_salary"),
          "Guaranteed Salary ($)",
          value = 0,
          min = 0,
          step = 100000
        ),
        selectInput(
          paste0("tm_team_", i, "_hardcap"),
          "Hard Capped",
          choices = c("None", "First Apron", "Second Apron"),
          selected = "None"
        ),
        h4("Assets"),
        fluidRow(
          column(4, strong("Description")),
          column(3, strong("Salary ($)")),
          column(4, strong("Destination"))
        ),
        div(id = paste0("tm_assets_container_", i), initial_asset),
        fluidRow(
          column(6, actionButton(
            paste0("tm_add_asset_", i),
            "Add Asset",
            icon = icon("plus-circle")
          )),
          column(6, actionButton(
            paste0("tm_remove_asset_", i),
            "Remove Asset",
            icon = icon("minus-circle")
          ))
        )
      )
    )
  })

  fluidRow(panels)
})

# Validate trade on button click
observeEvent(input$tm_validate, {
  n <- as.integer(req(input$tm_num_teams))
  req(n >= 2L, n <= 4L)

  config <- list(
    salary_cap = as.numeric(req(input$tm_salary_cap)),
    apron1     = as.numeric(req(input$tm_apron1)),
    apron2     = as.numeric(req(input$tm_apron2))
  )

  team_labels <- paste("Team", seq_len(n))
  results <- character(n)
  names(results) <- team_labels

  # Build per-team outgoing/incoming from assets
  # Asset on team i with dest j: team i sends out, team j receives
  outgoing <- vector("list", n)
  incoming <- vector("list", n)
  for (i in seq_len(n)) {
    outgoing[[i]] <- numeric(0)
    incoming[[i]] <- numeric(0)
  }

  for (i in seq_len(n)) {
    n_assets <- tm_asset_counts[[paste0("team_", i)]] %||% 1L
    for (j in seq_len(n_assets)) {
      dest_id <- input[[paste0("tm_team_", i, "_asset_", j, "_dest")]]
      if (is.null(dest_id) || dest_id == "") next
      dest <- as.integer(dest_id)
      sal <- input[[paste0("tm_team_", i, "_asset_", j, "_sal")]]
      sal_val <- if (is.na(sal) || is.null(sal) || identical(sal, "")) 0 else as.numeric(sal)

      if (dest != i && dest >= 1L && dest <= n) {
        outgoing[[i]] <- c(outgoing[[i]], sal_val)
        incoming[[dest]] <- c(incoming[[dest]], sal_val)
      }
    }
  }

  for (i in seq_len(n)) {
    current_guaranteed <- as.numeric(input[[paste0("tm_team_", i, "_salary")]])
    if (is.na(current_guaranteed)) current_guaranteed <- 0
    hard_cap <- input[[paste0("tm_team_", i, "_hardcap")]]
    if (is.null(hard_cap)) hard_cap <- "None"

    results[i] <- is_trade_legal(
      outgoing_sal       = outgoing[[i]],
      incoming_sal       = incoming[[i]],
      current_guaranteed = current_guaranteed,
      hard_cap           = hard_cap,
      config             = config
    )
  }

  tm_validation_result$results <- results
})

# Render validation results
output$tm_results <- renderUI({
  res <- tm_validation_result$results
  if (is.null(res)) {
    return(p("Click 'Validate Trade' to check if the trade is legal for each team."))
  }

  fails <- res[startsWith(res, "FAIL")]
  if (length(fails) == 0L) {
    return(div(
      class = "alert alert-success",
      h4("Trade Valid"),
      p("The trade passes salary rules for all teams.")
    ))
  }

  fail_items <- lapply(names(fails), function(team) {
    tags$li(
      strong(team), ": ", fails[[team]]
    )
  })

  div(
    class = "alert alert-danger",
    h4("Trade Invalid"),
    p("The following team(s) fail validation:"),
    tags$ul(fail_items)
  )
})
