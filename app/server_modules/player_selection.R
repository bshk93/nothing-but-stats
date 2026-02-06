# Player Selection Module ----
# Bank of players, add-by-click, side-by-side stats comparison

selected_players <- reactiveValues(players = character(0))

# Season data for player selection tab
player_selection_season_df <- reactive({
  req(input$player_selection_season)
  dfs %>% filter(SEASON == input$player_selection_season)
})

# Player bank: all players for the season with per-game stats and W-L
output$player_bank <- renderDT({
  req(player_selection_season_df())
  season_df <- player_selection_season_df()

  # Per-player stats (summarize_per_game works on grouped df)
  stats <- season_df %>%
    group_by(PLAYER) %>%
    summarize_per_game(formatting = FALSE)

  # TEAM: most recent team for that season (in case of trades)
  team_lookup <- season_df %>%
    group_by(PLAYER) %>%
    arrange(DATE) %>%
    slice(n()) %>%
    ungroup() %>%
    select(PLAYER, TEAM)

  bank <- stats %>%
    left_join(team_lookup, by = "PLAYER") %>%
    mutate(TEAM = str_c(TEAM, " ", get_logo(TEAM, height = 20))) %>%
    select(PLAYER, TEAM, WL, G, MPG, PPG, RPG, APG, SPG, BPG, TOPG, GMSC, FG, `3P`, FT, TS)

  format_as_datatable(
    bank,
    escape = FALSE,
    page_length = 25,
    scroll_x = TRUE,
    selection = list(mode = "single", target = "row")
  )
})

# Add player when a row is clicked in the bank (use cell_clicked so selection is correct when table is sorted)
observeEvent(input$player_bank_cell_clicked, {
  info <- input$player_bank_cell_clicked
  req(info, is.list(info), length(info$row) > 0L)
  # PLAYER is first column (col 0 in DT); value is the player name
  req(info$col == 0L)
  player <- as.character(info$value)
  req(nzchar(player))
  if (!player %in% selected_players$players) {
    selected_players$players <- c(selected_players$players, player)
  }
})

# Remove button observers (one per possible index, up to 50)
lapply(1:50, function(i) {
  observeEvent(input[[paste0("remove_player_", i)]], {
    if (i >= 1L && i <= length(selected_players$players)) {
      selected_players$players <- selected_players$players[-i]
    }
  })
})

# Selected players list with remove buttons
output$selected_players_list <- renderUI({
  players <- selected_players$players
  if (length(players) == 0L) {
    return(p("No players selected. Click a row in the player bank above to add players."))
  }

  season_df <- player_selection_season_df()
  team_lookup <- season_df %>%
    group_by(PLAYER) %>%
    arrange(DATE) %>%
    slice(n()) %>%
    ungroup() %>%
    select(PLAYER, TEAM)

  tags <- lapply(seq_along(players), function(i) {
    p <- players[i]
    team_row <- team_lookup %>% filter(PLAYER == p)
    team <- if (nrow(team_row) > 0L) team_row$TEAM[1] else ""
    team_logo <- if (nzchar(team)) get_logo(team, height = 24) else ""
    span(
      style = "display: inline-block; margin-right: 10px; margin-bottom: 8px;",
      span(HTML(str_c(p, " ", team_logo)), style = "margin-right: 6px;"),
      actionButton(
        paste0("remove_player_", i),
        label = NULL,
        icon = icon("times"),
        style = "padding: 2px 6px;"
      )
    )
  })

  tagList(tags)
})

# Stats comparison: side-by-side (one column per player, one row per stat)
output$selected_stats_ui <- renderUI({
  players <- selected_players$players
  if (length(players) == 0L) {
    return(p("Select players from the bank above to see a side-by-side stats comparison."))
  }
  DTOutput("selected_stats")
})

output$selected_stats <- renderDT({
  players <- selected_players$players
  req(length(players) > 0L, player_selection_season_df())

  season_df <- player_selection_season_df() %>%
    filter(PLAYER %in% players)

  stats <- season_df %>%
    group_by(PLAYER) %>%
    summarize_per_game(formatting = FALSE)

  # Pivot to long then wide for side-by-side (include WL)
  stat_cols <- c("WL", "G", "MPG", "PPG", "RPG", "APG", "SPG", "BPG", "TOPG", "GMSC", "FG", "3P", "FT", "TS")
  stat_cols <- intersect(stat_cols, names(stats))

  # Coerce all stat columns to character so pivot_longer can combine WL (char) with numeric cols
  y <- stats %>%
    select(PLAYER, any_of(stat_cols)) %>%
    mutate(across(any_of(stat_cols), as.character)) %>%
    pivot_longer(names_to = "CATEGORY", cols = -PLAYER, values_drop_na = TRUE) %>%
    pivot_wider(names_from = "PLAYER", values_from = "value")

  # Ensure column order: CATEGORY first, then players in selection order
  player_order <- intersect(players, names(y))
  y <- y %>% select(CATEGORY, any_of(player_order))

  format_as_datatable(
    y,
    escape = FALSE,
    page_length = 25,
    scroll_x = TRUE
  )
})
