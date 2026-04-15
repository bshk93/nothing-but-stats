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

# Remove by player name (from JS button click)
observeEvent(input$remove_player_name, {
  name <- input$remove_player_name
  selected_players$players <- selected_players$players[selected_players$players != name]
})

# Sync drag order
observeEvent(input$player_rank_order, {
  new_order <- input$player_rank_order
  valid <- intersect(new_order, selected_players$players)
  if (length(valid) == length(selected_players$players)) {
    selected_players$players <- valid
  }
}, ignoreNULL = TRUE, ignoreInit = TRUE)

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
  
  items_html <- paste(
    sapply(seq_along(players), function(i) {
      p_name <- players[i]
      team <- team_lookup$TEAM[team_lookup$PLAYER == p_name][1]
      team <- if (!is.na(team) && nzchar(team)) team else ""
      logo  <- if (nzchar(team)) get_logo(team, height = 24) else ""
      sprintf(
        '<div class="drag-item" data-player="%s" style="display:flex;align-items:center;gap:8px;padding:6px 10px;margin-bottom:4px;background:#f5f5f5;border:1px solid #ddd;border-radius:4px;cursor:grab;">
           <span style="color:#aaa;">☰</span>
           <span style="min-width:24px;font-weight:bold;color:#888;">#%d</span>
           <span>%s %s</span>
           <button onclick="removePlayer(\'%s\')" style="margin-left:auto;border:none;background:none;cursor:pointer;color:#999;">✕</button>
         </div>',
        p_name, i, p_name, logo, p_name
      )
    }),
    collapse = "\n"
  )
  
  # Inject sortable.js from CDN + glue logic
  tagList(
    tags$head(
      tags$script(src = "https://cdn.jsdelivr.net/npm/sortablejs@1.15.0/Sortable.min.js")
    ),
    HTML(paste0(
      '<div id="drag-list">', items_html, '</div>',
      '<script>
        // Destroy existing Sortable instance if present
        if (window._playerSortable) {
          window._playerSortable.destroy();
        }
        var el = document.getElementById("drag-list");
        window._playerSortable = Sortable.create(el, {
          animation: 150,
          ghostClass: "drag-ghost",
          onEnd: function() {
            var order = Array.from(el.querySelectorAll(".drag-item"))
                             .map(function(d) { return d.getAttribute("data-player"); });
            Shiny.setInputValue("player_rank_order", order, {priority: "event"});
            el.querySelectorAll(".drag-item").forEach(function(d, idx) {
              d.querySelectorAll("span")[1].textContent = "#" + (idx + 1);
            });
          }
        });
      
        function removePlayer(name) {
          Shiny.setInputValue("remove_player_name", name, {priority: "event"});
        }
      </script>'
    ))
  )
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
