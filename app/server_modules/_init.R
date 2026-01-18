# Server Initialization ----
# Shared reactives, observers, and helper functions used across modules

myPassword <- "krisdunn"

# Helper function for popups ----
popup <- function(input_info, type) {
  
  if (type == 'player-season') {
    my_player <- input_info %>%
      .[3] %>%
      str_extract('[A-Z-]+, [A-Z-]+')
    
    my_player_teams <- dfs %>%
      filter(PLAYER == my_player) %>%
      distinct(SEASON, TEAM) %>%
      group_by(TEAM) %>%
      arrange(TEAM, SEASON) %>%
      summarize(SEASON = str_c(SEASON, collapse = ', ')) %>%
      arrange(SEASON) %>%
      mutate(SEASON = str_c(' (', SEASON, ')'),
             TEAM = get_logo(TEAM, height = 30, align = 'left')) %>%
      mutate(TXT = str_c(TEAM, SEASON)) %>%
      pull(TXT) %>%
      str_c(collapse = '<br>')
    
    showModal(modalDialog(
      title = HTML(str_c('<img src="',
                         bios %>% filter(Name == my_player) %>% pull(`Img URL`),
                         '">',
                         '<br>',
                         my_player_teams)),
      renderDataTable({
        summarize_per_game(
          get_dfs_everything() %>%
            filter(PLAYER == my_player) %>%
            group_by(PLAYER, SEASON)
        ) %>%
          select(SEASON, G, MPG, PPG, RPG, APG, SPG, BPG, TOPG, FG, `3P`, FT, GMSC) %>%
          left_join(my_ranks %>%
                      filter(PLAYER == my_player) %>%
                      select(SEASON, G_RANK,
                             MPG_RANK, PPG_RANK, RPG_RANK,
                             APG_RANK, SPG_RANK, BPG_RANK,
                             TOPG_RANK,
                             FG_RANK = FGPCT_RANK,
                             `3P_RANK` = `3PPCT_RANK`,
                             FT_RANK = FTPCT_RANK,
                             GMSC_RANK),
                    by = c('SEASON')) %>%
          mutate(
            across(
              ends_with('RANK'),
              ~ str_c(' <span style="color:',
                      case_when(
                        . <= 5 ~ '#FFC514',
                        . <= 20 ~ '#006AE0',
                        TRUE ~ '#959595'
                      ),
                      '">',
                      '(',
                      .,
                      case_when(
                        . %% 10 == 1 ~ 'st',
                        . %% 10 == 2 ~ 'nd',
                        . %% 10 == 3 ~ 'rd',
                        TRUE ~ 'th'
                      ),
                      ')</span>')
            )
          ) %>%
          mutate(
            across(
              c(G, MPG, PPG, RPG, APG, SPG, BPG, TOPG, FG, `3P`, FT, GMSC),
              ~ str_c(., coalesce(get(str_c(cur_column(), '_RANK')), ''))
            )
          ) %>%
          select(SEASON, G, MPG, PPG, RPG, APG, SPG, BPG, TOPG, FG, `3P`, FT, GMSC)
      }, escape = FALSE, options = list(scrollX = TRUE)),
      easyClose = T,
      footer = NULL))
  } else if (type == 'boxscores') {
    my_game <- gamelist()[input_info,]
    
    showModal(modalDialog(
      title = str_c(my_game[1], ': ', my_game[4]),
      renderDataTable({
        get_box_score(get_dfs_everything(), my_game[1], my_game[4])
      }),
      easyClose = T,
      footer = NULL))
  } else if (type == 'team-season') {
    my_team <- str_extract(input_info[3], '^[A-Z]{3}')
    
    showModal(modalDialog(
      title = str_c(my_team,
                    ' ',
                    input$season2,
                    ' Season at a Glance'),
      renderDataTable({
        dfs %>%
          filter(SEASON == input$season2,
                 TEAM == my_team) %>%
          group_by(PLAYER) %>%
          summarize_per_game() %>%
          arrange(desc(G * as.numeric(MPG))) %>%
          select(PLAYER, G, MPG, PPG, RPG, APG, SPG, BPG,
                 GMSC, TS)
      }, options = list(pageLength = 15, scrollX = TRUE)),
      easyClose = T,
      footer = NULL
    ))
  }
}

# Observers ----
observeEvent(input$leaders_cells_selected, {
  req(input$leaders_cells_selected)
  popup(input$leaders_cell_clicked, 'player-season')
})

observeEvent(input$hof_points_cells_selected, {
  req(input$hof_points_cells_selected)
  popup(input$hof_points_cell_clicked, 'player-season')
})

observeEvent(input$most_improved_cells_selected, {
  req(input$most_improved_cells_selected)
  popup(input$most_improved_cell_clicked, 'player-season')
})

observeEvent(input$rookie_report_cells_selected, {
  req(input$rookie_report_cells_selected)
  popup(input$rookie_report_cell_clicked, 'player-season')
})

observeEvent(input$gamelog_rows_selected, {
  req(input$gamelog_rows_selected)
  popup(input$gamelog_rows_selected, 'boxscores')
})

observeEvent(input$team_stats_cells_selected, {
  req(input$team_stats_cells_selected)
  popup(input$team_stats_cell_clicked, 'team-season')
})

observeEvent(input$standings_cells_selected, {
  req(input$standings_cells_selected)
  popup(input$standings_cell_clicked, 'team-season')
})

updateSelectizeInput(
  session,
  'foo',
  choices = sort(unique(dfs$PLAYER)),
  server = TRUE
)

observe({
  query <- parseQueryString(session$clientData$url_search)
  
  if (!is.null(query$tab)) {
    updateTabItems(session, "tabs", selected = query$tab)
  }
  
  if (!is.null(query$player)) {
    updateSelectInput(session, "name", selected = query$player)
  }
})

# Shared Reactives ----
myPlayerData <- reactive({
  dfs %>%
    filter(PLAYER == input$name)
})

myPlayerPlayoffData <- reactive({
  dfs_playoffs %>%
    filter(PLAYER == input$name)
})

myCombinedData <- reactive({
  bind_rows(myPlayerData(), myPlayerPlayoffData())
})

myBiosData <- reactive({
  bios %>%
    filter(Name == input$name)
})

mySeasonDF <- reactive({
  dfs %>%
    filter(SEASON == input$season2)
})

myAwards <- reactive({
  get_allstars() %>%
    full_join(get_mvp(), by = c("PLAYER", "SEASON")) %>%
    full_join(get_dpoy(), by = c("PLAYER", "SEASON")) %>%
    full_join(get_6moy(), by = c("PLAYER", "SEASON")) %>%
    full_join(get_roy(), by = c("PLAYER", "SEASON")) %>%
    full_join(get_mip(), by = c("PLAYER", "SEASON")) %>%
    full_join(get_allnbn1(), by = c("PLAYER", "SEASON")) %>%
    full_join(get_allnbn2(), by = c("PLAYER", "SEASON")) %>%
    full_join(get_allnbn3(), by = c("PLAYER", "SEASON")) %>%
    full_join(get_alldef(), by = c("PLAYER", "SEASON")) %>%
    full_join(get_allrookie(), by = c("PLAYER", "SEASON")) %>%
    filter(PLAYER == input$name)
})

# Trivia game state and functions ----
trivia_game_state <- reactiveValues(
  current_question = NULL,
  points = 0,
  game_over = FALSE
)

trivia_questions <- reactive({
  df <- dfs %>% 
    group_by(PLAYER) %>% 
    summarize(
      FIRST_SEASON = min(SEASON),
      LAST_SEASON = max(SEASON),
      TEAMS = str_c(unique(TEAM), collapse = ', '),
      HIGH = max(P),
      G = n(),
      M = mean(M) %>% round(1),
      P = mean(P) %>% round(1),
      R = mean(R) %>% round(1),
      A = mean(A) %>% round(1),
      FG = (sum(FGM)/sum(FGA)) %>% round(3),
      `3PT` = (sum(`3PM`)/sum(`3PA`)) %>% round(3),
      FT = (sum(FTM)/sum(FTA)) %>% round(3)
    ) %>% 
    filter(G >= 50) %>% 
    mutate(difficulty = case_when(
      G < 100 ~ 1000,
      G < 200 ~ 500,
      G < 300 ~ 250,
      TRUE ~ 200
    )) %>% 
    mutate(question = str_c(
      "Teams played for: ", TEAMS, "\n",
      "First season: ", FIRST_SEASON, "\n",
      "Last season: ", LAST_SEASON, "\n",
      "Career games: ", G, "\n",
      "Career high: ", HIGH, "\n",
      "Career MPG: ", M, "\n",
      "Career P/R/A: ", str_c(P, "/", R, "/", A), "\n",
      "Career splits: ", str_c(FG, "/", `3PT`, "/", FT), "\n",
      "(All regular season stats)", "\n",
      "This question is worth ", difficulty, " points."
    )) %>% 
    select(question, difficulty, correct_answer = PLAYER)
  
  df
})

trivia_next_question <- function() {
  sample_n(trivia_questions(), 1)
}

observe({
  if (is.null(trivia_game_state$current_question)) {
    trivia_game_state$current_question <- trivia_next_question()
  }
})

# Game list reactive (used by popup and season dashboard)
gamelist <- reactive({
  mySeasonDF() %>%
    group_by(DATE, TEAM, OPP) %>%
    summarize(TEAM_PTS = sum(P), .groups = 'drop') %>%
    ungroup() %>%
    filter(!str_detect(OPP, "@")) %>%
    left_join(
      mySeasonDF() %>%
        group_by(DATE, TEAM, OPP) %>%
        summarize(TEAM_PTS = sum(P), .groups = 'drop') %>%
        ungroup() %>%
        select(DATE, OPP = TEAM, OPP_PTS = TEAM_PTS),
      by = c('DATE', 'OPP')
    ) %>%
    mutate(RESULT = glue("{TEAM} {TEAM_PTS} - {OPP_PTS} {OPP}")) %>%
    select(DATE, TEAM, OPP, RESULT) %>%
    arrange(desc(DATE))
})
