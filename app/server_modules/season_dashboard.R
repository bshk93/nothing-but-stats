# Season Dashboard Module ----
# All outputs for the Season Dashboard tab

#### Newsfeed ----
output$newsfeed <- renderDT({
  begin <- Sys.time()
  x <- news %>%
    filter(SEASON == input$season2) %>%
    select(-SEASON, -PLAYER) %>%
    format_as_datatable(
      escape = FALSE,
      page_length = 10
    )
  
  print(glue("[{sprintf('%.7f', round(Sys.time() - begin, 7))}] newsfeed generated."))
  
  x
})

#### Standings ----
output$standings <- renderDT({
  begin <- Sys.time()
  
  # Use pre-computed standings for the selected season
  y <- standings_precomputed[[input$season2]] %>%
    mutate(TEAM = str_c(TEAM, ' ', get_logo(TEAM, height = 20)))
  
  print(glue("[{sprintf('%.7f', round(Sys.time() - begin, 7))}] standings generated."))
  
  y
}, options = list(pageLength = 30, scrollX = TRUE), rownames = FALSE, escape = FALSE,
selection = list(mode = 'single',
                 target = 'cell',
                 selectable = matrix(c(1:30, rep(1, 30)), 30, 2)))

#### League Leaders ----
output$leaders <- renderDT({
  
  begin <- Sys.time()
  
  summary_df <- mySeasonDF() %>%
    group_by(PLAYER) %>%
    summarize(
      G = n(),
      across(
        c(GMSC, P, R, A, S, B, `3PM`),
        mean,
        .names = "{.col}PG"
      ),
      .groups = "drop"
    )
  
  categories <- c("GMSC", "P", "R", "A", "S", "B", "3PM")
  
  leaders_list <- map(categories, ~ leader_helper(.x, summary_df, dfs))
  
  # Combine into a single data frame with a new column `category`
  names(leaders_list) <- categories
  
  # Add row numbers for merging
  leaders_with_rn <- map(leaders_list, ~ .x %>% mutate(rn = row_number()))
  
  # Iteratively join all categories by `rn`
  y <- reduce(leaders_with_rn, full_join, by = "rn") %>%
    select(
      PLAYER_GMSC, GMSCPG,
      PLAYER_P, PPG,
      PLAYER_R, RPG,
      PLAYER_A, APG,
      PLAYER_S, SPG,
      PLAYER_B, BPG,
      PLAYER_3PM, `3PMPG`
    )
  
  print(glue("[{sprintf('%.7f', round(Sys.time() - begin, 7))}] leaders table generated."))
  
  y
  
},
escape = FALSE,
selection = list(mode = 'single', target = 'cell'),
options = list(scrollX = TRUE)
)

#### Team Stats ----
team_stats <- reactive({
  # Use pre-computed team stats for the selected season
  team_stats_precomputed[[input$season2]] %>%
    mutate(TEAM = str_c(TEAM, ' ', get_logo(TEAM, height = 20)))
})

output$team_stats <- renderDT({
  
  begin <- Sys.time()
  
  x <- team_stats()
  
  print(glue("[{sprintf('%.7f', round(Sys.time() - begin, 7))}] team stats generated."))
  
  x
},
options = list(pageLength = 30, scrollX = TRUE),
escape = FALSE,
selection = list(mode = 'single', target = 'cell',
                 selectable = matrix(c(1:30, rep(1, 30)), 30, 2))
)

#### Rookie Report ----
output$rookie_report <- renderDT({
  
  begin <- Sys.time()
  
  x <- mySeasonDF() %>%
    filter(ROOKIE) %>%
    summarize_player() %>%
    arrange(desc(GMSC))
  
  x <- x %>% 
    left_join(get_last_played_for_2(mySeasonDF()), by = 'PLAYER')
  
  y <- x %>%
    mutate(PLAYER = str_c(
      PLAYER, ' ', get_logo(TEAM, height = 20)
    )) %>%
    select(-TEAM, -c('FGAPG', 'FGMPG', '3PAPG', '3PMPG', 'FTAPG', 'FTMPG')) %>%
    format_as_datatable(
      escape = FALSE,
      page_length = 10,
      selection = list(mode = 'single', target = 'cell')
    )
  
  print(glue("[{sprintf('%.7f', round(Sys.time() - begin, 7))}] rookie report generated."))
  
  y
  
})

#### Most Improved ----
output$most_improved <- renderDT({
  
  begin <- Sys.time()
  
  x <- mySeasonDF() %>% 
    summarize_player()
  
  x <- x %>% 
    left_join(get_last_played_for_2(mySeasonDF()), by = 'PLAYER')
  
  y <- dfs %>% 
    filter(SEASON < input$season2) %>% 
    summarize_player() %>% 
    select(PLAYER, G_CAREER = G, GMSC_CAREER = GMSC)
  
  z <- x %>% 
    select(PLAYER, TEAM, G, GMSC) %>% 
    inner_join(y, by = "PLAYER") %>% 
    mutate(PLAYER = str_c(
      PLAYER, ' ', get_logo(TEAM, height = 20)
    )) %>% 
    mutate(GMSC_DIFF = round(GMSC - GMSC_CAREER, 2)) %>% 
    select(-TEAM) %>% 
    arrange(desc(GMSC_DIFF)) %>% 
    format_as_datatable(
      escape = FALSE, 
      page_length = 10,
      selection = list(mode = 'single', target = 'cell')
    )
  
  print(glue("[{sprintf('%.7f', round(Sys.time() - begin, 7))}] most improved summary generated."))
  
  z
  
})

#### Draft Lottery Preview ----
output$tankathon <- renderDT({
  
  begin <- Sys.time()
  
  x <- mySeasonDF() %>%
    distinct(TEAM, DATE, WL) %>%
    group_by(TEAM) %>%
    summarize(W = sum(case_when(WL == "W" ~ 1, T ~ 0)),
              L = n() - W) %>%
    mutate(CONF = get_conference(TEAM)) %>%
    group_by(CONF) %>%
    mutate(GB = (max(W - L) - (W - L))/2) %>%
    arrange(CONF, GB) %>%
    mutate(SEED = row_number()) %>%
    ungroup() %>%
    filter(SEED > 8) %>%
    mutate(PCT = W/(W+L),
           ODDS = rank(PCT, ties.method = "min")) %>%
    arrange(ODDS) %>%
    select(ODDS, TEAM, W, L) %>%
    mutate(temprank = row_number())
  
  odds <- tribble(
    ~temprank, ~P1, ~P2, ~P3, ~P4, ~P5, ~P6, ~P7, ~P8, ~P9, ~P10, ~P11, ~P12, ~P13, ~P14,
    1, 14, 13.4, 12.7, 12, 47.9, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
    2, 14, 13.4, 12.7, 12, 27.8, 20, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
    3, 14, 13.4, 12.7, 12, 14.8, 26, 7, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
    4, 12.5, 12.2, 11.9, 11.5, 7.2, 25.7, 16.7, 2.2, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
    5, 10.5, 10.5, 10.6, 10.5, 2.2, 19.6, 26.7, 8.7, 0.6, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
    6, 9.0, 9.2, 9.4, 9.6, NA_real_, 8.6, 29.8, 20.5, 3.7, 0.1, NA_real_, NA_real_, NA_real_, NA_real_,
    7, 7.5, 7.8, 8.1, 8.5, NA_real_, NA_real_, 19.7, 34.1, 12.9, 1.3, 0, NA_real_, NA_real_, NA_real_,
    8, 6, 6.3, 6.7, 7.2, NA_real_, NA_real_, NA_real_, 34.5, 32.1, 6.7, 0.4, 0, NA_real_, NA_real_,
    9, 4.5, 4.8, 5.2, 5.7, NA_real_, NA_real_, NA_real_, NA_real_, 50.7, 25.9, 3, 0.1, 0, NA_real_,
    10, 3, 3.3, 3.6, 4, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, 65.9, 19, 1.2, 0, 0,
    11, 2, 2.2, 2.4, 2.8, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, 77.6, 12.6, 0.4, 0,
    12, 1.5, 1.7, 1.9, 2.1, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, 86.1, 6.7, 0.1,
    13, 1, 1.1, 1.2, 1.4, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, 92.9, 2.3,
    14, 0.5, 0.6, 0.6, 0.7, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, 97.6
  )
  
  y <- x %>%
    left_join(odds, by = "temprank") %>%
    group_by(ODDS) %>%
    mutate_at(vars(starts_with("P")), function(x) (round(sum(x, na.rm = T)/n(), 1))) %>%
    select(-temprank) %>%
    mutate(TEAM = str_c(TEAM, ' ', get_logo(TEAM, height = 20)))
  
  print(glue("[{sprintf('%.7f', round(Sys.time() - begin, 7))}] tankathon stats generated."))
  
  y
}, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE, escape = FALSE)

#### Game Log ----
output$gamelog <- renderDT({
  begin <- Sys.time()
  
  x <- gamelist()
  
  print(glue("[{sprintf('%.7f', round(Sys.time() - begin, 7))}] gamelist generated."))
  
  x
  
}, selection = list(mode = 'single', target = 'row'),
options = list(pageLength = 30, scrollX = TRUE))

#### Season All-Stars ----
output$season_allstars <- renderDT({
  
  begin <- Sys.time()
  
  x <- get_allstars() %>%
    filter(SEASON <= input$season2) %>%
    group_by(PLAYER) %>%
    mutate(SELECTION = n()) %>%
    filter(SEASON == input$season2) %>%
    select(PLAYER, SELECTION)
  
  x <- x %>% 
    left_join(get_last_played_for_2(dfs), by = 'PLAYER')
  
  print(glue("[{sprintf('%.7f', round(Sys.time() - begin, 7))}] all-stars generated."))
  
  x
}, options = list(pageLength = 50, scrollX = TRUE), rownames = FALSE)

#### Season All-NBN ----
output$season_allnbn <- renderDT({
  bind_rows(
    get_allnbn1(),
    get_allnbn2(),
    get_allnbn3()
  ) %>% 
    filter(SEASON == input$season2) %>% 
    mutate(ALL_NBN = str_c(coalesce(medal1, ""), coalesce(medal2, ""), coalesce(medal3, ""))) %>% 
    select(-starts_with("medal")) %>% 
    format_as_datatable(escape = FALSE)
})
