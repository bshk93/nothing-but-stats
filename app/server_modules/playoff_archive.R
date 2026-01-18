# Playoff Archive Module ----
# All outputs for the Playoff Archive tab

output$playoff_bracket <- renderDT({
  x <- dfs_playoffs %>%
    filter(SEASON == str_c(input$seasonplayoffs, " Playoffs")) %>%
    group_by(SEASON, ROUND, TEAM, GAME, OPP, WL) %>%
    summarize(P = sum(P)) %>%
    mutate(OPP_RAW = str_replace(OPP, "@", ""))
  
  y <- x %>%
    left_join(x, by = c("SEASON", "ROUND", "GAME", "OPP_RAW" = "TEAM")) %>%
    mutate(SCORE = str_c(TEAM, " ", P.x, "-", P.y, " ", OPP.x)) %>%
    group_by(ROUND, TEAM, OPP_RAW) %>%
    mutate(tmp_x = first(OPP.x)) %>%
    mutate(HOME_TEAM = if_else(str_detect(tmp_x, "@"),
                               OPP_RAW,
                               TEAM)) %>%
    filter(TEAM == HOME_TEAM) %>%
    mutate(WINNER = case_when(WL.x == "W" ~ TEAM, TRUE ~ OPP_RAW)) %>%
    group_by(TEAM, ROUND) %>%
    mutate(SERIES1 = cumsum(WL.x == "W"),
           SERIES2 = cumsum(WL.y == "W")) %>%
    ungroup()
  
  seeds <- get_playoff_seeds() %>%
    filter(SEASON == str_c(input$seasonplayoffs, " Playoffs"))
  
  get_my_seed <- function(seeds, conf, seed) {
    seeds %>%
      filter(CONF == conf, SEED == seed) %>%
      mutate(TEAM = str_c(TEAM, ' (', seed, ')')) %>%
      pull(TEAM)
  }
  
  get_my_playoff_result <- function(y, t1, t2) {
    if (is.na(t1) || is.na(t2)) {
      return(NA)
    }
    
    t1 <- str_extract(t1, '^[A-Z]{3}')
    t2 <- str_extract(t2, '^[A-Z]{3}')
    
    z <- y %>%
      filter((TEAM == t1 & OPP_RAW == t2) |
               (TEAM == t2 & OPP_RAW == t1)) %>%
      mutate(SERIES_WINNER = case_when(
        SERIES1 > SERIES2 ~ TEAM,
        SERIES2 > SERIES1 ~ OPP_RAW,
        TRUE ~ "TIED"
      )) %>% 
      mutate(RESULT = str_c(SERIES_WINNER, ' (', SERIES1, '-', SERIES2, ')')) %>%
      tail(1) %>%
      pull(RESULT)
    
    if (length(z) == 0) {
      return(NA)
    }
    
    z
  }
  
  tribble(
    ~WEST_R1, ~WEST_R2, ~WCF, ~WCF_CHAMP, ~FINALS, ~ECF_CHAMP, ~ECF, ~EAST_R2, ~EAST_R1,
    
    get_my_seed(seeds, 'WEST', 1), NA, NA, NA, NA, NA, NA, NA, get_my_seed(seeds, 'EAST', 1),
    NA, get_my_playoff_result(y, get_my_seed(seeds, 'WEST', 1), get_my_seed(seeds, 'WEST', 8)), NA, NA, NA, NA, NA, get_my_playoff_result(y, get_my_seed(seeds, 'EAST', 1), get_my_seed(seeds, 'EAST', 8)), NA,
    get_my_seed(seeds, 'WEST', 8), NA, NA, NA, NA, NA, NA, NA, get_my_seed(seeds, 'EAST', 8),
    
    NA, NA,
    get_my_playoff_result(y, get_my_playoff_result(y, get_my_seed(seeds, 'WEST', 1), get_my_seed(seeds, 'WEST', 8)), get_my_playoff_result(y, get_my_seed(seeds, 'WEST', 4), get_my_seed(seeds, 'WEST', 5))),
    NA, NA, NA,
    get_my_playoff_result(y, get_my_playoff_result(y, get_my_seed(seeds, 'EAST', 1), get_my_seed(seeds, 'EAST', 8)), get_my_playoff_result(y, get_my_seed(seeds, 'EAST', 4), get_my_seed(seeds, 'EAST', 5))),
    NA, NA,
    
    get_my_seed(seeds, 'WEST', 4), NA, NA, NA, NA, NA, NA, NA, get_my_seed(seeds, 'EAST', 4),
    NA, get_my_playoff_result(y, get_my_seed(seeds, 'WEST', 4), get_my_seed(seeds, 'WEST', 5)), NA, NA, NA, NA, NA, get_my_playoff_result(y, get_my_seed(seeds, 'EAST', 4), get_my_seed(seeds, 'EAST', 5)), NA,
    get_my_seed(seeds, 'WEST', 5), NA, NA, NA, NA, NA, NA, NA, get_my_seed(seeds, 'EAST', 5),
    
    
    NA, NA, NA,
    get_my_playoff_result(
      y,
      get_my_playoff_result(y, get_my_playoff_result(y, get_my_seed(seeds, 'WEST', 1), get_my_seed(seeds, 'WEST', 8)), get_my_playoff_result(y, get_my_seed(seeds, 'WEST', 4), get_my_seed(seeds, 'WEST', 5))),
      get_my_playoff_result(y, get_my_playoff_result(y, get_my_seed(seeds, 'WEST', 2), get_my_seed(seeds, 'WEST', 7)), get_my_playoff_result(y, get_my_seed(seeds, 'WEST', 3), get_my_seed(seeds, 'WEST', 6)))
    ),
    
    get_my_playoff_result(
      y,
      get_my_playoff_result(
        y,
        get_my_playoff_result(y, get_my_playoff_result(y, get_my_seed(seeds, 'WEST', 1), get_my_seed(seeds, 'WEST', 8)), get_my_playoff_result(y, get_my_seed(seeds, 'WEST', 4), get_my_seed(seeds, 'WEST', 5))),
        get_my_playoff_result(y, get_my_playoff_result(y, get_my_seed(seeds, 'WEST', 2), get_my_seed(seeds, 'WEST', 7)), get_my_playoff_result(y, get_my_seed(seeds, 'WEST', 3), get_my_seed(seeds, 'WEST', 6)))
      ),
      get_my_playoff_result(
        y,
        get_my_playoff_result(y, get_my_playoff_result(y, get_my_seed(seeds, 'EAST', 1), get_my_seed(seeds, 'EAST', 8)), get_my_playoff_result(y, get_my_seed(seeds, 'EAST', 4), get_my_seed(seeds, 'EAST', 5))),
        get_my_playoff_result(y, get_my_playoff_result(y, get_my_seed(seeds, 'EAST', 2), get_my_seed(seeds, 'EAST', 7)), get_my_playoff_result(y, get_my_seed(seeds, 'EAST', 3), get_my_seed(seeds, 'EAST', 6)))
      )
    ),
    
    get_my_playoff_result(
      y,
      get_my_playoff_result(y, get_my_playoff_result(y, get_my_seed(seeds, 'EAST', 1), get_my_seed(seeds, 'EAST', 8)), get_my_playoff_result(y, get_my_seed(seeds, 'EAST', 4), get_my_seed(seeds, 'EAST', 5))),
      get_my_playoff_result(y, get_my_playoff_result(y, get_my_seed(seeds, 'EAST', 2), get_my_seed(seeds, 'EAST', 7)), get_my_playoff_result(y, get_my_seed(seeds, 'EAST', 3), get_my_seed(seeds, 'EAST', 6)))
    ),
    NA, NA, NA,
    
    
    get_my_seed(seeds, 'WEST', 3), NA, NA, NA, NA, NA, NA, NA, get_my_seed(seeds, 'EAST', 3),
    NA, get_my_playoff_result(y, get_my_seed(seeds, 'WEST', 3), get_my_seed(seeds, 'WEST', 6)), NA, NA, NA, NA, NA, get_my_playoff_result(y, get_my_seed(seeds, 'EAST', 3), get_my_seed(seeds, 'EAST', 6)), NA,
    get_my_seed(seeds, 'WEST', 6), NA, NA, NA, NA, NA, NA, NA, get_my_seed(seeds, 'EAST', 6),
    
    NA, NA,
    get_my_playoff_result(y, get_my_playoff_result(y, get_my_seed(seeds, 'WEST', 2), get_my_seed(seeds, 'WEST', 7)), get_my_playoff_result(y, get_my_seed(seeds, 'WEST', 3), get_my_seed(seeds, 'WEST', 6))),
    NA, NA, NA,
    get_my_playoff_result(y, get_my_playoff_result(y, get_my_seed(seeds, 'EAST', 2), get_my_seed(seeds, 'EAST', 7)), get_my_playoff_result(y, get_my_seed(seeds, 'EAST', 3), get_my_seed(seeds, 'EAST', 6))),
    NA, NA,
    
    get_my_seed(seeds, 'WEST', 2), NA, NA, NA, NA, NA, NA, NA, get_my_seed(seeds, 'EAST', 2),
    NA, get_my_playoff_result(y, get_my_seed(seeds, 'WEST', 2), get_my_seed(seeds, 'WEST', 7)), NA, NA, NA, NA, NA, get_my_playoff_result(y, get_my_seed(seeds, 'EAST', 2), get_my_seed(seeds, 'EAST', 7)), NA,
    get_my_seed(seeds, 'WEST', 7), NA, NA, NA, NA, NA, NA, NA, get_my_seed(seeds, 'EAST', 7)
    
  ) %>%
    datatable(
      rownames = FALSE,
      selection = list(mode = 'single', target = 'cell'),
      options = list(pageLength = 100, scrollX = TRUE)
    )
})

output$playoff_series <- renderDT({
  x <- dfs_playoffs %>%
    filter(SEASON == str_c(input$seasonplayoffs, " Playoffs")) %>%
    group_by(SEASON, ROUND, TEAM, GAME, OPP, WL) %>%
    summarize(P = sum(P)) %>%
    mutate(OPP_RAW = str_replace(OPP, "@", ""))
  
  x %>%
    left_join(x, by = c("SEASON", "ROUND", "GAME", "OPP_RAW" = "TEAM")) %>%
    mutate(SCORE = str_c(TEAM, " ", P.x, "-", P.y, " ", OPP.x)) %>%
    group_by(ROUND, TEAM, OPP_RAW) %>%
    mutate(tmp_x = first(OPP.x)) %>%
    mutate(HOME_TEAM = if_else(str_detect(tmp_x, "@"),
                               OPP_RAW,
                               TEAM)) %>%
    filter(TEAM == HOME_TEAM) %>%
    mutate(WINNER = case_when(WL.x == "W" ~ TEAM, TRUE ~ OPP_RAW)) %>%
    group_by(TEAM, ROUND) %>%
    mutate(SERIES1 = cumsum(WL.x == "W"),
           SERIES2 = cumsum(WL.y == "W")) %>%
    ungroup() %>%
    mutate(SERIES = str_c(TEAM, " ", SERIES1, "-", SERIES2, " ", OPP_RAW)) %>%
    select(ROUND, GAME, SCORE, WINNER, SERIES) %>%
    datatable(rownames = FALSE, options = list(pageLength = 100, scrollX = TRUE))
})
