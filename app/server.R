myPassword <- "crab"

log <- glue(" * [{Sys.time()}] Started instance...")
log <- c(log, glue(" * [{Sys.time()}] Loaded packages and scripts..."))

# Find all allstats files in R/ directory
dfs <- read_rds('data/dfs.rds')
dfs_playoffs <- read_rds('data/dfs_playoffs.rds')
dfs_everything <- rbind(dfs, dfs_playoffs)
bios <- read_rds('data/bios.rds')

champions <- get_champions(dfs_playoffs)

ach_metadata <- read_csv("data/metadata-achievements.csv")
log <- c(log, glue(" * [{Sys.time()}] Loaded data from disk..."))

my_ranks <- get_ranks(dfs)
log <- c(log, glue(" * [{Sys.time()}] Completed ranks calculation..."))

# server ----

function(input, output, session) {
  
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
            dfs_everything %>% 
              filter(PLAYER == my_player) %>%
              group_by(PLAYER, SEASON)
          ) %>% 
            select(SEASON, G, MPG, PPG, RPG, APG, SPG, BPG, FG, `3P`, FT, GMSC) %>% 
            left_join(my_ranks %>% 
                        filter(PLAYER == my_player) %>% 
                        select(SEASON, G_RANK, 
                               MPG_RANK, PPG_RANK, RPG_RANK, 
                               APG_RANK, SPG_RANK, BPG_RANK, 
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
                c(G, MPG, PPG, RPG, APG, SPG, BPG, FG, `3P`, FT, GMSC), 
                ~ str_c(., coalesce(get(str_c(cur_column(), '_RANK')), ''))
              )
            ) %>% 
            select(SEASON, G, MPG, PPG, RPG, APG, SPG, BPG, FG, `3P`, FT, GMSC)
        }, escape = FALSE, options = list(scrollX = TRUE)),
        #size = 'xl',
        easyClose = T,
        footer = NULL))
    } else if (type == 'boxscores') {
      my_game <- gamelist()[input_info,]
      
      showModal(modalDialog(
        title = str_c(my_game[1], ': ', my_game[4]),
        renderDataTable({
          get_box_score(dfs_everything, my_game[1], my_game[4])
        }),
        #size = 'xl',
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
        #size = 'xl',
        easyClose = T,
        footer = NULL
      ))
    }
    
  }
  
  observeEvent(input$leaders_cells_selected, {
    req(input$leaders_cells_selected)
    popup(input$leaders_cell_clicked, 'player-season')
  })
  
  observeEvent(input$hof_points_cells_selected, {
    req(input$hof_points_cells_selected)
    popup(input$hof_points_cell_clicked, 'player-season')
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
  
  output$player_summary <- renderText({
    glue(
      "{input$name} ({myBiosData() %>% pull(`Combo Pos.`)}):\n",
      "DOB: {myBiosData() %>% pull(DOB)}\n",
      "Age: {round(time_length(interval(myBiosData() %>% pull(DOB), today()), 'years'), 2)}\n",
      "Height: {myBiosData() %>% pull(Height)}\n",
      "Weight: {myBiosData() %>% pull(Weight)}\n",
      "From: {myBiosData() %>% pull(COLLEGE)}"
    )
  })
  
  output$headshot <- renderText({
    c('<img src="', myBiosData() %>% pull(`Img URL`), '">')
  })
  
  output$team_history_logo <- renderText({
    get_logo(input$team_history, height = 100, align = 'left')
  })
  
  # Tab 1: Season Standings, etc. ----
  output$newsfeed <- renderDT({
    get_newsfeed(dfs) %>% 
      
      filter(SEASON == input$season2) %>% 
      select(-SEASON, -PLAYER) %>% 
      
      format_as_datatable(
        escape = FALSE,
        page_length = 10
      )
  })
  
  output$standings <- renderDT({
    x <- mySeasonDF() %>% 
      group_by(DATE, TEAM, OPP) %>% 
      summarize(TEAM_PTS = sum(P)) %>% 
      ungroup() %>% 
      mutate(OPP = str_replace(OPP, "@", ""))
    
    x %>% 
      left_join(
        x %>% 
          select(DATE, OPP = TEAM, OPP_PTS = TEAM_PTS)
      ) %>% 
      group_by(TEAM) %>% 
      summarize(
        W = sum(TEAM_PTS > OPP_PTS),
        L = sum(TEAM_PTS < OPP_PTS),
        PPG = mean(TEAM_PTS),
        OPPG = mean(OPP_PTS)
      ) %>% 
      mutate(PCT = round(W / (W+L), 3),
             DIFF = round(PPG - OPPG, 1),
             PPG = round(PPG, 1),
             OPPG = round(OPPG, 1),
             CONF = get_conference(TEAM)) %>% 
      group_by(CONF) %>% 
      mutate(GB = (max(W - L) - (W - L))/2) %>% 
      arrange(CONF, GB) %>% 
      mutate(SEED = str_c(CONF, "-", row_number())) %>% 
      ungroup() %>% 
      select(SEED, TEAM, GB, W, L, PCT, PPG, OPPG, DIFF) %>% 
      mutate(TEAM = str_c(TEAM, ' ', get_logo(TEAM, height = 20)))
  }, options = list(pageLength = 30, scrollX = TRUE), rownames = FALSE, escape = FALSE,
  selection = list(mode = 'single', 
                   target = 'cell',
                   selectable = matrix(c(1:30, rep(1, 30)), 30, 2)))
  
  gamelist <- reactive({
    x <- mySeasonDF() %>% 
      group_by(DATE, TEAM, OPP) %>% 
      summarize(TEAM_PTS = sum(P)) %>% 
      ungroup()
    
    x %>% 
      filter(!str_detect(OPP, "@")) %>% 
      left_join(
        x %>% 
          select(DATE, OPP = TEAM, OPP_PTS = TEAM_PTS)
      ) %>% 
      mutate(RESULT = glue("{TEAM} {TEAM_PTS} - {OPP_PTS} {OPP}")) %>% 
      select(DATE, TEAM, OPP, RESULT) %>% 
      arrange(desc(DATE))
  })
  
  output$gamelog <- renderDT({
    
    gamelist()
    
  }, selection = list(mode = 'single', target = 'row'), 
     options = list(pageLength = 30, scrollX = TRUE))
  
  output$leaders <- renderDT({
    
    summary_df <- mySeasonDF() %>% 
      group_by(PLAYER) %>% 
      summarize(G = n(),
                GMSCPG = mean(GMSC),
                PPG = mean(P),
                RPG = mean(R),
                APG = mean(A),
                SPG = mean(S),
                BPG = mean(B),
                `3PMPG` = mean(`3PM`))
    
    leader_helper <- function(category, summary_df, dfs, min_games = 20) {
      tmpvarname1 <- str_c(category, 'PG')
      
      x <- summary_df %>% 
        filter(G >= min_games) %>% 
        arrange_at(tmpvarname1) %>% 
        arrange(desc(row_number())) %>% 
        head(10)
      
      teams <- c()
      for (i in 1:nrow(x)) {
        teams <- c(teams, get_last_played_for(x$PLAYER[[i]], dfs))
      }
      
      x$TEAM <- teams
      
      tmpvarname2 <- str_c('PLAYER_', category)
      
      x %>% 
        mutate(PLAYER = str_c(PLAYER, ' ', get_logo(TEAM, height = 20))) %>% 
        select({{ tmpvarname2 }} := PLAYER, tmpvarname1) %>% 
        #mutate(RANK = rank(get(tmpvarname1), ties.method = "min")) %>% 
        mutate(rn = row_number()) %>% 
        mutate({{ tmpvarname1 }} := round(get(tmpvarname1), 1))
        
    }
    
    leaders_gmsc <- leader_helper('GMSC', summary_df, dfs)
    leaders_p <- leader_helper('P', summary_df, dfs)
    leaders_r <- leader_helper('R', summary_df, dfs)
    leaders_a <- leader_helper('A', summary_df, dfs)
    leaders_s <- leader_helper('S', summary_df, dfs)
    leaders_b <- leader_helper('B', summary_df, dfs)
    leaders_3 <- leader_helper('3PM', summary_df, dfs)
    
    leaders_gmsc %>% 
      full_join(leaders_p, by = "rn") %>% 
      full_join(leaders_r, by = "rn") %>% 
      full_join(leaders_a, by = "rn") %>% 
      full_join(leaders_s, by = "rn") %>% 
      full_join(leaders_b, by = "rn") %>% 
      full_join(leaders_3, by = "rn") %>% 
      arrange(rn) %>% 
      select(-rn)
    
  }, 
    escape = FALSE, 
    selection = list(mode = 'single', target = 'cell'),
    options = list(scrollX = TRUE)
  )
  
  output$season_eff_plot <- renderPlotly({
    
    dfs %>% 
      filter(SEASON == input$season2) %>% 
      plot_efficiency_scatter()
    
  })
  
  output$season_scatter <- renderPlotly({
    mySeasonDF() %>% 
      plot_scatter_general(
        var_x = input$scatter_x,
        var_y = input$scatter_y,
        group = input$scatter_group,
        per_36 = input$scatter_36,
        min_games = input$scatter_min_games
      )
    
  })
  
  output$season_allstars <- renderDT({
    
    x <- get_allstars() %>% 
      filter(SEASON <= input$season2) %>% 
      group_by(PLAYER) %>% 
      mutate(SELECTION = n()) %>% 
      filter(SEASON == input$season2) %>% 
      select(PLAYER, SELECTION)
    
    teams <- c()
    for (i in 1:nrow(x)) {
      teams <- c(teams, get_last_played_for(x$PLAYER[[i]], dfs))
    }
    
    x$TEAM <- teams
    
    x
  }, options = list(pageLength = 50, scrollX = TRUE), rownames = FALSE)
  
  team_stats <- reactive({
    mySeasonDF() %>% 
      group_by(TEAM, DATE) %>% 
      summarize(
        P = sum(P), R = sum(R), A = sum(A), S = sum(S), B = sum(B), 
        TO = sum(TO), PF = sum(PF), `3PM` = sum(`3PM`), `3PA` = sum(`3PA`)
      ) %>% 
      group_by(TEAM) %>% 
      summarize(
        PPG = mean(P), RPG = mean(R), APG = mean(A), SPG = mean(S), BPG = mean(B),
        TOPG = mean(TO), PFPG = mean(PF), `3PMPG` = mean(`3PM`), `3PAPG` = mean(`3PA`)
      ) %>% 
      mutate_if(is.numeric, round, 2) %>% 
      mutate(`3PPCT` = round(`3PMPG`/`3PAPG`, 3)) %>% 
      mutate(TEAM = str_c(TEAM, ' ', get_logo(TEAM, height = 20)))
  })
  
  output$team_stats <- renderDT({
    team_stats()
  }, 
    options = list(pageLength = 30, scrollX = TRUE), 
    escape = FALSE,
    selection = list(mode = 'single', target = 'cell',
                     selectable = matrix(c(1:30, rep(1, 30)), 30, 2))
  )
  
  output$tankathon <- renderDT({
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
    
    x %>% 
      left_join(odds, by = "temprank") %>% 
      group_by(ODDS) %>% 
      mutate_at(vars(starts_with("P")), function(x) (round(sum(x, na.rm = T)/n(), 1))) %>% 
      select(-temprank) %>% 
      mutate(TEAM = str_c(TEAM, ' ', get_logo(TEAM, height = 20)))
  }, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE, escape = FALSE)
  
  
  # output$points_scored_allowed <- renderPlot({
  #   x <- mySeasonDF() %>% 
  #     group_by(DATE, TEAM, OPP) %>% 
  #     summarize(TEAM_PTS = sum(P)) %>% 
  #     ungroup() %>% 
  #     mutate(OPP = str_replace(OPP, "@", ""))
  #   
  #   x %>% 
  #     left_join(
  #       x %>% 
  #         select(DATE, OPP = TEAM, OPP_PTS = TEAM_PTS)
  #     ) %>% 
  #     group_by(TEAM) %>% 
  #     summarize(
  #       POINTS = mean(TEAM_PTS),
  #       POINTS_ALLOWED = mean(OPP_PTS)
  #     ) %>% 
  #     mutate(img_ref = str_c("www/logo-", tolower(TEAM), ".png")) %>% 
  #     ggplot() +
  #     geom_image(aes(x = POINTS, y = POINTS_ALLOWED, image = img_ref),
  #                size = 0.08, by = "width", asp = 1) +
  #     scale_y_reverse() +
  #     geom_abline(intercept = 0, slope = 1) +
  #     theme(aspect.ratio = 1)
  # })
  
  
  # Tab 2: Player Overview ----
  # Game log
  output$tbl <- renderDT({
    
    req(input$password == myPassword || myPassword == '')
    
    myCombinedData() %>% 
      select(
        DATE, TEAM, OPP, M, P, R, A, S, B, TO, 
        FG, `3P`, FT, PF, GMSC, WL
      ) %>% 
      arrange(desc(DATE)) %>% 
      
      format_as_datatable(
        filter = 'top'
      )
  })
  
  output$gamelog_plot <- renderPlotly({
    
    req(input$password == myPassword || myPassword == '')
    
    p <- myGamelogPlot() %>% 
      plot_ly(
        x = ~G,
        y = ~GMSC,
        type = 'bar',
        color = ~SEASON,
        height = 300
      ) %>%
      layout(yaxis = list(title=''),
             xaxis = list(title='', visible = F),
             legend = list(orientation='h')) 
    
    p
    
  })
  
  myGamelogPlot <- reactive({
    myCombinedData() %>% 
      arrange(DATE) %>% 
      mutate(G = row_number())
  })
  
  # News
  output$player_news <- renderDT({
    get_newsfeed(dfs) %>% 
      filter(PLAYER == input$name) %>% 
      select(-PLAYER) %>% 
      arrange(desc(DATE)) %>% 
      format_as_datatable(
        escape = FALSE,
        page_length = 10
      )
  })
  
  # Per game
  output$tbl_season <- renderDT({
    
    req(input$password == myPassword || myPassword == '')
    
    awards <- get_allstars() %>% 
      full_join(get_mvp()) %>% 
      full_join(get_dpoy()) %>% 
      full_join(get_6moy()) %>% 
      full_join(get_roy()) %>% 
      full_join(get_mip()) %>% 
      full_join(get_allnbn1()) %>% 
      full_join(get_allnbn2()) %>% 
      full_join(get_allnbn3()) %>% 
      filter(PLAYER == input$name)
    
    champs <- champions %>% 
      distinct(TEAM, SEASON) %>% 
      mutate(ring = "<img src='ring.png' height='20'></img>")
      
    x <- myCombinedData() %>% 
      group_by(SEASON, TEAM) %>% 
      mutate(first_date = min(DATE)) %>% # Display teams in correct order within year
      group_by(SEASON, first_date, TEAM) %>% 
      summarize_per_game() %>% 
      select(-first_date)
    
    x %>% 
      rbind(
        myCombinedData() %>% 
          summarize_per_game() %>% 
          mutate(SEASON = "CAREER", TEAM = "")
      ) %>% 
      left_join(awards, by = c("SEASON")) %>%
      group_by(SEASON) %>% 
      mutate(star    = case_when(row_number() == n() ~ star, TRUE ~ NA_character_),
             crown   = case_when(row_number() == n() ~ crown, TRUE ~ NA_character_),
             hand    = case_when(row_number() == n() ~ hand, TRUE ~ NA_character_),
             six     = case_when(row_number() == n() ~ six, TRUE ~ NA_character_),
             baby    = case_when(row_number() == n() ~ baby, TRUE ~ NA_character_),
             chart   = case_when(row_number() == n() ~ chart, TRUE ~ NA_character_),
             medal1  = case_when(row_number() == n() ~ medal1, TRUE ~ NA_character_),
             medal2  = case_when(row_number() == n() ~ medal2, TRUE ~ NA_character_),
             medal3  = case_when(row_number() == n() ~ medal3, TRUE ~ NA_character_)
      ) %>% 
      ungroup() %>% 
      left_join(champs, by = c("TEAM", "SEASON")) %>% 
      mutate(SEASON = str_c(SEASON, " ", coalesce(ring, ""), coalesce(star, ""), 
                            coalesce(crown, ""), coalesce(hand, ""), coalesce(six, ""),
                            coalesce(baby, ""), coalesce(chart, ""), coalesce(medal1, ""), 
                            coalesce(medal2, ""), coalesce(medal3, ""))) %>% 
      select(SEASON, everything(), -PLAYER, -star, -ring, -crown, -hand, -six, -baby, -chart,
             -starts_with("medal")) %>% 
      format_as_datatable(
        escape = FALSE
      ) %>% 
      formatStyle(
        "SEASON",
        target = "row",
        fontWeight = styleEqual(c("CAREER "), "bold", default = "normal")
      )
  })
  
  # Records
  output$records <- renderDT({
    
    req(input$password == myPassword || myPassword == '')
    
    x <- myCombinedData() %>% 
      group_by(SEASON) %>% 
      summarize(
        P = max(P), R = max(R), A = max(A), S = max(S),
        B = max(B), `3PM` = max(`3PM`), GMSC = max(GMSC)
      ) %>% 
      ungroup() 
    
    x %>% 
      rbind(
        x %>% 
          summarize(
            P = max(P), R = max(R), A = max(A), S = max(S),
            B = max(B), `3PM` = max(`3PM`), GMSC = max(GMSC)
          ) %>% 
          mutate(SEASON = "CAREER")
      ) %>% 
      format_as_datatable() %>% 
      formatStyle(
        "SEASON",
        target = "row",
        fontWeight = styleEqual(c("CAREER"), "bold", default = "normal")
      )
  })
  
  # Achievements - Season
  output$achievements_season <- renderDT({
    
    req(input$password == myPassword || myPassword == '')
    
    get_achievements_season(myPlayerData(), dfs, input$name, ach_metadata) %>% 
      format_as_datatable()
    
  })
  
  
  # Achievements - Game
  output$achievements_game <- renderDT({
    
    req(input$password == myPassword || myPassword == '')
    
    get_achievements_game(myCombinedData(), ach_metadata) %>% 
      format_as_datatable()
    
  })
  
  # NBN Rankings
  output$rankings <- renderDT({
    
    req(input$password == myPassword || myPassword == '')
    
    my_ranks %>% 
      filter(PLAYER == input$name) %>% 
      select(PLAYER, SEASON, G, M, P, R, A, S, B, `3PM`, FGPCT, `3PPCT`, FTPCT, GMSC, PCT,
             G_RANK, M_RANK, P_RANK, R_RANK, A_RANK, S_RANK, B_RANK, `3PM_RANK`, FGPCT_RANK,
             `3PPCT_RANK`, FTPCT_RANK) %>% 
      rbind(
        dfs %>% 
          group_by(PLAYER) %>% 
          summarize(
            G = n(),
            M = sum(M),
            P = sum(P),
            R = sum(R),
            A = sum(A),
            S = sum(S),
            B = sum(B),
            `3PM` = sum(`3PM`),
            FGPCT = sum(FGM)/sum(FGA),
            `3PPCT` = sum(`3PM`)/sum(`3PA`),
            FTPCT = sum(FTM)/sum(FTA),
            GMSC = mean(GMSC),
            PCT = sum(WL == "W")/n()
          ) %>% 
          ungroup() %>% 
          mutate(
            G_RANK = rank(desc(G), ties.method = "min"),
            M_RANK = rank(desc(M), ties.method = "min"),
            P_RANK = rank(desc(P), ties.method = "min"),
            R_RANK = rank(desc(R), ties.method = "min"),
            A_RANK = rank(desc(A), ties.method = "min"),
            S_RANK = rank(desc(S), ties.method = "min"),
            B_RANK = rank(desc(B), ties.method = "min"),
            `3PM_RANK` = rank(desc(`3PM`), ties.method = "min"),
            FGPCT_RANK = rank(desc(FGPCT), ties.method = "min", na.last = "keep"),
            `3PPCT_RANK` = rank(desc(`3PPCT`), ties.method = "min", na.last = "keep"),
            FTPCT_RANK = rank(desc(FTPCT), ties.method = "min", na.last = "keep")
          ) %>% 
          filter(PLAYER == input$name) %>% 
          mutate(SEASON = "ALL-TIME")
      ) %>% 
      select(-PLAYER, -GMSC, -PCT, -FGPCT, -`3PPCT`, -FTPCT) %>% 
      format_as_datatable() %>% 
      formatRound(
        columns = c(2:15),
        digits = 0
      ) %>% 
      formatStyle(
        "SEASON",
        target = "row",
        fontWeight = styleEqual(c("ALL-TIME"), "bold", default = "normal")
      )
  })
  
  
  # Tab 3: Player Comparison ----
  output$player_compare <- renderDT({
    
    req(input$password == myPassword || myPassword == '')
    x <- dfs_everything %>% 
      filter(PLAYER == input$playercomp1 | PLAYER == input$playercomp2)
    
    if (input$playercomp_season != 'CAREER') {
      x <- x %>% 
        filter(SEASON == input$playercomp_season)
    }
    
    y <- x %>%
      group_by(PLAYER) %>% 
      summarize(
        G = n(),
        MPG = (sum(M)/G) %>% round(2),
        PPG = (sum(P)/G) %>% round(2),
        APG = (sum(A)/G) %>% round(2),
        RPG = (sum(R)/G) %>% round(2),
        SPG = (sum(S)/G) %>% round(2),
        BPG = (sum(B)/G) %>% round(2),
        TOPG = (sum(TO)/G) %>% round(2),
        GMSC = (sum(GMSC)/G) %>% round(2),
        FG = (sum(FGM)/sum(FGA)) %>% round(3),
        `3P` = (sum(`3PM`)/sum(`3PA`)) %>% round(3),
        FT = (sum(FTM)/sum(FTA)) %>% round(3)
      ) %>% 
      pivot_longer(names_to = 'CATEGORY', 
                   cols = c('G', 'MPG', 'PPG', 'APG', 'RPG', 'SPG', 'BPG', 'TOPG',
                            'GMSC', 'FG', '3P', 'FT')) %>% 
      pivot_wider(names_from = 'PLAYER') 
    
    if (!has_name(y, input$playercomp1)) {
      y[input$playercomp1] <- '-'
    }
    
    if (!has_name(y, input$playercomp2)) {
      y[input$playercomp2] <- '-'
    }
    
    
    y %>% 
      select('CATEGORY', input$playercomp1, input$playercomp2) %>% 
      mutate(winner = case_when(
        .[[input$playercomp1]] > .[[input$playercomp2]] ~ 1,
        .[[input$playercomp1]] < .[[input$playercomp2]] ~ 2,
        TRUE ~ 0
      )) %>% 
      datatable(rownames = FALSE,
                options = list(
                  columnDefs = list(list(visible = FALSE, targets = c(3))),
                  pageLength = 50
                )) %>% 
      formatStyle(
        input$playercomp1,
        target = "cell",
        valueColumns = 'winner',
        fontWeight = styleEqual(c(1), "bold", default = "normal")
      ) %>% 
      formatStyle(
        input$playercomp2,
        target = "cell",
        valueColumns = 'winner',
        fontWeight = styleEqual(c(2), "bold", default = "normal")
      )
    
  })
  
  
  # Tab 4: Franchise/NBN records ----
  output$franchise_records <- renderDT({
    
    req(input$password == myPassword || myPassword == '')
    
    if (input$reg_flag && input$playoff_flag) {
      x <- rbind(dfs, dfs_playoffs %>% select(-GAME, -ROUND))
    } else if (input$reg_flag && !input$playoff_flag) {
      x <- dfs
    } else if (!input$reg_flag && input$playoff_flag) {
      x <- dfs_playoffs
    } else {
      x <- head(dfs, 0)
    }
    
    if (input$season1 != "ALL-TIME") {
      x <- x %>% 
        filter(str_detect(SEASON, input$season1))
    }
    
    if (input$team != "NBA") {
      x <- x %>% filter(TEAM == input$team)
    }
    
    x <- x %>% 
      group_by(PLAYER) %>% 
      summarize(
        G = n(),
        M = sum(M),
        P = sum(P),
        R = sum(R),
        A = sum(A),
        S = sum(S),
        B = sum(B),
        `3PM` = sum(`3PM`),
        GMSC = mean(GMSC)
      ) %>% 
      mutate(
        MPG = M/G,
        PPG = P/G,
        RPG = R/G,
        APG = A/G,
        SPG = S/G,
        BPG = B/G,
        `3PMPG` = `3PM`/G
      )
    
    if (input$per_36_flag) {
      x <- x %>% 
        
        mutate(
          MP36 = MPG*36/MPG,
          PP36 = PPG*36/MPG,
          RP36 = RPG*36/MPG,
          AP36 = APG*36/MPG,
          SP36 = SPG*36/MPG,
          BP36 = BPG*36/MPG,
          `3PMP36` = `3PMPG`*36/MPG,
          GMSCP36 = GMSC*36/MPG
        ) %>% 
        
        select(-ends_with('PG'))
    }
    
    x_end <- ncol(x)
    
    x %>% 
      datatable(
        options = list(scrollX = TRUE)
      ) %>% 
      formatRound(
        columns = 2:9,
        digits = 0
      ) %>% 
      formatRound(
        columns = 10:x_end,
        digits = 2
      )
  }) 
  
  output$hof_points <- renderDT({
    
    req(input$password == myPassword || myPassword == '')
    
    calculate_hof_points(dfs_everything, dfs_playoffs, dfs)
  }, options = list(scrollX = TRUE))
  
  
  # Tab 5: Franchise History ----
  output$franchise_history_yoy <- renderDT({
    
    req(input$password == myPassword || myPassword == '')
    
    x <- dfs_everything %>% 
      filter(TEAM == input$team_history)
    
    x_leaders <- x %>% 
      group_by(SEASON, PLAYER) %>% 
      summarize(G = n(), GMSC = mean(GMSC)) %>% 
      group_by(SEASON) %>% 
      arrange(SEASON, desc(GMSC)) %>% 
      mutate(GMSC_RNK = rank(desc(GMSC), ties.method = "min")) %>% 
      filter(GMSC_RNK <= 3) %>% 
      mutate(GMSC_LEADERS = str_c(GMSC_RNK, ". ", PLAYER, " (", round(GMSC, 2), ")")) %>% 
      distinct(SEASON, GMSC_RNK, GMSC_LEADERS) %>% 
      pivot_wider(names_from = "GMSC_RNK", values_from = "GMSC_LEADERS") %>% 
      mutate(GMSC_LEADERS = str_c(`1`, `2`, `3`, sep = "<br>")) %>% 
      select(SEASON, GMSC_LEADERS)
    
    x_season <- x %>% 
      distinct(SEASON, DATE, WL) %>% 
      group_by(SEASON) %>% 
      summarize(
        W = sum(case_when(WL == "W" ~ 1, TRUE ~ 0)), 
        L = sum(case_when(WL == "L" ~ 1, TRUE ~ 0))
      ) %>% 
      ungroup() %>% 
      
      mutate(PCT = round(W / (W + L), 3)) %>% 
      
      left_join(get_owners() %>% filter(TEAM == input$team_history) %>% select(-TEAM)) %>% 
      fill(OWNER) %>% 
      
      left_join(x_leaders) 
    
    x_total <- x %>% 
      distinct(DATE, WL) %>% 
      summarize(
        W = sum(case_when(WL == "W" ~ 1, TRUE ~ 0)), 
        L = sum(case_when(WL == "L" ~ 1, TRUE ~ 0))
      ) %>% 
      ungroup() %>% 
      mutate(PCT = round(W / (W + L), 3),
             SEASON = "TOTAL",
             GMSC_LEADERS = "-")
    
    bind_rows(x_season, x_total) %>% 
      
      datatable(
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: left; color:black; font-size:200% ;',
          "SEASON BY SEASON RESULTS"
        ),
        options = list(scrollX = TRUE),
        rownames = FALSE,
        escape = FALSE
      ) %>% 
      formatStyle(
        "SEASON",
        target = "row",
        fontWeight = styleEqual(c("TOTAL"), "bold", default = "normal")
      )
    
  })
  
  output$franchise_history_scatter <- renderPlotly({
    
    team_data <- calculate_team_offense_defense(dfs) %>% 
      ungroup() %>% 
      mutate(ids = str_c(SEASON, ' ', TEAM, '\nOFF: ', round(OFF_RTG, 2), '\nDEF: ', round(DEF_RTG, 2)),
             color = case_when(TEAM == input$team_history ~ 'red',
                               TRUE ~ 'blue'))
    
    team_data_selected <- team_data %>% 
      filter(TEAM == input$team_history) %>% 
      arrange(SEASON)
    
    team_data %>% 
      plot_ly(
        x = ~OFF_RTG, 
        y = ~DEF_RTG, 
        text = ~ids,
        #color = ~color,
        #colors = ~color,
        marker = list(
          color = ~color
        ),
        type = 'scatter', 
        mode = 'markers',
        showlegend = F
      ) %>% 
      
      add_trace(text = ~ids, hoverinfo = 'text', showlegend = F) %>% 
      
      add_annotations(
        ax = team_data_selected$OFF_RTG[-nrow(team_data_selected)],
        ay = team_data_selected$DEF_RTG[-nrow(team_data_selected)],
        x = team_data_selected$OFF_RTG[-1],
        y = team_data_selected$DEF_RTG[-1],
        xref = 'x',
        yref = 'y',
        axref = 'x',
        ayref = 'y',
        showarrow = T,
        text = ''
      ) %>% 
      layout(
        xaxis = list(
          range = c(-15, 15)
        ),
        yaxis = list(
          range = c(-15, 15)
        )
      )
    
  })
  
  output$franchise_history_legends <- renderDT({
    calculate_hof_points(
      dfs_everything, 
      dfs_playoffs, 
      dfs,
      team_filter = input$team_history
    )
  }, options = list(scrollX = TRUE))
  
  output$franchise_history_awards <- renderDT({
    
    req(input$password == myPassword || myPassword == '')
    
    bind_rows(
      get_foty(),
      get_coty()
    ) %>% 
      filter(TEAM == input$team_history) %>% 
      arrange(SEASON) %>% 
      datatable(
        options = list(scrollX = TRUE),
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: left; color:black; font-size:200% ;',
          "FRONT OFFICE AWARDS"
        ),
        rownames = FALSE
      )
  })
  
  output$franchise_history_leaders <- renderDT({
    
    req(input$password == myPassword || myPassword == '')
    
    ctg <- input$stat_cat_team_history
    
    x <- dfs_everything %>% 
      filter(TEAM == input$team_history) %>% 
      mutate(G = 1) %>% 
      select(PLAYER, DATE, ctg) 
    
    if (input$stattype1 == "All-Time") {
      out <- x %>% 
        select(-DATE) %>% 
        group_by(PLAYER) %>% 
        summarize_all(sum) %>% 
        mutate(RANK = rank(desc(.[[ctg]]), ties.method = 'min')) %>% 
        filter(RANK <= 10) %>% 
        arrange(RANK)
    } else if (input$stattype1 == "Single Game") {
      out <- x %>% 
        mutate(RANK = rank(desc(.[[ctg]]), ties.method = 'min')) %>% 
        filter(RANK <= 10) %>% 
        arrange(RANK)
    }
    
    out
      
  }, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  
  output$franchise_history_cum_diff <- renderPlot({
    
    req(input$password == myPassword || myPassword == '')
    
    x <- dfs_everything %>% 
      mutate(OPP_RAW = str_replace(OPP, "@", ""))
    
    y <- x %>% 
      group_by(SEASON, TEAM, OPP, OPP_RAW, DATE) %>% 
      summarize(P = sum(P)) %>% 
      ungroup()
    
    z <- y %>% 
      filter(TEAM == input$team_history) %>% 
      inner_join(
        y %>% select(OPP_RAW = TEAM, DATE, OPP_P = P),
        by = c('OPP_RAW', 'DATE')
      ) %>% 
      mutate(DIFF = P - OPP_P) %>% 
      arrange(DATE) %>% 
      mutate(CUM_DIFF = cumsum(DIFF),
             G = row_number()) 
    
    z %>% 
      ggplot(aes(x = G, y = CUM_DIFF)) +
      geom_line() +
      geom_point(aes(col = SEASON))
    
  })
  
  output$franchise_history_rings <- renderUI({
    
    x <- champions %>% 
      distinct(TEAM, SEASON) %>% 
      filter(TEAM == input$team_history) %>% 
      mutate(SEASON = str_replace(SEASON, ' Playoffs', ''))
    
    glue("Rings: {nrow(x)} ({str_c(x$SEASON, collapse = ', ')})") %>% 
      HTML()
    
  })
  
  output$franchise_history_retired <- renderUI({
    
    x <- get_retired_jerseys() %>% 
      filter(TEAM == input$team_history) %>% 
      mutate(TXT = str_c(NO, ': ', PLAYER)) %>% 
      pull(TXT) %>% 
      str_c(collapse = '<br/>')
    
    str_c("<br/>Retired Jerseys:<br/>", x) %>% 
      HTML()
    
  })
  
  
  # Tab 6: MVP race ----
  output$mvp_race <- renderDT({
    
    req(input$password == myPassword || myPassword == '')
    
    my_ranks %>% 
      filter(SEASON == input$season3) %>% 
      mutate(MVP_SCORE = (GMSC*G) * (PCT^2)) %>% 
      arrange(desc(MVP_SCORE)) %>% 
      mutate(MVP_SCORE = round(MVP_SCORE, 2),
             P = round(P, 1),
             R = round(R, 1),
             A = round(A, 1),
             GMSC = round(GMSC, 2),
             PCT = round(PCT, 3)) %>% 
      select(PLAYER, MVP_SCORE, PCT, P, R, A, GMSC) %>% 
      head(50)
    
  }, options = list(pageLength = 50))
  
  output$ppg_leaders <- renderPlot({
    
    req(input$password == myPassword || myPassword == '')
    
    dfs %>% 
      filter(SEASON == input$season3) %>% 
      group_by(PLAYER) %>% 
      summarize(PPG = sum(P) / n(),
                TS = sum(P) / (2*(sum(FGA) + (.44*sum(FTA))))) %>% 
      filter(PPG >= 20) %>% 
      ggplot(aes(x = PPG, y = TS)) + 
      geom_point() +
      geom_label(aes(label = PLAYER), nudge_y=-.01, label.size=.1) 
  })
  
  
  # Tab 7: Box scores ----
  output$boxscore_input <- renderUI({
    req(input$boxscoredate)
    selectInput('boxscore_output',
                'Select game:',
                dfs_everything %>% 
                  filter(DATE == input$boxscoredate) %>% 
                  distinct(TEAM, OPP) %>% 
                  filter(str_detect(OPP, '^@')) %>% 
                  mutate(x = str_c(TEAM, OPP)) %>% 
                  pull(x))
  })
  
  output$boxscore_selected <- renderDT({
    req(input$boxscore_output)
    
    get_box_score(dfs_everything, input$boxscoredate, input$boxscore_output)
  }, rownames = FALSE)
  
  # Tab 8: Playoffs ----
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
        mutate(RESULT = str_c(WINNER, ' (', SERIES1, '-', SERIES2, ')')) %>% 
        tail(1) %>% 
        pull(RESULT)
      
      if (length(z) == 0) {
        return(NA)
      }
      
      if (!str_detect(z, '4')) {
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
  
  # Tab 9: Power Rankings ----
  output$power_rankings <- renderPlot({

    xx <- dfs %>%
      
      filter(SEASON == input$pr_season) %>% 
      
      calculate_power_rankings() %>% 

      filter(TEAM %in% input$pr_teams)


    xx %>%

      ggplot(aes(x = DATE, y = TEAM_PR, color = COLOR)) +
      geom_line() +
      geom_point() +
      scale_color_identity(
        guide = 'legend',
        labels = unique(xx$TEAM),
        breaks = vget_team_color(unique(xx$TEAM))
      ) +
      scale_y_reverse(limits = c(30, 0)) 
    
  })
  
  output$power_rankings_table <- renderReactable({
    
    out_df <- dfs %>% 
      filter(SEASON == input$pr_season) %>% 
      calculate_power_rankings() %>% 
      select(DATE, TEAM, TEAM_PR) %>% 
      
      pivot_wider(id_cols = 'TEAM', names_from = 'DATE', values_from = 'TEAM_PR') %>% 
      
      select('TEAM', ncol(.):2) %>% 
      
      arrange(.[,2])
    
    my_pal <- function(x) {
      if (!is.na(x)){
        rgb(colorRamp(c("red", "green"))(x), maxColorValue = 255)
      } else {
        "#e9e9e9" #grey
      }
    }
    
    coldefs <- list(
      colDef(
        style = function(value, index, name) {
          color <- my_pal((31-value)/30)
          list(background = color)
        }
      )
    )
    
    coldefs <- rep(coldefs, ncol(out_df) - 1)
    names(coldefs) <- out_df %>% 
      select(-TEAM) %>% 
      names()
    
    reactable(
      out_df, 
      columns = coldefs,
      
      defaultColDef = colDef(
        align = 'center'
      ),
      
      defaultPageSize = 30,
      highlight = TRUE,
      compact = TRUE
    )
    
  })
  
  
  
  # Tab 10: NBN WALLSTREET ----
  output$ws_prices <- renderDT({
    
    diffs <- build_prices(dfs_everything)
    
    diffs %>% 
      mutate(
        CHG_LAST_10 = round((PRICE / lag(PRICE, 10) - 1) * 1, 4),
        CHG_LAST_82 = round((PRICE / lag(PRICE, 82) - 1) * 1, 4),
        HIGH_82 = rollmax(PRICE, 82, align = "right", fill = NA),
        LOW_82 = rollapply(PRICE, 82, min, align = "right", fill = NA)
      ) %>% 
      filter(row_number() == n()) %>% 
      select(TEAM, PRICE, CHG_LAST_10, CHG_LAST_82, HIGH_82, LOW_82) %>% 
      datatable(rownames = FALSE, options = list(pageLength = 100)) %>% 
      formatPercentage(c('CHG_LAST_10', 'CHG_LAST_82'), mark = ".", digits = 2) %>% 
      formatCurrency(c('PRICE', 'HIGH_82', 'LOW_82'))
  })
  
  temp_order <- eventReactive(
    input$button_order,
    {
      x <- tribble(
        ~USER, ~DATE, ~TEAM, ~TYPE, ~QUANTITY,
        toupper(input$order_user), today(tzone = 'EST'), input$order_team, input$order_type, input$order_quantity
      )
      
      sheet_append(
        '1JgyIRhnpe2f6UPFBd9KehQXJyfqmQDGH5jRzQC46cFI',
        x
      )
      
      return(glue('YOUR ORDER TO {input$order_type} {input$order_quantity} SHARES OF {input$order_team} WAS RECORDED. ',
                  "The execution price will be the price of {input$order_team} after games are played on {today(tzone = 'EST')}. ",
                  'Please note that the execution price may differ from the price currently shown in the event that ',
                  'the team has played games since the last stats update and today.'))
    }
  )
  
  output$result_order <- renderText({temp_order()})
  
  output$ws_positions <- renderDT({
    diffs <- build_prices(dfs_everything) %>% 
      select(DATE, TEAM, PRICE)
    
    ws_orders %>% 
      mutate(QUANTITY = case_when(
        TYPE == 'SELL' ~ -1 * QUANTITY,
        TRUE ~ QUANTITY
      )) %>% 
      left_join(diffs) %>% 
      left_join(diffs %>% 
                  filter(DATE == max(DATE)) %>% 
                  select(TEAM, PRICE_LATEST = PRICE)) %>% 
      mutate(NBYEN_FLOW = QUANTITY*PRICE,
             PROFIT_PER_SHARE = PRICE_LATEST - PRICE,
             PROFIT = PROFIT_PER_SHARE * QUANTITY)
      
  })
  
  output$wallstreet <- renderPlotly({
    
    diffs <- build_prices(dfs_everything) %>% 
      filter(TEAM %in% input$ws_teams) %>%
      group_by(TEAM, SEASON) %>% 
      mutate(rn = row_number(),
             #COLOR = vget_team_color(TEAM),
             N = str_c(SEASON, ' G', rn)) %>% 
      ungroup()
    
    if (!is.null(input$ws_date_min)) {
      diffs <- diffs %>%
        filter(DATE >= input$ws_date_min)
    }

    if (!is.null(input$ws_date_max)) {
      diffs <- diffs %>%
        filter(DATE <= input$ws_date_max)
    }
    
    if (nrow(diffs) == 0) {
      NULL
    } else if (length(input$ws_teams) == 1) {

      diffs %>%

        select(SEASON, TEAM, DATE, N, DIFF_DIFF, PCT_CHG, PRICE) %>%
        group_by(TEAM) %>% 

        ggplot(aes(x = DATE, y = PRICE)) +
        geom_line() +
        geom_point(aes(col = SEASON), size = 0.5) +
        scale_y_continuous(
          labels=scales::dollar_format()
        ) +
        #xlim(c(input$ws_date_min, input$ws_date_max)) +
        ggtitle('Team Value')
      
    } else {
      
      diffs %>% 
        
        select(rn, SEASON, TEAM, DATE, DIFF_DIFF, PCT_CHG, PRICE) %>% 
        
        ggplot(aes(x = DATE, y = PRICE, color = TEAM)) + 
        #ggplot(aes(x = DATE, y = PRICE, color = COLOR)) + 
        geom_line() +
        scale_y_continuous(
          labels=scales::dollar_format()
        ) +
        #xlim(c(input$ws_date_min, input$ws_date_max)) +
        ggtitle('Team Value')
      
    }
  })
  
  output$ws_div <- renderPlotly({
    x <- build_prices(dfs_everything) %>% 
      ungroup() %>% 
      mutate(CONF = toupper(get_conference(TEAM)), 
             DIV = case_when(
               TEAM %in% c('BOS', 'PHI', 'NYK', 'BKN', 'TOR') ~ 'ATLA',
               TEAM %in% c('MIL', 'CLE', 'CHI', 'IND', 'DET') ~ 'CENT',
               TEAM %in% c('ATL', 'MIA', 'WAS', 'ORL', 'CHA') ~ 'SOEA',
               TEAM %in% c('DEN', 'MIN', 'OKC', 'UTA', 'POR') ~ 'NOWE',
               TEAM %in% c('SAC', 'PHX', 'LAC', 'GSW', 'LAL') ~ 'PACI',
               TEAM %in% c('MEM', 'NOP', 'DAL', 'HOU', 'SAS') ~ 'SOWE',
               TRUE ~ NA_character_
             )) %>% 
      
      group_by(DATE, DIV) %>% 
      summarize(
        PCT_CHG = mean(PCT_CHG)
      ) %>% 
      group_by(DIV) %>% 
      arrange(DIV, DATE) %>% 
      mutate(PRICE = cumprod(PCT_CHG) * 100)
    
    if (!is.null(input$ws_date_min)) {
      x <- x %>% 
        filter(DATE >= input$ws_date_min)
    }
    
    if (!is.null(input$ws_date_max)) {
      x <- x %>% 
        filter(DATE <= input$ws_date_max)
    }
    
    x %>% 
      
      ggplot(aes(x = DATE, y = PRICE, color = DIV)) +
      geom_line() +
      scale_y_continuous(labels=scales::dollar_format()) +
      ggtitle('Division Indexes')
  })
  
  output$ws_text <- renderText({glue(
    "In general, if team does well, price goes up; if team does badly, price goes",
    "down. Prices are updated when teams play a game - this is all stats-based,",
    "after all."
  )})
  
  
  # Tab 11: EXPLORE ----
  output$explore_output <- renderDT({
    
    x <- dfs_everything %>% mutate(G = 1)
    
    vars_to_summarize <- input$explore_var
    
    if (str_detect(input$explore_var, 'PCT$')) {
      
      pctvar_m <- str_extract(input$explore_var, '^[A-Z]{2}') %>% 
        str_c('M')
      pctvar_a <- str_extract(input$explore_var, '^[A-Z]{2}') %>% 
        str_c('A')
      
      vars_to_summarize <- c(pctvar_m, pctvar_a)
      
    }
    
    
    if (str_detect(input$explore_level, 'in a season')) {
      
      x <- x %>% 
        group_by(PLAYER, SEASON) %>% 
        summarize_at(vars(vars_to_summarize), lst(sum, mean))
      
    } else if (str_detect(input$explore_level, 'in a career')) {
      
      x <- x %>% 
        group_by(PLAYER) %>% 
        summarize_at(vars(vars_to_summarize), lst(sum, mean))
      
    } else {
      
      x <- x %>% 
        select(c('PLAYER', 'SEASON', 'DATE', input$explore_var, 
                 'M', 'P', 'R', 'A', 'S', 'B', 'TO', 'PF', 'GMSC',
                 'FGM', 'FGA', '3PM', '3PA', 'FTM', 'FTA'))
      
    }
    
    
    if (str_detect(input$explore_level, '(total)')) {
      sortvar <- 'sum'
      x <- x %>%
        rename(!!input$explore_var := sum) %>%
        select(-mean)
    } else if (str_detect(input$explore_level, '(avg)')) {
      sortvar <- 'mean'
      x <- x %>%
        rename(!!input$explore_var := mean) %>%
        select(-sum)
    } else {
      sortvar <- input$explore_var
    }

    
    if (input$explore_type == 'The highest') {
      
      x <- arrange(x, desc(pick(input$explore_var)))
      
    } else if (input$explore_type == 'The lowest') {
      
      x <- arrange(x, pick(input$explore_var))
      
    } 
    
    
    x %>% 
      
      mutate_if(is.numeric, function(x) {round(x, 2)}) %>% 
      
      datatable(rownames = TRUE, options = list(pageLength = 100, scrollX = TRUE))
    
  })
  
  # Tab 13: Trade Machine ----
  
  parse_tm_assets <- function(assets) {
    assets %>% 
      str_split_1("\\n") %>% 
      map_dfr(function(x) {tibble(text = x)}) %>% 
      separate(text, 
               into = c('PLAYER', 'SALARY', 'TEAM_TO'),
               sep = ';') %>% 
      mutate(PLAYER = trim(toupper(PLAYER)),
             SALARY = as.numeric(trim(SALARY)),
             TEAM_TO = trim(toupper(TEAM_TO)))
  }
  
  tm_inputs <- eventReactive(input$tm_calculate, {
    
    x <- tribble(
      ~TEAM, ~GUARANTEED, ~CAPPED,
      input$tm_team_1, input$tm_guaranteed_1, input$tm_capped_1,
      input$tm_team_2, input$tm_guaranteed_2, input$tm_capped_2,
      input$tm_team_3, input$tm_guaranteed_3, input$tm_capped_3,
      input$tm_team_4, input$tm_guaranteed_4, input$tm_capped_4
    ) %>% 
      filter(trim(TEAM) != '' & trim(TEAM) != 'TEAM') %>% 
      mutate(CAP = input$tm_cap,
             APRON_1 = input$tm_apron_1,
             APRON_2 = input$tm_apron_2)
    
    outgoing <- c()
    incoming <- c()
    for (i in 1:nrow(x)) {
      oc <- parse_tm_assets(input[[str_c('tm_players_', i)]]) %>% 
        pull(SALARY) %>% 
        sum()
      outgoing <- c(outgoing, oc)
      
      ic <- 0
      for (j in 1:nrow(x)) {
        ic_addl <- parse_tm_assets(input[[str_c('tm_players_', j)]]) %>% 
          filter(TEAM_TO == input[[str_c('tm_team_', i)]]) %>% 
          pull(SALARY) %>% 
          sum()
        
        ic <- ic + ic_addl
      }
      incoming <- c(incoming, ic)
    }
    x$OUTGOING = outgoing
    x$INCOMING = incoming
    
    x %>% 
      mutate(
        SALARY_CHG = INCOMING - OUTGOING,
        SALARY_AFTER_TRADE = GUARANTEED + SALARY_CHG
      ) %>% 
      mutate(VALID = case_when(
        # Check hard cap
        CAPPED != 'None' & SALARY_AFTER_TRADE > APRON_2 ~ 'INVALID (HARD CAP)',
        CAPPED == 'Apron 1' & SALARY_AFTER_TRADE > APRON_1 ~ 'INVALID (HARD CAP)',
        
        # If salary going down, valid
        # If salary is under the cap after trade, valid
        SALARY_CHG <= 0 | SALARY_AFTER_TRADE < CAP ~ 'VALID',
        
        # If salary is over Apron 1, check for 110%
        SALARY_AFTER_TRADE >= APRON_1 & INCOMING > 1.1*OUTGOING ~ 'INVALID (110%)',
        
        # If salary is under Apron 1 (but over cap), check special cases
        OUTGOING <= 7250000 & INCOMING > 2*OUTGOING + 250000 ~ 'INVALID (2*OUT+250K)',
        OUTGOING <= 29000000 & INCOMING > OUTGOING + 7500000 ~ 'INVALID (OUT+7.5M)',
        INCOMING > 1.25*OUTGOING + 250000 ~ 'INVALID (125%)',
        
        TRUE ~ 'VALID'
      )) %>% 
      datatable() %>% 
      formatCurrency(c('CAP', 'APRON_1', 'APRON_2', 'INCOMING', 'OUTGOING', 
                       'SALARY_CHG', 'SALARY_AFTER_TRADE', 'GUARANTEED'),
                     digits = 0)
  })
  
  output$tm_output <- renderDT({
    
    tm_inputs()
    
  })
  
  
  
  # Misc. ----
  output$premiumfaq <- renderText({glue(
    "<br><b>What is PREMIUM?</b><br><br>",
    "We want to give NBN members an incentive to enter stats and reward those ",
    "who do so. By participating in stats committee, you can see much more ",
    "detailed information about the stats we collect for this league.<br><br>",
    
    "<b>What are the requirements for PREMIUM access?</b><br><br>",
    "Every month (ish), the password for PREMIUM will change. The password will ",
    "then be shared with members who are (1) members of stats committee and (2) ",
    "have entered approximately half of their team's stats over the last period. ",
    "<br><br>",
    
    "<b>Can I share the password with my FO?</b><br><br>",
    "Yes. We only care that each FO is participating in stats. So Kyle can have ",
    "access even though he is being carried by KVL. Sharing the password outside ",
    "of your FO will result in an immediate drone strike on your location.<br><br>",
    
    "<b>What if I'm busy or otherwise can't do stats for a bit?</b><br><br>",
    "It happens. Just reach out to bryn or chuck and we can make exceptions, ",
    "especially if you are typically an active stats member.<br><br>"
  )})
  
  output$changelog <- renderText({glue(
    "<br><b>Version 2.0.3</b> (2/24/2024)<br>",
    " * Added customizable scatter plot to Season Dashboard.", "<br>",
    " * Under the hood improvements.", "<br>",
    "<br>",
    
    "<br><b>Version 2.0.2</b> (2/21/2024)<br>",
    " * Added offensive and defensive rating scatterplots to Franchise Profile tab.", "<br>",
    "<br>",
    
    "<br><b>Version 2.0.1</b> (1/29/2024)<br>",
    " * Added retired jerseys to Franchise Profiles.", "<br>",
    " * Some pre-processing of data now occurs to hopefully help load times.", "<br>",
    " * Added Hall-of-Fame points tracker to NBN/Franchise Records tab.", "<br><br>",
    
    "<br><b>Version 2.0.0</b> (1/28/2024)<br>",
    " * Complete revamp of UI.", "<br>",
    " * Added team pop-ups, accessible by clicking on a team in Standings or Team Stats.", "<br><br>",
    
    "<br><b>Version 1.0.2</b> (1/26/2024)<br>",
    " * Added a Trade Machine tab (in beta).", "<br><br>",
    
    "<br><b>Version 1.0.1</b> (1/23/2024)<br>",
    " * Can now click through League Leaders and Game Log tables to get more information.", "<br>",
    " * Updates to Leage Leaders panel:", "<br>",
    "   * Now shows top 10 players instead of top 5 and implements a minimum games qualification of 20 games.", "<br>",
    "   * Includes GMSC as a stat.", "<br>",
    " * Fixed bug with Player Compare tab.", "<br>",
    " * MVP Race panel removed (nobody really uses it and there's GMSC leaders now which somewhat makes up for it).", "<br><br>", 
    "<br><b>Version 1.0.0</b> (1/16/2024)<br>",
    " * Initial release of non-beta version of Nothing But Stats!", "<br>",
    " * Added a per-36 toggle for NBN/Franchise record tables.", "<br>",
    "<br><br>"
  )})
  
  output$diag <- renderText({str_c(log, collapse = '\n')})
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
}
