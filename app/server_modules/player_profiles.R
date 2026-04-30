# Player Profiles Module ----
# All outputs for the Player Profiles tab

output$headshot <- renderText({
  c('<img src="', myBiosData() %>% pull(`Img URL`), '">')
})

output$player_summary <- renderText({
  player_awards <- get_all_player_awards(input$name)

  award_line <- function(type, label, count = TRUE) {
    n <- nrow(filter(player_awards, AWARD == type))
    if (n == 0) return("")
    if (count) str_c(n, "x ", label, "\n") else str_c(label, "\n")
  }

  glue(
    "{input$name} ({myBiosData() %>% pull(`Combo Pos.`)}):\n",
    "DOB: {myBiosData() %>% pull(DOB)}\n",
    "Age: {round(time_length(interval(myBiosData() %>% pull(DOB), today()), 'years'), 2)}\n",
    "Height: {myBiosData() %>% pull(Height)}\n",
    "Weight: {myBiosData() %>% pull(Weight)}\n",
    "Drafted: {myBiosData() %>% pull(`NBN D YR`)}: {myBiosData() %>% pull(`NBN D R`)}, {myBiosData() %>% pull(`NBN D P`)}\n",
    "From: {myBiosData() %>% pull(COLLEGE)}\n\n",
    "Awards:\n",
    "{award_line('All-Star', 'All-Star')}",
    "{award_line('Most Valuable Player', 'Most Valuable Player')}",
    "{award_line('Defensive Player of the Year', 'Defensive Player of the Year')}",
    "{award_line('Sixth Man of the Year', 'Sixth Man of the Year')}",
    "{award_line('Rookie of the Year', 'Rookie of the Year', count = FALSE)}",
    "{award_line('Most Improved Player', 'Most Improved')}",
    "{award_line('All-NBN First Team', 'All-NBN First Team')}",
    "{award_line('All-NBN Second Team', 'All-NBN Second Team')}",
    "{award_line('All-NBN Third Team', 'All-NBN Third Team')}",
    "{award_line('All-Defense', 'All-Defense')}",
    "{award_line('All-Rookie', 'All-Rookie')}"
  )
})

#### Season by Season ----
output$tbl_season <- renderDT({
  
  req(input$password == myPassword || myPassword == '')
  
  begin <- Sys.time()
  
  champs <- champions %>%
    distinct(TEAM, SEASON) %>%
    mutate(ring = "<img src='ring.png' height='20'></img>")
  
  x <- myCombinedData() %>%
    group_by(SEASON, TEAM) %>%
    mutate(first_date = min(DATE)) %>% # Display teams in correct order within year
    group_by(SEASON, first_date, TEAM) %>%
    summarize_per_game() %>%
    select(-first_date)
  
  y <- x %>%
    rbind(
      myCombinedData() %>%
        summarize_per_game() %>%
        mutate(SEASON = "CAREER", TEAM = "")
    ) %>%
    # Add a column to identify regular season vs playoffs before SEASON is modified
    mutate(SEASON_TYPE = case_when(
      SEASON == "CAREER" ~ "CAREER",
      str_detect(SEASON, " Playoffs") ~ "PLAYOFFS",
      TRUE ~ "REGULAR"
    )) %>%
    left_join(myAwards(), by = c("SEASON")) %>%
    group_by(SEASON) %>%
    mutate(star    = case_when(row_number() == n() ~ star, TRUE ~ NA_character_),
           crown   = case_when(row_number() == n() ~ crown, TRUE ~ NA_character_),
           hand    = case_when(row_number() == n() ~ hand, TRUE ~ NA_character_),
           six     = case_when(row_number() == n() ~ six, TRUE ~ NA_character_),
           baby    = case_when(row_number() == n() ~ baby, TRUE ~ NA_character_),
           chart   = case_when(row_number() == n() ~ chart, TRUE ~ NA_character_),
           medal1  = case_when(row_number() == n() ~ medal1, TRUE ~ NA_character_),
           medal2  = case_when(row_number() == n() ~ medal2, TRUE ~ NA_character_),
           medal3  = case_when(row_number() == n() ~ medal3, TRUE ~ NA_character_),
           fence   = case_when(row_number() == n() ~ fence, TRUE ~ NA_character_),
           seed    = case_when(row_number() == n() ~ seed, TRUE ~ NA_character_)
    ) %>%
    ungroup() %>%
    left_join(champs, by = c("TEAM", "SEASON")) %>%
    mutate(SEASON = str_c(SEASON, " ", coalesce(ring, ""), coalesce(star, ""),
                          coalesce(crown, ""), coalesce(hand, ""), coalesce(six, ""),
                          coalesce(baby, ""), coalesce(chart, ""), coalesce(medal1, ""),
                          coalesce(medal2, ""), coalesce(medal3, ""),
                          coalesce(fence, ""), coalesce(seed, ""))) %>%
    select(SEASON, SEASON_TYPE, everything(), -PLAYER, -star, -ring, -crown, -hand, -six, -baby, -chart,
           -starts_with("medal"), -fence, -seed) %>%
    {
      season_type_col <- which(names(.) == "SEASON_TYPE") - 1
      format_as_datatable(
        .,
        escape = FALSE,
        column_defs = list(
          list(visible = FALSE, targets = season_type_col)
        ),
        page_length = 1000
      )
    } %>%
    formatStyle(
      "SEASON",
      target = "row",
      fontWeight = styleEqual(c("CAREER "), "bold", default = "normal")
    ) %>%
    formatStyle(
      "SEASON_TYPE",
      target = "row",
      backgroundColor = styleEqual(
        c("REGULAR", "PLAYOFFS", "CAREER"),
        c("#f9f9f9", "#e8f4f8", "white")
      )
    )
  
  print(glue("[{sprintf('%.7f', round(Sys.time() - begin, 7))}] player per-season stats generated."))
  
  y
})

#### Game Log Plot ----
myGamelogPlot <- reactive({
  myCombinedData() %>%
    arrange(DATE) %>%
    mutate(G = row_number())
})

output$gamelog_plot <- renderPlotly({
  req(input$password == myPassword || myPassword == '')
  begin <- Sys.time()
  
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
  
  print(glue("[{sprintf('%.7f', round(Sys.time() - begin, 7))}] player game log plot generated."))
  
  p
  
})

#### Game/Career Highs ----
output$records <- renderDT({
  req(input$password == myPassword || myPassword == '')
  begin <- Sys.time()
  
  x <- myCombinedData() %>%
    group_by(SEASON) %>%
    summarize(
      P = max(P), R = max(R), A = max(A), S = max(S),
      B = max(B), `3PM` = max(`3PM`), GMSC = max(GMSC)
    ) %>%
    ungroup()
  
  y <- x %>%
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
  
  print(glue("[{sprintf('%.7f', round(Sys.time() - begin, 7))}] player season highs generated."))
  
  y
})

#### All-Time Totals and Rankings ----
output$rankings <- renderDT({
  req(input$password == myPassword || myPassword == '')
  begin <- Sys.time()
  
  req(input$password == myPassword || myPassword == '')
  
  x <- my_ranks %>%
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
  
  print(glue("[{sprintf('%.7f', round(Sys.time() - begin, 7))}] player ranks generated."))
  
  x
})

#### Game log ----
output$tbl <- renderDT({
  
  req(input$password == myPassword || myPassword == '')
  
  begin <- Sys.time()
  
  x <- myCombinedData() %>%
    select(
      DATE, TEAM, OPP, M, P, R, A, S, B, TO,
      FG, `3P`, FT, PF, GMSC, WL
    ) %>%
    arrange(desc(DATE)) %>%
    
    format_as_datatable(
      filter = 'top'
    )
  
  print(glue("[{sprintf('%.7f', round(Sys.time() - begin, 7))}] player game log generated."))
  
  x
})
