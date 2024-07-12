plot_scatter_general <- function(
  input_df,
  var_x,
  var_y,
  group,
  per_36 = FALSE,
  min_games = 1
) {
  
  if (group == 'PLAYER') {
    x <- summarize_player(input_df) %>% 
      filter(G >= min_games)
    
  } else if (group == 'TEAM') {
    x <- summarize_team(input_df)
  }
  
  x <- x %>% 
    
    mutate(hovertext = str_c(.[[group]])) %>% 
    
    select_at(c(var_x, var_y, 'hovertext', 'MPG')) %>% 
    set_names(c('v_x', 'v_y', 'hovertext', 'MPG')) 
  
  if (per_36) {
    
    if (str_detect(var_x, 'PG$') || var_x == 'GMSC') {
      x <- x %>% mutate(v_x = v_x / MPG * 36)
    }
    
    if (str_detect(var_y, 'PG$') || var_y == 'GMSC') {
      x <- x %>% mutate(v_y = v_y / MPG * 36)
    }
    
  }
  
  x %>% 
    
    filter(!is.na(v_x), !is.na(v_y)) %>% 
    
    plot_ly(
      x = ~v_x,
      y = ~v_y,
      type = 'scatter',
      mode = 'markers'
    ) %>% 
    
    add_trace(
      text = ~hovertext, 
      hovertemplate = paste(
        "%{text}<br>",
        "%{x},%{y}<br>"
      ),
      showlegend = F
    )
  
}

plot_efficiency_scatter <- function(dfs, min_ppg = 20) {
  
  x <- dfs %>% 
    group_by(PLAYER) %>% 
    summarize_per_game() %>% 
    mutate(PPG = as.numeric(PPG)) %>% 
    filter(PPG >= min_ppg)
  
  teams <- c()
  for (i in 1:nrow(x)) {
    teams <- c(teams, get_last_played_for(x$PLAYER[[i]], dfs))
  }
  
  x$TEAM <- teams
  
  x %>% 
    mutate(hovertext = str_c(PLAYER, ' (', TEAM, ')'),
           imgloc = str_c("<img src='logo-", tolower(TEAM), ".png'></img>")) %>% 
    
    plot_ly(
      x = ~PPG, 
      y = ~TS, 
      #color = ~TEAM,
      type = 'scatter', 
      mode = 'markers',
      showlegend = F,
      marker = list(size = 10)
    ) %>% 
    
    add_trace(
      text = ~hovertext, 
      hovertemplate = paste(
        "%{text}<br>",
        "%{x} PPG<br>",
        "%{y}% TS<br>"
      ),
      showlegend = F
    )
  
  }