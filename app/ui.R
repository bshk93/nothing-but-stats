dfs <- read_rds('data/dfs.rds')
dfs_playoffs <- read_rds('data/dfs_playoffs.rds')

player_teams <- bind_rows(dfs, dfs_playoffs) %>% 
  arrange(PLAYER, DATE) %>% 
  group_by(PLAYER) %>% 
  mutate(last_played = last(TEAM)) %>% 
  ungroup() %>% 
  mutate(NAME = str_c(PLAYER, ' (', last_played, ')')) %>% 
  distinct(PLAYER, NAME)

named_names <- player_teams$PLAYER %>% 
  set_names(player_teams$NAME)

# UI ----

sidebar <- dashboardSidebar(
  passwordInput("password", "PREMIUM Password:"),
  sidebarMenu(
    menuItem(
      "Season Dashboard",
       tabName = "tab_dash", 
       icon = icon("dashboard")
    ),
    menuItem(
      "Power Rankings",
      tabName = "tab_prs", 
      icon = icon("arrow-trend-up")
    ),
    menuItem(
      "Box Scores",
      tabName = "tab_box", 
      icon = icon("table")
    ),
    menuItem(
      "Playoff Archive",
      tabName = "tab_playoffs", 
      icon = icon("code-fork")
    ),
    menuItem(
      "Data Explorer",
      tabName = "tab_explore", 
      icon = icon("magnifying-glass-chart")
    ),
    menuItem(
      "Trade Machine",
      tabName = "tab_tm", 
      icon = icon("trademark")
    ),
    menuItem(
      "Player Overview",
      tabName = "tab_player", 
      icon = icon("user"),
      badgeLabel = "Prem",
      badgeColor = "yellow"
    ),
    menuItem(
      "Player Compare",
      tabName = "tab_compare", 
      icon = icon("user-group"),
      badgeLabel = "Prem",
      badgeColor = "yellow"
    ),
    menuItem(
      "NBN/Franchise Records",
      tabName = "tab_records", 
      icon = icon("ranking-star"),
      badgeLabel = "Prem",
      badgeColor = "yellow"
    ),
    menuItem(
      "Franchise History",
      tabName = "tab_franchise", 
      icon = icon("book"),
      badgeLabel = "Prem",
      badgeColor = "yellow"
    ),
    menuItem(
      "NBN Wall Street",
      tabName = "tab_ws", 
      icon = icon("dollar-sign")
    ),
    menuItem(
      "About",
      tabName = "tab_about", 
      icon = icon("question")
    ),
    menuItem(
      "Diagnostics",
      tabName = "tab_diag",
      icon = icon("stethoscope")
    )
  )
)

body <- dashboardBody(
  
  tags$style(
    type = 'text/css',
    '.modal-dialog { width: fit-content !important; }'
  ),
  
  tabItems(
    tabItem(
      tabName = "tab_dash",
      selectizeInput(
        'season2',
        'Choose a Season:',
        c("23-24", "22-23", "21-22", "20-21")
      ),

      h2("AROUND THE NBN"),
      DTOutput("newsfeed"),

      h2("REGULAR SEASON STANDINGS"),
      DTOutput("standings"),

      h2("LEAGUE LEADERS"),
      DTOutput("leaders"),
      
      h2("PLAYER/TEAM SCATTER"),
      selectizeInput(
        'scatter_x',
        'X = ',
        c('GMSC', 'PPG', 'RPG', 'APG', 'SPG', 'BPG', 'FG', '3P', 'FT', 'TS',
          'FGMPG', 'FGAPG', '3PMPG', '3PAPG', 'FTMPG', 'FTAPG'),
        selected = 'TS'
      ),
      selectizeInput(
        'scatter_y',
        'Y = ',
        c('GMSC', 'PPG', 'RPG', 'APG', 'SPG', 'BPG', 'FG', '3P', 'FT', 'TS',
          'FGMPG', 'FGAPG', '3PMPG', '3PAPG', 'FTMPG', 'FTAPG'),
        selected = 'PPG'
      ),
      selectizeInput(
        'scatter_group',
        'By:',
        c('PLAYER', 'TEAM')
      ),
      checkboxInput('scatter_36', 'Per 36', value = FALSE),
      sliderInput('scatter_min_games', 'Min. Games (for players)', 
                  min = 1, max = 82, 
                  value = 1,
                  step = 1,
                  round = TRUE),
      plotlyOutput("season_scatter"),

      h2("TEAM STATS"),
      DTOutput("team_stats"),

      h2("DRAFT LOTTERY PREVIEW"),
      DTOutput("tankathon"),

      h2("GAME LOG"),
      DTOutput("gamelog"),
      
      h2("SEASON AWARDS"),
      h3("All-Stars"),
      DTOutput("season_allstars")

      # h2("POINTS SCORED VS ALLOWED"),
      # plotOutput("points_scored_allowed")
    ),
    
    tabItem(
      tabName = "tab_prs",
      selectizeInput(
        'pr_season',
        'Choose a Season',
        c("23-24", "22-23", "21-22", "20-21")
      ),
      selectizeInput(
        'pr_teams',
        'Select at least one team:',
        allteams,
        selected = FALSE,
        multiple = TRUE
      ),
      plotOutput("power_rankings", width = "130%"),
      
      reactableOutput("power_rankings_table")
    ),
    
    tabItem(
      tabName = "tab_box",
      dateInput(
        'boxscoredate',
        'Choose a date:'
      ),
      uiOutput('boxscore_input'),
      DTOutput('boxscore_selected')
    ),
    
    tabItem(
      tabName = "tab_playoffs",
      selectizeInput(
        'seasonplayoffs',
        'Choose a Season:',
        c("22-23", "21-22", "20-21")
      ),
      DTOutput("playoff_bracket"),
      DTOutput("playoff_series")
    ),
    
    tabItem(
      tabName = "tab_explore",
      selectizeInput(
        'explore_type',
        'I want to explore:',
        c('The highest', 'The lowest'),
        selected = 'The highest',
        multiple = FALSE
      ),
      
      selectizeInput(
        'explore_var',
        'Values of:',
        c('G', 'M', 'GMSC', 
          'P', 'R', 'A', 'S', 'B', 'TO', 
          'FGM', 'FGA', #'FG_PCT',
          '3PM', '3PA', #'3P_PCT',
          'FTM', 'FTA', #'FT_PCT',
          'OR', 'PF'),
        selected = 'P',
        multiple = FALSE
      ),
      
      selectizeInput(
        'explore_level',
        '',
        c('in a game', 'in a season (total)', 'in a season (avg)', 'in a career (total)', 'in a career (avg)')
      ),
      
      DTOutput('explore_output')
    ),
    
    tabItem(
      tabName = "tab_tm",
      h2("Cap Details"),
      
      currencyInput("tm_cap", "Salary Cap", value = 136021000, format = 'dollar'),
      currencyInput("tm_apron_1", "Apron 1", value = 172346000, format = 'dollar'),
      currencyInput("tm_apron_2", "Apron 2", value = 182794000, format = 'dollar'),
      
      h2("Players"),
      
      p("Provide information for up to 4 teams here. In the player box, format your text like:"),
      p("PLAYER;SALARY;TEAM_TO"),
      p("where SALARY is a number (no dollar sign) and TEAM_TO is where the player is being traded to. One player should be listed on each line."),
      
      fluidRow(
        
        column(3,
               h3("Team 1"),
               selectizeInput(
                 'tm_team_1',
                 'TEAM',
                 c('TEAM', allteams),
                 selected = 'SAS',
                 multiple = FALSE
               ),
               textAreaInput("tm_players_1", "Player;Salary;[team to]", "Doncic;30000000;UTA"),
               currencyInput("tm_guaranteed_1", "Guaranteed Salary", 
                             value = 150000000, format = 'dollar'),
               selectizeInput("tm_capped_1", "Hard Cap",
                              c('None', 'Apron 1', 'Apron 2'))),
        
        column(3,
               h3("Team 2"),
               selectizeInput(
                 'tm_team_2',
                 'TEAM',
                 c('TEAM', allteams),
                 selected = 'UTA',
                 multiple = FALSE
               ),
               textAreaInput("tm_players_2", "Player;Salary;[team to]", "Mitchell;20000000;SAS\nIsaac;10000000;SAS"),
               currencyInput("tm_guaranteed_2", "Guaranteed Salary", 
                             value = 137000000, format = 'dollar'),
               selectizeInput("tm_capped_2", "Hard Cap",
                              c('None', 'Apron 1', 'Apron 2'))),
        
        column(3,
               h3("Team 3"),
               selectizeInput(
                 'tm_team_3',
                 'TEAM',
                 c('TEAM', allteams),
                 multiple = FALSE
               ),
               textAreaInput("tm_players_3", "Player;Salary;[team to]"),
               currencyInput("tm_guaranteed_3", "Guaranteed Salary", 
                             value = 0, format = 'dollar'),
               selectizeInput("tm_capped_3", "Hard Cap",
                              c('None', 'Apron 1', 'Apron 2'))),
        
        column(3,
               h3("Team 4"),
               selectizeInput(
                 'tm_team_4',
                 'TEAM',
                 c('TEAM', allteams),
                 multiple = FALSE
               ),
               textAreaInput("tm_players_4", "Player;Salary;[team to]"),
               currencyInput("tm_guaranteed_4", "Guaranteed Salary", 
                             value = 0, format = 'dollar'),
               selectizeInput("tm_capped_4", "Hard Cap",
                              c('None', 'Apron 1', 'Apron 2'))),
        
      ),
      
      actionButton("tm_calculate", "Check Validity"),
      
      DTOutput('tm_output'),
      
      h2("Notes"),
      
      p("This trade machine is IN BETA. This means it's not quite ready for people to use blindly."),
      
      p("The CBA rules that are implemented in this trade machine are as follows:"),
      p(" 1. If the trade sends a team over their hard cap, it is invalid for that team."),
      p(" 2. If the team's salary is going down or below the cap after the trade, it is valid for that team."),
      p(" 3. If the trade sends a team over the first apron, they must take on 110% or less of their outgoing salary."),
      p(" 4. If a team is under the first apron but over the cap after the trade, the following rules apply:"),
      p("    a. If the outgoing is at most 7.25M, the team can take up to 2*OUTGOING + 250,000."),
      p("    b. If the outgoing is at most 29M, the team can take up to OUTGOING + 7,5000,000."),
      p("    c. Otherwise, the team can take up to 125% of the OUTGOING, plus 250,000."),
      
      p("This trade machine does not consider whether players are eligible for trading.",
        "Please check that yourself!"),
      
      p("In general: always double-check and sanity check the trade machine.")
    ),
    
    tabItem(
      tabName = "tab_player",
      selectizeInput(
        'name',
        'Choose a Player:',
        named_names
      ),
      htmlOutput("headshot"),
      verbatimTextOutput("player_summary"),
      DTOutput("tbl_season"),
      plotlyOutput("gamelog_plot"),
      h2("GAME/CAREER HIGHS"),
      DTOutput("records"),
      h2("ALL-TIME TOTALS AND RANKINGS"),
      DTOutput("rankings"),
      h2("ACHIEVEMENTS (SEASON)"),
      DTOutput("achievements_season"),
      h2("ACHIEVEMENTS (GAME)"),
      DTOutput("achievements_game"),
      h2("GAME LOG"),
      DTOutput("tbl")
    ),
    
    tabItem(
      tabName = "tab_compare",
      selectizeInput(
        'playercomp1',
        'Choose Player 1:',
        named_names
      ),
      selectizeInput(
        'playercomp2',
        'Choose Player 2:',
        named_names
      ),
      selectizeInput(
        'playercomp_season',
        'Choose Season:',
        c('CAREER', 
          '23-24',
          '22-23', '22-23 Playoffs',
          '21-22', '21-22 Playoffs',
          '20-21', '20-21 Playoffs')
      ),
      
      DTOutput('player_compare')
    ),
    
    tabItem(
      tabName = "tab_records",
      
      h2('NBN/Franchise Leaders'),
      selectizeInput(
        'team',
        'Choose a Team:',
        c("NBA", allteams)
      ),
      selectizeInput(
        'season1',
        'Choose a Season:',
        c("ALL-TIME", "23-24", "22-23", "21-22", "20-21")
      ),
      checkboxInput("reg_flag", "Include Regular Season", value = TRUE),
      checkboxInput("playoff_flag", "Include Playoffs", value = FALSE),
      checkboxInput("per_36_flag", "Per 36", value = FALSE),
      DTOutput("franchise_records"),
      
      h2('NBN Hall-of-Fame Points'),
      DTOutput('hof_points')
    ),
    
    tabItem(
      tabName = "tab_franchise",
      selectizeInput(
        'team_history',
        'Choose a Team:',
        allteams
      ),
      htmlOutput("team_history_logo"),
      htmlOutput("franchise_history_rings"),
      htmlOutput("franchise_history_retired"),
      DTOutput("franchise_history_yoy"),
      plotlyOutput("franchise_history_scatter"),
      DTOutput("franchise_history_awards"),
      h2("TEAM LEGENDS"),
      DTOutput("franchise_history_legends"),
      h2("TEAM LEADERS"),
      selectizeInput(
        'stat_cat_team_history',
        'Choose a statistical category:',
        c('G', 'P', 'R', 'A', 'S', 'B', '3PM', 'GMSC')
      ),
      selectizeInput(
        'stattype1',
        'All-Time or Single Game Leaders:',
        c('All-Time', 'Single Game')
      ),
      DTOutput("franchise_history_leaders"),
      h2("CUMULATIVE POINT DIFFERENTIAL"),
      plotOutput("franchise_history_cum_diff")
    ),
    
    tabItem(
      tabName = "tab_ws",
      DTOutput("ws_prices"),
      
      h2("Price Charts"),
      selectizeInput(
        'ws_teams',
        'Select at least one team:',
        allteams,
        selected = 'SAS',
        multiple = TRUE
      ),
      dateInput(
        'ws_date_min',
        'Choose a minimum date:',
        value = '2019-01-01'
      ),
      dateInput(
        'ws_date_max',
        'Choose a maximum date:'
      ),
      plotlyOutput("wallstreet"),
      plotlyOutput("ws_div")
    ),
    
    tabItem(
      tabName = "tab_about",
      h2("Why Premium?"),
      htmlOutput("premiumfaq"),
      h2("Changelog"),
      htmlOutput("changelog")
    ),
    
    tabItem(
      tabName = "tab_diag",
      verbatimTextOutput("diag")
    )
  )
)

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(
    title = h5(HTML(glue(
      "{prettyNum(18*(nrow(dfs)+nrow(dfs_playoffs)), big.mark=',')} stats entered and counting<br/>",
      "Last updated: {max(max(dfs$DATE), max(dfs_playoffs$DATE))}"
      ))),
    titleWidth = 250
  ),
  
  sidebar,
  body,
  title = "Nothing But Stats!",
  skin = "yellow"
)