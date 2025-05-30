# Load ----
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

# Dashboard Sidebar ----

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      "Season Dashboard",
       tabName = "tab_dash", 
       icon = icon("dashboard")
    ),
    menuItem(
      "Playoff Archive",
      tabName = "tab_playoffs", 
      icon = icon("code-fork")
    ),
    menuItem(
      "Hall of Fame & League History",
      tabName = "tab_awards",
      icon = icon("award")
    ),
    menuItem(
      "League Stats & Records",
      tabName = "tab_records", 
      icon = icon("ranking-star")
      # badgeLabel = "Prem",
      # badgeColor = "yellow"
    ),
    menuItem(
      "Player Profiles",
      tabName = "tab_player", 
      icon = icon("user")
      # badgeLabel = "Prem",
      # badgeColor = "yellow"
    ),
    menuItem(
      "Franchise Profiles",
      tabName = "tab_franchise",
      icon = icon("book")
      # badgeLabel = "Prem",
      # badgeColor = "yellow"
    ),
    menuItem(
      "Power Rankings",
      tabName = "tab_prs", 
      icon = icon("arrow-trend-up")
    ),
    menuItem(
      "Frivolities",
      tabName = "tab_frivolities",
      icon = icon("face-laugh")
    ),
    menuItem(
      "Box Scores",
      tabName = "tab_box", 
      icon = icon("table")
    ),
    # menuItem(
    #   "The Lab (TM)",
    #   tabName = "tab_lab",
    #   icon = icon("microscope")
    # ),
    # menuItem(
    #   "Data Explorer",
    #   tabName = "tab_explore", 
    #   icon = icon("magnifying-glass-chart")
    # ),
    menuItem(
      "Player Compare",
      tabName = "tab_compare", 
      icon = icon("user-group")
      # badgeLabel = "Prem",
      # badgeColor = "yellow"
    ),
    menuItem(
      "Trade Machine",
      tabName = "tab_tm", 
      icon = icon("trademark")
    ),
    menuItem(
      "NBN Trivia!",
      tabName = "tab_trivia",
      icon = icon("puzzle-piece")
    ),
    menuItem(
      "NBN Wall Street",
      tabName = "tab_ws", 
      icon = icon("dollar-sign")
    )
  )
)

# Dashboard Body ----
body <- dashboardBody(
  
  # css stuff
  tags$style(
    type = 'text/css',
    '.modal-dialog { width: fit-content !important; }'
  ),
  
  tags$style(HTML("
    .plot-container {
      width: 100%;
      height: 100%;
      padding-bottom: 100%; /* Aspect ratio trick */
      position: relative;
    }

    .plot-container .plotly {
      position: absolute;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
    }
  ")),
  
  tabItems(
    ## Season Dashboard ----
    tabItem(
      tabName = "tab_dash",
      selectizeInput(
        'season2',
        'Choose a Season:',
        c("24-25", "23-24", "22-23", "21-22", "20-21")
      ),

      h2("AROUND THE NBN"),
      DTOutput("newsfeed"),

      h2("REGULAR SEASON STANDINGS"),
      DTOutput("standings"),

      h2("LEAGUE LEADERS"),
      DTOutput("leaders"),

      h2("TEAM STATS"),
      DTOutput("team_stats"),
      
      h2("ROOKIE REPORT"),
      p("Note: 'Rookie' indicates only that this is the player's first season in the NBN. The below list may contain the players who have played in the NBA, but not in the NBN."),
      DTOutput("rookie_report"),
      
      h2("MOST IMPROVED"),
      DTOutput("most_improved"),

      h2("DRAFT LOTTERY PREVIEW"),
      DTOutput("tankathon"),

      h2("GAME LOG"),
      DTOutput("gamelog"),
      
      h2("SEASON AWARDS"),
      h3("All-Stars"),
      DTOutput("season_allstars"),
      
      h3("All-NBN Teams"),
      DTOutput("season_allnbn")
    ),
    
    ## Trivia ----
    tabItem(
      tabName = "tab_trivia",
      verbatimTextOutput("trivia_question"),
      uiOutput("trivia_answer"),
      actionButton("trivia_submit", "Submit Answer"),
      textOutput("trivia_streak"),
      textOutput("trivia_result"),
      actionButton("trivia_restart", "Start Over")#, style = "display:none;")#,
      #tableOutput("trivia_leaderboard")
    ),
    
    # ## NBYen ----
    # tabItem(
    #   tabName = "tab_nbyen",
    #   DTOutput("nbyen_table"),
    #   plotlyOutput("nbyen_plot")
    # ),
    
    ## Playoff Archive ----
    tabItem(
      tabName = "tab_playoffs",
      selectizeInput(
        'seasonplayoffs',
        'Choose a Season:',
        c("24-25", "23-24", "22-23", "21-22", "20-21")
      ),
      DTOutput("playoff_bracket"),
      DTOutput("playoff_series")
    ),
    
    ## Player Profiles ----
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
      # selectizeInput('plot_season_var', "Pick a variable:", 
      #                c('GMSC', 'PPG', 'RPG', 'APG', 'SPG', 'BPG', 'FG', '3P', 'FT', 'TS')),
      # checkboxInput("plot_season_36", "Per 36", value = FALSE),
      # plotlyOutput("plot_season"),
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
    
    ## Franchise Profiles ----
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
      div(class = "plot-container", plotlyOutput("franchise_history_scatter", height = "100%")),
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
    
    ## League Stats ----
    tabItem(
      tabName = "tab_records",
      
      h2('Career Totals'),
      selectizeInput(
        'team',
        'Choose a Team:',
        c("NBA", allteams)
      ),
      selectizeInput(
        'season1',
        'Choose a Season:',
        c("ALL-TIME", "24-25", "23-24", "22-23", "21-22", "20-21")
      ),
      checkboxInput("reg_flag", "Include Regular Season", value = TRUE),
      checkboxInput("playoff_flag", "Include Playoffs", value = FALSE),
      checkboxInput("per_36_flag", "Per 36", value = FALSE),
      DTOutput("franchise_records"),
      
      h2('Game Highs'),
      p("Includes any game in which a player recorded at least 5 points, rebounds, assists, steals, or blocks."),
      DTOutput("game_high_player"),
      
      h2('Season Highs'),
      DTOutput("season_high_player"),
      
      h2('Team Game Highs'),
      DTOutput("game_high_team"),
      
      h2('Team Season Highs'),
      DTOutput("season_high_team"),
      
      h2('Team Season Offensive/Defensive/Overall Ratings'),
      p("Offensive rating == 'How many more points do they score than the opponent typically allows?'"),
      p("Defensive rating == 'How many fewer points do they allow than the opponent typically scores?'"),
      p("Overall rating == Offensive rating + Defensive rating"),
      DTOutput("team_ratings"),
      
      h2('(Regular Season) Stat Race'),
      selectizeInput(
        'race_var',
        'Select a stat to compare:',
        c('M', 'P', 'R', 'A', 'S', 'B', '3PM'),
        selected = 'P'
      ),
      selectizeInput(
        'race_players',
        'Select at least two players:',
        named_names,
        selected = c("BEAL, BRADLEY", "CURRY, STEPHEN"),
        multiple = TRUE
      ),
      selectizeInput(
        'race_season',
        'Choose a Season:',
        c("ALL-TIME", "24-25", "23-24", "22-23", "21-22", "20-21")
      ),
      plotOutput("stat_race_plot")
    ),
    
    ## Hall of Fame + Awards ----
    tabItem(
      tabName = "tab_awards",
      
      h2('NBN Hall-of-Fame Points'),
      p("HOF points are calculated using GMSC, wins, and playoff performance."),
      DTOutput('hof_points'),
      p("Players with at least 100 HOF points:"),
      plotlyOutput('hof_plot_bar'),
      
      h2('League Champions'),
      DTOutput('league_champs'),
      
      h2('Season Awards History'),
      DTOutput('season_awards_history'),
      
      h2('Front Office Awards'),
      DTOutput('front_office_awards'),
      
      h2('All-NBN Teams'),
      DTOutput('all_nbn')
    ),
    
    ## Power Rankings ----
    tabItem(
      tabName = "tab_prs",
      selectizeInput(
        'pr_season',
        'Choose a Season',
        c("24-25", "23-24", "22-23", "21-22", "20-21")
      ),
      # selectizeInput(
      #   'pr_teams',
      #   'Select at least one team:',
      #   allteams,
      #   selected = FALSE,
      #   multiple = TRUE
      # ),
      # plotOutput("power_rankings", width = "130%"),
      
      reactableOutput("power_rankings_table")
    ),
    
    ## Frivolities ----
    tabItem(
      tabName = "tab_frivolities",
      h2("Roster Stability"),
      p("Values represent how many of the minutes played in season N went to players who were on the team in season N-1."),
      plotlyOutput("stability"),
      h2("Who They Played For"),
      DTOutput("most_teams")
    ),
    
    ## Box Scores ----
    tabItem(
      tabName = "tab_box",
      dateInput(
        'boxscoredate',
        'Choose a date:'
      ),
      uiOutput('boxscore_input'),
      DTOutput('boxscore_selected')
    ),
    
    ## Player Compare ----
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
          '24-25',
          '23-24', '23-24 Playoffs',
          '22-23', '22-23 Playoffs',
          '21-22', '21-22 Playoffs',
          '20-21', '20-21 Playoffs')
      ),
      
      DTOutput('player_compare')
    ),
    
    ## Trade Machine ----
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
    
    ## NBN Wall Street ----
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
    
    ## Currently Unused Pages ----
    
    ### Data Explorer (Unserved) ----
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
    
    ### The Lab ----
    tabItem(
      tabName = "tab_lab",
      
      h2("Team Stats By Season"),
      selectizeInput(
        'lab_team_season_var',
        'Choose a var:',
        c('GMSC', 'PPG', 'RPG', 'APG', 'SPG', 'BPG', 'FG', '3P', 'FT', 'TS')
      ),
      plotlyOutput("lab_team_season"),
      
      h2("Individual Stats By Season"),
      selectizeInput(
        'lab_player_season_var',
        'Choose a var:',
        c('GMSC', 'PPG', 'RPG', 'APG', 'SPG', 'BPG', 'FG', '3P', 'FT', 'TS')
      ),
      plotlyOutput("lab_player_season")
    ),
    
    ### About ----
    tabItem(
      tabName = "tab_about",
      h2("Why Premium?"),
      htmlOutput("premiumfaq")
    )
  )
)

# Dashboard Page ----
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
