sidebar <- dashboardSidebar(
  passwordInput("password", "PREMIUM Password:"),
  sidebarMenu(id = "tabs",
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
      icon = icon("ranking-star"),
      badgeLabel = "Prem",
      badgeColor = "yellow"
    ),
    menuItem(
      "Player Profiles",
      tabName = "tab_player", 
      icon = icon("user"),
      badgeLabel = "Prem",
      badgeColor = "yellow"
    ),
    menuItem(
      "Franchise Profiles",
      tabName = "tab_franchise",
      icon = icon("book"),
      badgeLabel = "Prem",
      badgeColor = "yellow"
    ),
    menuItem(
      "Franchise H2H",
      tabName = "tab_head_to_head",
      icon = icon("handshake")
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
    menuItem(
      "Player Compare",
      tabName = "tab_compare", 
      icon = icon("user-group"),
      badgeLabel = "Prem",
      badgeColor = "yellow"
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
    ),
    menuItem(
      "Owner Stats",
      tabName = "tab_owner_stats",
      icon = icon("users")
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
    
    .custom-banner {
      background-color: #a3bcd6;
      color: #2c3e50;
      padding: 15px;
      font-size: 16px;
      cursor: pointer;
      border-radius: 4px;
      margin-bottom: 10px;
      transition: background-color 0.3s;
    }
    
    .custom-banner:hover {
      background-color: #92adc8;
    }
    
    .custom-banner-content {
      display: none;
      background-color: #f7f9fb;
      color: #2c3e50;
      padding: 12px;
      border-left: 4px solid #6b8ca4;
      border-radius: 0 0 4px 4px;
      margin-top: -8px;
      margin-bottom: 10px;
    }
  
    .custom-banner-content.show {
      display: block;
      animation: fadeIn 0.3s ease-in-out;
    }
  
    @keyframes fadeIn {
      from { opacity: 0; }
      to { opacity: 1; }
    }
  ")),
  
  tags$script(HTML("
    $(document).on('shiny:connected', function() {
      $('#custom-banner').on('click', function() {
        $('#custom-banner-content').toggleClass('show');
      });
    });
  ")),
  
  div(id = "custom-banner", class = "custom-banner", "The 2025-26 NBN Season is dedicated to the memory of KyleWTF <3"),
  div(
    id = "custom-banner-content", 
    class = "custom-banner-content", 
    HTML("
      <p>KyleWTF was a longtime owner and member of the Board of Directors in the NBN. His passionate involvement in the league spans back to 2016.</p>
      
      <p>Kyle led the Oklahoma City Thunder through four season in the post-restart NBN, and played a critical role in establishing the league's security practices and laying down a foundation that will help the league thrive far into the future.</p>
      
      <p>Through the 2025-26 season and beyond, the NBN will remember Kyle, his love for the league and its community, and the impact he had on the NBN and all of the individuals that make up our community.</p>
      
      <p>RIP Kyle, and go Thunder!</p>
    ")
  ),
  
  tabItems(
    ## Season Dashboard ----
    tabItem(
      tabName = "tab_dash",
      selectizeInput(
        'season2',
        'Choose a Season:',
        c("25-26", "24-25", "23-24", "22-23", "21-22", "20-21")
      ),

      h2("AROUND THE NBN"),
      DTOutput("newsfeed"),

      h2("REGULAR SEASON STANDINGS"),
      p("Click a team to show season roster and stats."),
      DTOutput("standings"),

      h2("LEAGUE LEADERS"),
      p("Click a name to see player profile."),
      DTOutput("leaders"),

      h2("TEAM STATS"),
      DTOutput("team_stats"),
      
      h2("ROOKIE REPORT"),
      p("Note: 'Rookie' indicates only that this is the player's first season in the NBN. The below list may contain the players who have played in the NBA, but not in the NBN."),
      DTOutput("rookie_report"),
      
      h2("MOST IMPROVED"),
      p("Biggest GMSC increases this season compared to career average GMSC prior to this season."),
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

    ## Trade Machine ----
    tabItem(
      tabName = "tab_tm",
      h2("Trade Machine"),

      fluidRow(
        column(4, numericInput("tm_salary_cap", "Salary Cap ($)", value = 136021000, min = 0, step = 100000)),
        column(4, numericInput("tm_apron1", "First Apron ($)", value = 172346000, min = 0, step = 100000)),
        column(4, numericInput("tm_apron2", "Second Apron ($)", value = 182794000, min = 0, step = 100000))
      ),

      fluidRow(
        column(12, selectInput("tm_num_teams", "Number of teams", choices = 2:4, selected = 2))
      ),

      uiOutput("tm_team_panels"),

      actionButton("tm_validate", "Validate Trade"),

      uiOutput("tm_results")
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
      # h2("ACHIEVEMENTS (SEASON)"),
      # DTOutput("achievements_season"),
      # h2("ACHIEVEMENTS (GAME)"),
      # DTOutput("achievements_game"),
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
        c("ALL-TIME", "25-26", "24-25", "23-24", "22-23", "21-22", "20-21")
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
      
      h2('Team Win/Loss Streaks'),
      p("Includes streaks of 10 or more games."),
      DTOutput("wl_streaks"),
      
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
        selected = c("BEAL, BRADLEY", "DONCIC, LUKA"),
        multiple = TRUE
      ),
      selectizeInput(
        'race_season',
        'Choose a Season:',
        c("ALL-TIME", "25-26", "24-25", "23-24", "22-23", "21-22", "20-21")
      ),
      plotOutput("stat_race_plot")
    ),
    
    ## Hall of Fame + Awards ----
    tabItem(
      tabName = "tab_awards",
      
      h2('League Champions'),
      DTOutput('league_champs'),
      
      h2('Season Awards History'),
      DTOutput('season_awards_history'),
      
      h2('Front Office Awards'),
      DTOutput('front_office_awards'),
      
      h2('All-NBN Teams'),
      DTOutput('all_nbn'),
      
      h2('NBN Hall-of-Fame Points'),
      p("HOF points are calculated using GMSC, wins, and playoff performance."),
      DTOutput('hof_points')
    ),
    
    ## Head to Head ----
    tabItem(
      tabName = "tab_head_to_head",
      h2("Head to Head"),
      fluidRow(
        column(6,
               selectizeInput(
                 'h2h_team1',
                 'Team 1:',
                 allteams,
                 selected = NULL,
                 multiple = FALSE
               )
        ),
        column(6,
               selectizeInput(
                 'h2h_team2',
                 'Team 2:',
                 allteams,
                 selected = NULL,
                 multiple = FALSE
               )
        )
      ),
      h3("All-Time Record"),
      DTOutput("h2h_alltime"),
      h3("Playoff Series Record"),
      DTOutput("h2h_playoff_series"),
      h3("Playoff Series Details"),
      uiOutput("h2h_playoff_details")
    ),
    
    ## Power Rankings ----
    tabItem(
      tabName = "tab_prs",
      selectizeInput(
        'pr_season',
        'Choose a Season',
        c("25-26", "24-25", "23-24", "22-23", "21-22", "20-21")
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
      DTOutput("most_teams"),
      h2("Playoff Risers and Sinkers"),
      p("Career differences in GMSC per minute between regular season and postseason."),
      p("Only includes: games where the player logged at least 5 minutes; players who have been the playoffs at least 3 times; and seasons in which the player made the postseason."),
      DTOutput("playoff_risers")
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
          '25-26',
          '24-25', '24-25 Playoffs', 
          '23-24', '23-24 Playoffs',
          '22-23', '22-23 Playoffs',
          '21-22', '21-22 Playoffs',
          '20-21', '20-21 Playoffs')
      ),
      
      DTOutput('player_compare')
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
    
    ## Owner Stats ----
    tabItem(
      tabName = "tab_owner_stats",
      h2("Owner Statistics"),
      p("Win-loss records for all team owners based on games played during their ownership periods."),
      DTOutput("owner_stats")
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
