# Server ----
# Main server function - sources all modules

function(input, output, session) {
  
  # Source initialization (reactives, observers, helpers)
  source("server_modules/_init.R", local = TRUE)
  
  # Source all feature modules
  source("server_modules/season_dashboard.R", local = TRUE)
  source("server_modules/playoff_archive.R", local = TRUE)
  source("server_modules/player_profiles.R", local = TRUE)
  source("server_modules/franchise_profiles.R", local = TRUE)
  source("server_modules/league_stats.R", local = TRUE)
  source("server_modules/hall_of_fame.R", local = TRUE)
  source("server_modules/power_rankings.R", local = TRUE)
  source("server_modules/frivolities.R", local = TRUE)
  source("server_modules/box_scores.R", local = TRUE)
  source("server_modules/player_compare.R", local = TRUE)
  source("server_modules/trivia.R", local = TRUE)
  source("server_modules/nbyen.R", local = TRUE)
  source("server_modules/nbn_wall_street.R", local = TRUE)
  source("server_modules/owner_stats.R", local = TRUE)
  source("server_modules/head_to_head.R", local = TRUE)
  source("server_modules/trade_machine.R", local = TRUE)

}
