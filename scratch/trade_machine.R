# 1. Define the Financial Environment (Update these annually)
# Values for 2026-27 Season Projections
nba_config <- list(
  salary_cap = 136021000,
  apron1     = 172346000,
  apron2     = 182794000
)

# 2. The Legality Function
is_trade_legal <- function(outgoing_sal, incoming_sal, current_guaranteed, 
                           config = nba_config) {
  
  # Calculate salary after the trade
  post_trade_salary <- current_guaranteed - sum(outgoing_sal, na.rm = TRUE) + sum(incoming_sal, na.rm = TRUE)
  
  # DETERMINE STATUS BASED ON POST-TRADE SALARY
  
  # TIER 1: SECOND APRON TEAM
  if (post_trade_salary > config$apron2) {
    if (length(outgoing_sal) > 1) {
      return("FAIL: Second Apron teams cannot combine multiple player salaries (aggregation) to match a larger incoming salary.")
    }
    if (incoming_sal > outgoing_sal) {
      return("FAIL: Second Apron teams cannot take back more salary than they send out (must be <= 100%).")
    }
    return("PASS")
  }
  
  # TIER 2: FIRST APRON TEAM
  else if (post_trade_salary > config$apron1) {
    if (incoming_sal > outgoing_sal) {
      return("FAIL: First Apron teams cannot take back more salary than they send out (must be <= 100%).")
    }
    return("PASS")
  }
  
  # TIER 3: NON-APRON TEAM (Standard Matching Brackets)
  else {
    if (outgoing_sal <= 7250000) {
      max_incoming <- (outgoing_sal * 2.00) + 250000
    } else if (outgoing_sal <= 29000000) {
      max_incoming <- outgoing_sal + 7500000
    } else {
      max_incoming <- (outgoing_sal * 1.25) + 250000
    }
    
    if (incoming_sal > max_incoming) {
      return(paste("FAIL: Salary matching exceeded. Max allowed is", max_incoming))
    }
    return("PASS")
  }
}