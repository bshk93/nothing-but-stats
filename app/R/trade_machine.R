# Trade Machine Validation Logic ----
# Financial environment and legality checks for NBA-style trades

# 1. Define the Financial Environment (Update these annually)
# Values for 2026-27 Season Projections
nba_config <- list(
  salary_cap = 136021000,
  apron1     = 172346000,
  apron2     = 182794000
)

# 2. The Legality Function
is_trade_legal <- function(outgoing_sal, incoming_sal, current_guaranteed,
                           hard_cap, config = nba_config) {

  # Calculate salary after the trade
  post_trade_salary <- current_guaranteed - sum(outgoing_sal, na.rm = TRUE) + sum(incoming_sal, na.rm = TRUE)

  if (hard_cap == "Second Apron") {
    if (post_trade_salary > config$apron2) {
        return("FAIL: Team is hard capped at the second apron.")
    }
  }

  if (hard_cap == "First Apron") {
    if (post_trade_salary > config$apron1) {
        return("FAIL: Team is hard capped at the first apron.")
    }
  }
  # DETERMINE STATUS BASED ON POST-TRADE SALARY

  out_sum <- sum(outgoing_sal, na.rm = TRUE)
  in_sum  <- sum(incoming_sal, na.rm = TRUE)

  # TIER 1: SECOND APRON TEAM
  if (post_trade_salary > config$apron2) {
    if (sum(!is.na(outgoing_sal)) > 1) {
      return("FAIL: Second Apron teams cannot combine multiple player salaries (aggregation) to match a larger incoming salary.")
    }
    if (in_sum > out_sum) {
      return("FAIL: Second Apron teams cannot take back more salary than they send out (must be <= 100%).")
    }
    return("PASS")
  }

  # TIER 2: FIRST APRON TEAM
  else if (post_trade_salary > config$apron1) {
    if (in_sum > out_sum) {
      return("FAIL: First Apron teams cannot take back more salary than they send out (must be <= 100%).")
    }
    return("PASS")
  }

  # TIER 3: NON-APRON TEAM (Standard Matching Brackets)
  else {
    if (out_sum <= 7250000) {
      max_incoming <- (out_sum * 2.00) + 250000
    } else if (out_sum <= 29000000) {
      max_incoming <- out_sum + 7500000
    } else {
      max_incoming <- (out_sum * 1.25) + 250000
    }

    if (in_sum > max_incoming) {
      return(paste("FAIL: Salary matching exceeded. Max allowed is", max_incoming))
    }
    return("PASS")
  }
}
