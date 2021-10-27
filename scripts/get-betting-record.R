library(cfbfastR)
library(dplyr)

get_betting_scorecard <- function(season = 2021, exclude_fcs = FALSE){
  
  if(season < 2013 | season > 2021) {
    print("Please enter a valid year later than 2013.")
    stop()
    }
  
  # Grabbing data from API
  ds_betting_ <- cfbd_betting_lines(year = season)
  
  # Formatting / cleaning data
  ds_betting <- ds_betting_ |>
    filter(provider == "consensus") |>
    mutate(
      home_conference = if_else(is.na(home_conference), "FCS", home_conference),
      away_conference = if_else(is.na(away_conference), "FCS", away_conference),
      spread = as.numeric(spread),
      #
      favorite = if_else(spread < 0, home_team, away_team),
      underdog = if_else(spread > 0, home_team, away_team),
      expected_mov = abs(spread),
      actual_mov = if_else(home_team == favorite, home_score - away_score, away_score - home_score),
      perf_v_spread = actual_mov - expected_mov,
      #
      expected_over_under = over_under,
      actual_over_under = home_score + away_score
    ) |>
    select(-c(spread_open, over_under, over_under_open, home_moneyline, away_moneyline)) |>
    arrange(week) |>
    filter(!is.na(home_score),
           !is.na(away_score)
    )
  
  # Removes games with FCS participants if requested
  if(exclude_fcs){
    ds_betting <- ds_betting |>
      filter(
        home_conference != "FCS",
        away_conference != "FCS"
      )
  }
  
  favorites <- ds_betting |>
    select(c(game_id, favorite, expected_mov, actual_mov, perf_v_spread, expected_over_under, actual_over_under)) |>
    rename(team = favorite)
  
  underdogs <- ds_betting |>
    select(c(game_id, underdog, expected_mov, actual_mov, perf_v_spread, expected_over_under, actual_over_under)) |>
    mutate(
      expected_mov = -expected_mov,
      actual_mov = -actual_mov,
      perf_v_spread = -perf_v_spread
    ) |>
    rename(team = underdog)
  
  combined <- rbind(favorites, underdogs)
  
  ds_summary <- combined |>
    mutate(
      covered = case_when(
        actual_mov > 0 & actual_mov >= expected_mov ~ TRUE, 
        actual_mov < 0 & actual_mov >= expected_mov ~ TRUE, 
        TRUE ~ FALSE
      ),
      upset_allowed = if_else(expected_mov > 0 & actual_mov < 0, TRUE, FALSE),
      upset_achieved = if_else(expected_mov < 0 & actual_mov > 0, TRUE, FALSE)
    ) |>
    group_by(team) |>
    summarize(
      n_games = n(),
      n_games_covered = sum(covered, na.rm = T),
      p_games_covered = (n_games_covered / n_games),
      n_upsets_allowed = sum(upset_allowed, na.rm = T),
      n_upsets_achieved = sum(upset_achieved, na.rm = T),
      avg_expected_mov = mean(expected_mov, na.rm = T) |> round(2),
      avg_actual_mov = mean(actual_mov, na.rm = T) |> round(2),
      n_games_hitting_over = sum(actual_over_under > expected_over_under, na.rm = T),
      p_games_hitting_over = n_games_hitting_over / sum(!is.na(expected_over_under)),
      n_games_hitting_under = sum(actual_over_under < expected_over_under, na.rm = T),
      p_games_hitting_under = n_games_hitting_under / sum(!is.na(expected_over_under)),
      #
      avg_perf_v_spread = mean(perf_v_spread, na.rm = T) |> round(2)
    ) |> 
    filter(n_games > 1)
  
  # Getting logos, team colors
  team_info <- cfbd_team_info(year = 2021) |>
    select(school, color, logos, conference, team_id) |>
    unnest(cols = c(logos)) |>
    distinct(team_id, .keep_all = TRUE) |>
    rename(team = school)
  
  # Folding logos, team colors, etc. into betting dataset
  summary <- left_join(ds_summary, team_info, by = "team") |>
    filter(!is.na(team_id))
  
  return(summary)
  
}

get_betting_scorecard()

