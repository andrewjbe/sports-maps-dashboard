library(cfbfastR)
library(tidyverse)

source("./scripts/get-betting-record.R")


data_ <- cfbd_ratings_sp(year = 2021) |> 
  select(year, team, conference, rating, ranking, offense_ranking, offense_rating, defense_rating, defense_ranking)

standings <- cfbd_game_records(year = 2021)

betting <- get_betting_scorecard(exclude_fcs = TRUE)

data <- data_ |>
  filter(team != "nationalAverages") |>
  left_join(standings |> 
              select(team, total_games, total_wins, total_losses, total_ties)) |>
  left_join(betting |>
              select(team, n_upsets_allowed, n_upsets_achieved, avg_perf_v_spread, n_games)) |> 
  mutate(
    diff = offense_rating - defense_rating,
    win_pct = total_wins / total_games,
    index = (130 - offense_ranking) + (130 - defense_ranking) - 26 * total_losses
  )  |>
  arrange(desc(index))
