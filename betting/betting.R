library(cfbfastR)
library(ggthemes)
library(tidyverse)
library(lubridate)
library(ggimage)

# Collecting data
# ds_betting_ <- cfbd_betting_lines(year = 2021) 
  
source("get-betting-record.R")


ds_betting_ <- tibble()

years <- c(2021:2021) 

for(i in years){
  
  temp <- cfbd_betting_lines(year = i)
  
  ds_betting_ <- rbind(ds_betting_, temp)
  
  rm(temp)
  print(paste("Retrieved", i, "data"))
  
  }

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
         !is.na(away_score),
         # home_conference != "FCS",
         # away_conference != "FCS",
         )

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

# summary |>
#   filter(grepl("ACC|Big 12|Big Ten|FBS Independents|Pac-12|SEC", conference)) |>
# ggplot(aes(x = avg_expected_mov, y = avg_actual_mov)) +
#   geom_image(aes(image = logos), size = 0.1) +
#   geom_vline(xintercept = 0) +
#   geom_hline(yintercept = 0) +
#   geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
#   labs(
#     title = "FBS Team Performance vs. Betting Lines",
#     subtitle = paste("2021-2022 Season, through", today()),
#     x = "Avg. Expected Margin of Victory",
#     y = "Avg. Actual Margin of Victory"
#   ) +
#   lims(y = c(-30, 30),
#        x = c(-30, 30)) +
# #  facet_wrap(~conference, nrow = 2, ncol = 3) +
#   theme_gdocs() 
#    

p_avg_perf <- summary |>
  filter(grepl("ACC|Big 12|Big Ten|FBS Independents|Pac-12|SEC", conference)) |>
  ggplot(aes(x = reorder(team, avg_perf_v_spread), y = avg_perf_v_spread)) +
  geom_rect(inherit.aes = F, xmin = 0, xmax = 150, ymin = min(summary$avg_perf_v_spread), ymax = 0,
            alpha = 0.009, fill = "#FF3131") +
  geom_rect(inherit.aes = F, xmin = 0, xmax = 150, ymin = 0, ymax = max(summary$avg_perf_v_spread),
            alpha = 0.009, fill = "green") +
  geom_col(aes(fill = team)) +
  annotate(geom = "text", x = 40, y = max(summary$avg_perf_v_spread) / 2, label = "Outperforms Expectations\n(on Average)") +
  annotate(geom = "text", x = 40, y = -max(summary$avg_perf_v_spread) / 2, label = "Underperforms Expectations\n(on Average)") +
  scale_fill_manual(values = summary$color, breaks = summary$team) +
  guides(fill = "none") +
  coord_flip() +
  theme_clean() +
  labs(
    title = "Average Performance vs. Spread",
    subtitle = paste0("All P5 / Independent FBS teams, ", min(years), "-", max(years)),
    y = "Average (Actual Margin of Victory - Expected Margin of Victory)",
    x = ""
  )
p_avg_perf


p_covers <- summary |>
  filter(grepl("ACC|Big 12|Big Ten|FBS Independents|Pac-12|SEC", conference)) |>
  ggplot(aes(x = reorder(team, p_games_covered), y = (p_games_covered), fill = team)) +
  geom_col() +
  geom_hline(yintercept = 0.5, size = 2, linetype = "dashed", color = "red") +
  coord_flip(ylim = c(0, NA)) +
  scale_fill_manual(values = summary$color, breaks = summary$team) +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = "none") +
  theme_clean() +
  labs(
    title = "Percent of Games Covering the Spread",
    subtitle = paste0("All P5 / Independent FBS teams, ", min(years), "-", max(years)),
    y = "% of Spreads Covered",
    x = "",
    caption = "'Covering the spread' is understood to mean an underdog 
    1) winning or 2) losing by less than predicted,
    or a favorite winning by more than predicted."
  )
p_covers


