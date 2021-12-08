library(tidyverse)


ds_teams <- read_csv("misc/data-school-appearances.csv")
ds_matchups <- read_csv("misc/data-matchup-appearances.csv")
ds_celebs <- read_csv("misc/data-celebs.csv")

# Best Win % on CFB Gameday
ds_teams |> 
  filter(Appearances > 1) |>
  arrange(desc(`Win Pct`))

# Most apperances
ds_teams |>
  arrange(desc(Appearances))

# Worst win %
ds_teams |>
  filter(Appearances > 1) |>
  arrange(`Win Pct`)

# Celebs ------------

ds_celebs |>
  separate(col = Record, into = c("correct_picks", "incorrect_picks"), sep = "–", convert = TRUE) |>
  filter(correct_picks + incorrect_picks >= 10) |>
  arrange(desc(`Win Pct`)) 

