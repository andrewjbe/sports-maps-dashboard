library(cfbfastR)
library(tidyverse)
library(ggimage)
library(ggthemes)

ds <- cfbd_drives(year = 2021)
ds_teams <- cfbd_team_info(year = 2021) %>%
  unnest(cols = c(logos)) %>%
  distinct(team_id, .keep_all = TRUE)

ds <- ds %>%
  mutate(
    time_elapsed = (time_minutes_elapsed) * 60 + time_seconds_elapsed,
    start_clock = paste0("Q", start_period, " ", time_minutes_start, ":", time_seconds_start),
    end_clock = paste0("Q", end_period, " ", time_minutes_end, ":", time_seconds_end)
  )

ds %>%
  select(offense, defense, yards, drive_result, time_elapsed, plays, start_clock, end_clock) %>%
  slice_max(order_by = yards, n = 25) %>%
  rename(Offense = offense, Defense = defense, Yards = yards, `Drive Result` = drive_result, `Time Elapsed` = time_elapsed,
         Plays = plays, `Time at Start` = start_clock, `Time at End` = end_clock) %>%
  write_csv(., "Top 25 CFB Drives (Yards) - Week 1.csv")

ds %>%
  select(offense, defense, yards, drive_result, time_elapsed, plays, start_clock, end_clock) %>%
  slice_max(order_by = time_elapsed, n = 25) %>%
  rename(Offense = offense, Defense = defense, Yards = yards, `Drive Result` = drive_result, `Time Elapsed` = time_elapsed,
         Plays = plays, `Time at Start` = start_clock, `Time at End` = end_clock) %>%
  write_csv(., "Top 25 CFB Drives (Time) - Week 1.csv")

ds %>%
  select(offense, defense, yards, drive_result, time_elapsed, plays, start_clock, end_clock) %>%
  slice_max(order_by = plays, n = 25) %>%
  rename(Offense = offense, Defense = defense, Yards = yards, `Drive Result` = drive_result, `Time Elapsed` = time_elapsed,
         Plays = plays, `Time at Start` = start_clock, `Time at End` = end_clock) %>%
  write_csv(., "Top 25 CFB Drives (Plays) - Week 1.csv")

top10_drives <- ds %>%
  left_join(., ds_teams %>% select(school, color, logos), by = c("offense" = "school")) %>% 
  rename(off_logos = logos) %>%
  left_join(., ds_teams %>% select(school, logos), by = c("defense" = "school")) %>% 
  rename(def_logos = logos) %>%
  slice_max(order_by = yards, n = 10) %>%
  mutate(
    off_logos = case_when(
      offense == "Holy Cross" ~ 
        "https://a.espncdn.com/i/teamlogos/ncaa/500/107.png",
      offense == "Western Illinois" ~ 
        "https://a.espncdn.com/i/teamlogos/ncaa/500/2710.png",
      TRUE ~ off_logos
    ),
    color = case_when(
      offense == "Holy Cross" ~ "#602D89",
      offense == "Western Illinois" ~ "	#663399",
      TRUE ~ color
    ),
    def_logos = case_when(
      defense == "Jacksonville State" ~ 
        "https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/55.png",
      defense == "Northwestern State" ~ 
        "https://a.espncdn.com/i/teamlogos/ncaa/500/2466.png",
      defense == "Wagner" ~ 
        "https://a.espncdn.com/i/teamlogos/ncaa/500/2681.png",
      TRUE ~ def_logos
    )
  ) %>%
  mutate(
    color = case_when(
      offense == "Holy Cross" ~ "#602D89",
      offense == "Western Illinois" ~ "#663399",
      TRUE ~ color
    )
  )

ggplot(top10_drives, aes(x = reorder(drive_id, yards), y = yards, fill = offense)) +
  geom_col() +
  geom_label(aes(label = paste(yards, "yards")), fill = "white", nudge_y = -2, alpha = 0.8, label.size = NA,
             family = "Windows Command Prompt") +
  geom_label(aes(label = paste(plays, "plays")), fill = "white", nudge_y = -4, alpha = 0.8, label.size = NA,
             family = "Windows Command Prompt") +
  geom_image(aes(image = off_logos, y = yards + 0.8)) +
  geom_image(aes(image = def_logos, y = 75)) +
  annotate(geom = "text", x = 14.5, y = 75, label = "Defense", color = "#55FFFF", family = "Windows Command Prompt") +
  annotate(geom = "text", x = 14.5, y = 100, label = "Offense", color = "#55FFFF", family = "Windows Command Prompt") +
  scale_fill_manual(breaks = top10_drives$offense, values = top10_drives$color) +
  coord_flip(ylim = c(75, NA)) +
  vapoRwave::jwz() +
  labs(
    title = "Longest CFB Drives of Week 1",
    x = "",
    y = ""
  ) + 
  guides(
    fill = "none",
    y = "none")
