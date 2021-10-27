library(cfbfastR)
library(tidyverse)
library(ggimage)
library(ggthemes)
library(ggpmisc)
library(ggpubr)

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

# More going for it on 4th down???

df_ <- tibble()
  
for(i in c(2013:2021)) {
  
  temp <- cfbfastR::cfbd_plays(year = i,
                       season_type = "regular") |>
    mutate(season = i)
  
  df_ <- rbind(df_, temp)

  print(paste(i, "loaded..."))
  
  }

df_4th <- df_ |>
  filter(down == 4,
         !grepl("Penalty|Timeout|Kickoff", play_type),
         grepl("Big 12|Pac-12|SEC|Big Ten|FBS Independents|ACC", offense_conference)
         ) |>
  mutate(
    gofor = if_else(grepl("Punt|Field Goal", play_type), FALSE, TRUE)
  )

df_seasons <- df_4th |>
  group_by(season, offense_conference) |>
  summarize(
    n = n(),
    n_gofor = sum(gofor),
    pct_gofor = n_gofor / n
  ) 

df_seasons |>
  filter(season != 2020) |>
  ggplot(aes(x = season, y = pct_gofor)) +
  stat_smooth(method = "lm", se = F) +
  geom_line(size = 1, alpha = .5) +
  geom_point(size = 2) +
  stat_cor() +
  scale_y_continuous(limits = c(0,0.5), labels = scales::percent) +
  scale_x_continuous(breaks = c(2009, 2011, 2013, 2015, 2017, 2019, 2021)) +
  labs(
    title = "CFB 4th down conversion attempt rate by season",
    subtitle = "All P5 / Independent teams, 2009-2021",
    x = "Season",
    y = "% 4th downs w/out punt, FG attempt",
    caption = "Any fourth down which didn't result in a punt, field goal attempt,
    time out, or penalty is counted as an attempted conversion.
    Data from https://collegefootballdata.com/"
  ) +
  theme_clean() +
  facet_wrap(~offense_conference)

df_outcomes <- df_4th |>
  mutate(
   # convert = if_else(gofor & distance <= yards_gained & !grepl("Kickoff", play_type) | scoring, TRUE, FALSE),
    convert = if_else(gofor & (grepl("for a 1ST", play_text) & !grepl("Kickoff", play_type) | scoring), TRUE, FALSE)
  ) |>
  group_by(season, offense_conference) |>
  summarize(
    n = n(),
    n_gofor = sum(gofor),
    pct_gofor = n_gofor / n,
    n_converted = sum(convert),
    pct_converted = n_converted / n_gofor
  ) 

df_outcomes |>
ggplot(aes(x = season, y = pct_converted)) +
  geom_line(size = 1, alpha = .5) +
  geom_point(size = 2) +
  stat_smooth(method = "lm", se = F) + 
  stat_cor() +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  scale_x_continuous(breaks = c(2009, 2011, 2013, 2015, 2017, 2019, 2021)) +
  theme_clean() +
  labs(
    title = "CFB 4th down conversion success rate",
    subtitle = "All P5 + Independent teams, 2009-2021",
    x = "Season",
    y = "% of 4th down conversion attempts resulting in a 1st down or a score",
    caption = "Any fourth down conversion attempt which ended with a 1st down or
    a score for the offense is counted as a succesful conversion.
    Data from https://collegefootballdata.com/"
  ) +
  facet_wrap(~offense_conference)

q <- df_4th |>
  group_by(season, gofor) |>
  summarize(
    n = n(),
    avg_dist = mean(distance)
  ) 

df_4th |>
  filter(season == 2021) |>
  group_by(offense_conference) |>
  summarize(
    n = n(),
    n_gofor = sum(gofor),
    pct_gofor = n_gofor / n
  ) |>
  filter(n > 9) |>
  arrange(desc(pct_gofor))

# Old AP Power Proj. Map code from Dashboard:

# counties <- counties(cb = TRUE, resolution = "20m")
# st_crs(counties) <- 4326
# 
# counties <- counties %>%
#   mutate(
#     n = row_number()
#   )
# 
# closest_ <- list()
# for (i in seq_len(nrow(counties))) {
#   closest_[[i]] <- pnts_sf[which.max(
#     (ds_teams$points) * (1 / sf::st_distance(pnts_sf, counties[i, ]))
#     ), ]
# # print(paste0(round(100 * i / nrow(counties), 2), "%"))
# }
# closest_ <- rbindlist(closest_)
# 
# closest_ <- closest_ %>%
#   dplyr::select(school, color, logos) %>%
#   mutate(
#     n = row_number()
#   )
# 
# counties_ <- left_join(counties, closest_, by = "n")
# 
# # counties_pop <- get_estimates(geography = "county", product = "population") %>%
# #   rename(estimate = value)
# # write_csv(counties_pop, "counties_pop.csv")
# 
# counties_pop <- read_csv("counties_pop.csv")
# 
# counties_ <- merge(counties_, counties_pop, by = "GEOID")
# 
# counties_ <- counties_ %>%
#   rename(
#     population = estimate
#   ) %>%
#   dplyr::select(!variable)
# 
# # Removes Alaska
# counties_ <- counties_ %>%
#   filter(STATEFP != "02",
#          STATEFP != "72")
# 
# counties_grouped <- counties_ %>%
#   group_by(school, logos, color) %>%
#   summarise(
#     n = n(),
#     sum_land = sum(ALAND) * 0.000000386102,
#     sum_water = sum(AWATER) * 0.000000386102,
#     sum_total = sum_land + sum_water,
#     total_pop = sum(population)
#   ) %>%
#   st_cast("MULTIPOLYGON")
# 
# counties_grouped <- counties_grouped %>%
#   left_join(., ds_teams)
# 
#   
# ds_teams <- pnts_sf %>% 
#   mutate(
#     intersection = as.integer(st_intersects(geometry, counties_grouped)), 
#     current_overlord = if_else(is.na(intersection), '', counties_grouped$school[intersection])
#     ) %>%
#   dplyr::select(current_overlord, school) %>%
#   left_join(ds_teams, ., by = "school")
# 
# n_subjects <- ds_teams %>% group_by(current_overlord) %>% summarize(n_capitols = n(), capitol_list = paste(school, collapse = ", ")) %>%
#   rename(school = current_overlord)
# 
# ds_teams <- ds_teams %>%
#   left_join(., n_subjects, by = "school")
#   
# counties_grouped <- counties_grouped %>%
#   left_join(., n_subjects, by = "school")

# <!-- Power Projection Map {data-navmenu="Current Power Projection Map"} -->
#   <!-- ===================================== -->
#   
#   <!-- Row  -->
#   <!-- ----------------------------------------------------------------------- -->
#   
#   <!-- ### Power Projection Map | `r today()` -->
#   
#   <!-- ```{r} -->
#   
#   <!-- logoIcons <- icons( -->
#                              <!--   iconUrl = counties_grouped$logos, -->
#                              <!--   iconWidth = 25, iconHeight = 25 -->
#                              <!-- ) -->
#   
#   <!-- leaflet() %>% -->
#   <!--   setView(lng = -95.24580, lat = 38.95909, zoom = 4) %>% -->
#   <!--   addPolygons(data = counties_grouped, smoothFactor = 0.2, color = ~color, fillOpacity = 0.8, label = ~school, weight = 0.8, -->
#                        <!--               highlightOptions = highlightOptions(color = 'white', weight = 1, -->
#                                                                                 <!--                                                   bringToFront = FALSE)) %>% -->
#   <!--   addCircleMarkers(data = ds_teams, label = ~school, stroke = T, fillOpacity = 0.8, weight = 0.75, color = "black", fillColor = ~color, radius = 5, -->
#                             <!--                    popup = paste0("<center><img src=", ds_teams$logos, " width = '50' height = '50'>", -->
#                                                                      <!--                                   "<br><hr><b>", ds_teams$school, "</center>", -->
#                                                                      <!--                                   "</b><br>", ds_teams$conference, -->
#                                                                      <!--                                   "<br>Mascot: ", ds_teams$mascot -->
#                                                                      <!--                                   )) %>% -->
#   <!--   addCircleMarkers(data = counties_grouped, label = "", lat = ~latitude, lng = ~longitude, -->
#                             <!--                    stroke = F, color = "white", fill = "white", fillOpacity = 0.8, radius = 16) %>% -->
#   <!--   addMarkers(data = counties_grouped, label = ~school,  lat = ~latitude, lng = ~longitude, -->
#                       <!--              popup = paste0( -->
#                                                           <!--     "<center><b>", counties_grouped$school, "'s</b> Domain<br></center>", -->
#                                                           <!--     "<hr>", -->
#                                                           <!--     "No. of AP points: ", format(counties_grouped$points, nsmall = 1, big.mark = ","), "<br>", -->
#                                                           <!--     "Current AP Poll Rank: ", format(counties_grouped$rank, nsmall = 1, big.mark = ","), "<br>", -->
#                                                           <!--     "<hr>", -->
#                                                           <!--     "Total land area: ", format(round(counties_grouped$sum_land, 1), nsmall = 1, big.mark = ","), " sq. miles<br>", -->
#                                                           <!--     "Total water area: ", format(round(counties_grouped$sum_water, 1), nsmall = 1, big.mark = ","), " sq. miles<br>", -->
#                                                           <!--     "No. of Counties: ", format(counties_grouped$n, nsmall = 1, big.mark = ","), "<br>", -->
#                                                           <!--     "Total Population: ", format(counties_grouped$total_pop, big.mark = ",") -->
#                                                           <!--   ), icon = logoIcons) %>% -->
#   <!--   addTiles()  -->
#   <!--   # setMaxBounds(lng1 = -130, -->
#   <!--   #              lat1 = 20, -->
#   <!--   #              lng2 = -65, -->
#   <!--   #              lat2 = 55) -->
#   
#   <!-- ``` -->
#   
#   <!-- > Each county is "controlled" by the team with the highest "power projection" score relative to that county. This score is equal to schools' total AP Poll votes (in the most recent poll) divided by the distance between the school and the nearest county border, in kilometers. In other words, each county is controlled by the strongest nearby team, with stronger teams having a longer "reach". Data source: https://github.com/saiemgilani/cfbfastR-data -->
# 
# <!-- Row -->
# <!-- ----------------------------------------------------------------------- -->
# 
# <!-- ### Biggest Domain {.no-mobile} -->
# 
# <!-- ```{r} -->
# <!-- top_domain <- counties_grouped %>% ungroup() %>% slice_max(order_by = sum_total, n = 1) %>% pull(var = school) -->
# 
# <!-- renderValueBox({ -->
# 
# <!--   valueBox( -->
# <!--     paste0( -->
# <!--       counties_grouped %>% ungroup() %>% slice_max(order_by = sum_total, n = 1) %>% pull(var = school) -->
# <!--     ), -->
# <!--     color = ds_teams %>% filter(school == top_domain) %>% pull(var = color) -->
# <!--   ) %>% bs_embed_tooltip( -->
# <!--     title = paste(format(round(counties_grouped %>% ungroup() %>% slice_max(order_by = sum_total, n = 1) %>% pull(var = sum_total), 1), nsmall = 1, big.mark = ","), "sq. miles") -->
# <!--   ) -->
# 
# <!-- }) -->
# <!-- ``` -->
# 
# <!-- ### Biggest Water Domain {.no-mobile} -->
# 
# <!-- ```{r} -->
# <!-- top_water <- counties_grouped %>% ungroup() %>% slice_max(order_by = sum_water, n = 1) %>% pull(var = school) -->
# 
# <!-- renderValueBox({ -->
# 
# <!--   valueBox( -->
# <!--     paste0( -->
# <!--       counties_grouped %>% ungroup() %>% slice_max(order_by = sum_water, n = 1) %>% pull(var = school) -->
# <!--     ), -->
# <!--     color = ds_teams %>% filter(school == top_water) %>% pull(var = color) -->
# <!--   ) %>% bs_embed_tooltip( -->
# <!--     title = paste(format(round(counties_grouped %>% ungroup() %>% slice_max(order_by = sum_water, n = 1) %>% pull(var = sum_total), 1), nsmall = 1, big.mark = ","), "sq. miles") -->
# <!--   ) -->
# 
# <!-- }) -->
# <!-- ``` -->
# 
# <!-- ### CFB Stadiums Controlled {.no-mobile} -->
# 
# <!-- ```{r} -->
# <!-- top_subjects <- n_subjects %>% slice_max(order_by = n_capitols, n = 1, with_ties = FALSE) -->
# 
# <!-- renderValueBox({ -->
# 
# <!--   valueBox( -->
# <!--     paste0( -->
# <!--       top_subjects$school[1] -->
# <!--     ), -->
# <!--     color = ds_teams %>% filter(school == top_subjects$school[1]) %>% pull(var = color) -->
# <!--   ) %>% bs_embed_tooltip( -->
# <!--     title = paste0(top_subjects$school[1], " currently has the most imperial subjects in their domain (", top_subjects$n_capitols[1], ")." ) -->
# <!--   ) -->
# 
# <!-- }) -->
# <!-- ``` -->
# 
# <!-- ### Most Counties Controlled {.no-mobile} -->
# 
# <!-- ```{r} -->
# <!-- top_counties <- counties_grouped %>% ungroup() %>% slice_max(order_by = n, n = 1) %>% pull(var = school) -->
# 
# <!-- renderValueBox({ -->
# 
# <!--   valueBox( -->
# <!--     paste0( -->
# <!--       counties_grouped %>% ungroup() %>% slice_max(order_by = n, n = 1) %>% pull(var = school) -->
# <!--     ), -->
# <!--     color = ds_teams %>% filter(school == top_counties) %>% pull(var = color) -->
# <!--   ) %>% bs_embed_tooltip( -->
# <!--     title = paste(format(round(counties_grouped %>% ungroup() %>% slice_max(order_by = n, n = 1) %>% pull(var = n), 1), nsmall = 1, big.mark = ","), "counties") -->
# <!--   ) -->
# 
# <!-- }) -->
# <!-- ``` -->
# 
# <!-- ### Largest Population {.no-mobile} -->
# 
# <!-- ```{r} -->
# <!-- top_pop <- counties_grouped %>% ungroup() %>% slice_max(order_by = total_pop, n = 1) %>% pull(var = school) -->
# 
# <!-- renderValueBox({ -->
# 
# <!--   valueBox( -->
# <!--     paste0( -->
# <!--       counties_grouped %>% ungroup() %>% slice_max(order_by = total_pop, n = 1) %>% pull(var = school) -->
# <!--     ), -->
# <!--     color = ds_teams %>% filter(school == top_pop) %>% pull(var = color) -->
# <!--   ) %>% bs_embed_tooltip( -->
# <!--     title = paste(format(round(counties_grouped %>% ungroup() %>% slice_max(order_by = total_pop, n = 1) %>% pull(var = total_pop), 1), big.mark = ","), "people") -->
# <!--   ) -->
# 
# <!-- }) -->
# <!-- ``` -->
# 
# <!-- Stat Explorer {data-navmenu="Current Power Projection Map"} -->
# <!-- ===================================== -->
# 
# <!-- Row  -->
# <!-- -------------- -->
# 
# <!-- ### Power Projection Domain Stats -->
# 
# <!-- ```{r} -->
# 
# <!--   DT::datatable( -->
# <!--     data = counties_grouped %>%  -->
# <!--       dplyr::select(c(logos, school, n, sum_land, sum_water, sum_total, total_pop, points, rank, n_capitols, capitol_list)) %>% -->
# <!--       st_drop_geometry() %>% -->
# <!--       mutate( -->
# <!--         sum_land = round(sum_land, 2), -->
# <!--         sum_water = round(sum_water, 2), -->
# <!--         sum_total = round(sum_total, 2), -->
# <!--         total_pop = round(total_pop, 2), -->
# <!--         logos = paste0("<img src='", logos, "' width='50' height='50'>") -->
# <!--       ) %>% -->
# <!--       rename( -->
# <!--         Team = school, -->
# <!--         'Counties Controlled' = n, -->
# <!--         'Total Land Area (sq. mi.)' = sum_land, -->
# <!--         'Total Water Area (sq. mi.)' = sum_water, -->
# <!--         'Total Territory (sq. mi.)' = sum_total, -->
# <!--         'Total Population' = total_pop, -->
# <!--         `Points in Latest AP Poll` = points, -->
# <!--         `Rank in Latest AP Poll` = rank, -->
# <!--         `No. CFB Stadiums Controlled` = n_capitols, -->
# <!--         `Stadiums Controlled` = capitol_list -->
# <!--       ), -->
# <!--     rownames = FALSE, options = list(scrollY = 550, pageLength = 100, dom = 'Bfrtip', -->
# <!--     buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),  -->
# <!--     extensions = 'Buttons', -->
# <!--     escape = FALSE -->
# <!--   ) %>% -->
# <!--   formatRound(c('Total Land Area (sq. mi.)', 'Total Water Area (sq. mi.)', 'Total Territory (sq. mi.)', 'Total Population'), digits = 0) -->
# 
# 
# <!-- ``` -->

