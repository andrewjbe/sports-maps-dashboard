library(flexdashboard)
library(cfbfastR)
library(tidyverse)
library(raster)
library(data.table)
library(ggimage)
library(lubridate)
library(ggthemes)
library(sf)
library(tigris)
library(leaflet)
library(RCurl)
library(png)
library(plotly)
library(bslib)
library(bsplus)
library(DT)
library(lwgeom)

options(scipen = 999)

# Sys.setenv(CFBD_API_KEY="")

ds_teams <- cfbd_team_info(year = year(today())) %>%
  unnest(cols = c(logos)) %>%
  distinct(team_id, .keep_all = TRUE) %>%
  # Illinois is all screwed up for some reason
  mutate(
    latitude = if_else(school == "Illinois", 40.1020, latitude),
    longitude = if_else(school == "Illinois", -88.2272, longitude)
  )

ds_results_ <- cfbd_game_info(year = year(today()))

# pnts_sf <- st_as_sf(ds_teams, coords = c("longitude", "latitude"))
# st_crs(pnts_sf) <- 4326
# 
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
#   closest_[[i]] <- pnts_sf[which.min(sf::st_distance(pnts_sf, counties[i, ])
#   ), ]
#   print(paste0(round(100 * i / nrow(counties), 2), "%"))
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
# counties_grouped.os <- counties_ %>%
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
# counties_grouped.os <- counties_grouped %>%
#   left_join(., ds_teams)
# 
# save(counties_grouped.os, file = "old_school_imperialism_base_map.Rdata")

load("old_school_imperialism_base_map.Rdata")

ds_results <- ds_results_ %>%
  filter(!is.na(home_points)) %>%
  mutate(
    winner = if_else(home_points > away_points, home_team, away_team),
    loser = if_else(home_points < away_points, home_team, away_team)
  ) %>%
  dplyr::select(winner, loser)

for(i in (1:nrow(ds_results))){
  
  counties_grouped.os <- counties_grouped.os %>%
    mutate(
      school = if_else(school == ds_results$loser[i], ds_results$winner[i], school)
    )

  }


fill_team <- colorFactor(ds_teams$color, ds_teams$school)

logoIcons <- icons(
  iconUrl = counties_grouped.os$logos,
  iconWidth = 25, iconHeight = 25
)

leaflet() %>%
  setView(lng = -95.24580, lat = 38.95909, zoom = 4) %>%
  addPolygons(data = counties_grouped.os, smoothFactor = 0.2, color = ~fill_team(school), fillOpacity = 0.8, label = ~school,
              highlightOptions = highlightOptions(color = 'white', weight = 1,
                                                  bringToFront = FALSE)) %>%
  addCircleMarkers(data = ds_teams, label = ~school, stroke = T, fillOpacity = 0.8, weight = 0.75, color = "black", fillColor = ~color, radius = 5,
                   popup = paste0("<center><img src=", ds_teams$logos, " width = '50' height = '50'>",
                                  "<br><hr><b>", ds_teams$school, "</center>",
                                  "</b><br>", ds_teams$conference,
                                  "<br>Mascot: ", ds_teams$mascot
                   )) %>%
  # addCircleMarkers(data = counties_grouped.os, label = "", lat = ~latitude, lng = ~longitude,
  #                  stroke = F, color = "white", fill = "white", fillOpacity = 0.8, radius = 16) %>%
  # addMarkers(data = counties_grouped.os, label = ~school,  lat = ~latitude, lng = ~longitude,
  #            popup = paste0(
  #              "<center><b>", counties_grouped.os$school, "'s</b> Domain<br></center>",
  #              "<hr>",
  #              "No. of AP points: ", format(counties_grouped.os$points, nsmall = 1, big.mark = ","), "<br>",
  #              "Current AP Poll Rank: ", format(counties_grouped.os$rank, nsmall = 1, big.mark = ","), "<br>",
  #              "<hr>",
  #              "Total land area: ", format(round(counties_grouped.os$sum_land, 1), nsmall = 1, big.mark = ","), " sq. miles<br>",
  #              "Total water area: ", format(round(counties_grouped.os$sum_water, 1), nsmall = 1, big.mark = ","), " sq. miles<br>",
  #              "No. of Counties: ", format(counties_grouped.os$n, nsmall = 1, big.mark = ","), "<br>",
  #              "Total Population: ", format(counties_grouped.os$total_pop, big.mark = ",")
  #            ), icon = logoIcons) %>%
  addTiles() 












