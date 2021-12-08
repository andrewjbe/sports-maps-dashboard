library(flexdashboard)
library(shiny)
library(tidyverse)
library(data.table)
library(hoopR)
library(sf)
library(leaflet)

# library(tigris)

ds_teams <- read_csv("schools_cities_coords_logos_fixed.csv")

ds_results <- espn_mbb_scoreboard(season = 2020)

# source("keys.R")


pnts_sf <- st_as_sf(ds_teams, coords = c("lng", "lat"))
st_crs(pnts_sf) <- 4326

# counties <- counties(cb = T, resolution = "500k")
# counties <- st_transform(counties, 4326)
# saveRDS(counties, "counties.RDS")

counties <- read_rds("counties.RDS")

counties <- counties |>
  mutate(n = row_number()) |>
  distinct()

closest_ <- list()
for (i in seq_len(nrow(counties))) {
  closest_[[i]] <- pnts_sf[which.min(
    #   (ds_teams$points) * (1 / sf::st_distance(pnts_sf, counties[i, ]))
    # st_distance(pnts_sf, st_centroid(counties[i,]))
  ), ]
  
  if(i %% 322 == 0){print(paste0(round(100 * i / nrow(counties), 2), "%"))}
  
}
closest_ <- rbindlist(closest_)

closest_ <- closest_ |>
  dplyr::select(team) |>
  mutate(n = row_number())

counties_ <- left_join(counties, closest_, by = "n")

# counties_pop <- get_estimates(geography = "county", product = "population") %>%
#   rename(estimate = value)
# write_csv(counties_pop, "counties_pop.csv")

counties_pop <- read_csv("counties_pop.csv")

counties_ <- merge(counties_, counties_pop, by = "GEOID")

counties_ <- counties_ %>%
  rename(
    population = estimate
  ) %>%
  dplyr::select(!variable)

# Removes Alaska
counties_ <- counties_ %>%
  filter(STATEFP != "02",
         STATEFP != "72")

counties_grouped <- counties_ %>%
  group_by(team) %>%
  summarise(
    n = n(),
    sum_land = sum(ALAND) * 0.000000386102,
    sum_water = sum(AWATER) * 0.000000386102,
    sum_total = sum_land + sum_water,
    total_pop = sum(population)
  ) %>%
  st_cast("MULTIPOLYGON") |>
  left_join(hoopR::espn_mbb_teams(), by = "team") |>
  mutate(color = if_else(!is.na(color), paste0("#", color), as.character(NA)))

# Summary dataframe
counties_sum <- counties_grouped %>%
  group_by(team) %>%
  summarize(
    n_territories = n(),
    sum_total = sum(sum(sum_total, na.rm = T)),
    sum_land = sum(sum(sum_land, na.rm = T)),
    sum_water = sum(sum(sum_water, na.rm = T)),
    sum_population = sum(sum(total_pop, na.rm = T)),
    sum_n = sum(sum(n, na.rm = T))
  ) %>%
  distinct()

# function to define logo size; scales with log(territory size)
logoIcons <- icons(
  iconUrl = counties_grouped$logo,
  iconWidth = (as.numeric(log(st_area(counties_grouped))) - 21) * 9,
  iconHeight = (as.numeric(log(st_area(counties_grouped))) - 21) * 9
)

# function to apply the correct colors to each territory 
fill_team <- colorFactor(counties_grouped$color, counties_grouped$team, na.color = "grey", ordered = TRUE)

# GG plot version of the map






# This is the actual map -------------------------------------------------------
leaflet() %>%
  setView(lng = -95.24580, lat = 38.95909, zoom = 4) %>%
  addPolygons(data = counties_grouped,
              smoothFactor = 0.2,
              color = "black",
              fillColor = ~fill_team(team),
              fillOpacity = 0.9,
              label = ~team,
              weight = 1,
              group = "Current Map",
              highlightOptions = highlightOptions(color = 'white', weight = 1, bringToFront = FALSE)
  ) |>
# addCircleMarkers(data = ds_teams,
#                  label = ~team,
#                  stroke = T,
#                  fillOpacity = 0.8,
#                  weight = 0.75,
#                  color = "black",
#                  fillColor = ~color,
#                  radius = 5,
#                  group = "Toggle School Locations",
#                  popup = paste0("<center><img src=", ds_teams$logos, " width = '50' height = '50'>",
#                                 "<br><hr><b>", ds_teams$team, "</center>",
#                                 "</b><br>", ds_teams$conference,
#                                 "<br>Mascot: ", ds_teams$mascot
#                  )) %>%
  addMarkers(data = st_centroid(counties_grouped), label = ~team, icon = ~logoIcons, group = "Current Map",
             popup = paste0(
               "<center><b>", counties_grouped$city, " Territory, home of the ", counties_grouped$mascot, "</b><br></center>",
               "<center>Currently Controlled by ", counties_grouped$team, "<br></center>",
               "<hr>",
               "Territory Area: ", format(round(counties_grouped$sum_land, 1), nsmall = 1, big.mark = ","), " sq. miles<br>",
               "Territory Water area: ", format(round(counties_grouped$sum_water, 1), nsmall = 1, big.mark = ","), " sq. miles<br>",
               "No. of Counties in Territory: ", format(counties_grouped$n, nsmall = 1, big.mark = ","), "<br>",
               "Territory Population: ", format(counties_grouped$total_pop, big.mark = ",")
             )) 
#   addPolylines(data = counties,
#                color = "black",
#                weight = 0.2,
#                smoothFactor = 0,
#                opacity = 1,
#                group = "Toggle County Borders") %>%
#   addPolylines(data = states,
#                color = "black",
#                weight = 0.4,
#                smoothFactor = 0,
#                opacity = 1,
#                group = "Toggle State Borders") %>%
#   addTiles() %>%
#   addLayersControl(
#     baseGroups = c("Current Map"),
#     overlayGroups = c("Toggle School Locations", "Toggle County Borders", "Toggle State Borders"),
#     options = layersControlOptions(collapsed = FALSE)
#   ) %>%
#   hideGroup(c("Toggle School Locations", "Toggle County Borders", "Toggle State Borders"))



