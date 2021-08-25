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

ds_teams_ <- cfbd_team_info(year = year(today())) %>%
  unnest(cols = c(logos)) %>%
  distinct(team_id, .keep_all = TRUE)

# Make this reactive?
ds_rank <- cfbd_rankings(year = year(today()), season_type = "regular") %>%
  filter(poll == "AP Top 25",
         !is.na(points),
         week == max(week)) %>%
  dplyr::select(c(week, poll, season_type, rank, school, first_place_votes, points))

ds_teams <- ds_teams_ %>%
  left_join(., ds_rank)

pnts_sf <- st_as_sf(ds_teams, coords = c("longitude", "latitude"))
st_crs(pnts_sf) <- 4326

counties <- counties(cb = TRUE, resolution = "20m")
st_crs(counties) <- 4326

counties <- counties %>%
  mutate(
    n = row_number()
  )

closest_ <- list()
for (i in seq_len(nrow(counties))) {
  closest_[[i]] <- pnts_sf[which.min(sf::st_distance(pnts_sf, counties[i, ])
  ), ]
  print(paste0(round(100 * i / nrow(counties), 2), "%"))
}
closest_ <- rbindlist(closest_)

closest_ <- closest_ %>%
  dplyr::select(school, color, logos) %>%
  mutate(
    n = row_number()
  )

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
  group_by(school, logos, color) %>%
  summarise(
    n = n(),
    sum_land = sum(ALAND) * 0.000000386102,
    sum_water = sum(AWATER) * 0.000000386102,
    sum_total = sum_land + sum_water,
    total_pop = sum(population)
  ) %>%
  st_cast("MULTIPOLYGON")

counties_grouped <- counties_grouped %>%
  left_join(., ds_teams)


fill_team <- colorFactor(ds_teams$color, ds_teams$school)

logoIcons <- icons(
  iconUrl = counties_grouped$logos,
  iconWidth = 25, iconHeight = 25
)

leaflet() %>%
  setView(lng = -95.24580, lat = 38.95909, zoom = 4) %>%
  addPolygons(data = counties_grouped, smoothFactor = 0.2, color = ~fill_team(school), fillOpacity = 0.8, label = ~school,
              highlightOptions = highlightOptions(color = 'white', weight = 1,
                                                  bringToFront = FALSE)) %>%
  addCircleMarkers(data = ds_teams, label = ~school, stroke = T, fillOpacity = 0.8, weight = 0.75, color = "black", fillColor = ~color, radius = 5,
                   popup = paste0("<center><img src=", ds_teams$logos, " width = '50' height = '50'>",
                                  "<br><hr><b>", ds_teams$school, "</center>",
                                  "</b><br>", ds_teams$conference,
                                  "<br>Mascot: ", ds_teams$mascot
                   )) %>%
  addCircleMarkers(data = counties_grouped, label = "", lat = ~latitude, lng = ~longitude,
                   stroke = F, color = "white", fill = "white", fillOpacity = 0.8, radius = 16) %>%
  addMarkers(data = counties_grouped, label = ~school,  lat = ~latitude, lng = ~longitude,
             popup = paste0(
               "<center><b>", counties_grouped$school, "'s</b> Domain<br></center>",
               "<hr>",
               "No. of AP points: ", format(counties_grouped$points, nsmall = 1, big.mark = ","), "<br>",
               "Current AP Poll Rank: ", format(counties_grouped$rank, nsmall = 1, big.mark = ","), "<br>",
               "<hr>",
               "Total land area: ", format(round(counties_grouped$sum_land, 1), nsmall = 1, big.mark = ","), " sq. miles<br>",
               "Total water area: ", format(round(counties_grouped$sum_water, 1), nsmall = 1, big.mark = ","), " sq. miles<br>",
               "No. of Counties: ", format(counties_grouped$n, nsmall = 1, big.mark = ","), "<br>",
               "Total Population: ", format(counties_grouped$total_pop, big.mark = ",")
             ), icon = logoIcons) %>%
  addTiles() 
# setMaxBounds(lng1 = -130,
#              lat1 = 20,
#              lng2 = -65,
#              lat2 = 55)


















