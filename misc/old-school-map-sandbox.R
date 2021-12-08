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
library(mapview)

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

ds_results_ <- cfbd_game_info(year = 2021)

pnts_sf <- st_as_sf(ds_teams, coords = c("longitude", "latitude"))
st_crs(pnts_sf) <- 4326

counties <- counties(cb = TRUE, resolution = "20m")
counties <- st_transform(counties, 4326)

counties <- counties %>%
  mutate(
    n = row_number()
  )

closest_ <- list()
for (i in seq_len(nrow(counties))) {
  closest_[[i]] <- pnts_sf[which.min(sf::st_distance(pnts_sf, st_centroid(counties[i, ]))
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

counties_grouped.os <- counties_ %>%
  group_by(school, logos, color) %>%
  summarise(
    n = n(),
    sum_land = sum(ALAND) * 0.000000386102,
    sum_water = sum(AWATER) * 0.000000386102,
    sum_total = sum_land + sum_water,
    total_pop = sum(population)
  ) %>%
  st_cast("MULTIPOLYGON")

counties_grouped.os <- counties_grouped.os %>%
  left_join(., ds_teams)

save(counties_grouped.os, file = "old_school_imperialism_base_map-3.Rdata")

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
      school = if_else(school == ds_results$loser[i], ds_results$winner[i], school),
      conference = if_else(school == ds_results$loser[i], ds_results$conference[i], conference)
    )

  }


fill_team <- colorFactor(ds_teams$color, ds_teams$school)
fill_conf <- colorFactor(viridis::turbo(n = n_distinct(counties_grouped.os$conference)), ds_teams$conference)

logoIcons <- icons(
  iconUrl = counties_grouped.os$logos,
  iconWidth = 25, iconHeight = 25
)



# Map snapshots ======================================================================================================

library(mapview)

logoIcons.os <- icons(
  iconUrl = counties_grouped.os$logos,
  iconWidth = (as.numeric(log(st_area(counties_grouped.os))) - 21) * 25,
  iconHeight = (as.numeric(log(st_area(counties_grouped.os))) - 21) * 25
)

# County borders overlay
counties <- counties(cb = TRUE, resolution = "20m")
counties <- st_transform(counties, 4326)
  
# Reprojection

epsg2163 <- leafletCRS(
  crsClass = "L.Proj.CRS",
  code = "ESRI:102003",
  proj4def = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
  resolutions = 2^(16:7)
)

# change to 'color = ~faction' to switch to SEC vs. Alliance map

m <- leaflet(options = leafletOptions(crs = epsg2163),
             height = 1600, 
             width = 3000) %>%
  setView(lng = -98.24580, lat = 38.85909, zoom = 5) %>%
  addPolygons(data = counties_grouped.os, 
              smoothFactor = 0.2, 
              color = "white", 
              fillColor = ~fill_team(school), 
              fillOpacity = 0.9, 
              label = ~school, 
              weight = 0,
              stroke = F
              ) %>%
  addMarkers(data = st_centroid(counties_grouped.os), label = ~school, icon = logoIcons.os,
             popup = paste0(
               "<center><b>", counties_grouped.os$city, " Territory, home of the ", counties_grouped.os$mascot, "</b><br></center>",
               "<center>Currently Controlled by ", counties_grouped.os$school, "<br></center>",
               "<hr>",
               "Territory Area: ", format(round(counties_grouped.os$sum_land, 1), nsmall = 1, big.mark = ","), " sq. miles<br>",
               "Territory Water area: ", format(round(counties_grouped.os$sum_water, 1), nsmall = 1, big.mark = ","), " sq. miles<br>",
               "No. of Counties in Territory: ", format(counties_grouped.os$n, nsmall = 1, big.mark = ","), "<br>",
               "Territory Population: ", format(counties_grouped.os$total_pop, big.mark = ",")
             )) %>%
  addPolylines(data = counties, color = "black", weight = 0.2, smoothFactor = 0, opacity = 1)  %>%
#  addPolylines(data = states, color = "black", weight = 1, smoothFactor = 0, opacity = 1) %>%
  addPolylines(data = counties_grouped.os, color = "black", weight = 1.5, smoothFactor = 0, opacity = 1)  
  # addCircleMarkers(data = ds_teams, label = ~school, stroke = T, fillOpacity = 0.8, weight = 0.75, color = "black", fillColor = ~color, radius = 5,
  #                  popup = paste0("<center><img src=", ds_teams$logos, " width = '50' height = '50'>",
  #                                 "<br><hr><b>", ds_teams$school, "</center>",
  #                                 "</b><br>", ds_teams$conference,
  #                                 "<br>Mascot: ", ds_teams$mascot
  #                  ))

# m

mapshot(m, file = paste0("imperialism-map-", today(), ".png"), selfcontained = F)

# alliance / sec map -----------------------------------------------------------

leaflet() %>%
  setView(lng = -95.24580, lat = 38.95909, zoom = 5) %>%
  addPolygons(data = counties_grouped.os, smoothFactor = 0.2, color = ~faction, fillOpacity = 0.9, label = ~school, weight = 0.8,
              highlightOptions = highlightOptions(color = 'white', weight = 1,
                                                  bringToFront = FALSE)) %>%
  # addCircleMarkers(data = ds_teams, label = ~school, stroke = T, fillOpacity = 0.8, weight = 0.75, color = "black", fillColor = ~color, radius = 5,
  #                  popup = paste0("<center><img src=", ds_teams$logos, " width = '50' height = '50'>",
  #                                 "<br><hr><b>", ds_teams$school, "</center>",
  #                                 "</b><br>", ds_teams$conference,
  #                                 "<br>Mascot: ", ds_teams$mascot
  #                  )) %>%
  addMarkers(data = st_centroid(counties_grouped.os), label = ~school, icon = logoIcons.os,
             popup = paste0(
               "<center><b>", counties_grouped.os$city, " Territory, home of the ", counties_grouped.os$mascot, "</b><br></center>",
               "<center>Currently Controlled by ", counties_grouped.os$school, "<br></center>",
               "<hr>",
               "Territory Area: ", format(round(counties_grouped.os$sum_land, 1), nsmall = 1, big.mark = ","), " sq. miles<br>",
               "Territory Water area: ", format(round(counties_grouped.os$sum_water, 1), nsmall = 1, big.mark = ","), " sq. miles<br>",
               "No. of Counties in Territory: ", format(counties_grouped.os$n, nsmall = 1, big.mark = ","), "<br>",
               "Territory Population: ", format(counties_grouped.os$total_pop, big.mark = ",")
             )
  ) 


counties_sum_conf.os <- counties_grouped.os %>%
  group_by(conference) %>%
  summarize(
    n_territories = n(),
    sum_total = sum(sum(sum_total, na.rm = T)),
    sum_land = sum(sum(sum_land, na.rm = T)),
    sum_water = sum(sum(sum_water, na.rm = T)),
    sum_population = sum(sum(total_pop, na.rm = T)),
    sum_n = sum(sum(n, na.rm = T))
  ) %>%
  st_drop_geometry() %>%
  mutate(conference = if_else(is.na(conference), "FCS", conference)) %>%
  mutate(
    across(-c(conference), ~ .x |> round(2) |> format(big.mark = ","))
  ) %>%
  arrange(desc(n_territories)) %>%
  rename(
    Conference = conference,
    `Territories Controlled` = n_territories,
    `Total Area Controlled (sq. miles)` = sum_total,
    `Total Land Controlled (sq. miles)` = sum_land,
    `Total Water Controlled (sq. miles)` = sum_water,
    `Total Population` = sum_population,
    `US Counties Controlled` = sum_n
  )

write_csv(counties_sum_conf.os, "conference-imp-table.csv")



# Power proj. -----------
library(flexdashboard)
library(cfbfastR)
library(tidyverse)
library(ggimage)
library(sf)
library(tigris)
library(leaflet)
library(RCurl)
library(png)
library(bslib)
library(bsplus)
library(DT)
library(lwgeom)
library(raster)
library(data.table)
library(plotly)
library(lubridate)
library(ggthemes)
library(mapview)

options(scipen = 999)

# Sys.setenv(CFBD_API_KEY="")

# j <- 13

ds_teams_ <- cfbd_team_info(year = year(today())) %>%
  unnest(cols = c(logos)) %>%
  distinct(team_id, .keep_all = TRUE) %>%
  # Illinois' location is all screwed up for some reason
  mutate(
    latitude = if_else(school == "Illinois", 40.1020, latitude),
    longitude = if_else(school == "Illinois", -88.2272, longitude),
    color = if_else(school == "LSU", "#461D7C", color),
    color = if_else(school == "Iowa", "#FCD116", color)
  )

# Make this reactive?
ds_rank <- cfbd_rankings(year = year(today()), season_type = "regular") %>%
  filter(poll == "AP Top 25",
         !is.na(points)) %>%
  filter(week == max(week)) %>%
  # filter(week == j) %>%
  dplyr::select(c(week, poll, season_type, rank, school, first_place_votes, points))

ds_teams <- ds_teams_ %>%
  left_join(., ds_rank) %>%
  distinct()

pnts_sf <- st_as_sf(ds_teams, coords = c("longitude", "latitude"))
st_crs(pnts_sf) <- 4326

counties <- counties(cb = TRUE, resolution = "20m")
counties <- st_transform(counties, 4326)

counties <- counties %>%
  mutate(
    n = row_number()
  ) %>%
  distinct()

closest_ <- list()
for (i in seq_len(nrow(counties))) {
  closest_[[i]] <- pnts_sf[which.max(
    (ds_teams$points) * (1 / sf::st_distance(pnts_sf, counties[i, ]))
    ), ]
  
 if(i %% 322 == 0){print(paste0(round(100 * i / nrow(counties), 2), "%"))}

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


ds_teams <- pnts_sf %>%
  mutate(
    intersection = as.integer(st_intersects(geometry, counties_grouped)),
    current_overlord = if_else(is.na(intersection), '', counties_grouped$school[intersection])
    ) %>%
  dplyr::select(current_overlord, school) %>%
  left_join(ds_teams, ., by = "school")

n_subjects <- ds_teams %>% group_by(current_overlord) %>% summarize(n_capitols = n(), capitol_list = paste(school, collapse = ", ")) %>%
  rename(school = current_overlord)

ds_teams <- ds_teams %>%
  left_join(., n_subjects, by = "school")

counties_grouped <- counties_grouped %>%
  left_join(., n_subjects, by = "school") %>%
  mutate(
    color = if_else(school == "LSU", "#461D7C", color),
    color = if_else(school == "Iowa", "#FCD116", color),
    color = if_else(school == "SMU", "#354CA1", color),
    color = if_else(school == "TCU", "#ffffff", color),
    color = if_else(school == "Michigan State", "#ffffff", color),
    color = if_else(school == "UCLA", "#F2A900", color),
    color = if_else(school == "USC", "#990000", color),
    color = if_else(school == "UT San Antonio", "#F15A22", color),
    color = if_else(school == "Cincinnati", "#444444", color),
    # overlapping logos, edit as neeeded
    # logos = if_else(grepl("Ohio State", school), "https://upload.wikimedia.org/wikipedia/commons/4/48/BLANK_ICON.png", logos),
    # logos = if_else(grepl("Texas A&M", school), "https://upload.wikimedia.org/wikipedia/commons/4/48/BLANK_ICON.png", logos),
    # logos = if_else(grepl("BYU", school), "https://upload.wikimedia.org/wikipedia/commons/4/48/BLANK_ICON.png", logos),
    logos = if_else(school == "Oregon", "https://a.espncdn.com/i/teamlogos/ncaa/500-dark/2483.png", logos),
    logos = if_else(school == "Oklahoma", "https://a.espncdn.com/i/teamlogos/ncaa/500-dark/201.png", logos),
    logos = if_else(school == "Clemson", "https://a.espncdn.com/i/teamlogos/ncaa/500-dark/228.png", logos),
    logos = if_else(school == "Kansas State", "https://a.espncdn.com/i/teamlogos/ncaa/500-dark/2306.png", logos)
  )

# Power Projection Map -

logoIcons <- icons(
  iconUrl = counties_grouped$logos,
#  iconWidth = 30, iconHeight = 30,
  iconWidth = (as.numeric(log(st_area(counties_grouped))) - 20) * 20,
  iconHeight = (as.numeric(log(st_area(counties_grouped))) - 20) * 20
)

# Reprojection

epsg2163 <- leafletCRS(
  crsClass = "L.Proj.CRS",
  code = "ESRI:102003",
  proj4def = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
  resolutions = 2^(16:7)
)

p <- leaflet(options = leafletOptions(crs = epsg2163),
             height = 1600, 
             width = 3000) %>%
  setView(lng = -98.24580, 
          lat = 38.85909, 
          zoom = 5) %>%
  addPolygons(data = counties_grouped, 
              smoothFactor = 0.2, 
              color = ~color, 
              fillOpacity = 0.8, 
              label = ~school, 
              weight = 0.8,
              highlightOptions = highlightOptions(color = 'white', weight = 1,
                                                  bringToFront = FALSE)) %>%
  # addCircleMarkers(data = ds_teams, label = ~school, stroke = T, fillOpacity = 0.8, weight = 0.75, color = "black", fillColor = ~color, radius = 5,
  #                  popup = paste0("<center><img src=", ds_teams$logos, " width = '50' height = '50'>",
  #                                 "<br><hr><b>", ds_teams$school, "</center>",
  #                                 "</b><br>", ds_teams$conference,
  #                                 "<br>Mascot: ", ds_teams$mascot
  #                  )) %>%
  # addCircleMarkers(data = counties_grouped, label = "", lat = ~latitude, lng = ~longitude,
  #                  stroke = F, color = "white", fill = "white", fillOpacity = 0.8, radius = 16) %>%
  addMarkers(data = st_centroid(counties_grouped, of_largest_polygon = T), label = ~school, # lat = ~latitude, lng = ~longitude,
             icon = logoIcons) %>%
  addPolylines(data = counties, color = "black", weight = 0.2, smoothFactor = 0, opacity = 1)  %>%
  #  addPolylines(data = states, color = "black", weight = 1, smoothFactor = 0, opacity = 1) %>%
  addPolylines(data = counties_grouped, color = "black", weight = 1.5, smoothFactor = 0, opacity = 1)  
p


mapshot(p, file = paste0("pp-map-week", j, ".png"), selfcontained = F)



# Season gif image generator ----

library(flexdashboard)
library(cfbfastR)
library(tidyverse)
library(ggimage)
library(sf)
library(tigris)
library(leaflet)
library(RCurl)
library(png)
library(bslib)
library(bsplus)
library(DT)
library(lwgeom)
library(raster)
library(data.table)
library(plotly)
library(lubridate)
library(ggthemes)
library(mapview)

options(scipen = 999)

# Sys.setenv(CFBD_API_KEY="")

year <- 2020
j <- 2

ds_teams_ <- cfbd_team_info(year = year) %>%
  unnest(cols = c(logos)) %>%
  distinct(team_id, .keep_all = TRUE) %>%
  # Illinois' location is all screwed up for some reason
  mutate(
    latitude = if_else(school == "Illinois", 40.1020, latitude),
    longitude = if_else(school == "Illinois", -88.2272, longitude),
    color = if_else(school == "LSU", "#461D7C", color),
    color = if_else(school == "Iowa", "#FCD116", color)
  )

# Make this reactive?
ds_rank <- cfbd_rankings(year = year, season_type = "regular") %>%
  filter(poll == "AP Top 25",
         !is.na(points)) %>%
  filter(week == j) %>%
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
  closest_[[i]] <- pnts_sf[which.max(
    (ds_teams$points) * (1 / sf::st_distance(pnts_sf, counties[i, ])^2)
  ), ]
  
  if(i %% 10 == 0){print(paste0(round(100 * i / nrow(counties), 2), "% completed..."))}
  
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


ds_teams <- pnts_sf %>%
  mutate(
    intersection = as.integer(st_intersects(geometry, counties_grouped)),
    current_overlord = if_else(is.na(intersection), '', counties_grouped$school[intersection])
  ) %>%
  dplyr::select(current_overlord, school) %>%
  left_join(ds_teams, ., by = "school")

n_subjects <- ds_teams %>% group_by(current_overlord) %>% summarize(n_capitols = n(), capitol_list = paste(school, collapse = ", ")) %>%
  rename(school = current_overlord)

ds_teams <- ds_teams %>%
  left_join(., n_subjects, by = "school")

counties_grouped <- counties_grouped %>%
  left_join(., n_subjects, by = "school") %>%
  mutate(
    color = if_else(school == "LSU", "#461D7C", color),
    color = if_else(school == "Iowa", "#FCD116", color),
    color = if_else(school == "SMU", "#354CA1", color),
    color = if_else(school == "TCU", "#ffffff", color),
    color = if_else(school == "Michigan State", "#ffffff", color),
    color = if_else(school == "UCLA", "#F2A900", color),
    color = if_else(school == "USC", "#990000", color),
    color = if_else(school == "Cincinnati", "#E00122", color),
    color = if_else(school == "UT San Antonio", "#F15A22", color),
    # overlapping logos, edit as neeeded
    #  logos = if_else(grepl("Alabama", school), "https://upload.wikimedia.org/wikipedia/commons/4/48/BLANK_ICON.png", logos),
    logos = if_else(school == "Oregon", "https://a.espncdn.com/i/teamlogos/ncaa/500-dark/2483.png", logos),
    logos = if_else(school == "Oklahoma", "https://a.espncdn.com/i/teamlogos/ncaa/500-dark/201.png", logos),
    logos = if_else(school == "Kansas State", "https://a.espncdn.com/i/teamlogos/ncaa/500-dark/2306.png", logos)
  )


# Power Projection Map -

logoIcons <- icons(
  iconUrl = counties_grouped$logos,
  #  iconWidth = 30, iconHeight = 30,
  iconWidth = (as.numeric(log(st_area(counties_grouped))) - 20) * 25,
  iconHeight = (as.numeric(log(st_area(counties_grouped))) - 20) * 25
)

p <- leaflet(height = 1600, width = 3000) %>%
  setView(lng = -95.24580, lat = 38.95909, zoom = 6) %>%
  addPolygons(data = counties_grouped, smoothFactor = 0.2, color = ~color, fillOpacity = 0.8, label = ~school, weight = 0.8,
              highlightOptions = highlightOptions(color = 'white', weight = 1,
                                                  bringToFront = FALSE)) %>%
  addMarkers(data = st_centroid(counties_grouped, of_largest_polygon = T), label = ~school, # lat = ~latitude, lng = ~longitude,
             icon = ~logoIcons) %>%
  addPolylines(data = counties, color = "black", weight = 0.2, smoothFactor = 0, opacity = 1)  %>%
  #  addPolylines(data = states, color = "black", weight = 1, smoothFactor = 0, opacity = 1) %>%
  addPolylines(data = counties_grouped.os, color = "black", weight = 1.5, smoothFactor = 0, opacity = 1)  
p

mapshot(p, file = paste0(year, "-pp-map-week-", j, ".png"), selfcontained = F)

gc()
rm(list = ls())


