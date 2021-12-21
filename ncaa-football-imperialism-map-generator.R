library(flexdashboard)
library(cfbfastR)
library(tidyverse)
library(sf)
library(tigris) #
library(leaflet)
library(RCurl)
library(png) #
library(bslib)
library(bsplus)
library(DT)
library(lwgeom)
library(raster)
library(data.table)
library(plotly) #
library(lubridate)
library(ggthemes)
library(rgdal)
library(maptools)
library(extrafont)

options(scipen = 999)

# Insert your CFBD API key here (nothing will run otherwise):
# https://saiemgilani.github.io/cfbfastR/reference/register_cfbd.html
# Sys.setenv(CFBD_API_KEY="")

# This is where I have my keys stored; comment out / delete if using above
source("keys.R")

yr <- 2021
wk <- 1

for(i in 1:15) {
  get_imperialism_map(yr = 2021, wk = i)
}

get_imperialism_map <- function(yr, wk) {
  
ds_teams <- cfbd_team_info(year = year(today())) %>%
  unnest(cols = c(logos)) %>%
  distinct(team_id, .keep_all = TRUE) %>%
  # Fixing team colors / logos / names / etc.
  # Illinois' location is all screwed up for some reason
  mutate(
    latitude = if_else(school == "Illinois", 40.1020, latitude),
    longitude = if_else(school == "Illinois", -88.2272, longitude),
    color = if_else(school == "LSU", "#461D7C", color),
    color = if_else(school == "Iowa", "#FCD116", color),
    color = if_else(school == "SMU", "#354CA1", color),
    color = if_else(school == "TCU", "#ffffff", color),
    color = if_else(school == "Utah", "#CC0000", color),
    color = if_else(school == "UCLA", "#F2A900", color),
    color = if_else(school == "Miami (OH)", "#000000", color),
    color = if_else(school == "Fresno State", "#13284c", color),
    color = if_else(school == "Wisconsin", "#FFFFFF", color),
    color = if_else(school == "USC", "#990000", color),
    color = if_else(school == "Cincinnati", "#444444", color),
    color = if_else(school == "UT San Antonio", "#F15A22", color),
    color = if_else(school == "Pittsburgh", "#FFB81C", color),
    color = if_else(school == "West Virginia", "#EAAA00", color),
    color = if_else(school == "Washington State", "#5E6A71", color),
    color = if_else(school == "Tennessee", "#58595B", color),
    color = if_else(school == "Utah State", "#6890B8", color),
    color = if_else(school == "California", "#FDB515", color),
    color = if_else(school == "Minnesota", "#FFCC33", color),
    color = if_else(school == "San Diego State", "#FFFFFF", color),
    logos = if_else(school == "Oregon", "https://a.espncdn.com/i/teamlogos/ncaa/500-dark/2483.png", logos),
    logos = if_else(school == "Oklahoma", "https://a.espncdn.com/i/teamlogos/ncaa/500-dark/201.png", logos),
    logos = if_else(school == "Kansas State", "https://a.espncdn.com/i/teamlogos/ncaa/500-dark/2306.png", logos),
    logos = if_else(school == "Clemson", "https://a.espncdn.com/i/teamlogos/ncaa/500-dark/228.png", logos),
    logos = if_else(school == "Air Force", "https://a.espncdn.com/i/teamlogos/ncaa/500-dark/2005.png", logos),
    logos = if_else(school == "Michigan State", "https://a.espncdn.com/i/teamlogos/ncaa/500-dark/127.png", logos),
    logos = if_else(school == "Nevada", "https://a.espncdn.com/i/teamlogos/ncaa/500-dark/2440.png", logos),
  #  logos = if_else(school == "California", "https://a.espncdn.com/i/teamlogos/ncaa/500-dark/25.png", logos),
    logos = if_else(school == "North Texas", "https://kuathletics.com/wp-content/uploads/2021/11/North_Texas_Mean_Green_logo.svg_.png", logos),
    school = if_else(school == "UT San Antonio", "UTSA", school)
  )

# function to apply the correct colors to each territory 
fill_team <- colorFactor(ds_teams$color, ds_teams$school, na.color = "grey", ordered = TRUE)

# Creating sf object w/ a point for every stadium location
pnts_sf <- st_as_sf(ds_teams, coords = c("longitude", "latitude"))
st_crs(pnts_sf) <- 4326

# County borders overlay -- saved to RDS to save load time
# counties <- counties(cb = TRUE, resolution = "20m")
# counties <- st_transform(counties, 4326)
# saveRDS(counties, "counties_shape.RDS")

counties <- readRDS("counties_shape.RDS")

# states <- states(cb = TRUE, resolution = "20m")
# states <- st_transform(states, 4326)
# saveRDS(states, "states_shape.RDS")

states <- readRDS("states_shape.RDS")

ds_results_ <- cfbd_game_info(year = yr, season_type = "both")
ds_results_ <- ds_results_ |>
  mutate(week = if_else(season_type == "postseason", max(week) + 1, as.numeric(week))) |>
  filter(week <= wk)

# Loading in base map
load("old_school_imperialism_base_map-3.Rdata")

# Moving Hawaii
# hawaii <- counties_grouped.os |>
#   filter(school == "Hawai'i") |>
#   sf::as_Spatial()
# hawaii <- elide(hawaii, shift = c(1000000, -1800000))
# 
# counties_grouped.os <- counties_grouped.os |>
#   filter(school != "Hawai'i") |>
#   as_Spatial()
#   
# proj4string(counties_grouped.os) <- proj4string(hawaii)
# counties_grouped.os <- rbind(counties_grouped.os, hawaii)
# 
# counties_grouped.os <- as(counties_grouped.os, "sf")

counties_grouped.os <- counties_grouped.os %>%
  # Have to change everything here too
  # TODO: replace this with something less cumbersome. Just write a function up top?
  mutate(
    color = if_else(school == "LSU", "#461D7C", color),
    color = if_else(school == "Iowa", "#FCD116", color),
    color = if_else(school == "SMU", "#354CA1", color),
    color = if_else(school == "TCU", "#ffffff", color),
    color = if_else(school == "Utah", "#CC0000", color),
    color = if_else(school == "UCLA", "#F2A900", color),
    color = if_else(school == "Miami (OH)", "black", color),
    color = if_else(school == "Wisconsin", "#FFFFFF", color),
    color = if_else(school == "Fresno State", "#13284c", color),
    color = if_else(school == "USC", "#990000", color),
    color = if_else(school == "UT San Antonio", "#F15A22", color),
    color = if_else(school == "Cincinnati", "#444444", color),
    color = if_else(school == "Washington State", "#5E6A71", color),
    color = if_else(school == "Tennessee", "#58595B", color),
    color = if_else(school == "Pittsburgh", "#FFB81C", color),
    color = if_else(school == "West Virginia", "#EAAA00", color),
    color = if_else(school == "Utah State", "#6890B8", color),
    color = if_else(school == "Minnesota", "#FFCC33", color),
    color = if_else(school == "California", "#FDB515", color),
    color = if_else(school == "San Diego State", "#FFFFFF", color),
    logos = if_else(school == "Oregon", "https://a.espncdn.com/i/teamlogos/ncaa/500-dark/2483.png", logos),
    logos = if_else(school == "Oklahoma", "https://a.espncdn.com/i/teamlogos/ncaa/500-dark/201.png", logos),
    logos = if_else(school == "Kansas State", "https://a.espncdn.com/i/teamlogos/ncaa/500-dark/2306.png", logos),
    logos = if_else(school == "Clemson", "https://a.espncdn.com/i/teamlogos/ncaa/500-dark/228.png", logos),
    logos = if_else(school == "Air Force", "https://a.espncdn.com/i/teamlogos/ncaa/500-dark/2005.png", logos),
    logos = if_else(school == "Nevada", "https://a.espncdn.com/i/teamlogos/ncaa/500-dark/2440.png", logos),
    logos = if_else(school == "Michigan State", "https://a.espncdn.com/i/teamlogos/ncaa/500-dark/127.png", logos),
    logos = if_else(school == "North Texas", "https://kuathletics.com/wp-content/uploads/2021/11/North_Texas_Mean_Green_logo.svg_.png", logos),
    school = if_else(school == "UT San Antonio", "UTSA", school)
  )

ds_results <- ds_results_ %>%
  filter(!is.na(home_points)) %>%
  mutate(
    winner = if_else(home_points > away_points, home_team, away_team),
    # winner = if_else(winner %in% ds_teams$school & winner != "UTSA", winner, paste(winner, "(FCS)")),
    loser = if_else(home_points < away_points, home_team, away_team)
  ) %>%
  dplyr::select(winner, loser) %>%
  left_join(., ds_teams %>% dplyr::select(school, logos, color, conference) %>% rename(winner = school), by = "winner") %>%
  rename(winner_logos = logos, winner_color = color) %>%
  mutate(
    # default logo for if the others are missing
    winner_logos = if_else(is.na(winner_logos), "https://www.ncaa.com/modules/custom/casablanca_core/img/sportbanners/football.svg", winner_logos)
  ) %>%
  distinct()

# This is the loop that iterates through each game result and gives the loser's land to the winner
# TODO: ideally I should only have to change the name, then have this point to a different df w/ the logo, color, etc. 
for(i in (1:nrow(ds_results))){
  
  counties_grouped.os <- counties_grouped.os %>%
    mutate(
      conference = if_else(school == ds_results$loser[i], ds_results$conference[i], conference),
      logos = if_else(school == ds_results$loser[i], ds_results$winner_logos[i], logos),
      school = if_else(school == ds_results$loser[i], ds_results$winner[i], school)
    )
  
  if(i %% (nrow(ds_results) / 4) == 0){
    print(paste0(
      round(100 * (i / nrow(ds_results)), 2), "% done...")
      )
  }
  
}

counties_grouped.os <- counties_grouped.os |>
  mutate(
    school = if_else(school == "UT San Antonio", "UTSA", school),
    logos = if_else(school == "UTSA", "http://a.espncdn.com/i/teamlogos/ncaa/500/2636.png", logos)
  )

# Summary dataframe
# counties_sum.os <- counties_grouped.os %>%
#   group_by(school) %>%
#   summarize(
#     n_territories = n(),
#     sum_total = sum(sum(sum_total, na.rm = T)),
#     sum_land = sum(sum(sum_land, na.rm = T)),
#     sum_water = sum(sum(sum_water, na.rm = T)),
#     sum_population = sum(sum(total_pop, na.rm = T)),
#     sum_n = sum(sum(n, na.rm = T))
#   ) %>%
#   left_join(ds_teams %>% dplyr::select(school, logos), by = "school") %>%
#   distinct()

# This is the actual map -------------------------------------------------------
library(mapview)

logoIcons.os <- icons(
  iconUrl = counties_grouped.os$logos,
  iconWidth = (as.numeric(log(st_area(counties_grouped.os))) - 21) * 25,
  iconHeight = (as.numeric(log(st_area(counties_grouped.os))) - 21) * 25
)

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

file_name <- paste0("imp-maps-", yr, "/imperialism-map-", yr, "-week-", wk, ".png")

mapshot(m, file = file_name, selfcontained = F)

# Adding label

img <- readPNG(file_name)
h <- as.numeric(dim(img)[1])
w <- as.numeric(dim(img)[2])

text_to_plot <- 
  tibble(x = 0.6,
         y = 0.93,
         text = paste0("CFB Imperialism Map - ",  yr, " season, Week ", wk))
final_img <- ggplot(data = text_to_plot) + 
  annotation_raster(img, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
  geom_text(aes(x = x, y= y, label = text), size = 1200, family = "Windows Command Prompt") +
  xlim(0,1) +
  ylim(0,1) +
  theme_map() 

# final_img

ggsave(file_name, plot = final_img, device = "png", width = w, height = h, limitsize = FALSE, dpi = 10)

print(paste("Week", wk, "complete!"))

}
