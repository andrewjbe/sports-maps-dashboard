# sports-maps-dashboard
A simple interactive dashboard for displaying maps related to NCAAM Basketball, Football, and other sports. 

* Link to deployed NCAA Basketball map: https://werdna63.shinyapps.io/ncaa-maps/

* Link to deployed NCAA Football map: https://werdna63.shinyapps.io/ncaa-football-dashboard/

# Basketball Map

The main map pulls data from ESPN and Kenpom.com to create a "power projection map". Each county is controlled by the most powerful nearby school (each school's strength in each county is calculated by dividing the team's Adj. EM score by the distance between the school and the closest county border), with only the top 30 teams able to own land in order to keep things simple.

# Football Map

The dashboard includes two main pages. The first is the "imperialism map", wherein each team begins with the closest counties to their stadium and each game's loser cedes any land they hold to the game's winner. Currently, it only shows the most recent version of the map. 

The 'power projection' map pulls data from the [CFBFastR Package](https://saiemgilani.github.io/cfbfastR/index.html) to create a "power projection map". Each county is controlled by the most powerful nearby school (each school's strength in each county is calculated by dividing the team's point total in the latest AP Poll by the distance between the school and the closest county border). Thus, teams who get more respect in the AP Poll will have stronger power projection scores and a greater "reach", while teams that slide in the polls will find their lands dwindling.

The code for the football dashboard is `ncaa-football-dashboard.Rmd` . This is dependent on data in `counties_pop.csv`, `poll_lsit.csv`, `counties_shape.RDS`, and `states_shape.RDS`, however, and requires a CFB API key to run (see here: https://saiemgilani.github.io/cfbfastR/reference/register_cfbd.html). 

# Betting

The `/betting/` folder has some snippets of code related to comparing CFB team performance to betting lines. `/scripts/get_betting_record.R` contains a script with a function that returns a cleaned betting "scorecard" for every team in a given year, and has the option to exclude games including FCS teams.
