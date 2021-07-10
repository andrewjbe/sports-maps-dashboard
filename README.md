# sports-maps-dashboard
A simple interactive dashboard for displaying maps related to NCAAM Basketball, Football, and other sports. 

Link to deployed NCAA Basketball map: https://werdna63.shinyapps.io/ncaa-maps/
Link to deployed NCAA Football map: https://werdna63.shinyapps.io/ncaa-football-dashboard/

The main map pulls data from ESPN and Kenpom.com to create a "power projection map". Each county is controlled by the most powerful nearby school (each school's strength in each county is calculated by dividing the team's Adj. EM score by the distance between the school and the closest county border), with only the top 30 teams able to own land in order to keep things simple.
