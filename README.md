# sports-maps-dashboard
A simple interactive dashboard for displaying maps related to NCAAM Basketball, Football, and other sports. 

The main map pulls data from ESPN and Kenpom.com to create a "power projection map". Each county is controlled by the most powerful nearby school (each school's strength in each county is calculated by dividing the team's Adj. EM score by the distance between the school and the closest county border), with only the top 30 teams able to own land in order to keep things simple.
