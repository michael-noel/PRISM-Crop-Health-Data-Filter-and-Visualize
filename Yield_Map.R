##############################################################################
# title         : Yield_Map.R;
# purpose       : generate maps of yield data gathered as part of PRISM crop cuts;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Laguna, PI, Nov. 2014;
# inputs        : Filtered PRISM data;
# outputs       : maps of yield data;
# remarks 1     : ;
# Licence:      : GPL2;
##############################################################################

#### load packages ####
library(rgdal)
library(ggplot2)
library(RColorBrewer)
library(doBy)
#### end load packages ####

#### Load data for mapping
source("Filter_Aggregator_Yield_Data.R") # yield data, returns "yield" data frame we use for map
source("Filter_Aggregator_Injury_Data.R") # pest injury data with Lon/Lat data for mapping

PHL <- readOGR(dsn = "Data", layer = "PHL_NE_50m")
PHL.fortify <- fortify(PHL)
map <- ggplot(PHL.fortify) + geom_map(map = PHL.fortify, aes(x = long, y = lat, map_id = id), fill = "#333333")
#### End load data ####

out <- join(yield, PRISM, by = "locID", type = "left") # join the two datasets to take the lat/lon from injuries only for locIDs with yield information

yield.summary <- summaryBy(Yield+gps1.Latitude+gps1.Longitude~Municipality, data = out, FUN = median, na.rm = TRUE) # Create a summary data frame to generate the map

map + geom_point(data = yield.summary, aes(x = gps1.Longitude.median, y =  gps1.Latitude.median, size = Yield.median, colour = Yield.median)) + 
  scale_size_continuous("Yield\n(Tons/ha)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Yield\n(Tons/ha)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("2014 Wet Season Observed Yield") +
  coord_map()
ggsave("Injury_Graphs/Yield_Map.png", width = 8, height = 8, units = "in")

#eos
