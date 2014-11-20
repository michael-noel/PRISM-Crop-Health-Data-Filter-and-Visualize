##############################################################################
# title         : Weed_maps.R;
# purpose       : Map weed data from PRISM;
# producer      : prepared by A. Sparks;
# last update   : in Suphanburi, Thailand, Oct 2014;
# inputs        : Aggregated PRISM data;
# outputs       : Maps of PRISM data;
# remarks 1     : ;
# Licence:      : GPL2;
##############################################################################

#### load packages ####
library(rgdal)
library(ggplot2)
library(RColorBrewer)
#### end load packages ####

#### Load data for mapping
source("Filter_Aggregator_Data.R")

PHL <- readOGR(dsn = "Data", layer = "PHL_NE_50m")
PHL.fortify <- fortify(PHL)
map <- ggplot(PHL.fortify) + geom_map(map = PHL.fortify, aes(x = long, y = lat, map_id = id), fill = "#333333")
#### End load data ####

#### Begin mapping #####
# Bakanae
map + geom_point(data = weedabove.summary, aes(x = lon, y = lat, size = rating, colour = rating)) + 
  scale_size_continuous("Median\nWeed\nAbove\nRating", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Median\nWeed\nAbove\nRating") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Weeds Above Canopy") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Injury_Graphs/Weed_above_map.png", width = 8, height = 8, units = "in")

# BLB
map + geom_point(data = weedbelow.summary, aes(x = lon, y = lat, size = rating, colour = rating)) + 
  scale_size_continuous("Median\nWeed\nBelow\nRating", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Median\nWeed\nBelow\nRating") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Weed Below the Canopy") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Injury_Graphs/Weed_below_map.png", width = 8, height = 8, units = "in")

# Grass
map + geom_point(data = grass.summary, aes(x = lon, y = lat, size = rating, colour = rating)) + 
  scale_size_continuous("Median\nGrassy\nWeed\nRating", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Median\nGrassy\nWeed\nRating") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Grasses") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Injury_Graphs/Grassy_weed_map.png", width = 8, height = 8, units = "in")

# Broadleaf weed
map + geom_point(data = broadleaf.summary, aes(x = lon, y = lat, size = rating, colour = rating)) + 
  scale_size_continuous("Median\nBroadleaf\nWeed\nRanking", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Median\nBroadleaf\nWeed\nRanking") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Broadleaf Weeds") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Injury_Graphs/Broadleaf_weed_map.png", width = 8, height = 8, units = "in")

# Sedge
map + geom_point(data = sedge.summary, aes(x = lon, y = lat, size = rating, colour = rating)) + 
  scale_size_continuous("Median\nSedge\nWeed\nRank", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Median\nSedge\nWeed\nRank") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Sedge Ranking") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Injury_Graphs/Sedge_map.png", width = 8, height = 8, units = "in")

# Small weeds
map + geom_point(data = small.summary, aes(x = lon, y = lat, size = rating, colour = rating)) + 
  scale_size_continuous("Median\nSmall\nWeed\nRanking", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Median\nSmall\nWeed\nRanking") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Small Weed Ranking") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Injury_Graphs/Small_weed_map.png", width = 8, height = 8, units = "in")

#### End mapping ####

# eos
