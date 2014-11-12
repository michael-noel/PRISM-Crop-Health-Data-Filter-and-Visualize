##############################################################################
# title         : Systemic_disease_maps.R;
# purpose       : Map systemic disease and insect injury data from PRISM;
# producer      : prepared by A. Sparks;
# last update   : in Bangkok, Thailand, Oct 2014;
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
source("Filter_Aggregator_Injury_Data.R")

PHL <- readOGR(dsn = "Data", layer = "PHL_NE_50m")
PHL.fortify <- fortify(PHL)
map <- ggplot(PHL.fortify) + geom_map(map = PHL.fortify, aes(x = long, y = lat, map_id = id), fill = "#333333")
#### End load data ####

#### Begin mapping #####

# Bug burn
map + geom_point(data = bbn.summary, aes(x = lon, y = lat, size = rating, colour = rating)) + 
  scale_size_continuous("Mean\nRating", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Mean\nRating") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Bug Burn") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Graphs/Bug_burn_map.png", width = 8, height = 8, units = "in")

# Hopper burn
map + geom_point(data = hbn.summary, aes(x = lon, y = lat, size = rating, colour = rating)) + 
  scale_size_continuous("Mean\nRating", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Mean\nRating") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Hopper Burn") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Graphs/Hopper_burn_map.png", width = 8, height = 8, units = "in")

# Tungro
map + geom_point(data = tun.summary, aes(x = lon, y = lat, size = rating, colour = rating)) + 
  scale_size_continuous("Mean\nRating", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Mean\nRating") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Tungro") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Graphs/Tungro_map.png", width = 8, height = 8, units = "in")

# Grassy stunt
map + geom_point(data = grs.summary, aes(x = lon, y = lat, size = rating, colour = rating)) + 
  scale_size_continuous("Mean\nRating", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Mean\nRating") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Grassy Stunt Virus") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Graphs/Grassy_stunt_map.png", width = 8, height = 8, units = "in")

# Ragged Stunt
map + geom_point(data = rgd.summary, aes(x = lon, y = lat, size = rating, colour = rating)) + 
  scale_size_continuous("Mean\nRating", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Mean\nRating") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Ragged Stunt Virus") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Graphs/Ragged_stunt_map.png", width = 8, height = 8, units = "in")

# Orange leaf
map + geom_point(data = olf.summary, aes(x = lon, y = lat, size = rating, colour = rating)) + 
  scale_size_continuous("Mean\nRating", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Mean\nRating") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Orange Leaf") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Graphs/Orange_leaf_map.png", width = 8, height = 8, units = "in")

# Yellow dwarf
map + geom_point(data = ylo.summary, aes(x = lon, y = lat, size = rating, colour = rating)) + 
  scale_size_continuous("Mean\nRating", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Mean\nRating") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Yellow Dwarf") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Graphs/Yellow_dwarf_map.png", width = 8, height = 8, units = "in")

#### End mapping ####

# eos

