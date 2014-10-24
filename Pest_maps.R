##############################################################################
# title         : Pest_maps.R;
# purpose       : Map insect injury data from PRISM;
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
# Leaffolder
map + geom_point(data = lfd.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Median\nLeaffolder\nInjury\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Median\nLeaffolder\nInjury\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Leaffolder Injury Incidence") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Graphs/Leaffolder_map.png", width = 8, height = 8, units = "in")

# Leafminer
map + geom_point(data = lfm.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Median\nLeafminer\nInjury\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Median\nLeafminer\nInjury\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Leafminer Injury Incidence") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Graphs/Leafminer_map.png", width = 8, height = 8, units = "in")

# Thrips
map + geom_point(data = thp.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Median\nThrip\nInjury\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Median\nThrip\nInjury\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Thrip Injury Incidence") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Graphs/Thrip_map.png", width = 8, height = 8, units = "in")

# Whorl Maggot
map + geom_point(data = whm.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Median\nWhorl\nMaggot\nInjury\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Median\nWhorl \nMaggot\nInjury\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Whorl Maggot Incidence") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Graphs/Whorl_maggot_map.png", width = 8, height = 8, units = "in")

# Other defoliator
map + geom_point(data = def.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Median\nOther\nDefoliator\nInjury\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Median\nOther\nDefoliator\nInjury\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Other Defoliator Incidence") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Graphs/Other_defoliator_map.png", width = 8, height = 8, units = "in")

# White head
map + geom_point(data = wht.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Median\nWhite\nHead\nInjury\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Median\nWhite\nHead\nInjury\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("White Head Incidence") +
  coord_map() +
ggsave("Graphs/White_head_map.png", width = 8, height = 8, units = "in")

# Rice grain bug
map + geom_point(data = rgb.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Median\nRice\nGrain\nBug\nInjury\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Median\nRice\nGrain\nBug\nInjury\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Rice Grain Bug Incidence") +
  coord_map() +
ggsave("Graphs/Rice_grain_bug_map.png", width = 8, height = 8, units = "in")

# Rice bug
map + geom_point(data = rbg.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Median\nRice\nBug\nInjury\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Median\nRice\nBug\nInjury\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Rice Bug Incidence") +
  coord_map() +
  ggsave("Graphs/Rice_bug_map.png", width = 8, height = 8, units = "in")

# Dead heart
map + geom_point(data = dht.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Median\nDead\nHeart\nInjury\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Median\nDead\nHeart\nInjury\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Dead Heart Incidence") +
  coord_map() +
  facet_grid(. ~ visit)
  ggsave("Graphs/Dead_heart_map.png", width = 8, height = 8, units = "in")

#### End mapping ####

# eos
