##############################################################################
# title         : Pest_maps.R;
# purpose       : Map insect injury data from PRISM;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Philippines, Nov. 2014;
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
# Leaffolder
map + geom_point(data = lfd.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Mean\nLeaffolder\nInjury\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Mean\nLeaffolder\nInjury\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Leaffolder Injury Incidence") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Graphs/Leaffolder_map.png", width = 8, height = 8, units = "in")

# Leafminer
map + geom_point(data = lfm.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Mean\nLeafminer\nInjury\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Mean\nLeafminer\nInjury\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Leafminer Injury Incidence") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Graphs/Leafminer_map.png", width = 8, height = 8, units = "in")

# Thrips
map + geom_point(data = thp.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Mean\nThrip\nInjury\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Mean\nThrip\nInjury\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Thrip Injury Incidence") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Graphs/Thrip_map.png", width = 8, height = 8, units = "in")

# Whorl Maggot
map + geom_point(data = whm.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Mean\nWhorl\nMaggot\nInjury\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Mean\nWhorl \nMaggot\nInjury\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Whorl Maggot Incidence") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Graphs/Whorl_maggot_map.png", width = 8, height = 8, units = "in")

# Other defoliator
map + geom_point(data = def.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Mean\nOther\nDefoliator\nInjury\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Mean\nOther\nDefoliator\nInjury\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Other Defoliator Incidence") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Graphs/Other_defoliator_map.png", width = 8, height = 8, units = "in")

# White head
map + geom_point(data = wht.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Mean\nWhite\nHead\nInjury\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Mean\nWhite\nHead\nInjury\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("White Head Incidence") +
  coord_map() +
ggsave("Graphs/White_head_map.png", width = 8, height = 8, units = "in")

# Rice grain bug
map + geom_point(data = rgb.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Mean\nRice\nGrain\nBug\nInjury\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Mean\nRice\nGrain\nBug\nInjury\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Rice Grain Bug Incidence") +
  coord_map() +
ggsave("Graphs/Rice_grain_bug_map.png", width = 8, height = 8, units = "in")

# Rice bug
map + geom_point(data = rbg.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Mean\nRice\nBug\nInjury\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Mean\nRice\nBug\nInjury\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Rice Bug Incidence") +
  coord_map() +
  ggsave("Graphs/Rice_bug_map.png", width = 8, height = 8, units = "in")

# Dead heart
map + geom_point(data = dht.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Mean\nDead\nHeart\nInjury\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Mean\nDead\nHeart\nInjury\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Dead Heart Incidence") +
  coord_map() +
  facet_grid(. ~ visit)
  ggsave("Graphs/Dead_heart_map.png", width = 8, height = 8, units = "in")

# Rat damage
map + geom_point(data = rat.summary, aes(x = lon.mean, y = lat.mean, size = injury.mean, colour = injury.mean)) + 
  scale_size_continuous("Mean\nRat\nDamage\nIncidence\nat Hill\nLevel (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Mean\nRat\nDamage\nIncidence\nat Hill\nLevel (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Rat Damage Incidence") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Graphs/Rat_Map.png", width = 8, height = 8, units = "in")

# golden apple snail damage
map + geom_point(data = gas.summary, aes(x = lon.mean, y = lat.mean, size = injury.mean, colour = injury.mean)) + 
  scale_size_continuous("Mean\nSnail\nDamage\nIncidence\nat Hill\nLevel (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Mean\nSnail\nDamage\nIncidence\nat Hill\nLevel (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Snail Damage Incidence") +
  coord_map()
ggsave("Graphs/GAS_Map.png", width = 8, height = 8, units = "in")
#### End mapping ####

# eos
