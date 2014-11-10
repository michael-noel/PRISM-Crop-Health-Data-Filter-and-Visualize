##############################################################################
# title         : Nonsystemic_disease_maps.R;
# purpose       : Map nonsystemic disease data from PRISM;
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
library(ggsubplot)
library(RColorBrewer)
#### end load packages ####

#### Load data for mapping
source("Filter_Aggregator_Injury_Data.R")

PHL <- readOGR(dsn = "Data", layer = "PHL_NE_50m")
PHL.fortify <- fortify(PHL)
map <- ggplot(PHL.fortify) + geom_map(map = PHL.fortify, aes(x = long, y = lat, map_id = id), fill = "#333333")
#### End load data ####

#### Begin mapping #####
# Bakanae
map + geom_point(data = bak.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Median\nTiller\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Median\nTiller\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Bakanae") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Graphs/Bakanae_map.png", width = 8, height = 8, units = "in")

# BLB
map + geom_point(data = blb.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Median\nLeaf\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Median\nLeaf\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Bacterial Leaf Blight") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Graphs/Bacterial_leaf_blight_map.png", width = 8, height = 8, units = "in")

# Brown Spot
map + geom_point(data = bst.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Median\nLeaf\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Median\nLeaf\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Brown Spot") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Graphs/Brown_spot_map.png", width = 8, height = 8, units = "in")

# BLS
map + geom_point(data = bls.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Median\nLeaf\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Median\nLeaf\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Bacterial Leaf Streak") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Graphs/Bacterial_leaf_streak_map.png", width = 8, height = 8, units = "in")
  
# False Smut
map + geom_point(data = fsm.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Median\nPanicle\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Median\nPanicle\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("False Smut") +
  coord_map()
ggsave("Graphs/False_smut_map.png", width = 8, height = 8, units = "in")

# Dirty Panicle
map + geom_point(data = dip.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Median\nPanicle\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Median\nPanicle\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Dirty Panicle") +
  coord_map()
ggsave("Graphs/Dirty_panicle_map.png", width = 8, height = 8, units = "in")

# Leaf Blast
map + geom_point(data = lba.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Median\nLeaf\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Median\nLeaf\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Leaf Blast") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Graphs/Leaf_blast_map.png", width = 8, height = 8, units = "in")

# Neck Blast
map + geom_point(data = nba.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Median\nPanicle\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Median\nPanicle\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Neck Blast") +
  coord_map()
ggsave("Graphs/Neck_blast_map.png", width = 8, height = 8, units = "in")

# Narrow Brown Spot
map + geom_point(data = nbs.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Median\nLeaf\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Median\nLeaf\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Narrow Brown Spot") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Graphs/Narrow_brown_spot_map.png", width = 8, height = 8, units = "in")

# Leaf Scald
map + geom_point(data = lsc.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Median\nLeaf\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Median\nLeaf\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Leaf Scald") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Graphs/Leaf_scald_map.png", width = 8, height = 8, units = "in")

# Red Stripe
map + geom_point(data = rsp.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Median\nLeaf\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Median\nLeaf\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Red Stripe") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Graphs/Red_stripe_map.png", width = 8, height = 8, units = "in")

# Sheath Rot
map + geom_point(data = shr.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Median\nTiller\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Median\nTiller\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Sheath Rot") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Graphs/Sheath_rot_map.png", width = 8, height = 8, units = "in")

# Sheath Blight
map + geom_point(data = shb.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Median\nTiller\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Median\nTiller\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Sheath Blight") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Graphs/Sheath_blight_map.png", width = 8, height = 8, units = "in")

# Stem Rot
map + geom_point(data = str.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + 
  scale_size_continuous("Median\nTiller\nIncidence (%)", range = c(3, 15)) +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Median\nTiller\nIncidence (%)") + 
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") + 
  guides(size = "none") +
  ggtitle("Stem Rot") +
  coord_map() +
  facet_grid(. ~ visit)
ggsave("Graphs/Stem_rot_map.png", width = 8, height = 8, units = "in")

#### End mapping ####

# eos
