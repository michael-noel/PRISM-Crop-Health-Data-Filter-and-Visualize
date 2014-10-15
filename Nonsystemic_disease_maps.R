##############################################################################
# title         : Nonsystemic_disease_maps.R;
# purpose       : Map nonsystemic disease data from PRISM;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Laguna, PHL, Oct 2014;
# inputs        : Aggregated PRISM data;
# outputs       : Maps of PRISM data;
# remarks 1     : ;
# Licence:      : GPL2;
##############################################################################
library(rgdal)
library(ggplot2)
library(raster)
library(plyr)
library(doBy)
library(RColorBrewer)

source("Filter_Aggregator_Data.R")

locID.map <- readOGR(dsn = "Data/", layer = "locID")
map.fortify <- fortify(locID.map, region = "NAME_2")
names(map.fortify) <- c("long", "lat", "order", "hole", "piece", "group", "Municipality")

PHL <- getData("GADM", country = "PHL", level = 0)
PHL.fortify <- fortify(PHL)

ggplot(PHL.fortify) +
  geom_map(map = PHL.fortify, aes(x = long, y = lat, map_id = id), fill = "#333333") +
  geom_point(data = bst.summary, aes(x = lon, y = lat, size = perc.injury, colour = perc.injury)) + scale_size_continuous(range = c(3, 14), "Percent Incidence") +
  scale_colour_gradientn(colours = brewer.pal(7, "YlOrRd"), "Percent Incidence") + 
  guides(size = "none") +
  ggtitle("BLB") +
  coord_map() +
  facet_grid(. ~ visit)
  

# eos
