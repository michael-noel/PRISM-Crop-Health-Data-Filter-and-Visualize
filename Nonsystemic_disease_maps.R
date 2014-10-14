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

source("Filter_Aggregator_Data.R")

locID.map <- readOGR(dsn = "Data/", layer = "locID")
map.fortify <- fortify(locID.map, region = "NAME_2")
names(map.fortify) <- c("long", "lat", "order", "hole", "piece", "group", "Municipality")

PHL <- getData("GADM", country = "PHL", level = 0)
PHL.fortify <- fortify(PHL)

blb.map <- join(blb.summary, map.fortify, by = "Municipality")

ggplot(PHL.fortify) +
  geom_map(map = PHL.fortify, aes(x = long, y = lat, map_id = id), fill = "#666666") +
  geom_polygon(data = blb.map, aes(x = long, y = lat, fill = injury, colour = injury), alpha = 0.65) +
  coord_map() +
  facet_grid(. ~ visit)
  

# eos
