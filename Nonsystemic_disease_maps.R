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

PHL <- getData("GADM", country = "PHL", level = 0)
PHL.fortify <- fortify(PHL)

bak.map <- join(bak, locID.map, by = "locID")

ggplot() +
  geom_map(data = bak, aes(map_id = locID, fill = value)) +
  facet_grid(. ~ visit) +
  coord_map()

# eos

http://stackoverflow.com/questions/22096787/how-keep-information-from-shapefile-after-fortify

library(plyr)      # for join(...)
library(rgdal)     # for readOGR(...)
library(ggplot2)   # for fortify(...)

mapa <- readOGR(dsn=".",layer="shapefile name w/o .shp extension")
map@data$id <- rownames(mapa@data)
mapa@data   <- join(mapa@data, data, by="CD_GEOCODI")
mapa.df     <- fortify(mapa)
mapa.df     <- join(mapa.df,mapa@data, by="id")

