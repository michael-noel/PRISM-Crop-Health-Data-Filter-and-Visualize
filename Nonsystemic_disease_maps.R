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

source("Filter_Aggregator_Data.R")

PHL <- getData("GADM", country = "PHL", level = 3)

PHL.extent <- extent(PHL)
bak.raster <- raster(as.numeric(bak[, 12]))
bak.raster <- rasterize(bak, PHL, field = "injury", fun = "median")
bak.map <- rasterToPolygons()

# eos


s100 <- matrix(c(267573.9, 2633781, 213.29545, 262224.4, 2633781, 69.78261, 263742.7, 2633781, 51.21951, 259328.4, 2633781, 301.98413, 264109.8, 2633781, 141.72414, 255094.8, 2633781, 88.90244),  ncol=3,  byrow=TRUE)
colnames(s100) <- c('X', 'Y', 'Z')

library(raster)
# set up an 'empty' raster, here via an extent object derived from your data
e <- extent(s100[,1:2])
e <- e + 1000 # add this as all y's are the same

r <- raster(e, ncol=10, nrow=2)
# or r <- raster(xmn=, xmx=,  ...

# you need to provide a function 'fun' for when there are multiple points per cell
x <- rasterize(s100[, 1:2], r, s100[,3], fun=mean)
plot(x)