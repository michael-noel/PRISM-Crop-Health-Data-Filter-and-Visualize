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
library("rgdal")
library("reshape2")

source("Filter_Aggregator_Data.R")

locID.map <- readOGR(dsn = "Data/", layer = "locID")
locID.ID <- sapply(slot(locID.map, "polygons"), function(x) slot(x, "ID"))

bak.map <- dcast(bak, locID ~ visit, value.var = "injury") # conver to wide using visit column
bak.map <- merge(bak.map, locID.map, by = "locID", all.y = TRUE) # right outer join to give us ALL the locIDs from locID.map so we can merge with the shapefile to map
bak.map <- bak.map[, -c(4:9)] # drop the extra columns

o <- match(bak.map$locID, locID.map$locID)
p <- bak.map[o, ]
row.names(p) <- locID.ID
bak.map <- spCbind(locID.map, p)


# eos