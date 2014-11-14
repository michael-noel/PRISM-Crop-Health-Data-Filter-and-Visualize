##############################################################################
# title         : Cleaned_Data_for_Regions.R;
# purpose       : Create csv files of filtered data submissions for each region;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Laguna, PHL, Nov 2014;
# inputs        : Filterd PRISM data;
# outputs       : CSV files of Filtered PRISM data for regions;
# remarks 1     : ;
# Licence:      : GPL2;
##############################################################################

source("Filter_Aggregator_Injury_Data.R")

#### Summarise Data ####
injuries <- data.frame(PRISM[, c(1, 8:9, 12, 16:19, 22:30)], 
                       tillers, leaves, panicles,
                       bak[, 9], blb[, 9], bls[, 9], bst[, 9], dip[, 9], fsm[, 9], grs[, 8], nba[, 9], nbs[, 9], olf[, 8], rgd[, 8], rsp[, 9], shr[, 9], shb[, 9], str[, 9], tun[, 8], ylo[, 8], 
                       gas[, 9], rat[, 9], 
                       bbn[, 8], hbn[, 8], 
                       weedbelow[, 8], weedabove[, 8], broadleaf[, 8], grass[, 8], sedge[, 8], small[, 8], 
                       lfd[, 9], lfm[, 9], thp[, 9], whm[, 9], def[, 9], wht[, 9], rbg[, 9], rgb[, 9], dht[, 9])

names(injuries) <- c("SubmissionDate", 
                     "lat", "lon", "locID", 
                     "Municipality", "Province", "Region",
                     "ObserverName", 
                     "PlantingMethod", "Variety", 
                     "CropGrowthStage", "WaterStatus", "OverallCropCanopy", "FoliageColor", "CropDensity", "OverallSCroptatus", "CropStatusRating",
                     "Tillers", "Leaves", "Panicles",
                     "Bakanae", "Bacterial Leaf Blight", "Bacterial Leaf Streak", "BrownSpot", "DirtyPanicle", "FalseSmut", "GrassyStunt", "NeckBlast", "NarrowBrownSpot", "OrangeLeaf", "RaggesdStunt", "RedStripe", "SheathRot", "SheathBlight", "StemRot", "Tungro", "YellowDwarf",
                     "GoldenAppleSnail", "Rat",
                     "Bug Burn", "Hopper Burn",
                     "Weedbelow", "Weedabove", "Broadleaf", "Grass", "Sedge", "Small",
                     "LeafFolder", "LeafMiner", "Thrip", "WhorlMaggot", "Defoliator", "WhiteHead", "RiceBug", "RiceGrainBug", "DeadHeart")
#### End Summarising ####

#### Export Data ####
## Raw Data ##
write.csv(arrange(subset(PRISM, Region == "III"), Municipality, SubmissionDate), "Crop Health Data for Regions/RegionIII_Raw.csv", row.names = FALSE)
write.csv(arrange(subset(PRISM, Region == "IV-B"), Municipality, SubmissionDate), "Crop Health Data for Regions/RegionIV-B_Raw.csv", row.names = FALSE)
write.csv(arrange(subset(PRISM, Region == "V"), Municipality, SubmissionDate), "Crop Health Data for Regions/RegionV_Raw.csv", row.names = FALSE)
write.csv(arrange(subset(PRISM, Region == "VI"), Municipality, SubmissionDate), "Crop Health Data for Regions/RegionVI_Raw.csv", row.names = FALSE)
write.csv(arrange(subset(PRISM, Region == "VII"), Municipality, SubmissionDate), "Crop Health Data for Regions/RegionVII_Raw.csv", row.names = FALSE)
write.csv(arrange(subset(PRISM, Region == "VIII"), Municipality, SubmissionDate), "Crop Health Data for Regions/RegionVIII_Raw.csv", row.names = FALSE)
write.csv(arrange(subset(PRISM, Region == "CAR"), Municipality, SubmissionDate), "Crop Health Data for Regions/RegionCAR_Raw.csv", row.names = FALSE)

## End Raw Data ##

## Summarised Data ##
write.csv(arrange(subset(injuries, Region == "III"), Municipality, SubmissionDate), "Crop Health Data for Regions/RegionIII_Summary.csv", row.names = FALSE)
write.csv(arrange(subset(injuries, Region == "IV-B"), Municipality, SubmissionDate), "Crop Health Data for Regions/RegionIV-B_Summary.csv", row.names = FALSE)
write.csv(arrange(subset(injuries, Region == "V"), Municipality, SubmissionDate), "Crop Health Data for Regions/RegionV_Summary.csv", row.names = FALSE)
write.csv(arrange(subset(injuries, Region == "VI"), Municipality, SubmissionDate), "Crop Health Data for Regions/RegionVI_Summary.csv", row.names = FALSE)
write.csv(arrange(subset(injuries, Region == "VII"), Municipality, SubmissionDate), "Crop Health Data for Regions/RegionVII_Summary.csv", row.names = FALSE)
write.csv(arrange(subset(injuries, Region == "VIII"), Municipality, SubmissionDate), "Crop Health Data for Regions/RegionVIII_Summary.csv", row.names = FALSE)
write.csv(arrange(subset(injuries, Region == "CAR"), Municipality, SubmissionDate), "Crop Health Data for Regions/RegionCAR_Summary.csv", row.names = FALSE)
## End Summarised Data ##

#### End Data Export ####

#eos
