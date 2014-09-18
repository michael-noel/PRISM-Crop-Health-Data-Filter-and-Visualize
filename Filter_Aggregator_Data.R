##############################################################################
# title         : Filter_Aggregator_Data.R;
# purpose       : Filter PRISM data as pulled from ODK Aggregator;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Philippines, Sep. 2014;
# inputs        : Raw PRISM data;
# outputs       : Filtered PRISM data;
# remarks 1     : ;
# Licence:      : GPL2;
##############################################################################

#### load packages ####
library(rgdal) # to load shape files
library(ggplot2) # for the map
library(raster) # for getData
library(reshape)
library(doBy)
library(sqldf)
#### end load packages ####

PHL <- getData("GADM", country = "PHL", level = 2)

# CAR     August 18-22, 2014 (Joey)
# Reg3    August 11-15 (c/o PhilRice)
# Reg4B   August 11-15, 2014 (Nancy)
# Reg5    July 7-11 (c/o PhilRice)
# Reg6    May 26-30, 2014 (Joey and Hannah)
# Reg7    August 4-8, 2014 (Joey) 
# Reg8    August 11-15. 2014 (Joey)

PRISM <- read.csv("~/Google Drive/tmp/PRISM_Crop_and_Injuries_V1_0_results.csv")
PRISM[, 2] <- as.Date(PRISM[, 2])
PRISM[, 3] <- as.Date(PRISM[, 3])
PRISM[, 4] <- as.Date(PRISM[, 4])
PRISM[, 16] <- as.character(PRISM[, 16])
PRISM[, 17] <- as.character(PRISM[, 17])
PRISM[, 18] <- as.character(PRISM[, 18])

names(PRISM)[names(PRISM) == "group_contact.village_name"] <- "Barangay"
names(PRISM)[names(PRISM) == "group_contact.town_name"] <- "Municipality"
names(PRISM)[names(PRISM) == "group_contact.province_name"] <- "Province"
names(PRISM)[names(PRISM) == "group_contact.region_name"] <- "Region"

PRISM <- subset(PRISM, start > "2014-07-31") # No observations were taken before this date, safe to remove all these data

#### Remove more training events
PRISM <- sqldf("SELECT * FROM PRISM WHERE Province NOT IN ('Kalinga', 'J', 'A', 'Rizal') AND datetime NOT IN ('2014-08-07', '2014-08-18', '2014-08-19', '2014-08-20', '2014-08-21', '2014-08-22')")
PRISM <- subset(PRISM, PRISM$Province != "Bohol" & datetime != '2014-08-06' | datetime != '2014-08-07' | datetime != '2014-08-08')

#### Rename the provinces to proper names ####
PRISM[, 18][PRISM[, 18] == "Camarines sur"] <- "Camarines Sur"
PRISM[, 18][PRISM[, 18] == "Cam.Sur"] <- "Camarines Sur"
PRISM[, 18][PRISM[, 18] == "Cam.sur"] <- "Camarines Sur"
PRISM[, 18][PRISM[, 18] == "Cam.Surm"] <- "Camarines Sur"
PRISM[, 18][PRISM[, 18] == "Cam. Sur"] <- "Camarines Sur"
PRISM[, 18][PRISM[, 18] == "Cam Sur"] <- "Camarines Sur"
PRISM[, 18][PRISM[, 18] == "Cam sur"] <- "Camarines Sur"
PRISM[, 18][PRISM[, 18] == "Cs"] <- "Camarines Sur"
PRISM[, 18][PRISM[, 18] == "Occ.Mindoro"] <- "Occidental Mindoro"
PRISM[, 18][PRISM[, 18] == "Occ.mindoro"] <- "Occidental Mindoro"
PRISM[, 18][PRISM[, 18] == "occidental mindoro"] <- "Occidental Mindoro"

#### Rename the Munincipalities to proper names ####
PRISM[, 17][PRISM[, 17] == "pilar"] <- "Pilar"
PRISM[, 17][PRISM[, 17] == "Sta.Cruz"] <- "Santa Cruz"
PRISM[, 17][PRISM[, 17] == "Sta. Cruz"] <- "Santa Cruz"
PRISM[, 17][PRISM[, 17] == "RIZAL"] <- "Rizal"
PRISM[, 17][PRISM[, 17] == "Tabuk city"] <- "Tabuk City"
PRISM[, 17][PRISM[, 17] == "San miguel"] <- "San Miguel"
PRISM[, 17][PRISM[, 17] == "Tabuk"] <- "Tabuk City"
PRISM[, 17][PRISM[, 17] == "Tabui"] <- "Tabuk City"
PRISM[, 17][PRISM[, 17] == "sablayan"] <- "Sablayan"

#### Rename the regions to proper names ####
PRISM[, 19][PRISM[, 19] == "3"] <- "III"
PRISM[, 19][PRISM[, 19] == "5"] <- "IV-B"
PRISM[, 19][PRISM[, 19] == "6"] <- "V"
PRISM[, 19][PRISM[, 19] == "7"] <- "VI"
PRISM[, 19][PRISM[, 19] == "8"] <- "VII"
PRISM[, 19][PRISM[, 19] == "9"] <- "VIII"
PRISM[, 19][PRISM[, 19] == "16"] <- "CAR"

#### Correct region numbers ####
PRISM <- within(PRISM, Region[Province == "Bohol"] <- "VII") # Fixes "NA" and "12" mis-entries

#### Santa Cruz, region IV-B has one visit incorrectly recorded as visit 2, when it's the first ####
tmp <- subset(PRISM, PRISM$Municipality == "Santa Cruz")
tmp[, 22][tmp[, 22] == "2nd"] <- "1st"
PRISM <- PRISM[PRISM[, 17] != "Santa Cruz", ] 
PRISM <- rbind(PRISM, tmp)

# Some visits are incorrectly recorded, there are no third or fourth visits
PRISM[, 22][PRISM[, 22] == "4th"] <- "1st"
PRISM[, 22][PRISM[, 22] == "3rd"] <- "1st"

#### Merge the site ID columns ####
missing <- is.na(PRISM[, 12]) # create logical index for NAs in PRISM[, 12]
PRISM[, 12][missing] <- PRISM[, 13][missing] # replace NAs with values from PRISM[, 13]
PRISM <- PRISM[, -13] # drop column 13 now

##### Visit number one or two? #####
visit <- PRISM[, grep(pattern = "visitNo_label", colnames(PRISM), perl = TRUE)]
visit <- data.frame(PRISM[, c(2, 12, 16:18)], visit)

#### Growth stage ####
gs <- PRISM[, grep(pattern = "crop_stage", colnames(PRISM), perl = TRUE)]

#### Tiller, panicl and leaf counts ####
tiller <- apply(PRISM[, grep(pattern = "tiller_hill", colnames(PRISM), perl = TRUE)], 1, sum)
panicle <- apply(PRISM[, grep(pattern = "panicle_hill", colnames(PRISM), perl = TRUE)], 1, sum)
leaves <- apply(PRISM[, grep(pattern = "leaves_tiller", colnames(PRISM), perl = TRUE)], 1, sum)

#### generate data frames of non-systemic diseases, from 10 observations, for graphing ####
bak <- data.frame(PRISM[, 16:18], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "bakanae", colnames(PRISM), perl = TRUE)], 1, sum))
blb <- data.frame(PRISM[, 16:18], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "bacterialleafblight", colnames(PRISM), perl = TRUE)], 1, sum))
bls <- data.frame(PRISM[, 16:18], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "bacterialleafstreak", colnames(PRISM), perl = TRUE)], 1, sum))
bst <- data.frame(PRISM[, 16:18], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "(?<!narrow)(?i)brownspot", colnames(PRISM), perl = TRUE)], 1, sum))
fsm <- data.frame(PRISM[, 16:18], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "falsesmut", colnames(PRISM), perl = TRUE)], 1, sum))
fsm <- na.omit(subset(fsm, fsm$visit == "2nd")) # no false smut before heading

dip <- data.frame(PRISM[, 16:18], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "dirtypanicle", colnames(PRISM), perl = TRUE)], 1, sum))
dip <- na.omit(subset(dip, fsm$visit == "2nd")) # no dirty panicle before heading

lba <- data.frame(PRISM[, 16:18], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "leafblast", colnames(PRISM), perl = TRUE)], 1, sum))
nba <- data.frame(PRISM[, 16:18], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "neckblast", colnames(PRISM), perl = TRUE)], 1, sum))
nba <- na.omit(subset(nba, nba$visit == "2nd")) # no neck blast

nbs <- data.frame(PRISM[, 16:18], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "narrowbrownspot", (colnames(PRISM)), perl = TRUE)], 1, sum))
lsc <- data.frame(PRISM[, 16:18], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "leafscald", colnames(PRISM), perl = TRUE)], 1, sum))
rsp <- data.frame(PRISM[, 16:18], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "redstripe", colnames(PRISM), perl = TRUE)], 1, sum))
shr <- data.frame(PRISM[, 16:18], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "sheathrot", colnames(PRISM), perl = TRUE)], 1, sum))
shb <- data.frame(PRISM[, 16:18], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "sheathblight", colnames(PRISM), perl = TRUE)], 1, sum))
str <- data.frame(PRISM[, 16:18], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "stemrot", colnames(PRISM), perl = TRUE)], 1, sum))

names(bak) <- names(blb) <- names(bls) <- names(bst) <- names(fsm) <- names(dip) <- names(lba) <- names(nba) <- names(nbs) <- names(lsc) <- names(rsp) <- names(shr) <- names(shb) <- names(str) <- c("Municipality", "Province", "Region", "visit", "growth stage", "tiller", "panicle", "leaves", "injury")

# summarize the above dataframes
bak <- summaryBy(injury+leaves~Municipality+Region+visit, fun = "mean", data = bak, keep.names = TRUE)
blb <- summaryBy(injury+leaves~Municipality+Region+visit, fun = "mean", data = blb, keep.names = TRUE)
bls <- summaryBy(injury+leaves~Municipality+Region+visit, fun = "mean", data = bls, keep.names = TRUE)
bst <- summaryBy(injury+leaves~Municipality+Region+visit, fun = "mean", data = bst, keep.names = TRUE)
fsm <- summaryBy(injury+panicle~Municipality+Region+visit, fun = "mean", data = fsm, keep.names = TRUE)
dip <- summaryBy(injury+panicle~Municipality+Region+visit, fun = "mean", data = dip, keep.names = TRUE)
lba <- summaryBy(injury+leaves~Municipality+Region+visit, fun = "mean", data = lba, keep.names = TRUE)
nba <- summaryBy(injury+panicle~Municipality+Region+visit, fun = "mean", data = nba, keep.names = TRUE)
nbs <- summaryBy(injury+leaves~Municipality+Region+visit, fun = "mean", data = nbs, keep.names = TRUE)
lsc <- summaryBy(injury+leaves~Municipality+Region+visit, fun = "mean", data = lsc, keep.names = TRUE)
rsp <- summaryBy(injury+leaves~Municipality+Region+visit, fun = "mean", data = rsp, keep.names = TRUE)
shb <- summaryBy(injury+tiller~Municipality+Region+visit, fun = "mean", data = shb, keep.names = TRUE)
str <- summaryBy(injury+tiller~Municipality+Region+visit, fun = "mean", data = str, keep.names = TRUE)

#### generate data frames of systemic diseases, snail and bug/hopper burn ####
gas <- data.frame(PRISM[, 16:18], visit$visit, gs, tiller, leaves, apply(PRISM[, grep(pattern = "gas", colnames(PRISM), perl = TRUE)], 1, sum))
gas <- na.omit(subset(gas, gas$visit == "1st")) # should not be any snail damage assessments at ripening
names(gas) <- c("Municipality", "Province", "Region", "visit", "growth stage", "tiller", "leaves", "injury")

bbn <- data.frame(PRISM[, 16:18], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "bugburn", colnames(PRISM), perl = TRUE)], 1, sum))
hbn <- data.frame(PRISM[, 16:18], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "hopperburn", colnames(PRISM), perl = TRUE)], 1, sum))

tun <- data.frame(PRISM[, 16:18], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "tugnro", colnames(PRISM), perl = TRUE)], 1, sum))
grs <- data.frame(PRISM[, 16:18], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "grassy", colnames(PRISM), perl = TRUE)], 1, sum))
rgd <- data.frame(PRISM[, 16:18], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "ragged", colnames(PRISM), perl = TRUE)], 1, sum))
olf <- data.frame(PRISM[, 16:18], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "orangeleaf", colnames(PRISM), perl = TRUE)], 1, sum))
yld <- data.frame(PRISM[, 16:18], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "yellowdwarf", colnames(PRISM), perl = TRUE)], 1, sum))

names(bbn) <- names(hbn) <- names(tun) <- names(grs) <- names(rgd) <- names(olf) <- names(yld) <- c("Municipality", "Province", "Region", "visit", "gs", "tiller", "panicle", "leaves", "injury")

# summarize the above dataframes
gas <- summaryBy(injury~Municipality+Province+Region, fun = "mean", keep.names = TRUE, data = gas)
bbn <- summaryBy(injury~Municipality+Province+Region+visit, fun = "mean", keep.names = TRUE, data = bbn)
hbn <- summaryBy(injury~Municipality+Province+Region+visit, fun = "mean", keep.names = TRUE, data = hbn)
tun <- summaryBy(injury~Municipality+Province+Region+visit, fun = "mean", keep.names = TRUE, data = tun)
grs <- summaryBy(injury~Municipality+Province+Region+visit, fun = "mean", keep.names = TRUE, data = grs)
rgd <- summaryBy(injury~Municipality+Province+Region+visit, fun = "mean", keep.names = TRUE, data = rgd)
olf <- summaryBy(injury~Municipality+Province+Region+visit, fun = "mean", keep.names = TRUE, data = olf)
yld <- summaryBy(injury~Municipality+Province+Region+visit, fun = "mean", keep.names = TRUE, data = yld)

#### generate data frames of weed data ####
weedabove <- data.frame(PRISM[, 16:18], visit$visit, gs, apply(PRISM[, grep(pattern = "weedabove_area", colnames(PRISM), perl = TRUE)], 1, mean))
weedbelow <- data.frame(PRISM[, 16:18], visit$visit, gs, apply(PRISM[, grep(pattern = "weedbelow_area", colnames(PRISM), perl = TRUE)], 1, mean))
broadleaf <- data.frame(PRISM[, 16:18], visit$visit, gs, apply(PRISM[, grep(pattern = "weed_broadleaved", colnames(PRISM), perl = TRUE)], 1, mean))
grasses <- data.frame(PRISM[, 16:18], visit$visit, gs, apply(PRISM[, grep(pattern = "weed_grass", colnames(PRISM), perl = TRUE)], 1, mean))
sedges <- data.frame(PRISM[, 16:18], visit$visit, gs, apply(PRISM[, grep(pattern = "weed_sedge", colnames(PRISM), perl = TRUE)], 1, mean))
small <- data.frame(PRISM[, 16:18], visit$visit, gs, apply(PRISM[, grep(pattern = "weed_small", colnames(PRISM), perl = TRUE)], 1, mean))
names(weedabove) <- names(weedbelow) <- names(broadleaf) <- names(grasses) <- names(sedges) <- names(small) <- c("Municipality", "Province", "Region", "visit", "gs", "rating")

# summarize the above dataframes
weedabove <- summaryBy(rating~Municipality+Province+Region+visit, fun = "mean", data = weedabove)
weedbelow <- summaryBy(rating~Municipality+Province+Region+visit, fun = "mean", data = weedbelow)
broadleaf <- summaryBy(rating~Municipality+Province+Region+visit, fun = "mean", data = broadleaf)
grasses <- summaryBy(rating~Municipality+Province+Region+visit, fun = "mean", data = grasses)
sedges <- summaryBy(rating~Municipality+Province+Region+visit, fun = "mean", data = sedges)
small <- summaryBy(rating~Municipality+Province+Region+visit, fun = "mean", data = small)

#eos 
