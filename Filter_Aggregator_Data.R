##############################################################################
# title         : Filter_Aggregator_Data.R;
# purpose       : Filter PRISM data as pulled from ODK Aggregator;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Laguna, PHL, Oct 2014;
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
library(maptools)
#### end load packages ####

# CAR     August 18-22, 2014 (Joey)
# Reg3    August 11-15 (c/o PhilRice)
# Reg4B   August 11-15, 2014 (Nancy)
# Reg5    July 7-11 (c/o PhilRice)
# Reg6    May 26-30, 2014 (Joey and Hannah)
# Reg7    August 4-8, 2014 (Joey) 
# Reg8    August 11-15. 2014 (Joey)

PRISM <- read.csv("~/Google Drive/tmp/PRISM_Crop_and_Injuries_V1_0_results.csv")

PRISM[, 2] <- as.character(substr(PRISM[, 2], 1, 10))
PRISM[, 3] <- as.character(substr(PRISM[, 3], 1, 10))
PRISM[, 4] <- as.character(substr(PRISM[, 4], 1, 10))
PRISM[, 16] <- as.character(PRISM[, 16])
PRISM[, 17] <- as.character(PRISM[, 17])
PRISM[, 18] <- as.character(PRISM[, 18])
PRISM[, 22] <- as.character(PRISM[, 22])

#### Name the columns for easier work ####
names(PRISM)[names(PRISM) == "group_contact.village_name"] <- "Barangay"
names(PRISM)[names(PRISM) == "group_contact.town_name"] <- "Municipality"
names(PRISM)[names(PRISM) == "group_contact.province_name"] <- "Province"
names(PRISM)[names(PRISM) == "group_contact.region_name"] <- "Region"

#### Remove more training events and other misc that are not real data ####
PRISM <- subset(PRISM, start >= "2014-07-31") # No observations were taken before this date, safe to remove all these data
PRISM <- subset(PRISM, start != "2014-09-18" & start != "2014-09-17" & start != "2014-09-21") # IRRI Training Event
PRISM <- subset(PRISM, Province != "J")
PRISM <- subset(PRISM, Province != "X")

# CAR Training Event
PRISM <- sqldf("Select * from PRISM WHERE Province NOT IN ('Kalinga', 'Rizal') OR datetime NOT IN ('2014-08-07', '2014-08-18', '2014-08-19', '2014-08-20', '2014-08-21', '2014-08-22')")

#Region VII Training Event
PRISM <- sqldf("Select * from PRISM WHERE Province NOT IN ('Bohol') OR datetime NOT IN ('2014-08-06', '2014-08-07', '2014-08-08', '2014-08-09')")

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
PRISM[, 18][PRISM[, 18] == "Occ.mdo"] <- "Occidental Mindoro"
PRISM[, 18][PRISM[, 18] == "Occ. Mindoro"] <- "Occidental Mindoro"
PRISM[, 18][PRISM[, 18] == "Occ. Mindorp"] <- "Occidental Mindoro"
PRISM[, 18][PRISM[, 15] == "Burabod"] <- "Sorsogon" # Someone doesn't know the difference between a town and a province
PRISM[, 18][PRISM[, 16] == "Miluya"] <- "Sorsogon" # Someone doesn't know the difference between a barangay and a province
PRISM[, 18][PRISM[, 18] == "bohol"] <- "Bohol"
PRISM[, 18][PRISM[, 16] == "Babalag East"] <- "Kalinga" # Incorrectly labeled as Rizal Province

#### Rename the Municipalities to proper names ####
PRISM[, 17][PRISM[, 17] == "pilar"] <- "Pilar"
PRISM[, 17][PRISM[, 17] == "Sta.Cruz"] <- "Santa Cruz"
PRISM[, 17][PRISM[, 17] == "Sta. Cruz"] <- "Santa Cruz"
PRISM[, 17][PRISM[, 17] == "RIZAL"] <- "Rizal"
PRISM[, 17][PRISM[, 17] == "Tabuk city"] <- "Tabuk City"
PRISM[, 17][PRISM[, 17] == "San miguel"] <- "San Miguel"
PRISM[, 17][PRISM[, 17] == "Tabuk"] <- "Tabuk City"
PRISM[, 17][PRISM[, 17] == "Tabui"] <- "Tabuk City"
PRISM[, 17][PRISM[, 17] == "sablayan"] <- "Sablayan"
PRISM[, 17][PRISM[, 17] == "Sta.cruz"] <- "Santa Cruz"
PRISM[, 17][PRISM[, 17] == "Palangui"] <- "Polangui"
PRISM[, 17][PRISM[, 15] == "Burabod"] <- "Castilla" # Someone doesn't know the difference between a town and a province
PRISM[, 17][PRISM[, 16] == "Miluya"] <- "Castilla" # Someone doesn't know the difference between a baragnay and a Town
PRISM[, 17][PRISM[, 16] == "Babalag East"] <- "Rizal" # No municipality was given

#### Rename the regions to proper names ####
PRISM[, 19][PRISM[, 19] == "3"] <- "III"
PRISM[, 19][PRISM[, 19] == "5"] <- "IV-B"
PRISM[, 19][PRISM[, 19] == "6"] <- "V"
PRISM[, 19][PRISM[, 19] == "7"] <- "VI"
PRISM[, 19][PRISM[, 19] == "8"] <- "VII"
PRISM[, 19][PRISM[, 19] == "9"] <- "VIII"
PRISM[, 19][PRISM[, 19] == "16"] <- "CAR"
PRISM[, 19][PRISM[, 16] == "Babalag East"] <- "CAR"

#### Correct region numbers ####
PRISM <- within(PRISM, Region[Province == "Bohol"] <- "VII") # Fixes "NA" and "12" mis-entries

#### Santa Cruz, region IV-B has one visit incorrectly recorded as visit 2, when it's the first ####
tmp <- subset(PRISM, PRISM$Municipality == "Santa Cruz" & start == "2014-09-02")
tmp[, 22][tmp[, 22] == "2nd"] <- "1st"
PRISM <- PRISM[PRISM[, 17] != "Santa Cruz", ] 
PRISM <- rbind(PRISM, tmp)

# Some visits are incorrectly recorded, there are no third or fourth visits
PRISM[, 22][PRISM[, 22] == "4th"] <- "1st"
PRISM[, 22][PRISM[, 22] == "3rd"] <- "1st"

PRISM[, 22][PRISM[, 22] == "1st"] <- "Booting"
PRISM[, 22][PRISM[, 22] == "2nd"] <- "Ripening"

#### Merge the site ID columns ####
missing <- is.na(PRISM[, 12]) # create logical index for NAs in PRISM[, 12]
PRISM[, 12][missing] <- PRISM[, 13][missing] # replace NAs with values from PRISM[, 13]
PRISM <- PRISM[, -13] # drop column 13 now
PRISM[, 12] <- as.numeric(PRISM[, 12]) # convert numbers to numeric format to remove leading zeros and remove any NAs from the data
names(PRISM[, 12]) <- "locID"
PRISM <- subset(PRISM, !is.na(PRISM[, 12]))

#### There are errors in site ID numbers, these are the ones that can be corrected ####
PRISM[, 12][PRISM[, 12] == 537] <- "5037"
PRISM <- subset(PRISM, !is.na(PRISM[, 12])) # remove any records missing a location ID

#### Bohol has three munincipalities that combine into one
bohol <- subset(PRISM, Province == "Bohol")
bohol[, 16] <- bohol[, 17]
PRISM <- PRISM[PRISM[, 17] != "Bohol", ] 
PRISM <- rbind(PRISM, bohol)

##### Visit number one or two? #####
visit <- PRISM[, grep(pattern = "visitNo_label", colnames(PRISM), perl = TRUE)]
visit <- data.frame(PRISM[, c(2, 12, 15:18)], visit)

#### Growth stage ####
gs <- PRISM[, grep(pattern = "crop_stage", colnames(PRISM), perl = TRUE)]

#### Tiller, panicld and leaf counts ####
tiller <- apply(PRISM[, grep(pattern = "tiller_hill", colnames(PRISM), perl = TRUE)], 1, sum)
panicle <- apply(PRISM[, grep(pattern = "panicle_hill", colnames(PRISM), perl = TRUE)], 1, sum)
leaves <- apply(PRISM[, grep(pattern = "leaves_tiller", colnames(PRISM), perl = TRUE)], 1, sum)



#### generate data frames of non-systemic diseases, from 10 observations, for graphing ####
bak <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "bakanae", colnames(PRISM), perl = TRUE)], 1, sum))
blb <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "bacterialleafblight", colnames(PRISM), perl = TRUE)], 1, sum))
bls <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "bacterialleafstreak", colnames(PRISM), perl = TRUE)], 1, sum))
bst <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "(?<!narrow)(?i)brownspot", colnames(PRISM), perl = TRUE)], 1, sum))
fsm <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "falsesmut", colnames(PRISM), perl = TRUE)], 1, sum))
fsm <- na.omit(subset(fsm, fsm$visit.visit == "Ripening")) # no false smut before heading
dip <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "dirtypanicle", colnames(PRISM), perl = TRUE)], 1, sum))
dip <- na.omit(subset(dip, fsm$visit.visit == "Ripening")) # no dirty panicle before heading
lba <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "leafblast", colnames(PRISM), perl = TRUE)], 1, sum))
nba <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "neckblast", colnames(PRISM), perl = TRUE)], 1, sum))
nba <- na.omit(subset(nba, nba$visit.visit == "Ripening")) # no neck blast until second visit
nbs <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "narrowbrownspot", (colnames(PRISM)), perl = TRUE)], 1, sum))
lsc <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "leafscald", colnames(PRISM), perl = TRUE)], 1, sum))
rsp <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "redstripe", colnames(PRISM), perl = TRUE)], 1, sum))
shr <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "sheathrot", colnames(PRISM), perl = TRUE)], 1, sum))
shb <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "sheathblight", colnames(PRISM), perl = TRUE)], 1, sum))
str <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "stemrot", colnames(PRISM), perl = TRUE)], 1, sum))

names(bak) <- names(blb) <- names(bls) <- names(bst) <- names(fsm) <- names(dip) <- names(lba) <- names(nba) <- names(nbs) <- names(lsc) <- names(rsp) <- names(shr) <- names(shb) <- names(str) <- c("locID", "Municipality", "Province", "Region", "visit", "growth stage", "tiller", "panicle", "leaves", "injury")

#### generate data frames of snail and rat damage ####
gas <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, tiller, leaves, apply(PRISM[, grep(pattern = "area_gas", colnames(PRISM), perl = TRUE)], 1, sum))
rat <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, tiller, leaves, apply(PRISM[, grep(pattern = "pest_rat", colnames(PRISM), perl = TRUE)], 1, sum))
names(gas) <- names(rat) <- c("locID", "Municipality", "Province", "Region", "visit", "growth stage", "tiller", "leaves", "injury")

#### generate data frames of systemic diseases, snail and bug/hopper burn ####
bbn <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "bugburn", colnames(PRISM), perl = TRUE)], 1, sum))
hbn <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "hopperburn", colnames(PRISM), perl = TRUE)], 1, sum))

tun <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "tungro", colnames(PRISM), perl = TRUE)], 1, sum))
grs <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "grassy", colnames(PRISM), perl = TRUE)], 1, sum))
rgd <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "ragged", colnames(PRISM), perl = TRUE)], 1, sum))
olf <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "orangeleaf", colnames(PRISM), perl = TRUE)], 1, sum))
yld <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "yellowdwarf", colnames(PRISM), perl = TRUE)], 1, sum))

names(bbn) <- names(hbn) <- names(tun) <- names(grs) <- names(rgd) <- names(olf) <- names(yld) <- c("locID", "Municipality", "Province", "Region", "visit", "gs", "tiller", "panicle", "leaves", "injury")

#### generate data frames of weed data ####
weedabove <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, apply(PRISM[, grep(pattern = "weedabove_area", colnames(PRISM), perl = TRUE)], 1, mean))
weedbelow <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, apply(PRISM[, grep(pattern = "weedbelow_area", colnames(PRISM), perl = TRUE)], 1, mean))
broadleaf <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, apply(PRISM[, grep(pattern = "weed_broadleaved", colnames(PRISM), perl = TRUE)], 1, mean))
grasses <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, apply(PRISM[, grep(pattern = "weed_grass", colnames(PRISM), perl = TRUE)], 1, mean))
sedges <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, apply(PRISM[, grep(pattern = "weed_sedge", colnames(PRISM), perl = TRUE)], 1, mean))
small <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, apply(PRISM[, grep(pattern = "weed_small", colnames(PRISM), perl = TRUE)], 1, mean))
names(weedabove) <- names(weedbelow) <- names(broadleaf) <- names(grasses) <- names(sedges) <- names(small) <- c("locID", "Municipality", "Province", "Region", "visit", "gs", "rating")

#### Pest injuries ####
lfd <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "leaffolder", colnames(PRISM), perl = TRUE)], 1, sum))
lfm <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "leafminer", colnames(PRISM), perl = TRUE)], 1, sum))
thp <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "thrip", colnames(PRISM), perl = TRUE)], 1, sum))
whm <- data.frame(PRISM[, c(12, 16:18)], visit$visit, gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "whorl", colnames(PRISM), perl = TRUE)], 1, sum))

names(lfd) <- names(lfm) <- names(thp) <- names(whm) <- c("locID", "Municipality", "Province", "Region", "visit", "gs", "tiller", "panicle", "leaves", "injury")

#eos 

