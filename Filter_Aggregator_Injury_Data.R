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
library(reshape)
library(doBy)
library(sqldf)
library(plyr)
library(lubridate)
#### end load packages ####

# CAR     August 18-22, 2014 (Joey)
# Reg3    August 11-15 (c/o PhilRice)
# Reg4B   August 11-15, 2014 (Nancy)
# Reg5    July 7-11 (c/o PhilRice)
# Reg6    May 26-30, 2014 (Joey and Hannah)
# Reg7    August 4-8, 2014 (Joey) 
# Reg8    August 11-15. 2014 (Joey)

PRISM <- read.csv("~/Google Drive/tmp/PRISM_Injuries.csv")
PRISM <- PRISM[, -2] # drop formhub uuid, not needed

PRISM[, 1] <- parse_date_time(PRISM[, 1], "b d, Y I:M:S p") # use lubridate to filter out duplicate entries
PRISM[, 2] <- as.character(as.Date(PRISM[, 2], "%b %d, %Y")) # these are just character, use for subsetting
PRISM[, 3] <- as.character(as.Date(PRISM[, 3], "%b %d, %Y")) # these are just character, use for subsetting
PRISM[, 4] <- as.character(as.Date(PRISM[, 4], "%b %d, %Y"))# these are just character, use for subsetting
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

#### Merge the site ID columns ####
missing <- is.na(PRISM[, 12]) # create logical index for NAs in PRISM
PRISM[, 13] <- as.numeric(as.character(PRISM[, 13])) # convert column 13 to class numeric for merging with column 12
PRISM[, 12][missing] <- PRISM[, 13][missing] # replace NAs with values from PRISM[, 13]
PRISM <- PRISM[, -13] # drop column 13 now
names(PRISM)[12] <- "locID"
PRISM <- subset(PRISM, !is.na(PRISM[, 12])) # remove any records missing a location ID

PRISM[, 12][PRISM[, 12] == 537] <- "5037" # There are errors in site ID numbers, these are the ones that can be corrected
PRISM[, 12][PRISM[, 12] == 98]  <- 5061 # There is no locID 98, this is the observation for 5061 at booting stage 

#### Rename the provinces to proper names ####
PRISM[, 17][PRISM[, 17] == "Camarines sur"] <- "Camarines Sur"
PRISM[, 17][PRISM[, 17] == "Cam.Sur"] <- "Camarines Sur"
PRISM[, 17][PRISM[, 17] == "Cam.sur"] <- "Camarines Sur"
PRISM[, 17][PRISM[, 17] == "Cam.Surm"] <- "Camarines Sur"
PRISM[, 17][PRISM[, 17] == "Cam. Sur"] <- "Camarines Sur"
PRISM[, 17][PRISM[, 17] == "Cam Sur"] <- "Camarines Sur"
PRISM[, 17][PRISM[, 17] == "Cam sur"] <- "Camarines Sur"
PRISM[, 17][PRISM[, 17] == "Cs"] <- "Camarines Sur"
PRISM[, 17][PRISM[, 17] == "Occ.Mindoro"] <- "Occidental Mindoro"
PRISM[, 17][PRISM[, 17] == "Occ.mindoro"] <- "Occidental Mindoro"
PRISM[, 17][PRISM[, 17] == "occidental mindoro"] <- "Occidental Mindoro"
PRISM[, 17][PRISM[, 17] == "Occ.mdo"] <- "Occidental Mindoro"
PRISM[, 17][PRISM[, 17] == "Occ. Mindoro"] <- "Occidental Mindoro"
PRISM[, 17][PRISM[, 17] == "Occ. Mindorp"] <- "Occidental Mindoro"
PRISM[, 17][PRISM[, 15] == "Burabod"] <- "Sorsogon" # Someone doesn't know the difference between a town and a province
PRISM[, 17][PRISM[, 16] == "Miluya"] <- "Sorsogon" # Someone doesn't know the difference between a barangay and a province
PRISM[, 17][PRISM[, 17] == "bohol"] <- "Bohol"
PRISM[, 17][PRISM[, 16] == "Babalag East"] <- "Kalinga" # Incorrectly labeled as Rizal Province

#### Rename the Municipalities to proper names ####
PRISM[, 16][PRISM[, 16] == "pilar"] <- "Pilar"
PRISM[, 16][PRISM[, 16] == "Sta.Cruz"] <- "Santa Cruz"
PRISM[, 16][PRISM[, 16] == "Sta. Cruz"] <- "Santa Cruz"
PRISM[, 16][PRISM[, 16] == "RIZAL"] <- "Rizal"
PRISM[, 16][PRISM[, 16] == "Tabuk city"] <- "Tabuk City"
PRISM[, 16][PRISM[, 16] == "San miguel"] <- "San Miguel"
PRISM[, 16][PRISM[, 16] == "San  miguel"] <- "San Miguel"
PRISM[, 16][PRISM[, 16] == "san Miguel"] <- "San Miguel"
PRISM[, 16][PRISM[, 16] == "San  Miguel"] <- "San Miguel"
PRISM[, 16][PRISM[, 16] == "Sam Miguel"] <- "San Miguel"
PRISM[, 16][PRISM[, 16] == "Sanmiguel"] <- "San Miguel"
PRISM[, 16][PRISM[, 16] == "Tabuk"] <- "Tabuk City"
PRISM[, 16][PRISM[, 16] == "Tabui"] <- "Tabuk City"
PRISM[, 16][PRISM[, 16] == "sablayan"] <- "Sablayan"
PRISM[, 16][PRISM[, 16] == "Sta.cruz"] <- "Santa Cruz"
PRISM[, 16][PRISM[, 16] == "Sta cruz"] <- "Santa Cruz"
PRISM[, 16][PRISM[, 16] == "Palangui"] <- "Polangui"
PRISM[, 16][PRISM[, 15] == "Burabod"] <- "Castilla" # Someone doesn't know the difference between a town and a province
PRISM[, 16][PRISM[, 16] == "Miluya"] <- "Castilla" # Someone doesn't know the difference between a baragnay and a Town
PRISM[, 16][PRISM[, 16] == "Babalag East"] <- "Rizal" # No municipality was given
PRISM[, 16][PRISM[, 16] == "Alang alang"] <- "Alangalang"
PRISM[, 16][PRISM[, 16] == "Minalbac"] <- "Minalabac"
PRISM[, 16][PRISM[, 16] == "minalabac"] <- "Minalabac"
PRISM[, 16][PRISM[, 16] == "Polangue"] <- "Polangui"
PRISM[, 16][PRISM[, 16] == "Sorsogon"] <- "Castilla"
PRISM[, 16][PRISM[, 12] == 5061] <- "Castilla" #5061 is incorrectly placed in Pilar not Castilla

#### Rename the regions to proper names ####
PRISM[, 18][PRISM[, 18] == "3"] <- "III"
PRISM[, 18][PRISM[, 18] == "5"] <- "IV-B"
PRISM[, 18][PRISM[, 18] == "6"] <- "V"
PRISM[, 18][PRISM[, 18] == "7"] <- "VI"
PRISM[, 18][PRISM[, 18] == "8"] <- "VII"
PRISM[, 18][PRISM[, 18] == "9"] <- "VIII"
PRISM[, 18][PRISM[, 18] == "16"] <- "CAR"
PRISM[, 18][PRISM[, 15] == "Babalag East"] <- "CAR"
PRISM[, 18][PRISM[, 18] == "Sorsogon"] <- "V"

#### Correct region numbers ####
PRISM <- within(PRISM, Region[Province == "Bohol"] <- "VII") # Fixes "NA" and "12" mis-entries

#### Santa Cruz, region IV-B has one visit incorrectly recorded as visit 2, when it's the first ####
tmp <- subset(PRISM, Municipality == "Santa Cruz" & start == "2014-09-02")
tmp[, 21][tmp[, 21] == "2nd"] <- "1st"
PRISM <- PRISM[PRISM[, 16] != "Santa Cruz", ] 
PRISM <- rbind(PRISM, tmp)

# Some visits are incorrectly recorded, there are no third or fourth visits
#PRISM[, 22][PRISM[, 22] == "4th"] <- "1st"
#PRISM[, 22][PRISM[, 22] == "3rd"] <- "1st"

PRISM[, 21][PRISM[, 24] <= 60] <- "Booting"
PRISM[, 21][PRISM[, 24] >= 70] <- "Ripening"

#### Remove extra field observations ####
## Keep only the second observation per growth stage visit
PRISM <- subset(PRISM, locID != 7004 | datetime != "2014-08-27") # remove first observation at ripening, incorrect data collected
PRISM <- subset(PRISM, locID != 3050 | datetime != "2014-10-04") # Obviously one of the ripening observations does not go with this locID, where does it go?
PRISM <- subset(PRISM, locID != 3024 | group_crop_info.crop_stage != 80) # remove first observation at ripening, incorrect data collected
PRISM <- subset(PRISM, locID != 3028 | group_crop_info.crop_stage != 60) # remove first observation at ripening, incorrect data collected
PRISM <- subset(PRISM, locID != 17010 | SubmissionDate != "2014-09-20 22:39:02") # remove the first submission, keep the second

#### CAR Correction ####
PRISM[, 16][PRISM[, 15] == "Babalag East"] <- "Rizal"
PRISM[, 17][PRISM[, 15] == "Babalag East"] <- "Kalinga"

#### Bohol has three munincipalities that combine into one
bohol <- subset(PRISM, Province == "Bohol")
bohol[, 16] <- bohol[, 17]
PRISM <- PRISM[PRISM[, 17] != "Bohol", ] 
PRISM <- rbind(PRISM, bohol)

#### Start creating individual data frames for graphs and maps ####
## Visit number one or two? ##
visit <- PRISM[, grep(pattern = "visitNo_label", colnames(PRISM), perl = TRUE)]
visit <- data.frame(PRISM[, c(2, 12, 15:18)], visit)
colnames(visit) <- c("Date", "locID", "Barangay", "Municipality", "Province", "Region", "visit")

#### Growth stage ####
gs <- PRISM[, grep(pattern = "crop_stage", colnames(PRISM), perl = TRUE)]

#### tillers, panicle and leaf counts ####
tillers <- apply(PRISM[, grep(pattern = "tiller_hill", colnames(PRISM), perl = TRUE)], 1, sum)
panicles <- apply(PRISM[, grep(pattern = "panicle_hill", colnames(PRISM), perl = TRUE)], 1, sum)
leaves <- apply(PRISM[, grep(pattern = "leaves_tiller", colnames(PRISM), perl = TRUE)], 1, sum)

#### generate data frames of non-systemic diseases, from 10 observations, for graphing ####
bak <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers, apply(PRISM[, grep(pattern = "bakanae", colnames(PRISM), perl = TRUE)], 1, sum))
blb <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers*leaves, apply(PRISM[, grep(pattern = "bacterialleafblight", colnames(PRISM), perl = TRUE)], 1, sum))
bls <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers*leaves, apply(PRISM[, grep(pattern = "bacterialleafstreak", colnames(PRISM), perl = TRUE)], 1, sum))
bst <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers*leaves, apply(PRISM[, grep(pattern = "(?<!narrow)(?i)brownspot", colnames(PRISM), perl = TRUE)], 1, sum))
fsm <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, panicles, apply(PRISM[, grep(pattern = "falsesmut", colnames(PRISM), perl = TRUE)], 1, sum))
fsm <- na.omit(subset(fsm, visit.visit == "Ripening")) # no false smut before heading
dip <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, panicles, apply(PRISM[, grep(pattern = "dirtypanicle", colnames(PRISM), perl = TRUE)], 1, sum))
dip <- na.omit(subset(dip, visit.visit == "Ripening")) # no dirty panicles before heading
lba <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers*leaves, apply(PRISM[, grep(pattern = "leafblast", colnames(PRISM), perl = TRUE)], 1, sum))
nba <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers, apply(PRISM[, grep(pattern = "neckblast", colnames(PRISM), perl = TRUE)], 1, sum))
nba <- na.omit(subset(nba, nba$visit.visit == "Ripening")) # no neck blast until second visit
nbs <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers*leaves, apply(PRISM[, grep(pattern = "narrowbrownspot", (colnames(PRISM)), perl = TRUE)], 1, sum))
lsc <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers*leaves, apply(PRISM[, grep(pattern = "leafscald", colnames(PRISM), perl = TRUE)], 1, sum))
rsp <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers*leaves, apply(PRISM[, grep(pattern = "redstripe", colnames(PRISM), perl = TRUE)], 1, sum))
shr <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers, apply(PRISM[, grep(pattern = "sheathrot", colnames(PRISM), perl = TRUE)], 1, sum))
shb <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers, apply(PRISM[, grep(pattern = "sheathblight", colnames(PRISM), perl = TRUE)], 1, sum))
str <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers, apply(PRISM[, grep(pattern = "stemrot", colnames(PRISM), perl = TRUE)], 1, sum))

names(bak) <- names(blb) <- names(bls) <- names(bst) <- names(fsm) <- names(dip) <- names(lba) <- names(nba)  <- names(nbs)  <- names(lsc) <- names(rsp) <- names(shr) <- names(shb) <- names(str) <- c("lat", "lon", "locID", "Municipality", "Province", "Region", "visit", "organ", "injury")

bak.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = bak, FUN = median, na.rm = TRUE)
blb.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = blb, FUN = median, na.rm = TRUE)
bls.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = bls, FUN = median, na.rm = TRUE)
bst.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = bst, FUN = median, na.rm = TRUE)
fsm.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = fsm, FUN = median, na.rm = TRUE)
dip.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = dip, FUN = median, na.rm = TRUE)
lba.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = lba, FUN = median, na.rm = TRUE)
nba.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = nba, FUN = median, na.rm = TRUE)
nbs.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = nbs, FUN = median, na.rm = TRUE)
lsc.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = lsc, FUN = median, na.rm = TRUE)
rsp.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = rsp, FUN = median, na.rm = TRUE)
shr.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = shr, FUN = median, na.rm = TRUE)
shb.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = shb, FUN = median, na.rm = TRUE)
str.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = str, FUN = median, na.rm = TRUE)

names(bak.summary) <- names(blb.summary) <- names(bls.summary) <- names(bst.summary) <- names(fsm.summary) <- names(dip.summary) <- names(lba.summary) <- names(nba.summary) <- names(nbs.summary) <- names(lsc.summary) <- names(rsp.summary) <- names(shr.summary) <- names(shb.summary) <- names(str.summary) <- c("Municipality", "visit", "injury", "organ", "lat", "lon")

# summarise by the percent injury ocurring for mapping
bak.summary <- mutate(bak.summary, perc.injury = (injury/organ)*100)
blb.summary <- mutate(blb.summary, perc.injury = (injury/organ)*100)
bls.summary <- mutate(bls.summary, perc.injury = (injury/organ)*100)
bst.summary <- mutate(bst.summary, perc.injury = (injury/organ)*100)
fsm.summary <- mutate(fsm.summary, perc.injury = (injury/organ)*100)
dip.summary <- mutate(dip.summary, perc.injury = (injury/organ)*100)
lba.summary <- mutate(lba.summary, perc.injury = (injury/organ)*100)
nba.summary <- mutate(nba.summary, perc.injury = (injury/organ)*100)
nbs.summary <- mutate(nbs.summary, perc.injury = (injury/organ)*100)
lsc.summary <- mutate(lsc.summary, perc.injury = (injury/organ)*100)
rsp.summary <- mutate(rsp.summary, perc.injury = (injury/organ)*100)
shr.summary <- mutate(shr.summary, perc.injury = (injury/organ)*100)
shb.summary <- mutate(shb.summary, perc.injury = (injury/organ)*100)
str.summary <- mutate(str.summary, perc.injury = (injury/organ)*100)

#### generate data frames of snail and rat damage ####
gas <- data.frame(PRISM[, c(1, 8:9, 12, 16:18)], visit$visit, tillers, apply(PRISM[, grep(pattern = "area_gas", colnames(PRISM), perl = TRUE)], 1, mean))
rat <- data.frame(PRISM[, c(1, 8:9, 12, 16:18)], visit$visit, tillers, apply(PRISM[, grep(pattern = "pest_rat", colnames(PRISM), perl = TRUE)], 1, mean))

names(gas) <- names(rat) <- c("date", "lat", "lon", "locID", "Municipality", "Province", "Region", "visit", "tillers", "injury")

gas.summary <- summaryBy(injury+lat+lon~Municipality+visit, data = gas, FUN = median, na.rm = TRUE)
rat.summary <- summaryBy(injury+lat+lon~Municipality+visit, data = rat, FUN = mean, na.rm = TRUE)

#### generate data frames of systemic diseases, snail and bug/hopper burn ####
bbn <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, apply(PRISM[, grep(pattern = "bugburn", colnames(PRISM), perl = TRUE)], 1, mean))
hbn <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, apply(PRISM[, grep(pattern = "hopperburn", colnames(PRISM), perl = TRUE)], 1, mean))
tun <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, apply(PRISM[, grep(pattern = "tungro", colnames(PRISM), perl = TRUE)], 1, mean))
grs <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, apply(PRISM[, grep(pattern = "grassy", colnames(PRISM), perl = TRUE)], 1, mean))
rgd <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, apply(PRISM[, grep(pattern = "ragged", colnames(PRISM), perl = TRUE)], 1, mean))
olf <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, apply(PRISM[, grep(pattern = "orangeleaf", colnames(PRISM), perl = TRUE)], 1, mean))
ylo <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, apply(PRISM[, grep(pattern = "yellowdwarf", colnames(PRISM), perl = TRUE)], 1, mean))

names(bbn) <- names(hbn) <- names(tun) <- names(grs) <- names(rgd) <- names(olf) <- names(ylo) <- c("lat", "lon", "locID", "Municipality", "Province", "Region", "visit", "rating")

bbn.summary <- summaryBy(rating+lat+lon~Municipality+visit, data = bbn, FUN = median, na.rm = TRUE)
hbn.summary <- summaryBy(rating+lat+lon~Municipality+visit, data = hbn, FUN = median, na.rm = TRUE)
tun.summary <- summaryBy(rating+lat+lon~Municipality+visit, data = tun, FUN = median, na.rm = TRUE)
grs.summary <- summaryBy(rating+lat+lon~Municipality+visit, data = grs, FUN = median, na.rm = TRUE)
rgd.summary <- summaryBy(rating+lat+lon~Municipality+visit, data = rgd, FUN = median, na.rm = TRUE)
olf.summary <- summaryBy(rating+lat+lon~Municipality+visit, data = olf, FUN = median, na.rm = TRUE)
ylo.summary <- summaryBy(rating+lat+lon~Municipality+visit, data = ylo, FUN = median, na.rm = TRUE)

names(tun.summary) <- names(grs.summary) <- names(rgd.summary) <- names(olf.summary) <- names(ylo.summary) <- names(bbn.summary)  <- names(hbn.summary) <- c("Municipality", "visit", "rating", "lat", "lon")

#### generate data frames of weed data ####
weedabove <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, apply(PRISM[, grep(pattern = "weedabove_area", colnames(PRISM), perl = TRUE)], 1, mean))
weedbelow <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, apply(PRISM[, grep(pattern = "weedbelow_area", colnames(PRISM), perl = TRUE)], 1, mean))
broadleaf <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, apply(PRISM[, grep(pattern = "weed_broadleaved", colnames(PRISM), perl = TRUE)], 1, mean))
grass <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, apply(PRISM[, grep(pattern = "weed_grass", colnames(PRISM), perl = TRUE)], 1, mean))
sedge <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, apply(PRISM[, grep(pattern = "weed_sedge", colnames(PRISM), perl = TRUE)], 1, mean))
small <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, apply(PRISM[, grep(pattern = "weed_small", colnames(PRISM), perl = TRUE)], 1, mean))

names(weedabove) <- names(weedbelow) <- names(broadleaf) <- names(grass) <- names(sedge) <- names(small) <- c("lat", "lon", "locID", "Municipality", "Province", "Region", "visit", "rating")

weedabove.summary <- summaryBy(rating+lat+lon~Municipality+visit, data = weedabove, FUN = median, na.rm = TRUE)
weedbelow.summary <- summaryBy(rating+lat+lon~Municipality+visit, data = weedbelow, FUN = median, na.rm = TRUE)
broadleaf.summary <- summaryBy(rating+lat+lon~Municipality+visit, data = broadleaf, FUN = median, na.rm = TRUE)
grass.summary <- summaryBy(rating+lat+lon~Municipality+visit, data = grass, FUN = median, na.rm = TRUE)
sedge.summary <- summaryBy(rating+lat+lon~Municipality+visit, data = sedge, FUN = median, na.rm = TRUE)
small.summary <- summaryBy(rating+lat+lon~Municipality+visit, data = small, FUN = median, na.rm = TRUE)

names(weedabove.summary) <- names(weedbelow.summary) <- names(broadleaf.summary) <- names(grass.summary) <- names(sedge.summary) <- names(small.summary) <- c("Municipality", "visit", "rating", "lat", "lon")

#### Pest injuries ####
lfd <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers*leaves, apply(PRISM[, grep(pattern = "leaffolder", colnames(PRISM), perl = TRUE)], 1, sum))
lfm <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers*leaves, apply(PRISM[, grep(pattern = "leafminer", colnames(PRISM), perl = TRUE)], 1, sum))
thp <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers*leaves, apply(PRISM[, grep(pattern = "thrip", colnames(PRISM), perl = TRUE)], 1, sum))
whm <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers*leaves, apply(PRISM[, grep(pattern = "whorl", colnames(PRISM), perl = TRUE)], 1, sum))
def <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers*leaves, apply(PRISM[, grep(pattern = "defoliators", colnames(PRISM), perl = TRUE)], 1, sum))
wht <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers*tillers, apply(PRISM[, grep(pattern = "whitehead", colnames(PRISM), perl = TRUE)], 1, sum))
wht <- na.omit(subset(wht, visit.visit == "Ripening")) # no white head until second visit
rgb <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, panicles, apply(PRISM[, grep(pattern = "ricegrainbug", colnames(PRISM), perl = TRUE)], 1, sum))
rgb <- na.omit(subset(rgb, visit.visit == "Ripening")) # no grain bug damage until second visit
rbg <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, panicles, apply(PRISM[, grep(pattern = "ricebug", colnames(PRISM), perl = TRUE)], 1, sum))
dht <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers, apply(PRISM[, grep(pattern = "deadheart", colnames(PRISM), perl = TRUE)], 1, sum))

names(lfd) <- names(lfm) <- names(thp) <- names(whm) <- names(def) <- names(wht) <- names(rgb) <- names(rbg) <- names(dht) <- c("lat", "lon", "locID", "Municipality", "Province", "Region", "visit", "organ", "injury")

lfd.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = lfd, FUN = median, na.rm = TRUE)
lfm.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = lfm, FUN = median, na.rm = TRUE)
thp.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = thp, FUN = median, na.rm = TRUE)
whm.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = whm, FUN = median, na.rm = TRUE)
def.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = def, FUN = median, na.rm = TRUE)
wht.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = wht, FUN = median, na.rm = TRUE)
rgb.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = rgb, FUN = median, na.rm = TRUE)
rbg.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = rbg, FUN = median, na.rm = TRUE)
dht.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = dht, FUN = median, na.rm = TRUE)

names(lfd.summary) <- names(lfm.summary) <- names(thp.summary) <- names(whm.summary) <- names(def.summary) <- names(wht.summary) <- names(rgb.summary) <- names(rbg.summary) <- names(dht.summary) <- c("Municipality", "visit", "injury", "organ", "lat", "lon")

# summarise by the percent injury ocurring for mapping
lfd.summary <- mutate(lfd.summary, perc.injury = (injury/organ)*100)
lfm.summary <- mutate(lfm.summary, perc.injury = (injury/organ)*100)
thp.summary <- mutate(thp.summary, perc.injury = (injury/organ)*100)
whm.summary <- mutate(whm.summary, perc.injury = (injury/organ)*100)
def.summary <- mutate(def.summary, perc.injury = (injury/organ)*100)
wht.summary <- mutate(wht.summary, perc.injury = (injury/organ)*100)
rgb.summary <- mutate(rgb.summary, perc.injury = (injury/organ)*100)
rbg.summary <- mutate(rbg.summary, perc.injury = (injury/organ)*100)
dht.summary <- mutate(dht.summary, perc.injury = (injury/organ)*100)

#eos 
