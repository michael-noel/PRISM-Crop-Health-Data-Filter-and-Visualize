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
library(doBy)
library(sqldf)
library(plyr)
library(lubridate)
library(agricolae)
library(reshape2)
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

PRISM[, 1] <- parse_date_time(PRISM[, 1], "b d, Y I:M:S p") # use lubridate to filter out duplicate entries if necessary
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
#PRISM <- subset(PRISM, !is.na(PRISM[, 12])) # remove any records missing a location ID

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
PRISM[, 17][PRISM[, 17] == "Occ.mdo."] <- "Occidental Mindoro"
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
PRISM[, 18][PRISM[, 18] == 10] <- "VIII"

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
PRISM <- subset(PRISM, locID != 3024 | PRISM[, 24] != 80) # remove first observation at ripening, incorrect data collected
PRISM <- subset(PRISM, locID != 3028 | PRISM[, 24] != 60) # remove first observation at ripening, incorrect data collected
PRISM <- subset(PRISM, locID != 17010 | SubmissionDate != "2014-09-20 22:39:02") # remove the first submission, keep the second

#### CAR Correction ####
PRISM[, 16][PRISM[, 15] == "Babalag East"] <- "Rizal"
PRISM[, 17][PRISM[, 15] == "Babalag East"] <- "Kalinga"

#### Bohol has three munincipalities that combine into one
bohol <- subset(PRISM, Province == "Bohol")
bohol[, 16] <- bohol[, 17]
PRISM <- PRISM[PRISM[, 17] != "Bohol", ] 
PRISM <- rbind(PRISM, bohol)

#### Now create the aggregated data for graphing and mapping ####
source("Aggregate_Injuries.R")

#eos 

