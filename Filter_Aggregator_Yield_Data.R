##############################################################################
# title         : Filter_Aggregator_Data.R;
# purpose       : Filter PRISM data as pulled from ODK Aggregator;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Laguna, PHL, Nov. 2014;
# inputs        : Raw PRISM data;
# outputs       : Filtered PRISM data;
# remarks 1     : ;
# Licence:      : GPL2;
##############################################################################

#### load packages ####
library(reshape)
library(doBy)
library(plyr)
#### end load packages ####

PRISM <- read.csv("~/Google Drive/tmp/PRISM_Fertilizer_Pesticide_and_Yield_V1_0_results.csv")
PRISM[, 2] <- as.character(substr(PRISM[, 2], 1, 10))
PRISM[, 3] <- as.character(substr(PRISM[, 3], 1, 10))
PRISM[, 4] <- as.character(substr(PRISM[, 4], 1, 10))

#### Remove more training events and other misc that are not real data ####
PRISM <- subset(PRISM, start >= "2014-09-21") # No observations were taken before this date, safe to remove all these data

PRISM[, 13] <- as.character(PRISM[, 13])
PRISM[, 14] <- as.character(PRISM[, 14])
PRISM[, 15] <- as.character(PRISM[, 15])
PRISM[, 9] <- as.character(PRISM[, 9])

#### Name the columns for easier work ####
names(PRISM)[names(PRISM) == "group_contact.village"] <- "Barangay"
names(PRISM)[names(PRISM) == "group_contact.municipality"] <- "Municipality"
names(PRISM)[names(PRISM) == "group_contact.province"] <- "Province"
names(PRISM)[names(PRISM) == "group_contact.region"] <- "Region"

#### Rename the provinces to proper names ####
PRISM[, 14][PRISM[, 14] == "occidental mindoro"] <- "Occidental Mindoro"
PRISM[, 14][PRISM[, 14] == "Occ.mindoro"] <- "Occidental Mindoro"
PRISM[, 14][PRISM[, 9] == "007018"] <- "Bohol" # Someone moved our plots from Bohol to Cebu?

#### Rename the Municipalities to proper names ####
PRISM[, 13][PRISM[, 13] == "sablayan"] <- "Sablayan"
PRISM[, 13][PRISM[, 13] == "Sta.cruz"] <- "Santa Cruz"
PRISM[, 13][PRISM[, 13] == "Sta. Cruz"] <- "Santa Cruz"
PRISM[, 13][PRISM[, 13] == "Sta.Cruz"] <- "Santa Cruz"
PRISM[, 13][PRISM[, 13] == "Alang alang"] <- "Alangalang"

#### Rename the regions to proper names ####
PRISM[, 15][PRISM[, 15] == "region3"] <- "III"
PRISM[, 15][PRISM[, 15] == "region5"] <- "V"
PRISM[, 15][PRISM[, 15] == "region6"] <- "VI"
PRISM[, 15][PRISM[, 15] == "region7"] <- "VII"
PRISM[, 15][PRISM[, 15] == "region8"] <- "VIII"
PRISM[, 15][PRISM[, 15] == "region4b"] <- "IV-B"

PRISM[, 17][PRISM[, 17] == "1ha"] <- 1
PRISM[, 17] <- as.numeric(as.character(PRISM[, 17]))

#### Correct missing location IDs ####
ifelse(PRISM[, 11] == "recto sta. maria" & PRISM[, 13] == "Sablayan", as.numeric(PRISM[, 8] <- 17027), as.numeric(PRISM[, 8] <- PRISM[, 8]))
ifelse(PRISM[, 11] == "Rogelio Villa" & PRISM[, 13] == "Santa Cruz", as.numeric(PRISM[, 8] <- 17050), as.numeric(PRISM[, 8] <- PRISM[, 8]))
ifelse(PRISM[, 10] == "CONSUELO VILLAS" & PRISM[, 12] == "Santa Cruz", PRISM[, 8] <- 17041, FALSE)
ifelse(PRISM[, 10] == "ROWELL BAUTISTA" & PRISM[, 12] == "Santa Cruz", PRISM[, 8] <- 17049, FALSE)
ifelse(PRISM[, 10] == "Mahumot Eligio" & PRISM[, 12] == "San Miguel", PRISM[, 8] <- 7010, FALSE)
ifelse(PRISM[, 10] == "Salvador Superada" & PRISM[, 12] == "Alangalang", PRISM[, 8] <- 8006, FALSE)

#### Merge the site ID columns ####
missing <- is.na(PRISM[, 8]) # create logical index for NAs in PRISM[, 8]
PRISM[, 8][missing] <- PRISM[, 9][missing] # replace NAs with values from PRISM[, 8]
PRISM <- PRISM[, -9] # drop column 8 now
PRISM[, 8] <- as.numeric(PRISM[, 8])
#PRISM <- subset(PRISM, !is.na(PRISM[, 8])) # remove any records missing a location ID

#### Bohol has three munincipalities that combine into one
bohol <- subset(PRISM, Province == "Bohol")
bohol[, 12] <- bohol[, 13]
PRISM <- PRISM[PRISM[, 13] != "Bohol", ]
PRISM <- rbind(PRISM, bohol)

#### generate data frames for graphing ####
yield <- data.frame(PRISM[, c(8, 12:14)], apply(PRISM[, grep(pattern = "yieldtha", colnames(PRISM), perl = TRUE)], 1, mean))
names(yield) <- c("locID", "Municipality", "Province", "Region", "Yield")

#### Summarise the data by municipality ####
yield.mean.summary <- summaryBy(Yield~Municipality, data = yield, FUN = mean)

yield.mean.summary

#eos 
