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
library(lubridate)
library(plyr)
#### end load packages ####

PRISM <- read.csv("~/Google Drive/tmp/PRISM_Yield.csv")

PRISM[, 1] <- parse_date_time(PRISM[, 1], "b d, Y I:M:S p") # use lubridate to filter out duplicate entries if necessary
PRISM[, 2] <- as.character(as.Date(PRISM[, 2], "%b %d, %Y")) # these are just character, use for subsetting
PRISM[, 3] <- as.character(as.Date(PRISM[, 3], "%b %d, %Y")) # these are just character, use for subsetting
PRISM[, 4] <- as.character(as.Date(PRISM[, 4], "%b %d, %Y")) # these are just character, use for subsetting
PRISM[, 18] <- as.character(PRISM[, 18], "%b %d, %Y")

#### Remove more training events and other misc that are not real data ####
PRISM <- subset(PRISM, start >= "2014-09-21") # No observations were taken before this date, safe to remove all these data

PRISM[, 14] <- as.character(PRISM[, 14])
PRISM[, 15] <- as.character(PRISM[, 15])
PRISM[, 16] <- as.character(PRISM[, 16])
PRISM[, 10] <- as.character(PRISM[, 10])

#### Name the columns for easier work ####
names(PRISM)[names(PRISM) == "group_contact.village"] <- "Barangay"
names(PRISM)[names(PRISM) == "group_contact.municipality"] <- "Municipality"
names(PRISM)[names(PRISM) == "group_contact.province"] <- "Province"
names(PRISM)[names(PRISM) == "group_contact.region"] <- "Region"

#### Rename the provinces to proper names ####
PRISM[, 15][PRISM[, 15] == "occidental mindoro"] <- "Occidental Mindoro"
PRISM[, 15][PRISM[, 15] == "Occ.mindoro"] <- "Occidental Mindoro"
PRISM[, 15][PRISM[, 10] == "007018"] <- "Bohol" # Someone moved our plots from Bohol to Cebu?

#### Rename the Municipalities to proper names ####
PRISM[, 14][PRISM[, 14] == "sablayan"] <- "Sablayan"
PRISM[, 14][PRISM[, 14] == "Sta.cruz"] <- "Santa Cruz"
PRISM[, 14][PRISM[, 14] == "Sta. Cruz"] <- "Santa Cruz"
PRISM[, 14][PRISM[, 14] == "Sta.Cruz"] <- "Santa Cruz"
PRISM[, 14][PRISM[, 14] == "Alang alang"] <- "Alangalang"

#### Rename the regions to proper names ####
PRISM[, 16][PRISM[, 16] == "region3"] <- "III"
PRISM[, 16][PRISM[, 16] == "region5"] <- "V"
PRISM[, 16][PRISM[, 16] == "region6"] <- "VI"
PRISM[, 16][PRISM[, 16] == "region7"] <- "VII"
PRISM[, 16][PRISM[, 16] == "region8"] <- "VIII"
PRISM[, 16][PRISM[, 16] == "region4b"] <- "IV-B"

PRISM[, 18][PRISM[, 18] == "1ha"] <- 1
PRISM[, 18] <- as.numeric(as.character(PRISM[, 18]))

#### Correct missing location IDs ####
# ifelse(PRISM[, 11] == "recto sta. maria" & PRISM[, 13] == "Sablayan", as.numeric(PRISM[, 8] <- 17027), as.numeric(PRISM[, 8] <- PRISM[, 8]))
# ifelse(PRISM[, 11] == "Rogelio Villa" & PRISM[, 13] == "Santa Cruz", as.numeric(PRISM[, 8] <- 17050), as.numeric(PRISM[, 8] <- PRISM[, 8]))
# ifelse(PRISM[, 10] == "CONSUELO VILLAS" & PRISM[, 12] == "Santa Cruz", PRISM[, 8] <- 17041, FALSE)
# ifelse(PRISM[, 10] == "ROWELL BAUTISTA" & PRISM[, 12] == "Santa Cruz", PRISM[, 8] <- 17049, FALSE)
# ifelse(PRISM[, 10] == "Mahumot Eligio" & PRISM[, 12] == "San Miguel", PRISM[, 8] <- 7010, FALSE)
# ifelse(PRISM[, 10] == "Salvador Superada" & PRISM[, 12] == "Alangalang", PRISM[, 8] <- 8006, FALSE)

#### Merge the site ID columns ####
missing <- is.na(PRISM[, 9]) # create logical index for NAs in PRISM[, 8]
PRISM[, 9][missing] <- PRISM[, 10][missing] # replace NAs with values from PRISM[, 8]
PRISM <- PRISM[, -9] # drop column 8 now
PRISM[, 9] <- as.numeric(PRISM[, 9])
#PRISM <- subset(PRISM, !is.na(PRISM[, 8])) # remove any records missing a location ID

#### Bohol has three munincipalities that combine into one
bohol <- subset(PRISM, Province == "Bohol")
bohol[, 13] <- bohol[, 14]
PRISM <- PRISM[PRISM[, 14] != "Bohol", ]
PRISM <- rbind(PRISM, bohol)

#### generate data frames for graphing ####
yield <- data.frame(PRISM[, c(1, 9, 13:16)], round(apply(PRISM[, grep(pattern = "yieldtha", colnames(PRISM), perl = TRUE)], 1, mean), 2), PRISM[, 18])
names(yield) <- c("SubmissionDate", "locID", "Municipality", "Province", "Region", "Observer", "Yield", "Harvest Date")

#### Summarise the data by municipality ####
yield.mean.summary <- summaryBy(Yield~Municipality, data = yield, FUN = mean)

yield.mean.summary

#eos 
