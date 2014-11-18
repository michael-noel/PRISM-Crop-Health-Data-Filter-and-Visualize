##############################################################################
# title         : Aggregate_Injuries.R;
# purpose       : Aggregate PRISM injury data as pulled from ODK Aggregator;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Laguna, PHL, Nov 2014;
# inputs        : Filtered PRISM data from Filter_Aggregator_Yield_Data.R;
# outputs       : Aggregated data in data frames for graphing, mapping and summarizing;
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

#### Start creating individual data frames for graphs and maps ####
## Visit number one or two? ##
visit <- PRISM[, grep(pattern = "visitNo_label", colnames(PRISM), perl = TRUE)]
visit <- data.frame(PRISM[, c(1, 12, 15:18)], visit)
colnames(visit) <- c("Date", "locID", "Barangay", "Municipality", "Province", "Region", "visit")

#### Create "dates" dataframe for AUIPC calculations using AUDPC from Agricolae ####
dates <- c(1, 2) # Two visits per farmer field, just using relative for AUDPC calculation since there are only two and at different time periods

#### Growth stage ####
gs <- PRISM[, grep(pattern = "crop_stage", colnames(PRISM), perl = TRUE)]

#### tillers, panicle and leaf counts ####
## split into groups for weighted averages ##
tillers <- data.frame(PRISM[, grep(pattern = "group_1_[[:graph:]]*tiller_hill", colnames(PRISM), perl = TRUE)],
                      PRISM[, grep(pattern = "group_2_[[:graph:]]*tiller_hill", colnames(PRISM), perl = TRUE)],
                      PRISM[, grep(pattern = "group_3_[[:graph:]]*tiller_hill", colnames(PRISM), perl = TRUE)],
                      PRISM[, grep(pattern = "group_4_[[:graph:]]*tiller_hill", colnames(PRISM), perl = TRUE)],
                      PRISM[, grep(pattern = "group_5_[[:graph:]]*tiller_hill", colnames(PRISM), perl = TRUE)],
                      PRISM[, grep(pattern = "group_6_[[:graph:]]*tiller_hill", colnames(PRISM), perl = TRUE)],
                      PRISM[, grep(pattern = "group_7_[[:graph:]]*tiller_hill", colnames(PRISM), perl = TRUE)],
                      PRISM[, grep(pattern = "group_8_[[:graph:]]*tiller_hill", colnames(PRISM), perl = TRUE)],
                      PRISM[, grep(pattern = "group_9_[[:graph:]]*tiller_hill", colnames(PRISM), perl = TRUE)],
                      PRISM[, grep(pattern = "group_10_[[:graph:]]*tiller_hill", colnames(PRISM), perl = TRUE)])
names(tillers) <- c("Tillers_Hill_1", "Tillers_Hill_2", "Tillers_Hill_3", "Tillers_Hill_4", "Tillers_Hill_5", "Tillers_Hill_6", "Tillers_Hill_7", "Tillers_Hill_8", "Tillers_Hill_9", "Tillers_Hill_10")

panicles <- data.frame(PRISM[, grep(pattern = "group_1_[[:graph:]]*panicle_hill", colnames(PRISM), perl = TRUE)],
                       PRISM[, grep(pattern = "group_2_[[:graph:]]*panicle_hill", colnames(PRISM), perl = TRUE)],
                       PRISM[, grep(pattern = "group_3_[[:graph:]]*panicle_hill", colnames(PRISM), perl = TRUE)],
                       PRISM[, grep(pattern = "group_4_[[:graph:]]*panicle_hill", colnames(PRISM), perl = TRUE)],
                       PRISM[, grep(pattern = "group_5_[[:graph:]]*panicle_hill", colnames(PRISM), perl = TRUE)],
                       PRISM[, grep(pattern = "group_6_[[:graph:]]*panicle_hill", colnames(PRISM), perl = TRUE)],
                       PRISM[, grep(pattern = "group_7_[[:graph:]]*panicle_hill", colnames(PRISM), perl = TRUE)],
                       PRISM[, grep(pattern = "group_8_[[:graph:]]*panicle_hill", colnames(PRISM), perl = TRUE)],
                       PRISM[, grep(pattern = "group_9_[[:graph:]]*panicle_hill", colnames(PRISM), perl = TRUE)],
                       PRISM[, grep(pattern = "group_10_[[:graph:]]*panicle_hill", colnames(PRISM), perl = TRUE)])
names(panicles) <- c("Panicles_Hill_1", "Panicles_Hill_2", "Panicles_Hill_3", "Panicles_Hill_4", "Panicles_Hill_5", "Panicles_Hill_6", "Panicles_Hill_7", "Panicles_Hill_8", "Panicles_Hill_9", "Panicles_Hill_10")

leaves <- data.frame(PRISM[, grep(pattern = "group_1_[[:graph:]]*leaves_tiller", colnames(PRISM), perl = TRUE)],
                     PRISM[, grep(pattern = "group_2_[[:graph:]]*leaves_tiller", colnames(PRISM), perl = TRUE)],
                     PRISM[, grep(pattern = "group_3_[[:graph:]]*leaves_tiller", colnames(PRISM), perl = TRUE)],
                     PRISM[, grep(pattern = "group_4_[[:graph:]]*leaves_tiller", colnames(PRISM), perl = TRUE)],
                     PRISM[, grep(pattern = "group_5_[[:graph:]]*leaves_tiller", colnames(PRISM), perl = TRUE)],
                     PRISM[, grep(pattern = "group_6_[[:graph:]]*leaves_tiller", colnames(PRISM), perl = TRUE)],
                     PRISM[, grep(pattern = "group_7_[[:graph:]]*leaves_tiller", colnames(PRISM), perl = TRUE)],
                     PRISM[, grep(pattern = "group_8_[[:graph:]]*leaves_tiller", colnames(PRISM), perl = TRUE)],
                     PRISM[, grep(pattern = "group_9_[[:graph:]]*leaves_tiller", colnames(PRISM), perl = TRUE)],
                     PRISM[, grep(pattern = "group_10_[[:graph:]]*leaves_tiller", colnames(PRISM), perl = TRUE)])

names(leaves) <- c("Leaves_Hill_1", "Leaves_Hill_2", "Leaves_Hill_3", "Leaves_Hill_4", "Leaves_Hill_5", "Leaves_Hill_6", "Leaves_Hill_7", "Leaves_Hill_8", "Leaves_Hill_9", "Leaves_Hill_10")

#### generate data frames of non-systemic diseases, from 10 observations, for graphing ####
bak <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers, 
                  PRISM[, grep(pattern = "group_1_[[:graph:]]*bakanae", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_2_[[:graph:]]*bakanae", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_3_[[:graph:]]*bakanae", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_4_[[:graph:]]*bakanae", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_5_[[:graph:]]*bakanae", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_6_[[:graph:]]*bakanae", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_7_[[:graph:]]*bakanae", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_8_[[:graph:]]*bakanae", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_9_[[:graph:]]*bakanae", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_10_[[:graph:]]*bakanae", colnames(PRISM), perl = TRUE)])

blb <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers, leaves, 
                  PRISM[, grep(pattern = "group_1_[[:graph:]]*bacterialleafblight", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_2_[[:graph:]]*bacterialleafblight", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_3_[[:graph:]]*bacterialleafblight", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_4_[[:graph:]]*bacterialleafblight", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_5_[[:graph:]]*bacterialleafblight", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_6_[[:graph:]]*bacterialleafblight", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_7_[[:graph:]]*bacterialleafblight", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_8_[[:graph:]]*bacterialleafblight", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_9_[[:graph:]]*bacterialleafblight", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_10_[[:graph:]]*bacterialleafblight", colnames(PRISM), perl = TRUE)])

bls <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers, leaves, 
                  PRISM[, grep(pattern = "group_1_[[:graph:]]*bacterialleafstreak", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_2_[[:graph:]]*bacterialleafstreak", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_3_[[:graph:]]*bacterialleafstreak", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_4_[[:graph:]]*bacterialleafstreak", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_5_[[:graph:]]*bacterialleafstreak", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_6_[[:graph:]]*bacterialleafstreak", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_7_[[:graph:]]*bacterialleafstreak", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_8_[[:graph:]]*bacterialleafstreak", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_9_[[:graph:]]*bacterialleafstreak", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_10_[[:graph:]]*bacterialleafstreak", colnames(PRISM), perl = TRUE)])

bst <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers, leaves, 
                  PRISM[, grep(pattern = "group_1_[[:graph:]]*(?<!narrow)(?i)brownspot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_2_[[:graph:]]*(?<!narrow)(?i)brownspot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_3_[[:graph:]]*(?<!narrow)(?i)brownspot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_4_[[:graph:]]*(?<!narrow)(?i)brownspot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_5_[[:graph:]]*(?<!narrow)(?i)brownspot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_6_[[:graph:]]*(?<!narrow)(?i)brownspot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_7_[[:graph:]]*(?<!narrow)(?i)brownspot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_8_[[:graph:]]*(?<!narrow)(?i)brownspot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_9_[[:graph:]]*(?<!narrow)(?i)brownspot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_10_[[:graph:]]*(?<!narrow)(?i)brownspot", colnames(PRISM), perl = TRUE)])

fsm <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, panicles, 
                  PRISM[, grep(pattern = "group_1_[[:graph:]]*falsesmut", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_2_[[:graph:]]*falsesmut", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_3_[[:graph:]]*falsesmut", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_4_[[:graph:]]*falsesmut", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_5_[[:graph:]]*falsesmut", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_6_[[:graph:]]*falsesmut", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_7_[[:graph:]]*falsesmut", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_8_[[:graph:]]*falsesmut", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_9_[[:graph:]]*falsesmut", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_10_[[:graph:]]*falsesmut", colnames(PRISM), perl = TRUE)])

fsm.graph <- na.omit(subset(fsm, visit.visit == "Ripening")) # no false smut before heading

dip <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, panicles, 
                  PRISM[, grep(pattern = "group_1_[[:graph:]]*dirtypanicle", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_2_[[:graph:]]*dirtypanicle", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_3_[[:graph:]]*dirtypanicle", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_4_[[:graph:]]*dirtypanicle", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_5_[[:graph:]]*dirtypanicle", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_6_[[:graph:]]*dirtypanicle", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_7_[[:graph:]]*dirtypanicle", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_8_[[:graph:]]*dirtypanicle", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_9_[[:graph:]]*dirtypanicle", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_10_[[:graph:]]*dirtypanicle", colnames(PRISM), perl = TRUE)])

lba <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers, leaves, 
                  PRISM[, grep(pattern = "group_1_[[:graph:]]*leafblast", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_2_[[:graph:]]*leafblast", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_3_[[:graph:]]*leafblast", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_4_[[:graph:]]*leafblast", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_5_[[:graph:]]*leafblast", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_6_[[:graph:]]*leafblast", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_7_[[:graph:]]*leafblast", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_8_[[:graph:]]*leafblast", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_9_[[:graph:]]*leafblast", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_10_[[:graph:]]*leafblast", colnames(PRISM), perl = TRUE)])

nba <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, panicles, 
                  PRISM[, grep(pattern = "group_1_[[:graph:]]*neckblast", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_2_[[:graph:]]*neckblast", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_3_[[:graph:]]*neckblast", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_4_[[:graph:]]*neckblast", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_5_[[:graph:]]*neckblast", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_6_[[:graph:]]*neckblast", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_7_[[:graph:]]*neckblast", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_8_[[:graph:]]*neckblast", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_9_[[:graph:]]*neckblast", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_10_[[:graph:]]*neckblast", colnames(PRISM), perl = TRUE)])

nba.graph <- na.omit(subset(nba, nba$visit.visit == "Ripening")) # no neck blast until second visit

nbs <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers, leaves, 
                  PRISM[, grep(pattern = "group_1_[[:graph:]]*narrowbrownspot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_2_[[:graph:]]*narrowbrownspot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_3_[[:graph:]]*narrowbrownspot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_4_[[:graph:]]*narrowbrownspot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_5_[[:graph:]]*narrowbrownspot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_6_[[:graph:]]*narrowbrownspot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_7_[[:graph:]]*narrowbrownspot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_8_[[:graph:]]*narrowbrownspot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_9_[[:graph:]]*narrowbrownspot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_10_[[:graph:]]*narrowbrownspot", colnames(PRISM), perl = TRUE)])

lsc <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers, leaves, 
                  PRISM[, grep(pattern = "group_1_[[:graph:]]*leafscald", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_2_[[:graph:]]*leafscald", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_3_[[:graph:]]*leafscald", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_4_[[:graph:]]*leafscald", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_5_[[:graph:]]*leafscald", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_6_[[:graph:]]*leafscald", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_7_[[:graph:]]*leafscald", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_8_[[:graph:]]*leafscald", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_9_[[:graph:]]*leafscald", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_10_[[:graph:]]*leafscald", colnames(PRISM), perl = TRUE)])

rsp <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers, leaves, 
                  PRISM[, grep(pattern = "group_1_[[:graph:]]*redstripe", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_2_[[:graph:]]*redstripe", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_3_[[:graph:]]*redstripe", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_4_[[:graph:]]*redstripe", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_5_[[:graph:]]*redstripe", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_6_[[:graph:]]*redstripe", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_7_[[:graph:]]*redstripe", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_8_[[:graph:]]*redstripe", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_9_[[:graph:]]*redstripe", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_10_[[:graph:]]*redstripe", colnames(PRISM), perl = TRUE)])

shr <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers, 
                  PRISM[, grep(pattern = "group_1_[[:graph:]]*sheathrot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_2_[[:graph:]]*sheathrot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_3_[[:graph:]]*sheathrot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_4_[[:graph:]]*sheathrot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_5_[[:graph:]]*sheathrot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_6_[[:graph:]]*sheathrot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_7_[[:graph:]]*sheathrot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_8_[[:graph:]]*sheathrot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_9_[[:graph:]]*sheathrot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_10_[[:graph:]]*sheathrot", colnames(PRISM), perl = TRUE)])

shb <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers, 
                  PRISM[, grep(pattern = "group_1_[[:graph:]]*sheathblight", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_2_[[:graph:]]*sheathblight", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_3_[[:graph:]]*sheathblight", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_4_[[:graph:]]*sheathblight", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_5_[[:graph:]]*sheathblight", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_6_[[:graph:]]*sheathblight", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_7_[[:graph:]]*sheathblight", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_8_[[:graph:]]*sheathblight", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_9_[[:graph:]]*sheathblight", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_10_[[:graph:]]*sheathblight", colnames(PRISM), perl = TRUE)])

str <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers, 
                  PRISM[, grep(pattern = "group_1_[[:graph:]]*stemrot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_2_[[:graph:]]*stemrot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_3_[[:graph:]]*stemrot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_4_[[:graph:]]*stemrot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_5_[[:graph:]]*stemrot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_6_[[:graph:]]*stemrot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_7_[[:graph:]]*stemrot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_8_[[:graph:]]*stemrot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_9_[[:graph:]]*stemrot", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_10_[[:graph:]]*stemrot", colnames(PRISM), perl = TRUE)])

#### generate data frames of snail and rat damage ####
gas <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers,
                  PRISM[, grep(pattern = "group_1_[[:graph:]]*area_gas", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_2_[[:graph:]]*area_gas", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_3_[[:graph:]]*area_gas", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_4_[[:graph:]]*area_gas", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_5_[[:graph:]]*area_gas", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_6_[[:graph:]]*area_gas", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_7_[[:graph:]]*area_gas", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_8_[[:graph:]]*area_gas", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_9_[[:graph:]]*area_gas", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_10_[[:graph:]]*area_gas", colnames(PRISM), perl = TRUE)])

rat <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers,
                  PRISM[, grep(pattern = "group_1_[[:graph:]]*area_rat", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_2_[[:graph:]]*area_rat", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_3_[[:graph:]]*area_rat", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_4_[[:graph:]]*area_rat", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_5_[[:graph:]]*area_rat", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_6_[[:graph:]]*area_rat", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_7_[[:graph:]]*area_rat", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_8_[[:graph:]]*area_rat", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_9_[[:graph:]]*area_rat", colnames(PRISM), perl = TRUE)],
                  PRISM[, grep(pattern = "group_10_[[:graph:]]*area_rat", colnames(PRISM), perl = TRUE)])


#### generate data frames of systemic diseases, snail and bug/hopper burn ####
bbn <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, apply(PRISM[, grep(pattern = "bugburn", colnames(PRISM), perl = TRUE)], 1, mean))
hbn <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, apply(PRISM[, grep(pattern = "hopperburn", colnames(PRISM), perl = TRUE)], 1, mean))
tun <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, apply(PRISM[, grep(pattern = "tungro", colnames(PRISM), perl = TRUE)], 1, mean))
grs <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, apply(PRISM[, grep(pattern = "grassy", colnames(PRISM), perl = TRUE)], 1, mean))
rgd <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, apply(PRISM[, grep(pattern = "ragged", colnames(PRISM), perl = TRUE)], 1, mean))
olf <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, apply(PRISM[, grep(pattern = "orangeleaf", colnames(PRISM), perl = TRUE)], 1, mean))
ylo <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, apply(PRISM[, grep(pattern = "yellowdwarf", colnames(PRISM), perl = TRUE)], 1, mean))

names(bbn) <- names(hbn) <- names(tun) <- names(grs) <- names(rgd) <- names(olf) <- names(ylo) <- c("lat", "lon", "locID", "Municipality", "Province", "Region", "visit", "rating")

bbn.summary <- summaryBy(rating+lat+lon~Municipality+visit, data = bbn, FUN = mean, na.rm = TRUE)
hbn.summary <- summaryBy(rating+lat+lon~Municipality+visit, data = hbn, FUN = mean, na.rm = TRUE)
tun.summary <- summaryBy(rating+lat+lon~Municipality+visit, data = tun, FUN = mean, na.rm = TRUE)
grs.summary <- summaryBy(rating+lat+lon~Municipality+visit, data = grs, FUN = mean, na.rm = TRUE)
rgd.summary <- summaryBy(rating+lat+lon~Municipality+visit, data = rgd, FUN = mean, na.rm = TRUE)
olf.summary <- summaryBy(rating+lat+lon~Municipality+visit, data = olf, FUN = mean, na.rm = TRUE)
ylo.summary <- summaryBy(rating+lat+lon~Municipality+visit, data = ylo, FUN = mean, na.rm = TRUE)

names(tun.summary) <- names(grs.summary) <- names(rgd.summary) <- names(olf.summary) <- names(ylo.summary) <- names(bbn.summary)  <- names(hbn.summary) <- c("Municipality", "visit", "rating", "lat", "lon")

#### generate data frames of weed data ####
weedabove <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, apply(PRISM[, grep(pattern = "weedabove_area", colnames(PRISM), perl = TRUE)], 1, mean))
weedbelow <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, apply(PRISM[, grep(pattern = "weedbelow_area", colnames(PRISM), perl = TRUE)], 1, mean))
broadleaf <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, apply(PRISM[, grep(pattern = "weed_broadleaved", colnames(PRISM), perl = TRUE)], 1, mean))
grass <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, apply(PRISM[, grep(pattern = "weed_grass", colnames(PRISM), perl = TRUE)], 1, mean))
sedge <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, apply(PRISM[, grep(pattern = "weed_sedge", colnames(PRISM), perl = TRUE)], 1, mean))
small <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, apply(PRISM[, grep(pattern = "weed_small", colnames(PRISM), perl = TRUE)], 1, mean))

names(weedabove) <- names(weedbelow) <- names(broadleaf) <- names(grass) <- names(sedge) <- names(small) <- c("lat", "lon", "locID", "Municipality", "Province", "Region", "visit", "rating")

weedabove.summary <- summaryBy(rating+lat+lon~Municipality+visit, data = weedabove, FUN = mean, na.rm = TRUE)
weedbelow.summary <- summaryBy(rating+lat+lon~Municipality+visit, data = weedbelow, FUN = mean, na.rm = TRUE)
broadleaf.summary <- summaryBy(rating+lat+lon~Municipality+visit, data = broadleaf, FUN = mean, na.rm = TRUE)
grass.summary <- summaryBy(rating+lat+lon~Municipality+visit, data = grass, FUN = mean, na.rm = TRUE)
sedge.summary <- summaryBy(rating+lat+lon~Municipality+visit, data = sedge, FUN = mean, na.rm = TRUE)
small.summary <- summaryBy(rating+lat+lon~Municipality+visit, data = small, FUN = mean, na.rm = TRUE)

names(weedabove.summary) <- names(weedbelow.summary) <- names(broadleaf.summary) <- names(grass.summary) <- names(sedge.summary) <- names(small.summary) <- c("Municipality", "visit", "rating", "lat", "lon")

#### Pest injuries ####
lfd <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers*leaves, apply(PRISM[, grep(pattern = "leaffolder", colnames(PRISM), perl = TRUE)], 1, sum))
lfm <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers*leaves, apply(PRISM[, grep(pattern = "leafminer", colnames(PRISM), perl = TRUE)], 1, sum))
thp <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers*leaves, apply(PRISM[, grep(pattern = "thrip", colnames(PRISM), perl = TRUE)], 1, sum))
whm <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers*leaves, apply(PRISM[, grep(pattern = "whorl", colnames(PRISM), perl = TRUE)], 1, sum))
def <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers*leaves, apply(PRISM[, grep(pattern = "defoliators", colnames(PRISM), perl = TRUE)], 1, sum))
wht <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers*tillers, apply(PRISM[, grep(pattern = "whitehead", colnames(PRISM), perl = TRUE)], 1, sum))
wht.graph <- na.omit(subset(wht, visit.visit == "Ripening")) # no white head until second visit
rgb <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, panicles, apply(PRISM[, grep(pattern = "ricegrainbug", colnames(PRISM), perl = TRUE)], 1, sum))
rgb.graph <- na.omit(subset(rgb, visit.visit == "Ripening")) # no grain bug damage until second visit
rbg <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, panicles, apply(PRISM[, grep(pattern = "ricebug", colnames(PRISM), perl = TRUE)], 1, sum))
dht <- data.frame(PRISM[, c(8:9, 12, 16:18)], visit$visit, tillers, apply(PRISM[, grep(pattern = "deadheart", colnames(PRISM), perl = TRUE)], 1, sum))

names(lfd) <- names(lfm) <- names(thp) <- names(whm) <- names(def) <- names(wht) <- names(rgb) <- names(rbg) <- names(dht) <- c("lat", "lon", "locID", "Municipality", "Province", "Region", "visit", "organ", "injury")

lfd.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = lfd, FUN = mean, na.rm = TRUE)
lfm.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = lfm, FUN = mean, na.rm = TRUE)
thp.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = thp, FUN = mean, na.rm = TRUE)
whm.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = whm, FUN = mean, na.rm = TRUE)
def.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = def, FUN = mean, na.rm = TRUE)
wht.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = wht, FUN = mean, na.rm = TRUE)
rgb.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = rgb, FUN = mean, na.rm = TRUE)
rbg.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = rbg, FUN = mean, na.rm = TRUE)
dht.summary <- summaryBy(injury+organ+lat+lon~Municipality+visit, data = dht, FUN = mean, na.rm = TRUE)

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