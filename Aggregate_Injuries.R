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
visit[, 1] <- as.Date(visit[, 1])
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

#### generate weighted average vectors of non-systemic diseases, from 10 samples for each observation, for graphing ####
bak <- apply(data.frame(PRISM[, grep(pattern = "group_1_[[:graph:]]*bakanae", colnames(PRISM), perl = TRUE)]/tillers[, 1],
                  PRISM[, grep(pattern = "group_2_[[:graph:]]*bakanae", colnames(PRISM), perl = TRUE)]/tillers[, 2],
                  PRISM[, grep(pattern = "group_3_[[:graph:]]*bakanae", colnames(PRISM), perl = TRUE)]/tillers[, 3],
                  PRISM[, grep(pattern = "group_4_[[:graph:]]*bakanae", colnames(PRISM), perl = TRUE)]/tillers[, 4],
                  PRISM[, grep(pattern = "group_5_[[:graph:]]*bakanae", colnames(PRISM), perl = TRUE)]/tillers[, 5],
                  PRISM[, grep(pattern = "group_6_[[:graph:]]*bakanae", colnames(PRISM), perl = TRUE)]/tillers[, 6],
                  PRISM[, grep(pattern = "group_7_[[:graph:]]*bakanae", colnames(PRISM), perl = TRUE)]/tillers[, 7],
                  PRISM[, grep(pattern = "group_8_[[:graph:]]*bakanae", colnames(PRISM), perl = TRUE)]/tillers[, 8],
                  PRISM[, grep(pattern = "group_9_[[:graph:]]*bakanae", colnames(PRISM), perl = TRUE)]/tillers[, 9],
                  PRISM[, grep(pattern = "group_10_[[:graph:]]*bakanae", colnames(PRISM), perl = TRUE)]/tillers[, 10]), 1, mean)

blb <- apply(data.frame(PRISM[, grep(pattern = "group_1_[[:graph:]]*bacterialleafblight", colnames(PRISM), perl = TRUE)]/(leaves[, 1]*tillers[, 1]),
                        PRISM[, grep(pattern = "group_2_[[:graph:]]*bacterialleafblight", colnames(PRISM), perl = TRUE)]/(leaves[, 2]*tillers[, 2]),
                        PRISM[, grep(pattern = "group_3_[[:graph:]]*bacterialleafblight", colnames(PRISM), perl = TRUE)]/(leaves[, 3]*tillers[, 3]),
                        PRISM[, grep(pattern = "group_4_[[:graph:]]*bacterialleafblight", colnames(PRISM), perl = TRUE)]/(leaves[, 4]*tillers[, 4]),
                        PRISM[, grep(pattern = "group_5_[[:graph:]]*bacterialleafblight", colnames(PRISM), perl = TRUE)]/(leaves[, 5]*tillers[, 5]),
                        PRISM[, grep(pattern = "group_6_[[:graph:]]*bacterialleafblight", colnames(PRISM), perl = TRUE)]/(leaves[, 6]*tillers[, 6]),
                        PRISM[, grep(pattern = "group_7_[[:graph:]]*bacterialleafblight", colnames(PRISM), perl = TRUE)]/(leaves[, 7]*tillers[, 7]),
                        PRISM[, grep(pattern = "group_8_[[:graph:]]*bacterialleafblight", colnames(PRISM), perl = TRUE)]/(leaves[, 8]*tillers[, 8]),
                        PRISM[, grep(pattern = "group_9_[[:graph:]]*bacterialleafblight", colnames(PRISM), perl = TRUE)]/(leaves[, 9]*tillers[, 9]),
                        PRISM[, grep(pattern = "group_10_[[:graph:]]*bacterialleafblight", colnames(PRISM), perl = TRUE)]/(leaves[, 10]*tillers[, 10])), 1, mean)

bls <- apply(data.frame(PRISM[, grep(pattern = "group_1_[[:graph:]]*bacterialleafstreak", colnames(PRISM), perl = TRUE)]/(leaves[, 1]*tillers[, 1]),
                        PRISM[, grep(pattern = "group_2_[[:graph:]]*bacterialleafstreak", colnames(PRISM), perl = TRUE)]/(leaves[, 2]*tillers[, 2]),
                        PRISM[, grep(pattern = "group_3_[[:graph:]]*bacterialleafstreak", colnames(PRISM), perl = TRUE)]/(leaves[, 3]*tillers[, 3]),
                        PRISM[, grep(pattern = "group_4_[[:graph:]]*bacterialleafstreak", colnames(PRISM), perl = TRUE)]/(leaves[, 4]*tillers[, 4]),
                        PRISM[, grep(pattern = "group_5_[[:graph:]]*bacterialleafstreak", colnames(PRISM), perl = TRUE)]/(leaves[, 5]*tillers[, 5]),
                        PRISM[, grep(pattern = "group_6_[[:graph:]]*bacterialleafstreak", colnames(PRISM), perl = TRUE)]/(leaves[, 6]*tillers[, 6]),
                        PRISM[, grep(pattern = "group_7_[[:graph:]]*bacterialleafstreak", colnames(PRISM), perl = TRUE)]/(leaves[, 7]*tillers[, 7]),
                        PRISM[, grep(pattern = "group_8_[[:graph:]]*bacterialleafstreak", colnames(PRISM), perl = TRUE)]/(leaves[, 8]*tillers[, 8]),
                        PRISM[, grep(pattern = "group_9_[[:graph:]]*bacterialleafstreak", colnames(PRISM), perl = TRUE)]/(leaves[, 9]*tillers[, 9]),
                        PRISM[, grep(pattern = "group_10_[[:graph:]]*bacterialleafstreak", colnames(PRISM), perl = TRUE)]/(leaves[, 10]*tillers[, 10])), 1, mean)

bst <- apply(data.frame(PRISM[, grep(pattern = "group_1_[[:graph:]]*(?<!narrow)(?i)brownspot", colnames(PRISM), perl = TRUE)]/(leaves[, 1]*tillers[, 1]),
                  PRISM[, grep(pattern = "group_2_[[:graph:]]*(?<!narrow)(?i)brownspot", colnames(PRISM), perl = TRUE)]/(leaves[, 2]*tillers[, 2]),
                  PRISM[, grep(pattern = "group_3_[[:graph:]]*(?<!narrow)(?i)brownspot", colnames(PRISM), perl = TRUE)]/(leaves[, 3]*tillers[, 3]),
                  PRISM[, grep(pattern = "group_4_[[:graph:]]*(?<!narrow)(?i)brownspot", colnames(PRISM), perl = TRUE)]/(leaves[, 4]*tillers[, 4]),
                  PRISM[, grep(pattern = "group_5_[[:graph:]]*(?<!narrow)(?i)brownspot", colnames(PRISM), perl = TRUE)]/(leaves[, 5]*tillers[, 5]),
                  PRISM[, grep(pattern = "group_6_[[:graph:]]*(?<!narrow)(?i)brownspot", colnames(PRISM), perl = TRUE)]/(leaves[, 6]*tillers[, 6]),
                  PRISM[, grep(pattern = "group_7_[[:graph:]]*(?<!narrow)(?i)brownspot", colnames(PRISM), perl = TRUE)]/(leaves[, 7]*tillers[, 7]),
                  PRISM[, grep(pattern = "group_8_[[:graph:]]*(?<!narrow)(?i)brownspot", colnames(PRISM), perl = TRUE)]/(leaves[, 8]*tillers[, 8]),
                  PRISM[, grep(pattern = "group_9_[[:graph:]]*(?<!narrow)(?i)brownspot", colnames(PRISM), perl = TRUE)]/(leaves[, 9]*tillers[, 9]),
                  PRISM[, grep(pattern = "group_10_[[:graph:]]*(?<!narrow)(?i)brownspot", colnames(PRISM), perl = TRUE)]/(leaves[, 10]*tillers[, 10])), 1, mean)

fsm <- apply(data.frame(PRISM[, grep(pattern = "group_1_[[:graph:]]*falsesmut", colnames(PRISM), perl = TRUE)]/tillers[, 1],
                        PRISM[, grep(pattern = "group_2_[[:graph:]]*falsesmut", colnames(PRISM), perl = TRUE)]/tillers[, 2],
                        PRISM[, grep(pattern = "group_3_[[:graph:]]*falsesmut", colnames(PRISM), perl = TRUE)]/tillers[, 3],
                        PRISM[, grep(pattern = "group_4_[[:graph:]]*falsesmut", colnames(PRISM), perl = TRUE)]/tillers[, 4],
                        PRISM[, grep(pattern = "group_5_[[:graph:]]*falsesmut", colnames(PRISM), perl = TRUE)]/tillers[, 5],
                        PRISM[, grep(pattern = "group_6_[[:graph:]]*falsesmut", colnames(PRISM), perl = TRUE)]/tillers[, 6],
                        PRISM[, grep(pattern = "group_7_[[:graph:]]*falsesmut", colnames(PRISM), perl = TRUE)]/tillers[, 7],
                        PRISM[, grep(pattern = "group_8_[[:graph:]]*falsesmut", colnames(PRISM), perl = TRUE)]/tillers[, 8],
                        PRISM[, grep(pattern = "group_9_[[:graph:]]*falsesmut", colnames(PRISM), perl = TRUE)]/tillers[, 9],
                        PRISM[, grep(pattern = "group_10_[[:graph:]]*falsesmut", colnames(PRISM), perl = TRUE)]/tillers[, 10]), 1, mean)
fsm.graph <- na.omit(subset(fsm, visit.visit == "Ripening")) # no false smut before heading

dip <- apply(data.frame(PRISM[, grep(pattern = "group_1_[[:graph:]]*dirtypanicle", colnames(PRISM), perl = TRUE)]/tillers[, 1],
                        PRISM[, grep(pattern = "group_2_[[:graph:]]*dirtypanicle", colnames(PRISM), perl = TRUE)]/tillers[, 2],
                        PRISM[, grep(pattern = "group_3_[[:graph:]]*dirtypanicle", colnames(PRISM), perl = TRUE)]/tillers[, 3],
                        PRISM[, grep(pattern = "group_4_[[:graph:]]*dirtypanicle", colnames(PRISM), perl = TRUE)]/tillers[, 4],
                        PRISM[, grep(pattern = "group_5_[[:graph:]]*dirtypanicle", colnames(PRISM), perl = TRUE)]/tillers[, 5],
                        PRISM[, grep(pattern = "group_6_[[:graph:]]*dirtypanicle", colnames(PRISM), perl = TRUE)]/tillers[, 6],
                        PRISM[, grep(pattern = "group_7_[[:graph:]]*dirtypanicle", colnames(PRISM), perl = TRUE)]/tillers[, 7],
                        PRISM[, grep(pattern = "group_8_[[:graph:]]*dirtypanicle", colnames(PRISM), perl = TRUE)]/tillers[, 8],
                        PRISM[, grep(pattern = "group_9_[[:graph:]]*dirtypanicle", colnames(PRISM), perl = TRUE)]/tillers[, 9],
                        PRISM[, grep(pattern = "group_10_[[:graph:]]*dirtypanicle", colnames(PRISM), perl = TRUE)]/tillers[, 10]), 1, mean)

lba <- apply(data.frame(PRISM[, grep(pattern = "group_1_[[:graph:]]*leafblast", colnames(PRISM), perl = TRUE)]/(leaves[, 1]*tillers[, 1]),
                        PRISM[, grep(pattern = "group_2_[[:graph:]]*leafblast", colnames(PRISM), perl = TRUE)]/(leaves[, 2]*tillers[, 2]),
                        PRISM[, grep(pattern = "group_3_[[:graph:]]*leafblast", colnames(PRISM), perl = TRUE)]/(leaves[, 3]*tillers[, 3]),
                        PRISM[, grep(pattern = "group_4_[[:graph:]]*leafblast", colnames(PRISM), perl = TRUE)]/(leaves[, 4]*tillers[, 4]),
                        PRISM[, grep(pattern = "group_5_[[:graph:]]*leafblast", colnames(PRISM), perl = TRUE)]/(leaves[, 5]*tillers[, 5]),
                        PRISM[, grep(pattern = "group_6_[[:graph:]]*leafblast", colnames(PRISM), perl = TRUE)]/(leaves[, 6]*tillers[, 6]),
                        PRISM[, grep(pattern = "group_7_[[:graph:]]*leafblast", colnames(PRISM), perl = TRUE)]/(leaves[, 7]*tillers[, 7]),
                        PRISM[, grep(pattern = "group_8_[[:graph:]]*leafblast", colnames(PRISM), perl = TRUE)]/(leaves[, 8]*tillers[, 8]),
                        PRISM[, grep(pattern = "group_9_[[:graph:]]*leafblast", colnames(PRISM), perl = TRUE)]/(leaves[, 9]*tillers[, 9]),
                        PRISM[, grep(pattern = "group_10_[[:graph:]]*leafblast", colnames(PRISM), perl = TRUE)]/(leaves[, 10]*tillers[, 10])), 1, mean)

lsc <- apply(data.frame(PRISM[, grep(pattern = "group_1_[[:graph:]]*leafscald", colnames(PRISM), perl = TRUE)]/(leaves[, 1]*tillers[, 1]),
                        PRISM[, grep(pattern = "group_2_[[:graph:]]*leafscald", colnames(PRISM), perl = TRUE)]/(leaves[, 2]*tillers[, 2]),
                        PRISM[, grep(pattern = "group_3_[[:graph:]]*leafscald", colnames(PRISM), perl = TRUE)]/(leaves[, 3]*tillers[, 3]),
                        PRISM[, grep(pattern = "group_4_[[:graph:]]*leafscald", colnames(PRISM), perl = TRUE)]/(leaves[, 4]*tillers[, 4]),
                        PRISM[, grep(pattern = "group_5_[[:graph:]]*leafscald", colnames(PRISM), perl = TRUE)]/(leaves[, 5]*tillers[, 5]),
                        PRISM[, grep(pattern = "group_6_[[:graph:]]*leafscald", colnames(PRISM), perl = TRUE)]/(leaves[, 6]*tillers[, 6]),
                        PRISM[, grep(pattern = "group_7_[[:graph:]]*leafscald", colnames(PRISM), perl = TRUE)]/(leaves[, 7]*tillers[, 7]),
                        PRISM[, grep(pattern = "group_8_[[:graph:]]*leafscald", colnames(PRISM), perl = TRUE)]/(leaves[, 8]*tillers[, 8]),
                        PRISM[, grep(pattern = "group_9_[[:graph:]]*leafscald", colnames(PRISM), perl = TRUE)]/(leaves[, 9]*tillers[, 9]),
                        PRISM[, grep(pattern = "group_10_[[:graph:]]*leafscald", colnames(PRISM), perl = TRUE)]/(leaves[, 10]*tillers[, 10])), 1, mean)

nba <- apply(data.frame(PRISM[, grep(pattern = "group_1_[[:graph:]]*neckblast", colnames(PRISM), perl = TRUE)]/tillers[, 1],
                        PRISM[, grep(pattern = "group_2_[[:graph:]]*neckblast", colnames(PRISM), perl = TRUE)]/tillers[, 2],
                        PRISM[, grep(pattern = "group_3_[[:graph:]]*neckblast", colnames(PRISM), perl = TRUE)]/tillers[, 3],
                        PRISM[, grep(pattern = "group_4_[[:graph:]]*neckblast", colnames(PRISM), perl = TRUE)]/tillers[, 4],
                        PRISM[, grep(pattern = "group_5_[[:graph:]]*neckblast", colnames(PRISM), perl = TRUE)]/tillers[, 5],
                        PRISM[, grep(pattern = "group_6_[[:graph:]]*neckblast", colnames(PRISM), perl = TRUE)]/tillers[, 6],
                        PRISM[, grep(pattern = "group_7_[[:graph:]]*neckblast", colnames(PRISM), perl = TRUE)]/tillers[, 7],
                        PRISM[, grep(pattern = "group_8_[[:graph:]]*neckblast", colnames(PRISM), perl = TRUE)]/tillers[, 8],
                        PRISM[, grep(pattern = "group_9_[[:graph:]]*neckblast", colnames(PRISM), perl = TRUE)]/tillers[, 9],
                        PRISM[, grep(pattern = "group_10_[[:graph:]]*neckblast", colnames(PRISM), perl = TRUE)]/tillers[, 10]), 1, mean)
nba.graph <- na.omit(subset(nba, nba$visit.visit == "Ripening")) # no neck blast until second visit

nbs <- apply(data.frame(PRISM[, grep(pattern = "group_1_[[:graph:]]*narrowbrownspot", colnames(PRISM), perl = TRUE)]/(leaves[, 1]*tillers[, 1]),
                        PRISM[, grep(pattern = "group_2_[[:graph:]]*narrowbrownspot", colnames(PRISM), perl = TRUE)]/(leaves[, 2]*tillers[, 2]),
                        PRISM[, grep(pattern = "group_3_[[:graph:]]*narrowbrownspot", colnames(PRISM), perl = TRUE)]/(leaves[, 3]*tillers[, 3]),
                        PRISM[, grep(pattern = "group_4_[[:graph:]]*narrowbrownspot", colnames(PRISM), perl = TRUE)]/(leaves[, 4]*tillers[, 4]),
                        PRISM[, grep(pattern = "group_5_[[:graph:]]*narrowbrownspot", colnames(PRISM), perl = TRUE)]/(leaves[, 5]*tillers[, 5]),
                        PRISM[, grep(pattern = "group_6_[[:graph:]]*narrowbrownspot", colnames(PRISM), perl = TRUE)]/(leaves[, 6]*tillers[, 6]),
                        PRISM[, grep(pattern = "group_7_[[:graph:]]*narrowbrownspot", colnames(PRISM), perl = TRUE)]/(leaves[, 7]*tillers[, 7]),
                        PRISM[, grep(pattern = "group_8_[[:graph:]]*narrowbrownspot", colnames(PRISM), perl = TRUE)]/(leaves[, 8]*tillers[, 8]),
                        PRISM[, grep(pattern = "group_9_[[:graph:]]*narrowbrownspot", colnames(PRISM), perl = TRUE)]/(leaves[, 9]*tillers[, 9]),
                        PRISM[, grep(pattern = "group_10_[[:graph:]]*narrowbrownspot", colnames(PRISM), perl = TRUE)]/(leaves[, 10]*tillers[, 10])), 1, mean)

rsp <- apply(data.frame(PRISM[, grep(pattern = "group_1_[[:graph:]]*redstripe", colnames(PRISM), perl = TRUE)]/(leaves[, 1]*tillers[, 1]),
                        PRISM[, grep(pattern = "group_2_[[:graph:]]*redstripe", colnames(PRISM), perl = TRUE)]/(leaves[, 2]*tillers[, 2]),
                        PRISM[, grep(pattern = "group_3_[[:graph:]]*redstripe", colnames(PRISM), perl = TRUE)]/(leaves[, 3]*tillers[, 3]),
                        PRISM[, grep(pattern = "group_4_[[:graph:]]*redstripe", colnames(PRISM), perl = TRUE)]/(leaves[, 4]*tillers[, 4]),
                        PRISM[, grep(pattern = "group_5_[[:graph:]]*redstripe", colnames(PRISM), perl = TRUE)]/(leaves[, 5]*tillers[, 5]),
                        PRISM[, grep(pattern = "group_6_[[:graph:]]*redstripe", colnames(PRISM), perl = TRUE)]/(leaves[, 6]*tillers[, 6]),
                        PRISM[, grep(pattern = "group_7_[[:graph:]]*redstripe", colnames(PRISM), perl = TRUE)]/(leaves[, 7]*tillers[, 7]),
                        PRISM[, grep(pattern = "group_8_[[:graph:]]*redstripe", colnames(PRISM), perl = TRUE)]/(leaves[, 8]*tillers[, 8]),
                        PRISM[, grep(pattern = "group_9_[[:graph:]]*redstripe", colnames(PRISM), perl = TRUE)]/(leaves[, 9]*tillers[, 9]),
                        PRISM[, grep(pattern = "group_10_[[:graph:]]*redstripe", colnames(PRISM), perl = TRUE)]/(leaves[, 10]*tillers[, 10])), 1, mean)

shr <- apply(data.frame(PRISM[, grep(pattern = "group_1_[[:graph:]]*sheathrot", colnames(PRISM), perl = TRUE)]/tillers[, 1],
                        PRISM[, grep(pattern = "group_2_[[:graph:]]*sheathrot", colnames(PRISM), perl = TRUE)]/tillers[, 2],
                        PRISM[, grep(pattern = "group_3_[[:graph:]]*sheathrot", colnames(PRISM), perl = TRUE)]/tillers[, 3],
                        PRISM[, grep(pattern = "group_4_[[:graph:]]*sheathrot", colnames(PRISM), perl = TRUE)]/tillers[, 4],
                        PRISM[, grep(pattern = "group_5_[[:graph:]]*sheathrot", colnames(PRISM), perl = TRUE)]/tillers[, 5],
                        PRISM[, grep(pattern = "group_6_[[:graph:]]*sheathrot", colnames(PRISM), perl = TRUE)]/tillers[, 6],
                        PRISM[, grep(pattern = "group_7_[[:graph:]]*sheathrot", colnames(PRISM), perl = TRUE)]/tillers[, 7],
                        PRISM[, grep(pattern = "group_8_[[:graph:]]*sheathrot", colnames(PRISM), perl = TRUE)]/tillers[, 8],
                        PRISM[, grep(pattern = "group_9_[[:graph:]]*sheathrot", colnames(PRISM), perl = TRUE)]/tillers[, 9],
                        PRISM[, grep(pattern = "group_10_[[:graph:]]*sheathrot", colnames(PRISM), perl = TRUE)]/tillers[, 10]), 1, mean)

shb <- apply(data.frame(PRISM[, grep(pattern = "group_1_[[:graph:]]*sheathblight", colnames(PRISM), perl = TRUE)]/tillers[, 1],
                        PRISM[, grep(pattern = "group_2_[[:graph:]]*sheathblight", colnames(PRISM), perl = TRUE)]/tillers[, 2],
                        PRISM[, grep(pattern = "group_3_[[:graph:]]*sheathblight", colnames(PRISM), perl = TRUE)]/tillers[, 3],
                        PRISM[, grep(pattern = "group_4_[[:graph:]]*sheathblight", colnames(PRISM), perl = TRUE)]/tillers[, 4],
                        PRISM[, grep(pattern = "group_5_[[:graph:]]*sheathblight", colnames(PRISM), perl = TRUE)]/tillers[, 5],
                        PRISM[, grep(pattern = "group_6_[[:graph:]]*sheathblight", colnames(PRISM), perl = TRUE)]/tillers[, 6],
                        PRISM[, grep(pattern = "group_7_[[:graph:]]*sheathblight", colnames(PRISM), perl = TRUE)]/tillers[, 7],
                        PRISM[, grep(pattern = "group_8_[[:graph:]]*sheathblight", colnames(PRISM), perl = TRUE)]/tillers[, 8],
                        PRISM[, grep(pattern = "group_9_[[:graph:]]*sheathblight", colnames(PRISM), perl = TRUE)]/tillers[, 9],
                        PRISM[, grep(pattern = "group_10_[[:graph:]]*sheathblight", colnames(PRISM), perl = TRUE)]/tillers[, 10]), 1, mean)

str <- apply(data.frame(PRISM[, grep(pattern = "group_1_[[:graph:]]*stemrot", colnames(PRISM), perl = TRUE)]/tillers[, 1],
                        PRISM[, grep(pattern = "group_2_[[:graph:]]*stemrot", colnames(PRISM), perl = TRUE)]/tillers[, 2],
                        PRISM[, grep(pattern = "group_3_[[:graph:]]*stemrot", colnames(PRISM), perl = TRUE)]/tillers[, 3],
                        PRISM[, grep(pattern = "group_4_[[:graph:]]*stemrot", colnames(PRISM), perl = TRUE)]/tillers[, 4],
                        PRISM[, grep(pattern = "group_5_[[:graph:]]*stemrot", colnames(PRISM), perl = TRUE)]/tillers[, 5],
                        PRISM[, grep(pattern = "group_6_[[:graph:]]*stemrot", colnames(PRISM), perl = TRUE)]/tillers[, 6],
                        PRISM[, grep(pattern = "group_7_[[:graph:]]*stemrot", colnames(PRISM), perl = TRUE)]/tillers[, 7],
                        PRISM[, grep(pattern = "group_8_[[:graph:]]*stemrot", colnames(PRISM), perl = TRUE)]/tillers[, 8],
                        PRISM[, grep(pattern = "group_9_[[:graph:]]*stemrot", colnames(PRISM), perl = TRUE)]/tillers[, 9],
                        PRISM[, grep(pattern = "group_10_[[:graph:]]*stemrot", colnames(PRISM), perl = TRUE)]/tillers[, 10]), 1, mean)

#### generate data frames of snail and rat damage ####
gas <- apply(data.frame(PRISM[, grep(pattern = "group_1_[[:graph:]]*gas", colnames(PRISM), perl = TRUE)]/tillers[, 1],
                        PRISM[, grep(pattern = "group_2_[[:graph:]]*gas", colnames(PRISM), perl = TRUE)]/tillers[, 2],
                        PRISM[, grep(pattern = "group_3_[[:graph:]]*gas", colnames(PRISM), perl = TRUE)]/tillers[, 3],
                        PRISM[, grep(pattern = "group_4_[[:graph:]]*gas", colnames(PRISM), perl = TRUE)]/tillers[, 4],
                        PRISM[, grep(pattern = "group_5_[[:graph:]]*gas", colnames(PRISM), perl = TRUE)]/tillers[, 5],
                        PRISM[, grep(pattern = "group_6_[[:graph:]]*gas", colnames(PRISM), perl = TRUE)]/tillers[, 6],
                        PRISM[, grep(pattern = "group_7_[[:graph:]]*gas", colnames(PRISM), perl = TRUE)]/tillers[, 7],
                        PRISM[, grep(pattern = "group_8_[[:graph:]]*gas", colnames(PRISM), perl = TRUE)]/tillers[, 8],
                        PRISM[, grep(pattern = "group_9_[[:graph:]]*gas", colnames(PRISM), perl = TRUE)]/tillers[, 9],
                        PRISM[, grep(pattern = "group_10_[[:graph:]]*gas", colnames(PRISM), perl = TRUE)]/tillers[, 10]), 1, mean)

rat <- apply(data.frame(PRISM[, grep(pattern = "group_1_[[:graph:]]*rat", colnames(PRISM), perl = TRUE)]/tillers[, 1],
                        PRISM[, grep(pattern = "group_2_[[:graph:]]*rat", colnames(PRISM), perl = TRUE)]/tillers[, 2],
                        PRISM[, grep(pattern = "group_3_[[:graph:]]*rat", colnames(PRISM), perl = TRUE)]/tillers[, 3],
                        PRISM[, grep(pattern = "group_4_[[:graph:]]*rat", colnames(PRISM), perl = TRUE)]/tillers[, 4],
                        PRISM[, grep(pattern = "group_5_[[:graph:]]*rat", colnames(PRISM), perl = TRUE)]/tillers[, 5],
                        PRISM[, grep(pattern = "group_6_[[:graph:]]*rat", colnames(PRISM), perl = TRUE)]/tillers[, 6],
                        PRISM[, grep(pattern = "group_7_[[:graph:]]*rat", colnames(PRISM), perl = TRUE)]/tillers[, 7],
                        PRISM[, grep(pattern = "group_8_[[:graph:]]*rat", colnames(PRISM), perl = TRUE)]/tillers[, 8],
                        PRISM[, grep(pattern = "group_9_[[:graph:]]*rat", colnames(PRISM), perl = TRUE)]/tillers[, 9],
                        PRISM[, grep(pattern = "group_10_[[:graph:]]*rat", colnames(PRISM), perl = TRUE)]/tillers[, 10]), 1, mean)


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

def <- apply(data.frame(PRISM[, grep(pattern = "group_1_[[:graph:]]*defoliator", colnames(PRISM), perl = TRUE)]/(leaves[, 1]*tillers[, 1]),
                        PRISM[, grep(pattern = "group_2_[[:graph:]]*defoliator", colnames(PRISM), perl = TRUE)]/(leaves[, 2]*tillers[, 2]),
                        PRISM[, grep(pattern = "group_3_[[:graph:]]*defoliator", colnames(PRISM), perl = TRUE)]/(leaves[, 3]*tillers[, 3]),
                        PRISM[, grep(pattern = "group_4_[[:graph:]]*defoliator", colnames(PRISM), perl = TRUE)]/(leaves[, 4]*tillers[, 4]),
                        PRISM[, grep(pattern = "group_5_[[:graph:]]*defoliator", colnames(PRISM), perl = TRUE)]/(leaves[, 5]*tillers[, 5]),
                        PRISM[, grep(pattern = "group_6_[[:graph:]]*defoliator", colnames(PRISM), perl = TRUE)]/(leaves[, 6]*tillers[, 6]),
                        PRISM[, grep(pattern = "group_7_[[:graph:]]*defoliator", colnames(PRISM), perl = TRUE)]/(leaves[, 7]*tillers[, 7]),
                        PRISM[, grep(pattern = "group_8_[[:graph:]]*defoliator", colnames(PRISM), perl = TRUE)]/(leaves[, 8]*tillers[, 8]),
                        PRISM[, grep(pattern = "group_9_[[:graph:]]*defoliator", colnames(PRISM), perl = TRUE)]/(leaves[, 9]*tillers[, 9]),
                        PRISM[, grep(pattern = "group_10_[[:graph:]]*defoliator", colnames(PRISM), perl = TRUE)]/(leaves[, 10]*tillers[, 10])), 1, mean)

dht<- apply(data.frame(PRISM[, grep(pattern = "group_1_[[:graph:]]*deadheart", colnames(PRISM), perl = TRUE)]/tillers[, 1],
                       PRISM[, grep(pattern = "group_2_[[:graph:]]*deadheart", colnames(PRISM), perl = TRUE)]/tillers[, 2],
                       PRISM[, grep(pattern = "group_3_[[:graph:]]*deadheart", colnames(PRISM), perl = TRUE)]/tillers[, 3],
                       PRISM[, grep(pattern = "group_4_[[:graph:]]*deadheart", colnames(PRISM), perl = TRUE)]/tillers[, 4],
                       PRISM[, grep(pattern = "group_5_[[:graph:]]*deadheart", colnames(PRISM), perl = TRUE)]/tillers[, 5],
                       PRISM[, grep(pattern = "group_6_[[:graph:]]*deadheart", colnames(PRISM), perl = TRUE)]/tillers[, 6],
                       PRISM[, grep(pattern = "group_7_[[:graph:]]*deadheart", colnames(PRISM), perl = TRUE)]/tillers[, 7],
                       PRISM[, grep(pattern = "group_8_[[:graph:]]*deadheart", colnames(PRISM), perl = TRUE)]/tillers[, 8],
                       PRISM[, grep(pattern = "group_9_[[:graph:]]*deadheart", colnames(PRISM), perl = TRUE)]/tillers[, 9],
                       PRISM[, grep(pattern = "group_10_[[:graph:]]*deadheart", colnames(PRISM), perl = TRUE)]/tillers[, 10]), 1, mean)

lfd <- apply(data.frame(PRISM[, grep(pattern = "group_1_[[:graph:]]*leaffolder", colnames(PRISM), perl = TRUE)]/(leaves[, 1]*tillers[, 1]),
                        PRISM[, grep(pattern = "group_2_[[:graph:]]*leaffolder", colnames(PRISM), perl = TRUE)]/(leaves[, 2]*tillers[, 2]),
                        PRISM[, grep(pattern = "group_3_[[:graph:]]*leaffolder", colnames(PRISM), perl = TRUE)]/(leaves[, 3]*tillers[, 3]),
                        PRISM[, grep(pattern = "group_4_[[:graph:]]*leaffolder", colnames(PRISM), perl = TRUE)]/(leaves[, 4]*tillers[, 4]),
                        PRISM[, grep(pattern = "group_5_[[:graph:]]*leaffolder", colnames(PRISM), perl = TRUE)]/(leaves[, 5]*tillers[, 5]),
                        PRISM[, grep(pattern = "group_6_[[:graph:]]*leaffolder", colnames(PRISM), perl = TRUE)]/(leaves[, 6]*tillers[, 6]),
                        PRISM[, grep(pattern = "group_7_[[:graph:]]*leaffolder", colnames(PRISM), perl = TRUE)]/(leaves[, 7]*tillers[, 7]),
                        PRISM[, grep(pattern = "group_8_[[:graph:]]*leaffolder", colnames(PRISM), perl = TRUE)]/(leaves[, 8]*tillers[, 8]),
                        PRISM[, grep(pattern = "group_9_[[:graph:]]*leaffolder", colnames(PRISM), perl = TRUE)]/(leaves[, 9]*tillers[, 9]),
                        PRISM[, grep(pattern = "group_10_[[:graph:]]*leaffolder", colnames(PRISM), perl = TRUE)]/(leaves[, 10]*tillers[, 10])), 1, mean)

lfm <- apply(data.frame(PRISM[, grep(pattern = "group_1_[[:graph:]]*leafminer", colnames(PRISM), perl = TRUE)]/(leaves[, 1]*tillers[, 1]),
                        PRISM[, grep(pattern = "group_2_[[:graph:]]*leafminer", colnames(PRISM), perl = TRUE)]/(leaves[, 2]*tillers[, 2]),
                        PRISM[, grep(pattern = "group_3_[[:graph:]]*leafminer", colnames(PRISM), perl = TRUE)]/(leaves[, 3]*tillers[, 3]),
                        PRISM[, grep(pattern = "group_4_[[:graph:]]*leafminer", colnames(PRISM), perl = TRUE)]/(leaves[, 4]*tillers[, 4]),
                        PRISM[, grep(pattern = "group_5_[[:graph:]]*leafminer", colnames(PRISM), perl = TRUE)]/(leaves[, 5]*tillers[, 5]),
                        PRISM[, grep(pattern = "group_6_[[:graph:]]*leafminer", colnames(PRISM), perl = TRUE)]/(leaves[, 6]*tillers[, 6]),
                        PRISM[, grep(pattern = "group_7_[[:graph:]]*leafminer", colnames(PRISM), perl = TRUE)]/(leaves[, 7]*tillers[, 7]),
                        PRISM[, grep(pattern = "group_8_[[:graph:]]*leafminer", colnames(PRISM), perl = TRUE)]/(leaves[, 8]*tillers[, 8]),
                        PRISM[, grep(pattern = "group_9_[[:graph:]]*leafminer", colnames(PRISM), perl = TRUE)]/(leaves[, 9]*tillers[, 9]),
                        PRISM[, grep(pattern = "group_10_[[:graph:]]*leafminer", colnames(PRISM), perl = TRUE)]/(leaves[, 10]*tillers[, 10])), 1, mean)

rgb <- apply(data.frame(PRISM[, grep(pattern = "group_1_[[:graph:]]*ricegrainbug", colnames(PRISM), perl = TRUE)]/panicles[, 1],
                        PRISM[, grep(pattern = "group_2_[[:graph:]]*ricegrainbug", colnames(PRISM), perl = TRUE)]/panicles[, 2],
                        PRISM[, grep(pattern = "group_3_[[:graph:]]*ricegrainbug", colnames(PRISM), perl = TRUE)]/panicles[, 3],
                        PRISM[, grep(pattern = "group_4_[[:graph:]]*ricegrainbug", colnames(PRISM), perl = TRUE)]/panicles[, 4],
                        PRISM[, grep(pattern = "group_5_[[:graph:]]*ricegrainbug", colnames(PRISM), perl = TRUE)]/panicles[, 5],
                        PRISM[, grep(pattern = "group_6_[[:graph:]]*ricegrainbug", colnames(PRISM), perl = TRUE)]/panicles[, 6],
                        PRISM[, grep(pattern = "group_7_[[:graph:]]*ricegrainbug", colnames(PRISM), perl = TRUE)]/panicles[, 7],
                        PRISM[, grep(pattern = "group_8_[[:graph:]]*ricegrainbug", colnames(PRISM), perl = TRUE)]/panicles[, 8],
                        PRISM[, grep(pattern = "group_9_[[:graph:]]*ricegrainbug", colnames(PRISM), perl = TRUE)]/panicles[, 9],
                        PRISM[, grep(pattern = "group_10_[[:graph:]]*ricegrainbug", colnames(PRISM), perl = TRUE)]/panicles[, 10]), 1, mean)
rgb.graph <- na.omit(subset(rgb, visit.visit == "Ripening")) # no grain bug damage until second visit

rbg <- apply(data.frame(PRISM[, grep(pattern = "group_1_[[:graph:]]*ricebug", colnames(PRISM), perl = TRUE)]/panicles[, 1],
                        PRISM[, grep(pattern = "group_2_[[:graph:]]*ricebug", colnames(PRISM), perl = TRUE)]/panicles[, 2],
                        PRISM[, grep(pattern = "group_3_[[:graph:]]*ricebug", colnames(PRISM), perl = TRUE)]/panicles[, 3],
                        PRISM[, grep(pattern = "group_4_[[:graph:]]*ricebug", colnames(PRISM), perl = TRUE)]/panicles[, 4],
                        PRISM[, grep(pattern = "group_5_[[:graph:]]*ricebug", colnames(PRISM), perl = TRUE)]/panicles[, 5],
                        PRISM[, grep(pattern = "group_6_[[:graph:]]*ricebug", colnames(PRISM), perl = TRUE)]/panicles[, 6],
                        PRISM[, grep(pattern = "group_7_[[:graph:]]*ricebug", colnames(PRISM), perl = TRUE)]/panicles[, 7],
                        PRISM[, grep(pattern = "group_8_[[:graph:]]*ricebug", colnames(PRISM), perl = TRUE)]/panicles[, 8],
                        PRISM[, grep(pattern = "group_9_[[:graph:]]*ricebug", colnames(PRISM), perl = TRUE)]/panicles[, 9],
                        PRISM[, grep(pattern = "group_10_[[:graph:]]*ricebug", colnames(PRISM), perl = TRUE)]/panicles[, 10]), 1, mean)
rbg.graph <- na.omit(subset(rbg, visit.visit == "Ripening")) # no grain bug damage until second visit

thp <- apply(data.frame(PRISM[, grep(pattern = "group_1_[[:graph:]]*thrip", colnames(PRISM), perl = TRUE)]/(leaves[, 1]*tillers[, 1]),
                        PRISM[, grep(pattern = "group_2_[[:graph:]]*thrip", colnames(PRISM), perl = TRUE)]/(leaves[, 2]*tillers[, 2]),
                        PRISM[, grep(pattern = "group_3_[[:graph:]]*thrip", colnames(PRISM), perl = TRUE)]/(leaves[, 3]*tillers[, 3]),
                        PRISM[, grep(pattern = "group_4_[[:graph:]]*thrip", colnames(PRISM), perl = TRUE)]/(leaves[, 4]*tillers[, 4]),
                        PRISM[, grep(pattern = "group_5_[[:graph:]]*thrip", colnames(PRISM), perl = TRUE)]/(leaves[, 5]*tillers[, 5]),
                        PRISM[, grep(pattern = "group_6_[[:graph:]]*thrip", colnames(PRISM), perl = TRUE)]/(leaves[, 6]*tillers[, 6]),
                        PRISM[, grep(pattern = "group_7_[[:graph:]]*thrip", colnames(PRISM), perl = TRUE)]/(leaves[, 7]*tillers[, 7]),
                        PRISM[, grep(pattern = "group_8_[[:graph:]]*thrip", colnames(PRISM), perl = TRUE)]/(leaves[, 8]*tillers[, 8]),
                        PRISM[, grep(pattern = "group_9_[[:graph:]]*thrip", colnames(PRISM), perl = TRUE)]/(leaves[, 9]*tillers[, 9]),
                        PRISM[, grep(pattern = "group_10_[[:graph:]]*thrip", colnames(PRISM), perl = TRUE)]/(leaves[, 10]*tillers[, 10])), 1, mean)

whm <- apply(data.frame(PRISM[, grep(pattern = "group_1_[[:graph:]]*whorlmaggot", colnames(PRISM), perl = TRUE)]/(leaves[, 1]*tillers[, 1]),
                        PRISM[, grep(pattern = "group_2_[[:graph:]]*whorlmaggot", colnames(PRISM), perl = TRUE)]/(leaves[, 2]*tillers[, 2]),
                        PRISM[, grep(pattern = "group_3_[[:graph:]]*whorlmaggot", colnames(PRISM), perl = TRUE)]/(leaves[, 3]*tillers[, 3]),
                        PRISM[, grep(pattern = "group_4_[[:graph:]]*whorlmaggot", colnames(PRISM), perl = TRUE)]/(leaves[, 4]*tillers[, 4]),
                        PRISM[, grep(pattern = "group_5_[[:graph:]]*whorlmaggot", colnames(PRISM), perl = TRUE)]/(leaves[, 5]*tillers[, 5]),
                        PRISM[, grep(pattern = "group_6_[[:graph:]]*whorlmaggot", colnames(PRISM), perl = TRUE)]/(leaves[, 6]*tillers[, 6]),
                        PRISM[, grep(pattern = "group_7_[[:graph:]]*whorlmaggot", colnames(PRISM), perl = TRUE)]/(leaves[, 7]*tillers[, 7]),
                        PRISM[, grep(pattern = "group_8_[[:graph:]]*whorlmaggot", colnames(PRISM), perl = TRUE)]/(leaves[, 8]*tillers[, 8]),
                        PRISM[, grep(pattern = "group_9_[[:graph:]]*whorlmaggot", colnames(PRISM), perl = TRUE)]/(leaves[, 9]*tillers[, 9]),
                        PRISM[, grep(pattern = "group_10_[[:graph:]]*whorlmaggot", colnames(PRISM), perl = TRUE)]/(leaves[, 10]*tillers[, 10])), 1, mean)

wht<- apply(data.frame(PRISM[, grep(pattern = "group_1_[[:graph:]]*whitehead", colnames(PRISM), perl = TRUE)]/tillers[, 1],
                       PRISM[, grep(pattern = "group_2_[[:graph:]]*whitehead", colnames(PRISM), perl = TRUE)]/tillers[, 2],
                       PRISM[, grep(pattern = "group_3_[[:graph:]]*whitehead", colnames(PRISM), perl = TRUE)]/tillers[, 3],
                       PRISM[, grep(pattern = "group_4_[[:graph:]]*whitehead", colnames(PRISM), perl = TRUE)]/tillers[, 4],
                       PRISM[, grep(pattern = "group_5_[[:graph:]]*whitehead", colnames(PRISM), perl = TRUE)]/tillers[, 5],
                       PRISM[, grep(pattern = "group_6_[[:graph:]]*whitehead", colnames(PRISM), perl = TRUE)]/tillers[, 6],
                       PRISM[, grep(pattern = "group_7_[[:graph:]]*whitehead", colnames(PRISM), perl = TRUE)]/tillers[, 7],
                       PRISM[, grep(pattern = "group_8_[[:graph:]]*whitehead", colnames(PRISM), perl = TRUE)]/tillers[, 8],
                       PRISM[, grep(pattern = "group_9_[[:graph:]]*whitehead", colnames(PRISM), perl = TRUE)]/tillers[, 9],
                       PRISM[, grep(pattern = "group_10_[[:graph:]]*whitehead", colnames(PRISM), perl = TRUE)]/tillers[, 10]), 1, mean)
wht.graph <- na.omit(subset(wht, visit.visit == "Ripening")) # no white head until second visit

#eos
