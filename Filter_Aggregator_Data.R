#### load packages for maps ####
library(rgdal) # to load shape files
library(ggplot2) # for the map
library(raster) # for getData
library(reshape)
library(doBy)
library(sqldf)
#### end load packages ####

PHL <- getData("GADM", country = "PHL", level = 2)

# CAR     August 18-22, 2014 (Joey)
# Reg3    c/o PhilRice
# Reg4B   August 11-15, 2014 (Nancy)
# Reg5    c/o PhilRice
# Reg6    May 26-30, 2014 (Joey and Hannah)
# Reg7    August 4-8, 2014 (Joey) 
# Reg8    August 11-15. 2014 (Joey)

PRISM <- read.csv("~/Google Drive/tmp/PRISM_Crop_and_Injuries_V1_0_results.csv")
PRISM[, 2] <- as.Date(PRISM[, 2])
PRISM[, 16] <- as.character(PRISM[, 16])
PRISM[, 17] <- as.character(PRISM[, 17])
PRISM[, 18] <- as.character(PRISM[, 18])

names(PRISM)[names(PRISM) == "group_contact.village_name"] <- "Barangay"
names(PRISM)[names(PRISM) == "group_contact.town_name"] <- "Municipality"
names(PRISM)[names(PRISM) == "group_contact.province_name"] <- "Province"
names(PRISM)[names(PRISM) == "group_contact.region_name"] <- "Region"

PRISM <- subset(PRISM, 
                datetime != "2014-05-10" & # Training ???
                  datetime != "2014-05-26" &
                  datetime != "2014-05-27" &
                  datetime != "2014-05-28" &
                  datetime != "2014-05-29" &
                  datetime != "2014-05-30" &
                  datetime != "2014-07-07" &
                  datetime != "2014-07-08" &
                  datetime != "2014-07-09" &
                  datetime != "2014-07-10" &
                  datetime != "2014-07-11" &
                  datetime != "2014-08-11" &
                  datetime != "2014-08-04" &
                  datetime != "2014-08-05" &
                  datetime != "2014-08-06" &
                  datetime != "2014-08-07" &
                  datetime != "2014-08-08" &
                  datetime != "2014-08-14" &
                  datetime != "2014-08-15"
  )

PRISM[, 18][PRISM[, 18] == "Camarines sur"] <- "Camarines Sur"
PRISM[, 18][PRISM[, 18] == "Cam.Sur"] <- "Camarines Sur"
PRISM[, 18][PRISM[, 18] == "Cam. Sur"] <- "Camarines Sur"
PRISM[, 18][PRISM[, 18] == "Occ.Mindoro"] <- "Occidental Mindoro"
PRISM[, 18][PRISM[, 18] == "Occ.mindoro"] <- "Occidental Mindoro"
PRISM[, 18][PRISM[, 18] == "occidental mindoro"] <- "Occidental Mindoro"

PRISM[, 17][PRISM[, 17] == "pilar"] <- "Pilar"
PRISM[, 17][PRISM[, 17] == "Sta.Cruz"] <- "Santa Cruz"
PRISM[, 17][PRISM[, 17] == "Sta. Cruz"] <- "Santa Cruz"
PRISM[, 17][PRISM[, 17] == "RIZAL"] <- "Rizal"
PRISM[, 17][PRISM[, 17] == "Tabuk city"] <- "Tabuk City"
PRISM[, 17][PRISM[, 17] == "San miguel"] <- "San Miguel"
PRISM[, 18][PRISM[, 18] == "Rizal"] <- "Kalinga"
PRISM[, 17][PRISM[, 17] == "Tabuk"] <- "Tabuk City"
PRISM[, 17][PRISM[, 17] == "Tabui"] <- "Tabuk City"
PRISM[, 17][PRISM[, 17] == "sablayan"] <- "Sablayan"

## Remove all Kalinga training events
PRISM <- sqldf("SELECT * FROM PRISM WHERE Province NOT IN ('Kalinga', 'Gkm', 'Laguna')")

# Visit number one or two?
visit <- PRISM[, grep(pattern = "visitNo_label", colnames(PRISM), perl = TRUE)]
visit <- data.frame(PRISM[, 17:19], visit)

# Growth stage
gs <- PRISM[, grep(pattern = "crop_stage", colnames(PRISM), perl = TRUE)]

# Tiller, panicl and leaf counts
tiller <- apply(PRISM[, grep(pattern = "tiller_hill", colnames(PRISM), perl = TRUE)], 1, sum)
panicle <- apply(PRISM[, grep(pattern = "panicle_hill", colnames(PRISM), perl = TRUE)], 1, sum)
leaves <- apply(PRISM[, grep(pattern = "leaves_tiller", colnames(PRISM), perl = TRUE)], 1, sum)

# generate data frames of single diseases, from 10 observations, for graphing
bak <- data.frame(PRISM[, 17:19], gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "bakanae", colnames(PRISM), perl = TRUE)], 1, sum))
names(bak) <- c("Municipality", "Province", "growth_stage", "tiller", "panicle", "leaves", "bakanae")

blb <- data.frame(PRISM[, 17:19], gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "bacterialleafblight", colnames(PRISM), perl = TRUE)], 1, sum))
names(blb) <- c("Municipality", "Province", "Region", "growth stage", "tiller", "panicle", "leaves", "injury")

bls <- apply(PRISM[, grep(pattern = "bacterialleafstreak", colnames(PRISM), perl = TRUE)], 1, sum)
bst <- apply(PRISM[, grep(pattern = "(?<!narrow)(?i)brownspot", colnames(PRISM), perl = TRUE)], 1, sum)
fsm <- apply(PRISM[, grep(pattern = "falsesmut", colnames(PRISM), perl = TRUE)], 1, sum)
dip <- apply(PRISM[, grep(pattern = "dirtypanicle", colnames(PRISM), perl = TRUE)], 1, sum)
lba <- apply(PRISM[, grep(pattern = "leafblast", colnames(PRISM), perl = TRUE)], 1, sum)
nba <- apply(PRISM[, grep(pattern = "neckblast", colnames(PRISM), perl = TRUE)], 1, sum)
nbs <- apply(PRISM[, grep(pattern = "narrowbrownspot", (colnames(PRISM)), perl = TRUE)], 1, sum)
lsc <- apply(PRISM[, grep(pattern = "leafscald", colnames(PRISM), perl = TRUE)], 1, sum)
rsp <- apply(PRISM[, grep(pattern = "redstripe", colnames(PRISM), perl = TRUE)], 1, sum)
shr <- apply(PRISM[, grep(pattern = "sheathrot", colnames(PRISM), perl = TRUE)], 1, sum)
shb <- apply(PRISM[, grep(pattern = "sheathblight", colnames(PRISM), perl = TRUE)], 1, sum)
str <- apply(PRISM[, grep(pattern = "stemrot", colnames(PRISM), perl = TRUE)], 1, sum)

# how many observations per Municipality are there submitted so far?
ggplot(visit, aes(x = factor(Municipality))) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "bin", position = "dodge") +
  scale_y_continuous(name = "Number of visits") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Survey Visit Number")

# bar plot of bacterial leaf blight
ggplot(blb, aes(x = factor(Municipality), y = injury/leaves)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Bacterial Leaf\nBlight Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region")

