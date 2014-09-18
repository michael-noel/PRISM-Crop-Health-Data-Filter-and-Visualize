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

PRISM <- subset(PRISM, start == "2014-09-18") # IRRI Training Event
PRISM[, 17][PRISM[, 17] == "Los banos"] <- "Los Baños"
PRISM[, 17][PRISM[, 17] == "LOS BANOS"] <- "Los Baños"
PRISM[, 17][PRISM[, 17] == "Los Banos"] <- "Los Baños"

#### Rename the regions to proper names ####
PRISM[, 19][PRISM[, 19] == "4"] <- "IV-B"
PRISM[, 19][PRISM[, 19] == "5"] <- "IV-B"

#### Correct region numbers ####
PRISM <- within(PRISM, Region[Province == "Bohol"] <- "VII") # Fixes "NA" and "12" mis-entries

# Some visits are incorrectly recorded, there are no third or fourth visits
PRISM[, 22][PRISM[, 22] == "6th"] <- "1st"

PRISM[, 22][PRISM[, 22] == "1st"] <- "Booting"

#### Merge the site ID columns ####
missing <- is.na(PRISM[, 12]) # create logical index for NAs in PRISM[, 12]
PRISM[, 12][missing] <- PRISM[, 13][missing] # replace NAs with values from PRISM[, 13]
PRISM <- PRISM[, -13] # drop column 13 now

#### Growth stage ####
gs <- PRISM[, grep(pattern = "crop_stage", colnames(PRISM), perl = TRUE)]

#### Tiller, panicl and leaf counts ####
tiller <- apply(PRISM[, grep(pattern = "tiller_hill", colnames(PRISM), perl = TRUE)], 1, sum)
panicle <- apply(PRISM[, grep(pattern = "panicle_hill", colnames(PRISM), perl = TRUE)], 1, sum)
leaves <- apply(PRISM[, grep(pattern = "leaves_tiller", colnames(PRISM), perl = TRUE)], 1, sum)

#### generate data frames of non-systemic diseases, from 10 observations, for graphing ####
bak <- data.frame(PRISM[, 16:18], gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "bakanae", colnames(PRISM), perl = TRUE)], 1, sum))
blb <- data.frame(PRISM[, 16:18], gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "bacterialleafblight", colnames(PRISM), perl = TRUE)], 1, sum))
bls <- data.frame(PRISM[, 16:18], gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "bacterialleafstreak", colnames(PRISM), perl = TRUE)], 1, sum))
bst <- data.frame(PRISM[, 16:18], gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "(?<!narrow)(?i)brownspot", colnames(PRISM), perl = TRUE)], 1, sum))
fsm <- data.frame(PRISM[, 16:18], gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "falsesmut", colnames(PRISM), perl = TRUE)], 1, sum))
dip <- data.frame(PRISM[, 16:18], gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "dirtypanicle", colnames(PRISM), perl = TRUE)], 1, sum))
lba <- data.frame(PRISM[, 16:18], gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "leafblast", colnames(PRISM), perl = TRUE)], 1, sum))
nba <- data.frame(PRISM[, 16:18], gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "neckblast", colnames(PRISM), perl = TRUE)], 1, sum))
nbs <- data.frame(PRISM[, 16:18], gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "narrowbrownspot", (colnames(PRISM)), perl = TRUE)], 1, sum))
lsc <- data.frame(PRISM[, 16:18], gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "leafscald", colnames(PRISM), perl = TRUE)], 1, sum))
rsp <- data.frame(PRISM[, 16:18], gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "redstripe", colnames(PRISM), perl = TRUE)], 1, sum))
shr <- data.frame(PRISM[, 16:18], gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "sheathrot", colnames(PRISM), perl = TRUE)], 1, sum))
shb <- data.frame(PRISM[, 16:18], gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "sheathblight", colnames(PRISM), perl = TRUE)], 1, sum))
str <- data.frame(PRISM[, 16:18], gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "stemrot", colnames(PRISM), perl = TRUE)], 1, sum))

names(bak) <- names(blb) <- names(bls) <- names(bst) <- names(fsm) <- names(dip) <- names(lba) <- names(nba) <- names(nbs) <- names(lsc) <- names(rsp) <- names(shr) <- names(shb) <- names(str) <- c("Municipality", "Province", "Region", "growth stage", "tiller", "panicle", "leaves", "injury")

# summarize the above dataframes
bak <- summaryBy(injury+leaves~Municipality+Region, fun = "mean", data = bak, keep.names = TRUE)
blb <- summaryBy(injury+leaves~Municipality+Region, fun = "mean", data = blb, keep.names = TRUE)
bls <- summaryBy(injury+leaves~Municipality+Region, fun = "mean", data = bls, keep.names = TRUE)
bst <- summaryBy(injury+leaves~Municipality+Region, fun = "mean", data = bst, keep.names = TRUE)
fsm <- summaryBy(injury+panicle~Municipality+Region, fun = "mean", data = fsm, keep.names = TRUE)
dip <- summaryBy(injury+panicle~Municipality+Region, fun = "mean", data = dip, keep.names = TRUE)
lba <- summaryBy(injury+leaves~Municipality+Region, fun = "mean", data = lba, keep.names = TRUE)
nba <- summaryBy(injury+panicle~Municipality+Region, fun = "mean", data = nba, keep.names = TRUE)
nbs <- summaryBy(injury+leaves~Municipality+Region, fun = "mean", data = nbs, keep.names = TRUE)
lsc <- summaryBy(injury+leaves~Municipality+Region, fun = "mean", data = lsc, keep.names = TRUE)
rsp <- summaryBy(injury+leaves~Municipality+Region, fun = "mean", data = rsp, keep.names = TRUE)
shb <- summaryBy(injury+tiller~Municipality+Region, fun = "mean", data = shb, keep.names = TRUE)
shr <- summaryBy(injury+tiller~Municipality+Region, fun = "mean", data = shr, keep.names = TRUE)
str <- summaryBy(injury+tiller~Municipality+Region, fun = "mean", data = str, keep.names = TRUE)
names(bak) <- names(blb) <- names(bls) <- names(bst) <- names(fsm) <- names(dip) <- names(lba)  <- names(nba) <- names(nbs) <- names(lsc) <- names(rsp) <- names(shr) <- names(shr) <- names(shb) <- names(str) <-  c("Municipality", "Region", "injury", "organ")
disease_name <- c("Bakanae", "BLB", "BLS", "Brown Spot", "False Smut", "Dirty Panicle", "Leaf Blast", "Neck Blast", "Narrow Brown Spot", "Leaf Scald", "Red Stripe", "Sheath Rot", "Sheath Blight", "Stem Rot")
disease <- data.frame(disease_name, rbind(bak, blb, bls, bst, fsm, dip, lba, nba, nbs, lsc, rsp, shr, shb, str))
disease <- transform(disease, observation = injury/organ)

#### generate data frames of systemic diseases, snail and bug/hopper burn ####
gas <- data.frame(PRISM[, 16:18], gs, tiller, leaves, apply(PRISM[, grep(pattern = "pest_gas", colnames(PRISM), perl = TRUE)], 1, sum))
names(gas) <- c("Municipality", "Province", "Region", "growth stage", "tiller", "leaves", "injury")

rat <- data.frame(PRISM[, 16:18], gs, tiller, leaves, apply(PRISM[, grep(pattern = "pest_rat", colnames(PRISM), perl = TRUE)], 1, sum))
names(rat) <- c("Municipality", "Province", "Region", "growth stage", "tiller", "leaves", "injury")

bbn <- data.frame(PRISM[, 16:18], gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "bugburn", colnames(PRISM), perl = TRUE)], 1, sum))
hbn <- data.frame(PRISM[, 16:18], gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "hopperburn", colnames(PRISM), perl = TRUE)], 1, sum))

tun <- data.frame(PRISM[, 16:18], gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "tugnro", colnames(PRISM), perl = TRUE)], 1, sum))
grs <- data.frame(PRISM[, 16:18], gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "grassy", colnames(PRISM), perl = TRUE)], 1, sum))
rgd <- data.frame(PRISM[, 16:18], gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "ragged", colnames(PRISM), perl = TRUE)], 1, sum))
olf <- data.frame(PRISM[, 16:18], gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "orangeleaf", colnames(PRISM), perl = TRUE)], 1, sum))
yld <- data.frame(PRISM[, 16:18], gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "yellowdwarf", colnames(PRISM), perl = TRUE)], 1, sum))

names(bbn) <- names(hbn) <- names(tun) <- names(grs) <- names(rgd) <- names(olf) <- names(yld) <- c("Municipality", "Province", "Region", "gs", "tiller", "panicle", "leaves", "injury")

# summarize the above dataframes
gas <- summaryBy(injury~Municipality+Province+Region, fun = "mean", keep.names = TRUE, data = gas)

rat <- summaryBy(injury~Municipality+Province+Region, fun = "mean", keep.names = TRUE, data = rat)

bbn <- summaryBy(injury~Municipality+Province+Region, fun = "mean", keep.names = TRUE, data = bbn)
hbn <- summaryBy(injury~Municipality+Province+Region, fun = "mean", keep.names = TRUE, data = hbn)

tun <- summaryBy(injury~Municipality+Province+Region, fun = "mean", keep.names = TRUE, data = tun)
grs <- summaryBy(injury~Municipality+Province+Region, fun = "mean", keep.names = TRUE, data = grs)
rgd <- summaryBy(injury~Municipality+Province+Region, fun = "mean", keep.names = TRUE, data = rgd)
olf <- summaryBy(injury~Municipality+Province+Region, fun = "mean", keep.names = TRUE, data = olf)
yld <- summaryBy(injury~Municipality+Province+Region, fun = "mean", keep.names = TRUE, data = yld)

virus_name <- c("Tungro", "Grassy Stunt", "Ragged Stunt", "Orange Leaf", "Yellow Dwarf")
virus <- data.frame(virus_name, rbind(tun, grs, rgd, olf, yld))

#### generate data frames of weed data ####
weedabove <- data.frame(PRISM[, 16:18], gs, apply(PRISM[, grep(pattern = "weedabove_area", colnames(PRISM), perl = TRUE)], 1, mean))
weedbelow <- data.frame(PRISM[, 16:18], gs, apply(PRISM[, grep(pattern = "weedbelow_area", colnames(PRISM), perl = TRUE)], 1, mean))
broadleaf <- na.omit(data.frame(PRISM[, 16:18], gs, apply(PRISM[, grep(pattern = "weed_broadleaved", colnames(PRISM), perl = TRUE)], 1, mean)))
grasses <- na.omit(data.frame(PRISM[, 16:18], gs, apply(PRISM[, grep(pattern = "weed_grass", colnames(PRISM), perl = TRUE)], 1, mean)))
sedges <- na.omit(data.frame(PRISM[, 16:18], gs, apply(PRISM[, grep(pattern = "weed_sedge", colnames(PRISM), perl = TRUE)], 1, mean)))
small <- na.omit(data.frame(PRISM[, 16:18], gs, apply(PRISM[, grep(pattern = "weed_small", colnames(PRISM), perl = TRUE)], 1, mean)))
names(weedabove) <- names(weedbelow) <- names(broadleaf) <- names(grasses) <- names(sedges) <- names(small) <- c("Municipality", "Province", "Region", "gs", "rating")

# summarize the above dataframes
weedabove <- summaryBy(rating~Municipality+Province+Region, fun = "mean", keep.names = TRUE, data = weedabove)
weedbelow <- summaryBy(rating~Municipality+Province+Region, fun = "mean", keep.names = TRUE, data = weedbelow)
weeds <- data.frame(c("Weedabove", "Weedbelow"), rbind(weedabove, weedbelow))
names(weeds) <- c("Canopy", "Town", "Province", "Region", "Rating")

broadleaf <- summaryBy(rating~Municipality+Province+Region, fun = "mean", keep.names = TRUE, data = broadleaf)
grasses <- summaryBy(rating~Municipality+Province+Region, fun = "mean", keep.names = TRUE, data = grasses)
sedges <- summaryBy(rating~Municipality+Province+Region, fun = "mean", keep.names = TRUE, data = sedges)
small <- summaryBy(rating~Municipality+Province+Region, fun = "mean", keep.names = TRUE, data = small)

#Pests
lfd <- data.frame(PRISM[, 16:18], gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "leaffolder", colnames(PRISM), perl = TRUE)], 1, sum))
lfm <- data.frame(PRISM[, 16:18], gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "leafminer", colnames(PRISM), perl = TRUE)], 1, sum))
thp <- data.frame(PRISM[, 16:18], gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "thrip", colnames(PRISM), perl = TRUE)], 1, sum))
whm <- data.frame(PRISM[, 16:18], gs, tiller, panicle, leaves, apply(PRISM[, grep(pattern = "whorl", colnames(PRISM), perl = TRUE)], 1, sum))

names(lfd) <- names(lfm) <- names(thp) <- names(whm) <- c("Municipality", "Province", "Region", "gs", "tiller", "panicle", "leaves", "injury")

lfd <- summaryBy(injury~Municipality+Province+Region, fun = "mean", keep.names = TRUE, data = lfd)
lfm <- summaryBy(injury~Municipality+Province+Region, fun = "mean", keep.names = TRUE, data = lfm)
thp <- summaryBy(injury~Municipality+Province+Region, fun = "mean", keep.names = TRUE, data = thp)
whm <- summaryBy(injury~Municipality+Province+Region, fun = "mean", keep.names = TRUE, data = whm)

pest_name <- c("Leaf Folder", "Leaf Miner", "Thrip", "Whorl Maggot")
pests <- data.frame(pest_name, rbind(lfd, lfm, thp, whm))

# bar plot of bug burn
ggplot(bug, aes(x = factor(bug), y = injury)) +
  geom_histogram(aes(colour = factor(bug), fill = factor(bug)), stat = "identity", position = "dodge", alpha = 0.65) +
  scale_y_continuous(name = "Average Incidence") +
  scale_x_discrete(name = "Insect Burn") +
  scale_fill_discrete(name = "Insect Burn") +
  scale_colour_discrete(name = "Insect Burn") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("Bug and Hopper Burn") +
  theme(legend.position = "none")
ggsave("Graphs/Bug_and_hopper_burn.png", width = 8, units = "in")

# weeds
ggplot(weeds, aes(x = factor(Canopy), y = Rating)) +
  geom_histogram(aes(colour = factor(Canopy), fill = factor(Canopy)), stat = "identity", position = "dodge", alpha = 0.65) +
  scale_y_continuous(name = "Rating") +
  scale_x_discrete(name = "Canopy") +
  scale_fill_discrete(name = "Canopy") +
  scale_colour_discrete(name = "Canopy") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("Weed above and below") +
  theme(legend.position = "none")
ggsave("Graphs/Weed_above_and_below.png", width = 8, units = "in")

# bar plot of golden apple snail damage
ggplot(gas, aes(x = factor(Municipality), y = injury)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge", alpha = 0.65) +
  scale_y_continuous(name = "Missing Hills") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("Golden Apple Snail Damage")
ggsave("Graphs/GAS.png", width = 4.5, units = "in")

# bar plot of golden apple snail damage
ggplot(rat, aes(x = factor(Municipality), y = injury)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge", alpha = 0.65) +
  scale_y_continuous(name = "Rat Damage") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("Rat Damage")
ggsave("Graphs/Rat.png", width = 4.5, units = "in")

# bar plot of systemic diseases
ggplot(virus, aes(x = factor(virus_name), y = injury)) +
  geom_histogram(aes(colour = factor(virus_name), fill = factor(virus_name)), stat = "identity", position = "dodge", alpha = 0.65) +
  scale_y_continuous(name = "Average Incidence") +
  scale_x_discrete(name = "Disease") +
  scale_fill_discrete(name = "Disease") +
  scale_colour_discrete(name = "Disease") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("Systemic Diseases") +
  theme(legend.position = "none")
ggsave("Graphs/Systemic_diseases.png", width = 8, units = "in")

# bar plot of diseases
ggplot(disease, aes(x = factor(disease_name), y = observation)) +
  geom_histogram(aes(colour = factor(disease_name), fill = factor(disease_name)), stat = "identity", position = "dodge", alpha = 0.65) +
  scale_y_continuous(name = "Average Incidence") +
  scale_x_discrete(name = "Disease") +
  scale_fill_discrete(name = "Disease") +
  scale_colour_discrete(name = "Disease") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("Non-Systemic Diseases") +
  theme(legend.position = "none")
ggsave("Graphs/Diseases.png", width = 8, units = "in")

# bar plot of pests
ggplot(pests, aes(x = factor(pest_name), y = injury)) +
  geom_histogram(aes(colour = factor(pest_name), fill = factor(pest_name)), stat = "identity", position = "dodge", alpha = 0.65) +
  scale_y_continuous(name = "Average Incidence") +
  scale_x_discrete(name = "Pest Injury") +
  scale_fill_discrete(name = "Pest Injury") +
  scale_colour_discrete(name = "Pest Injury") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("Pest Injuries") +
  theme(legend.position = "none")
ggsave("Graphs/Pests.png", width = 8, units = "in")

# eos
