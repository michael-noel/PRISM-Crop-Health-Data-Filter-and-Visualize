##############################################################################
# title         : Correct_Political_Names_locID.R;
# purpose       : Correct political name units and find any duplicate locIDs and
#                 for use in mapping results;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Laguna, PHL, Oct 2014;
# inputs        : Raw PRISM data;
# outputs       : Filtered PRISM data;
# remarks 1     : ;
# Licence:      : GPL2;
##############################################################################

library("sqldf")

locID <- read.csv("~/Google Drive/tmp/Merge of Copy of _4_Monitoring and Copy of _1_Metadata.csv")
names(locID) <- c("locID", "Region", "Province", "Municpality", "Barangay")

locID <- subset(locID, Region != "")
locID <- subset(locID, locID != 3001 | Province != "Nueva Ecija")

locID[, 2] <- as.character(locID[, 2])
locID[, 3] <- as.character(locID[, 3])
locID[, 4] <- as.character(locID[, 4])
locID[, 5] <- as.character(locID[, 5])

#### Rename the regions to proper names ####
locID[, 2][locID[, 2] == "UCar"] <- "CAR"
locID[, 2][locID[, 2] == "HCar"] <- "CAR"
locID[, 2][locID[, 2] == "Car"] <- "CAR"

locID[, 2][locID[, 2] == 3] <- "III"
locID[, 2][locID[, 2] == "7R3"] <- "III"
locID[, 2][locID[, 2] == "R3"] <- "III"
locID[, 2][locID[, 2] == "Region 3"] <- "III"
locID[, 2][locID[, 2] == "Region3"] <- "III"

locID[, 2][locID[, 2] == 4] <- "IV-B"
locID[, 2][locID[, 2] == "R4B"] <- "IV-B"
locID[, 2][locID[, 2] == "Region 4B"] <- "IV-B"
locID[, 2][locID[, 2] == "Region4B"] <- "IV-B"

locID[, 2][locID[, 2] == 5] <- "V"
locID[, 2][locID[, 2] == 45] <- "V"
locID[, 2][locID[, 2] == "R5"] <- "V"
locID[, 2][locID[, 2] == "Region 5"] <- "V"
locID[, 2][locID[, 2] == "Region5"] <- "V"

locID[, 2][locID[, 2] == 6] <- "VI"
locID[, 2][locID[, 2] == "4Region 6"] <- "VI"
locID[, 2][locID[, 2] == "R6"] <- "VI"
locID[, 2][locID[, 2] == "Region 6"] <- "VI"
locID[, 2][locID[, 2] == "Region6"] <- "VI"

locID[, 2][locID[, 2] == 7] <- "VII"
locID[, 2][locID[, 2] == "R7"] <- "VII"
locID[, 2][locID[, 2] == "Region 7"] <- "VII"
locID[, 2][locID[, 2] == "Region7"] <- "VII"

locID[, 2][locID[, 2] == "8"] <- "VIII"
locID[, 2][locID[, 2] == "R8"] <- "VIII"
locID[, 2][locID[, 2] == "Region 8"] <- "VIII"
locID[, 2][locID[, 2] == "Region8"] <- "VIII"

# Provincial names
locID[, 3][locID[, 3] == "Camarines sur"] <- "Camarines Sur"
locID[, 3][locID[, 3] == "Cam.Sur"] <- "Camarines Sur"
locID[, 3][locID[, 3] == "Cam.sur"] <- "Camarines Sur"
locID[, 3][locID[, 3] == "Cam.Surm"] <- "Camarines Sur"
locID[, 3][locID[, 3] == "Cam. Sur"] <- "Camarines Sur"
locID[, 3][locID[, 3] == "Cam Sur"] <- "Camarines Sur"
locID[, 3][locID[, 3] == "Cam sur"] <- "Camarines Sur"
locID[, 3][locID[, 3] == "Cs"] <- "Camarines Sur"
locID[, 3][locID[, 3] == "Camarines sur"] <- "Camarines Sur"
locID[, 3][locID[, 3] == "Ccamarines sur"] <- "Camarines Sur"
locID[, 3][locID[, 3] == "Camarines"] <- "Camarines Sur"
locID[, 3][locID[, 3] == "Occ.Mindoro"] <- "Occidental Mindoro"
locID[, 3][locID[, 3] == "Occ.mindoro"] <- "Occidental Mindoro"
locID[, 3][locID[, 3] == "occidental mindoro"] <- "Occidental Mindoro"
locID[, 3][locID[, 3] == "Occidental Mindro"] <- "Occidental Mindoro"
locID[, 3][locID[, 3] == "Occ.mdo"] <- "Occidental Mindoro"
locID[, 3][locID[, 3] == "Occ. Mindoro"] <- "Occidental Mindoro"
locID[, 3][locID[, 3] == "Occ. Mindorp"] <- "Occidental Mindoro"
locID[, 3][locID[, 3] == "bohol"] <- "Bohol"

# Correct wrong regions using Province
locID[, 2][locID[, 3] == "Bohol"] <- "VII"

#### Rename the Municipalities to proper names ####
locID[, 4][locID[, 4] == "pilar"] <- "Pilar"
locID[, 4][locID[, 4] == "Rizak"] <- "Rizal"
locID[, 4][locID[, 4] == "Sta.Cruz"] <- "Santa Cruz"
locID[, 4][locID[, 4] == "Sta. Cruz"] <- "Santa Cruz"
locID[, 4][locID[, 4] == "RIZAL"] <- "Rizal"
locID[, 4][locID[, 4] == "Tabuk city"] <- "Tabuk City"
locID[, 4][locID[, 4] == "San miguel"] <- "San Miguel"
locID[, 4][locID[, 4] == "Muoz"] <- "Munoz"
locID[, 4][locID[, 4] == "Tabuk"] <- "Tabuk City"
locID[, 4][locID[, 4] == "Tabui"] <- "Tabuk City"
locID[, 4][locID[, 4] == "sablayan"] <- "Sablayan"
locID[, 4][locID[, 4] == "Sta.cruz"] <- "Santa Cruz"
locID[, 4][locID[, 4] == "Palangui"] <- "Polangui"
locID[, 4][locID[, 4] == "Appas"] <- "Tabuk City"

# Correct wrong or misspelled barangays
locID[, 5][locID[, 5] == "Apas"] <- "Appas"
locID[, 5][locID[, 4] == "Tabuk City"] <- "Appas"
locID[, 5][locID[, 5] == "Sto. Nino"] <- "Santo Nino"
locID[, 5][locID[, 5] == "Sto Nino"] <- "Santo Nino"
locID[, 5][locID[, 5] == "Sto Nio"] <- "Santo Nino"
locID[, 5][locID[, 5] == "Sto. Nio"] <- "Santo Nino"
locID[, 5][locID[, 5] == "San francisco"] <- "San Francisco"
locID[, 5][locID[, 5] == "Babalag east"] <- "Babalag East"
locID[, 5][locID[, 5] == "Babalag west"] <- "Babalag West"
locID[, 5][locID[, 5] == "San quintin"] <- "San Quentin"
locID[, 5][locID[, 5] == "Sitio Tigbi, Binongtoan"] <- "Binongtoan"
locID[, 5][locID[, 5] == "Sitio Bigaa, San Antonio Farm"] <- "San Antonio"
locID[, 5][locID[, 5] == "San Antonio Farm"] <- "San Antonio"
locID[, 5][locID[, 5] == "Mataoroc zone5"] <- "Mataoroc"
locID[, 5][locID[, 5] == "Sta. Rosa"] <- "Santa Rosa"
locID[, 5][locID[, 5] == "Sta. Monica"] <- "Santa Monica"
locID[, 5][locID[, 5] == "Gen. Emilio Aguinaldo"] <- "General Emilio Aguinaldo"
locID[, 5][locID[, 5] == "Liwan east"] <- "Liwan East"
locID[, 5][locID[, 5] == "Del carmen"] <- "Del Carmen"
locID[, 5][locID[, 5] == "Sa Jose"] <- "San Jose"
locID[, 5][locID[, 5] == "Santo Nino"] <- "Santo Niño"
locID[, 5][locID[, 5] == "Sini-baan"] <- "Siniba-an"

#### Summmarize the data ####
locID.summary <- sqldf("select * from locID group by locID")
locID.summary <- locID.summary[-2, ] # drop a duplicate 3001 since it appears to be at PhilRice, NE

#eos
