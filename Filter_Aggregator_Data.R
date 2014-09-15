#### load packages for maps ####
library(rgdal) # to load shape files
library(ggplot2) # for the map
library(raster) # for getData
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
PRISM <- subset(PRISM, 
                datetime != "2014-05-10" & # Training ???
                  datetime != "2014-05-26" &
                  datetime != "2014-05-27" &
                  datetime != "2014-05-28" &
                  datetime != "2014-05-29" &
                  datetime != "2014-05-30" &
                  datetime != "2014-06-05" & # Training???
                  datetime != "2014-07-07" &
                  datetime != "2014-07-08" &
                  datetime != "2014-07-09" &
                  datetime != "2014-07-10" &
                  datetime != "2014-07-11" &
                  datetime != "2014-07-16" & # Training???
                  datetime != "2014-07-24" & # Training???
                  datetime != "2014-08-11" &
                  datetime != "2014-08-04" &
                  datetime != "2014-08-05" &
                  datetime != "2014-08-06" &
                  datetime != "2014-08-07" &
                  datetime != "2014-08-08" &
                  datetime != "2014-08-12" &
                  datetime != "2014-08-13" &
                  datetime != "2014-08-14" &
                  datetime != "2014-08-15" &
                  datetime != "2014-08-18" & 
                  datetime != "2014-08-19" & 
                  datetime != "2014-08-20" & 
                  datetime != "2014-08-21" & 
                  datetime != "2014-08-22")

a <- na.omit(data.frame(PRISM$gps1.Longitude, PRISM$gps1.Latitude, PRISM$group_1.group_diseases_1.disease_brownspot))

a <- 
  
b <- ggplot(a, aes(x = PRISM.gps1.Longitude, y = PRISM.gps1.Latitude)) +
  geom_polygon(data = PHL, aes(x = long, y = lat, group = group), fill = "#666666", color = "#666666") +
  geom_point(aes(size = PRISM.group_1.group_diseases_1.disease_brownspot, color = PRISM.group_1.group_diseases_1.disease_brownspot), alpha = 0.65) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + # rotate Lat labels by 90 degrees
  ggtitle("Brown Spot Incidence") + xlab("Longitude") + ylab("Latitude") + # Main title and axis titles
  theme(panel.background = element_rect(fill = "#A6C5F8")) + # Change the colour of the main panel background
  scale_size_continuous(name = "Brown Spot\nIncidence") +
  scale_colour_continuous(name = "Brown Spot\nIncidence", low = "#ffffff", high = "#ff0000", guide = "legend") +
  coord_map()

