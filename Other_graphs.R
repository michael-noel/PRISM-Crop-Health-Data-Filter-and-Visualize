##############################################################################
# title         : Other_graphs.R;
# purpose       : generate graphs of PRISM data;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Philippines, Nov. 2014;
# inputs        : Filtered PRISM data;
# outputs       : graphs of PRISM data;
# remarks 1     : ;
# Licence:      : GPL2;
##############################################################################

#### load packages ####
library(ggplot2)
#### end load packages ####

source("Filter_Aggregator_Injury_Data.R")

# how many observations per Municipality are there submitted so far?
ggplot(visit, aes(x = factor(Municipality))) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "bin", position = "dodge", alpha = 0.65) +
  scale_y_continuous(name = "Number of farmer fields visited per PRISM site") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Survey Visits by Municipality")
ggsave("Graphs/Visits.png", width = 8, height = 8, units = "in")

# Graph of locID visits
ggplot(data = visit, aes(x = factor(locID))) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "bin", alpha = 0.65) +
  scale_y_continuous(name = "Number of visits per location") +
  scale_x_discrete(name = "Location ID") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Survey Visits by Location ID")
ggsave("Graphs/Loc_ID_Visits.png", width = 40, height = 8, units = "in")



# eos
