##############################################################################
# title         : Other_graphs.R;
# purpose       : generate graphs of PRISM data;
# producer      : prepared by A. Sparks;
# last update   : in Bangkok, Thailand, Oct. 2014;
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
  scale_y_continuous(name = "Number of farmer fields") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Survey Visits by Municipality")
ggsave("Graphs/Visits.png", width = 8, height = 8, units = "in")

# Rat damage
ggplot(rat, aes(x = factor(Municipality), y = injury)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Average Rat Damaged Hills") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("Rat Damage") +
  facet_grid(. ~ visit)
ggsave("Graphs/Rat.png", width = 8, height = 8, units = "in")

# golden apple snail damage
ggplot(gas, aes(x = factor(Municipality), y = injury)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Average Missing Hills Due to Snail") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("Golden Apple Snail Damage")
ggsave("Graphs/GAS.png", width = 8, height = 8, units = "in")

# eos
