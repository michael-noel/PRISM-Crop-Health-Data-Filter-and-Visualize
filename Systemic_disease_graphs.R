##############################################################################
# title         : Systemic_disease_graphs.R;
# purpose       : generate graphs of systemic diseases gathered as part of PRISM;
# producer      : prepared by A. Sparks;
# last update   : in Bangkok, Thailand, Oct. 2014;
# inputs        : Filtered PRISM data;
# outputs       : graphs of disease data;
# remarks 1     : ;
# Licence:      : GPL2;
##############################################################################

#### load packages ####
library(ggplot2)
#### end load packages ####

source("Filter_Aggregator_Injury_Data.R")

# bug burn
ggplot(bbn, aes(x = factor(Municipality), y = rating)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Rating of Area Affected") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Bug Burn Severity")
ggsave("Injury_Graphs/Bug_burn.png", width = 8, height = 8, units = "in")

# hopper burn
ggplot(hbn, aes(x = factor(Municipality), y = rating)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Rating of Area Affected") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Hopper Burn Severity")
ggsave("Injury_Graphs/Hopper_burn.png", width = 8, height = 8, units = "in")

# tungro damage
ggplot(tun, aes(x = factor(Municipality), y = rating)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Rating of Area Affected") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Tungro Area")
ggsave("Injury_Graphs/Tungro.png", width = 8, height = 8, units = "in")

# grassy stunt damage
ggplot(grs, aes(x = factor(Municipality), y = rating)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Rating of Area Affected") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Grassy Stunt Area")
ggsave("Injury_Graphs/Grassy_stunt.png", width = 8, height = 8, units = "in")

# grassy stunt damage
ggplot(rgd, aes(x = factor(Municipality), y = rating)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Rating of Area Affected") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Rice Ragged Stunt Area")
ggsave("Injury_Graphs/Rice_ragged_stunt.png", width = 8, height = 8, units = "in")

# grassy stunt damage
ggplot(olf, aes(x = factor(Municipality), y = rating)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Rating of Area Affected") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Orange Leaf Area Area")
ggsave("Injury_Graphs/Orange_leaf.png", width = 8, height = 8, units = "in")

ggplot(ylo, aes(x = factor(Municipality), y = rating)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Rating of Area Affected") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Yellow Dwarf Area Area")
ggsave("Injury_Graphs/Yellow_dwarf.png", width = 8, height = 8, units = "in")

# eos
