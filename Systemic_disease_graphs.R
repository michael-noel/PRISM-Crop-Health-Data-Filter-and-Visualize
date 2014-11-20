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

# rice ragged stunt damage
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


#### Progress curve graphs ####
# bug burn AUIPC
ggplot(bbn.wide, aes(x = factor(Municipality), y = damage)) + 
  geom_histogram(aes(colour = (Region), fill = factor(Region)), alpha = 0.65, stat = "identity") +
  scale_y_continuous(name = "Area under injury progress curve (larger equals more injuries)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("End of Season Summary\nBug Burn Severity by Municipality")
ggsave("Injury_Graphs/Bug_burn_AUIPC_Graph.png", width = 8, height = 8, units = "in")

# hopper burn AUIPC
ggplot(hbn.wide, aes(x = factor(Municipality), y = damage)) + 
  geom_histogram(aes(colour = (Region), fill = factor(Region)), alpha = 0.65, stat = "identity") +
  scale_y_continuous(name = "Area under injury progress curve (larger equals more injuries)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("End of Season Summary\nHopper Burn Severity by Municipality")
ggsave("Injury_Graphs/Hopper_burn_AUIPC_Graph.png", width = 8, height = 8, units = "in")

# tungro AUDPC
ggplot(rat.wide, aes(x = factor(Municipality), y = damage)) + 
  geom_histogram(aes(colour = (Region), fill = factor(Region)), alpha = 0.65, stat = "identity") +
  scale_y_continuous(name = "Area under disease progress curve (larger equals more injuries)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("End of Season Summary\nTungro by Municipality")
ggsave("Injury_Graphs/Tungro_AUIPC_Graph.png", width = 8, height = 8, units = "in")






# eos
