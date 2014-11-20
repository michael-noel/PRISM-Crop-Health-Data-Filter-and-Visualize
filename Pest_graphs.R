##############################################################################
# title         : Pest_graphs.R;
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

# Leaf folder
ggplot(lfd, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Incidence at Leaf Level (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Leaf Folder Damage")
ggsave("Injury_Graphs/Leaf_folder.png", width = 8, height = 8, units = "in")

# Leaf miner
ggplot(lfm, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Incidence at Leaf Level (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Leaf Miner Damage")
ggsave("Injury_Graphs/Leaf_miner.png", width = 8, height = 8, units = "in")

# Thrip
ggplot(thp, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Incidence at Leaf Level (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Thrip Damage")
ggsave("Injury_Graphs/Thrip.png", width = 8, height = 8, units = "in")

# Whorl maggot
ggplot(whm, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Incidence at Leaf Level (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Whorl Maggot Damage")
ggsave("Injury_Graphs/Whorl_maggot.png", width = 8, height = 8, units = "in")

# Other defoliator
ggplot(def, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Incidence at Leaf Level (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Other Defoliator Damage")
ggsave("Injury_Graphs/Defoliator.png", width = 8, height = 8, units = "in")

# White head
ggplot(wht.graph, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Incidence at Tiller Level (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("White Head Incidence")
ggsave("Injury_Graphs/White_Head.png", width = 8, height = 8, units = "in")

# Rice grain bug
ggplot(rgb.graph, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Incidence at Panicle Level (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Rice Grain Bug Damage")
ggsave("Injury_Graphs/Rice_Grain_Bug.png", width = 8, height = 8, units = "in")

# Rice bug
ggplot(rbg, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Incidence at Panicle Level (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Rice Bug Damage")
ggsave("Injury_Graphs/Rice_Bug.png", width = 8, height = 8, units = "in")

# Dead heart
ggplot(dht, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Incidence at Tiller Level (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Dead Heart")
ggsave("Injury_Graphs/Dead_Heart.png", width = 8, height = 8, units = "in")

# Rat damage during two visits
ggplot(rat, aes(x = factor(Municipality), y = injury)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Rat Damaged Hills") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("Rat Damage") +
  facet_grid(. ~ visit)
ggsave("Injury_Graphs/Rat_Graph.png", width = 8, height = 8, units = "in")

# golden apple snail damage
ggplot(gas, aes(x = factor(Municipality), y = injury)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Missing Hills Due to Snail") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("Golden Apple Snail Damage")
ggsave("Injury_Graphs/GAS_Graph.png", width = 8, height = 8, units = "in")

#### Progress curve graphs ####
# Rat AUIPC
ggplot(rat.wide, aes(x = factor(Municipality), y = damage)) + 
  geom_histogram(aes(colour = (Region), fill = factor(Region)), alpha = 0.65, stat = "identity") +
  scale_y_continuous(name = "Area under injury progress curve (larger equals more injuries)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("End of Season Summary\nRat Damage by Municipality")
ggsave("Injury_Graphs/Rat_AUIPC_Graph.png", width = 8, height = 8, units = "in")

# Leaf folder AUIPC
ggplot(lfd.wide, aes(x = factor(Municipality), y = damage)) + 
  geom_histogram(aes(colour = (Region), fill = factor(Region)), alpha = 0.65, stat = "identity") +
  scale_y_continuous(name = "Area under injury progress curve (larger equals more injuries)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("End of Season Summary\nLeaf Folder Damage by Municipality")
ggsave("Injury_Graphs/Leaf_folder_AUIPC_Graph.png", width = 8, height = 8, units = "in")

# Leaf miner AUIPC
ggplot(lfm.wide, aes(x = factor(Municipality), y = damage)) + 
  geom_histogram(aes(colour = (Region), fill = factor(Region)), alpha = 0.65, stat = "identity") +
  scale_y_continuous(name = "Area under injury progress curve (larger equals more injuries)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("End of Season Summary\nLeaf Miner Damage by Municipality")
ggsave("Injury_Graphs/Leaf_miner_AUIPC_Graph.png", width = 8, height = 8, units = "in")

# Thrip AUIPC
ggplot(thp.wide, aes(x = factor(Municipality), y = damage)) + 
  geom_histogram(aes(colour = (Region), fill = factor(Region)), alpha = 0.65, stat = "identity") +
  scale_y_continuous(name = "Area under injury progress curve (larger equals more injuries)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("End of Season Summary\nThrip Damage by Municipality")
ggsave("Injury_Graphs/Thrip_AUIPC_Graph.png", width = 8, height = 8, units = "in")

# Whorl maggot AUIPC
ggplot(whm.wide, aes(x = factor(Municipality), y = damage)) + 
  geom_histogram(aes(colour = (Region), fill = factor(Region)), alpha = 0.65, stat = "identity") +
  scale_y_continuous(name = "Area under injury progress curve (larger equals more injuries)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("End of Season Summary\nWhorl Maggot Damage by Municipality")
ggsave("Injury_Graphs/Whorl_maggot_AUIPC_Graph.png", width = 8, height = 8, units = "in")

# Other defoliator AUIPC
ggplot(def.wide, aes(x = factor(Municipality), y = damage)) + 
  geom_histogram(aes(colour = (Region), fill = factor(Region)), alpha = 0.65, stat = "identity") +
  scale_y_continuous(name = "Area under injury progress curve (larger equals more injuries)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("End of Season Summary\nOther Defoliator Damage by Municipality")
ggsave("Injury_Graphs/Defoliator_AUIPC_Graph.png", width = 8, height = 8, units = "in")

# White head AUIPC
ggplot(wht.wide, aes(x = factor(Municipality), y = damage)) + 
  geom_histogram(aes(colour = (Region), fill = factor(Region)), alpha = 0.65, stat = "identity") +
  scale_y_continuous(name = "Area under injury progress curve (larger equals more injuries)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("End of Season Summary\nWhite Head Incidence by Municipality")
ggsave("Injury_Graphs/White_Head_AUIPC_Graph.png", width = 8, height = 8, units = "in")

# Rice grain bug AUIPC
ggplot(rgb.wide, aes(x = factor(Municipality), y = damage)) + 
  geom_histogram(aes(colour = (Region), fill = factor(Region)), alpha = 0.65, stat = "identity") +
  scale_y_continuous(name = "Area under injury progress curve (larger equals more injuries)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("End of Season Summary\nRice Grain Bug Damage by Municipality")
ggsave("Injury_Graphs/Rice_Grain_Bug_AUIPC_Graph.png", width = 8, height = 8, units = "in")

# Rice bug AUIPC
ggplot(rbg.wide, aes(x = factor(Municipality), y = damage)) + 
  geom_histogram(aes(colour = (Region), fill = factor(Region)), alpha = 0.65, stat = "identity") +
  scale_y_continuous(name = "Area under injury progress curve (larger equals more injuries)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("End of Season Summary\nRice Bug Damage by Municipality")
ggsave("Injury_Graphs/Rice_Bug_AUIPC_Graph.png", width = 8, height = 8, units = "in")

# Dead heart AUIPC
ggplot(dht.wide, aes(x = factor(Municipality), y = damage)) + 
  geom_histogram(aes(colour = (Region), fill = factor(Region)), alpha = 0.65, stat = "identity") +
  scale_y_continuous(name = "Area under injury progress curve (larger equals more injuries)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("End of Season Summary\nDead Heart Incidence by Municipality")
ggsave("Injury_Graphs/Dead_Heart_AUIPC_Graph.png", width = 8, height = 8, units = "in")

# Golden apple snail AUIPC
ggplot(gas.wide, aes(x = factor(Municipality), y = damage)) + 
  geom_histogram(aes(colour = (Region), fill = factor(Region)), alpha = 0.65, stat = "identity") +
  scale_y_continuous(name = "Area under injury progress curve (larger equals more injuries)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("End of Season Summary\nGolden Apple Snail Damage by Municipality")
ggsave("Injury_Graphs/GAS_Graph.png", width = 8, height = 8, units = "in")


# eos
