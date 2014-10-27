##############################################################################
# title         : Pest_graphs.R;
# purpose       : generate graphs of PRISM data;
# producer      : prepared by A. Sparks;
# last update   : in Bangkok, Thailand, Oct. 2014;
# inputs        : Filtered PRISM data;
# outputs       : graphs of PRISM data;
# remarks 1     : ;
# Licence:      : GPL2;
##############################################################################

source("Filter_Aggregator_Injury_Data.R")

# Leaf folder
ggplot(lfd, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Leaf Incidence (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Leaf Folder Damage")
ggsave("Graphs/Leaf_folder.png", width = 8, height = 8, units = "in")

# Leaf miner
ggplot(lfm, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Leaf Incidence (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Leaf Miner Damage")
ggsave("Graphs/Leaf_miner.png", width = 8, height = 8, units = "in")

# Thrip
ggplot(thp, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Leaf Incidence (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Thrip Damage")
ggsave("Graphs/Thrip.png", width = 8, height = 8, units = "in")

# Whorl maggot
ggplot(whm, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Leaf Incidence (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Whorl Maggot Damage")
ggsave("Graphs/Whorl_maggot.png", width = 8, height = 8, units = "in")

# Other defoliator
ggplot(def, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Leaf Incidence (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Other Defoliator Damage")
ggsave("Graphs/Defoliator.png", width = 8, height = 8, units = "in")

# White head
ggplot(wht, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Tiller Incidence (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("White Head Incidence")
ggsave("Graphs/White_Head.png", width = 8, height = 8, units = "in")

# Rice grain bug
ggplot(rgb, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Panicle Incidence (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Rice Grain Bug Damage")
ggsave("Graphs/Rice_Grain_Bug.png", width = 8, height = 8, units = "in")

# Rice bug
ggplot(rbg, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Panicile Incidence (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Rice Bug Damage")
ggsave("Graphs/Rice_Bug.png", width = 8, height = 8, units = "in")

# Dead heart
ggplot(dht, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Tiller Incidence (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Dead Heart")
ggsave("Graphs/Dead_Heart.png", width = 8, height = 8, units = "in")

# eos
