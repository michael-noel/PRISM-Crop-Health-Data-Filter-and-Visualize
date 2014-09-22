##############################################################################
# title         : Pest_graphs.R;
# purpose       : generate graphs of PRISM data;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Philippines, Sep. 2014;
# inputs        : Filtered PRISM data;
# outputs       : graphs of PRISM data;
# remarks 1     : ;
# Licence:      : GPL2;
##############################################################################

source("Filter_Aggregator_Data.R")

# Leaf folder
ggplot(lfd, aes(x = factor(Municipality), y = injury/leaves)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge", alpha = 0.65) +
  scale_y_continuous(name = "Leaf Folder Injury Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("Leaf Folder Damage") +
  facet_grid(. ~ visit)
ggsave("Graphs/Leaf_folder.png", width = 8, units = "in")

# bar plot of leaf miner
ggplot(lfd, aes(x = factor(Municipality), y = injury/leaves)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge", alpha = 0.65) +
  scale_y_continuous(name = "Leaf Miner Injury Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("Leaf Miner Damage") +
  facet_grid(. ~ visit)
ggsave("Graphs/Leaf_miner.png", width = 8, units = "in")

# bar plot of thrip
ggplot(thp, aes(x = factor(Municipality), y = injury/leaves)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge", alpha = 0.65) +
  scale_y_continuous(name = "Thrip Injury Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("Thrip Damage") +
  facet_grid(. ~ visit)
ggsave("Graphs/Thrip.png", width = 8, units = "in")

# bar plot of whorl maggot
ggplot(whm, aes(x = factor(Municipality), y = injury/leaves)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge", alpha = 0.65) +
  scale_y_continuous(name = "Whorl Maggot Injury Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("Whorl Maggot Damage") +
  facet_grid(. ~ visit)
ggsave("Graphs/Whorl_maggot.png", width = 8, units = "in")

# eos
