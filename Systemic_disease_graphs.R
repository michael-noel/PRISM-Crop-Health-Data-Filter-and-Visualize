##############################################################################
# title         : Systemic_disease_graphs.R;
# purpose       : generate graphs of systemic diseases gathered as part of PRISM;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Philippines, Sep. 2014;
# inputs        : Filtered PRISM data;
# outputs       : graphs of disease data;
# remarks 1     : ;
# Licence:      : GPL2;
##############################################################################

source("Filter_Aggregator_Data.R")

# bar plot of bug burn
ggplot(bbn, aes(x = factor(Municipality), y = injury)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Average Area Affected Rating") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Bug Burn Severity")
ggsave("Graphs/Bug_burn.png", width = 8, units = "in")

# bar plot of hopper burn
ggplot(hbn, aes(x = factor(Municipality), y = injury)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Average Area Affected Rating") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Hopper Burn Severity")
ggsave("Graphs/Hopper_burn.png", width = 8, units = "in")

# bar plot of tungro damage
ggplot(tun, aes(x = factor(Municipality), y = injury)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Average Area Affected Rating") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Tungro Area")
ggsave("Graphs/Tungro.png", width = 8, units = "in")

# bar plot of grassy stunt damage
ggplot(grs, aes(x = factor(Municipality), y = injury)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Average Tiller Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Grassy Stunt Area")
ggsave("Graphs/Grassy_stunt.png", width = 8, units = "in")

# bar plot of grassy stunt damage
ggplot(rgd, aes(x = factor(Municipality), y = injury)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Average Tiller Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Rice Ragged Stunt Area")
ggsave("Graphs/Rice_ragged_stunt.png", width = 8, units = "in")

# bar plot of grassy stunt damage
ggplot(olf, aes(x = factor(Municipality), y = injury)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Average Tiller Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Orange Leaf Area Area")
ggsave("Graphs/Orange_leaf.png", width = 8, units = "in")

ggplot(yld, aes(x = factor(Municipality), y = injury)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Average Tiller Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Yellow Dwarf Area Area")
ggsave("Graphs/Yellow_dwarf.png", width = 8, units = "in")

# eos
