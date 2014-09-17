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
ggplot(bbn, aes(x = factor(Municipality), y = mean(injury))) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Average Leaf Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Bug Burn Severity")

# bar plot of hopper burn
ggplot(hbn, aes(x = factor(Municipality), y = mean(injury))) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Average Leaf Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Hopper Burn Severity")

# bar plot of golden apple snail damage
ggplot(gas, aes(x = factor(Municipality), y = injury)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Missing Hills") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("Golden Apple Snail Damage")

# bar plot of tungro damage
ggplot(tun, aes(x = factor(Municipality), y = injury)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Missing Hills") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Tungro Area")

# bar plot of grassy stunt damage
ggplot(grs, aes(x = factor(Municipality), y = injury)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Missing Hills") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Grassy Stunt Area")

# bar plot of grassy stunt damage
ggplot(rgd, aes(x = factor(Municipality), y = injury)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Missing Hills") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Rice Ragged Stunt Area")

# bar plot of grassy stunt damage
ggplot(olf, aes(x = factor(Municipality), y = injury)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Missing Hills") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Orange Leaf Area Area")

ggplot(yld, aes(x = factor(Municipality), y = injury)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Missing Hills") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Yellow Dwarf Area Area")

# eos
