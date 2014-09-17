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
ggplot(gas, aes(x = factor(Municipality), y = mean(injury))) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Average Damage") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("Golden Apple Snail Damage")

# eos
