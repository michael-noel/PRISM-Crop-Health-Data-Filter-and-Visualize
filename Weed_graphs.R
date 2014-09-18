##############################################################################
# title         : Weed_graphs.R;
# purpose       : generate graphs of weed data gathered as part of PRISM;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Philippines, Sep. 2014;
# inputs        : Filtered PRISM data;
# outputs       : graphs of PRISM data;
# remarks 1     : ;
# Licence:      : GPL2;
##############################################################################

source("Filter_Aggregator_Data.R")

# how many observations per Municipality are there submitted so far?
ggplot(weedabove, aes(x = factor(Municipality), y = rating.mean)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge", alpha = 0.65) +
  scale_y_continuous(name = "Weed above canopy rating") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Survey Visit Number")

ggplot(weedbelow, aes(x = factor(Municipality), y = rating.mean)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge", alpha = 0.65) +
  scale_y_continuous(name = "Weed below canopy rating") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Survey Visit Number")

#eos
