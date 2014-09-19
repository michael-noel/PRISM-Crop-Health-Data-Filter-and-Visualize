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

# Weed above canopy
ggplot(weedabove, aes(x = factor(Municipality), y = rating)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge", alpha = 0.65) +
  scale_y_continuous(name = "Weed above canopy rating") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Weed Above Canopy")
ggsave("Graphs/Weed_above.png", width = 8, units = "in")

# Weed below canopy
ggplot(weedbelow, aes(x = factor(Municipality), y = rating)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge", alpha = 0.65) +
  scale_y_continuous(name = "Weed below canopy rating") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Weed Below Canopy")
ggsave("Graphs/Weed_below.png", width = 8, units = "in")

# Grasses
ggplot(grasses, aes(x = factor(Municipality), y = rating)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge", alpha = 0.65) +
  scale_y_continuous(name = "Grassy weed ranking") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Grassy Weeds")
ggsave("Graphs/Grasses.png", width = 8, units = "in")

# Broadleaf weeds
ggplot(broadleaf, aes(x = factor(Municipality), y = rating)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge", alpha = 0.65) +
  scale_y_continuous(name = "Broadleaf weed ranking") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Broadleaf Weeds")
ggsave("Graphs/Broadleaves.png", width = 8, units = "in")

# Sedges
ggplot(sedges, aes(x = factor(Municipality), y = rating)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge", alpha = 0.65) +
  scale_y_continuous(name = "Sedge weed ranking") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Sedge Weeds")
ggsave("Graphs/Sedges.png", width = 8, units = "in")

#eos
