##############################################################################
# title         : Weed_graphs.R;
# purpose       : generate graphs of weed data gathered as part of PRISM;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Philippines, Oct. 2014;
# inputs        : Filtered PRISM data;
# outputs       : graphs of PRISM data;
# remarks 1     : ;
# Licence:      : GPL2;
##############################################################################

source("Filter_Aggregator_Data.R")

# Weed above canopy
ggplot(weedabove, aes(x = factor(Municipality), y = rating)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Weed Above Canopy Rating") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Weed Above Canopy")
ggsave("Graphs/Weed_above.png", width = 8, units = "in")

# Weed below canopy
ggplot(weedbelow, aes(x = factor(Municipality), y = rating)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Weed Below Canopy Rating") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Weed Below Canopy")
ggsave("Graphs/Weed_below.png", width = 8, units = "in")

# Grasses
ggplot(grasses, aes(x = factor(Municipality), y = rating)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Grasssy Weed Ranking") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Grassy Weeds")
ggsave("Graphs/Grasses.png", width = 8, units = "in")

# Broadleaf weeds
ggplot(broadleaf, aes(x = factor(Municipality), y = rating)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Broadleaf Weed Ranking") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Broadleaf Weeds")
ggsave("Graphs/Broadleaves.png", width = 8, units = "in")

# Sedges
ggplot(sedges, aes(x = factor(Municipality), y = rating)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Sedge Weed Ranking") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Sedge Weeds")
ggsave("Graphs/Sedges.png", width = 8, units = "in")

# Small seedlings
ggplot(small, aes(x = factor(Municipality), y = rating)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Sedge Weed Ranking") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Small Weeds")
ggsave("Graphs/Small_weeds.png", width = 8, units = "in")

#eos
