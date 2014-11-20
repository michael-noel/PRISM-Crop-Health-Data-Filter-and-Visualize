##############################################################################
# title         : Nonsystemic_disease_graphs.R;
# purpose       : generate graphs of non-systemic diseases gathered as part of PRISM;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Philippines, Oct. 2014;
# inputs        : Filtered PRISM data;
# outputs       : graphs of disease data;
# remarks 1     : ;
# Licence:      : GPL2;
##############################################################################

#### load packages ####
library(ggplot2)
#### end load packages ####

source("Filter_Aggregator_Injury_Data.R")

# bacterial leaf blight
ggplot(bak, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Incidence at Tiller Level (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Bakanae Incidence")
ggsave("Injury_Graphs/Bakanae.png", width = 8, height = 8, units = "in")

# bacterial leaf blight
ggplot(blb, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Incidence at Leaf Level (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Bacterial Leaf Blight Incidence")
ggsave("Injury_Graphs/BLB.png", width = 8, height = 8, units = "in")

# bacterial leaf streak
ggplot(bls, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Incidence at Leaf Level (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Bacterial Leaf Streak Incidence")
ggsave("Injury_Graphs/BLS.png", width = 8, height = 8, units = "in")

# brown spot
ggplot(bst, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Incidence at Leaf Level (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  facet_grid(. ~ visit) +
  ggtitle("Brown Spot Incidence")
ggsave("Injury_Graphs/Brown_Spot.png", width = 8, height = 8, units = "in")

# false smut
ggplot(fsm.graph, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Incidence at Panicle Level (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("False Smut Incidence")
ggsave("Injury_Graphs/FSM.png", width = 8, height = 8, units = "in")

# dirty panicle
ggplot(dip.graph, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Incidence at Panicle Level (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
ggtitle("Dirty Incidence at Panicle Level")
ggsave("Injury_Graphs/DP.png", width = 8, height = 8, units = "in")

# leaf blast
ggplot(lba, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Incidence at Leaf Level (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Leaf Blast Incidence")
ggsave("Injury_Graphs/Leaf_blast.png", width = 8, height = 8, units = "in")

# neck blast
ggplot(nba.graph, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Incidence at Panicle Level (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("Neck Blast Incidence")
ggsave("Injury_Graphs/Neck_blast.png", width = 8, height = 8, units = "in")

# narrow brown spot
ggplot(nbs, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Incidence at Leaf Level (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Narrow Brown Spot Incidence")
ggsave("Injury_Graphs/NBS.png", width = 8, height = 8, units = "in")

# leaf scald
ggplot(lsc, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Incidence at Leaf Level (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Leaf Scald Incidence")
ggsave("Injury_Graphs/LS.png", width = 8, height = 8, units = "in")

# red stripe
ggplot(rsp, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Incidence at Leaf Level (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Red Stripe Incidence")
ggsave("Injury_Graphs/Red_Stripe.png", width = 8, height = 8, units = "in")

# sheath rot
ggplot(shr, aes(x = Municipality, y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Incidence at Tiller Level (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Sheath Rot Incidence")
ggsave("Injury_Graphs/Sheath_Rot.png", width = 8, height = 8, units = "in")

# sheath blight
ggplot(shb, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Incidence at Tiller Level (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Sheath Blight Incidence")
ggsave("Injury_Graphs/Sheath_Blight.png", width = 8, height = 8, units = "in")

# stem rot
ggplot(str, aes(x = factor(Municipality), y = (injury/organ)*100)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Incidence at Tiller Level (%)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Stem Rot Incidence")
ggsave("Injury_Graphs/Stem_Rot.png", width = 8, height = 8, units = "in")

# eos 
