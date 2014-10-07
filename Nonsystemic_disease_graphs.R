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

source("Filter_Aggregator_Data.R")

# bar plot of bacterial leaf blight
ggplot(blb, aes(x = factor(Municipality), y = injury/leaves)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Average Tiller Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Bacterial Leaf Blight Incidence")
ggsave("Graphs/BLB.png", width = 8, units = "in")

# bar plot of bacterial leaf streak
ggplot(bls, aes(x = factor(Municipality), y = injury/leaves)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Average Tiller Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Bacterial Leaf Streak Incidence")
ggsave("Graphs/BLS.png", width = 8, units = "in")

# bar plot of brown spot
ggplot(bst, aes(x = factor(Municipality), y = injury/leaves)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Average Tiller Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  facet_grid(. ~ visit) +
  ggtitle("Brown Spot Incidence")
ggsave("Graphs/BS.png", width = 8, units = "in")

# bar plot of false smut
ggplot(fsm, aes(x = factor(Municipality), y = injury/panicle)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Average Tiller Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("False Smut Incidence")
ggsave("Graphs/FSM.png", width = 8, units = "in")

# bar plot of dirty panicle
ggplot(dip, aes(x = factor(Municipality), y = injury/panicle)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Average Tiller Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
ggsave("Graphs/DP.png", width = 8, units = "in")

# bar plot of leaf blast
ggplot(lba, aes(x = factor(Municipality), y = injury/leaves)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Average Tiller Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Leaf Blast Incidence")
ggsave("Graphs/Leaf_blast.png", width = 8, units = "in")

# bar plot of neck blast
ggplot(nba, aes(x = factor(Municipality), y = injury/panicle)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Average Tiller Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("Neck Blast Incidence")
ggsave("Graphs/Neck_blast.png", width = 8, units = "in")

# bar plot of narrow brown spot
ggplot(nbs, aes(x = factor(Municipality), y = injury/leaves)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Average Tiller Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Narrow Brown Spot Incidence")
ggsave("Graphs/NBS.png", width = 8, units = "in")

# bar plot of leaf scald
ggplot(lsc, aes(x = factor(Municipality), y = injury/leaves)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Average Tiller Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Leaf Scald Incidence")
ggsave("Graphs/LS.png", width = 8, units = "in")

# bar plot of red stripe
ggplot(rsp, aes(x = factor(Municipality), y = injury/leaves)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Average Tiller Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Red Stripe Incidence")
ggsave("Graphs/Red_Stripe.png", width = 8, units = "in")

# bar plot of sheath rot
ggplot(shr, aes(x = Municipality, y = injury/tiller)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Average Tiller Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Sheath Rot Incidence")
ggsave("Graphs/Sheath_Rot.png", width = 8, units = "in")

# bar plot of sheath blight
ggplot(shb, aes(x = factor(Municipality), y = injury/tiller)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Average Tiller Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Sheath Blight Incidence")
ggsave("Graphs/Sheath_Blight.png", width = 8, units = "in")

# bar plot of stem rot
ggplot(str, aes(x = factor(Municipality), y = injury/tiller)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Average Tiller Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Stem Rot Incidence")
ggsave("Graphs/Stem_Rot.png", width = 8, units = "in")

# eos 
