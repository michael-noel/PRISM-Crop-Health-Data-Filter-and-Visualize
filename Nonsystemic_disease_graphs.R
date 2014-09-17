##############################################################################
# title         : Nonsystemic_disease_graphs.R;
# purpose       : generate graphs of non-systemic diseases gathered as part of PRISM;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Philippines, Sep. 2014;
# inputs        : Filtered PRISM data;
# outputs       : graphs of disease data;
# remarks 1     : ;
# Licence:      : GPL2;
##############################################################################

source("Filter_Aggregator_Data.R")

# bar plot of bacterial leaf blight
ggplot(blb, aes(x = factor(Municipality), y = injury/leaves)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Average Leaf Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Bacterial Leaf Blight Incidence")

# bar plot of bacterial leaf streak
ggplot(bls, aes(x = factor(Municipality), y = injury/leaves)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Average Leaf Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Bacterial Leaf Streak Incidence")

# bar plot of brown spot
ggplot(bst, aes(x = factor(Municipality), y = injury/leaves)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Average Leaf Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Brown Spot Incidence")

# bar plot of false smut
ggplot(fsm, aes(x = factor(Municipality), y = injury/panicle)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Average Panicle Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("False Smut Incidence")

# bar plot of dirty panicle
ggplot(dip, aes(x = factor(Municipality), y = injury/panicle)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Average Panicle Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Dirty Panicle Incidence")

# bar plot of leaf blast
ggplot(lba, aes(x = factor(Municipality), y = injury/leaves)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Average Leaf Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Leaf Blast Incidence")

# bar plot of neck blast
ggplot(nba, aes(x = factor(Municipality), y = injury/panicle)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Average Panicle Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Neck Blast Incidence")

# bar plot of narrow brown spot
ggplot(nbs, aes(x = factor(Municipality), y = injury/leaves)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Average Leaf Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Narrow Brown Spot Incidence")

# bar plot of leaf scald
ggplot(lsc, aes(x = factor(Municipality), y = injury/leaves)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Average Leaf Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Leaf Scald Incidence")

# bar plot of red stripe
ggplot(rsp, aes(x = factor(Municipality), y = injury/leaves)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Average Leaf Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Red Stripe Incidence")

# bar plot of sheath rot
ggplot(shr, aes(x = factor(Municipality), y = injury/tiller)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Average Leaf Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Sheath Rot Incidence")

# bar plot of sheath blight
ggplot(shb, aes(x = factor(Municipality), y = injury/tiller)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Average Leaf Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Sheath Blight Incidence")

# bar plot of stem rot
ggplot(str, aes(x = factor(Municipality), y = injury/tiller)) +
  geom_histogram(aes(colour = factor(Region), fill = factor(Region)), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Average Leaf Incidence") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  facet_grid(. ~ visit) +
  ggtitle("Stem Rot Incidence")

# eos 

