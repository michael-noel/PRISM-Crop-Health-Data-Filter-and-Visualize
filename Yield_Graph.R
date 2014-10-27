##############################################################################
# title         : Yield_Graph.R;
# purpose       : generate graphs of yield data gathered as part of PRISM;
# producer      : prepared by A. Sparks;
# last update   : in Bangkok, Thailand, Oct. 2014;
# inputs        : Filtered PRISM data;
# outputs       : graphs of yield data;
# remarks 1     : ;
# Licence:      : GPL2;
##############################################################################

source("Filter_Aggregator_Yield_Data.R")

ggplot(yield, aes(x = factor(Municipality), y = Yield)) +
  geom_boxplot(aes(colour = factor(Region), fill = factor(Region)), alpha = 0.65, outlier.colour = "darkred", outlier.size = 4) +
  scale_y_continuous(name = "Yield (Tons/Ha)") +
  scale_x_discrete(name = "Municipality") +
  scale_fill_discrete(name = "Region") +
  scale_colour_discrete(name = "Region") +
  theme(axis.text.x = element_text(angle = 35, hjust = 0.8)) +
  ggtitle("Wet Season 2014")
ggsave("Graphs/Yield.png", width = 8, height = 8, units = "in")
