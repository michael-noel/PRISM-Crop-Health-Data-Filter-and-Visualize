##############################################################################
# title         : Generate_PRISM_Graphs_and_Maps.R;
# purpose       : generate graphs and maps of PRISM data;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Philippines, Nov. 2014;
# inputs        : na;
# outputs       : na;
# remarks 1     : simply sources other files to generate the output files;
# Licence:      : GPL2;
##############################################################################

source("Other_graphs.R") # Graphs of visit numbers by location and site

source("Nonsystemic_disease_graphs.R")
source("Nonsystemic_disease_maps.R")
source("Pest_graphs.R")
source("Pest_maps.R")
source("Systemic_disease_graphs.R")
source("Systemic_disease_maps.R")
source("Weed_graphs.R")
source("Yield_graph.R")
source("Yield_map.R")

source("Cleaned_Data_for_Regions.R")

#eos
