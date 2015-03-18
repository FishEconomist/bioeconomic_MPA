#### Spatial base layer ####
# Protection scenarios (Figure 1)
print("Now calculating: Protection scenarios")

# MPA size frequency obtained from WDPA database downloaded from http://www.protectedplanet.net/search?marine=1 in March 2015
# calculations of MPA size frequency are done in MPA_size.org
MPA_size_dist_coast <- read.csv("MPA_size_dist_coast.csv")
MPA_size_dist_mar <- read.csv("MPA_size_dist_mar.csv")
