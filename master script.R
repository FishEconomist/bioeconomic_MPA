rm(list=ls())
#### User input ####
# total model run time in years (e.g. 2001:2100 would be 100 years)
time <- 2001:2100
# time step in years
dt <- 1
# cell size in km
cell_size <- 50
# default projection
proj  <- "+proj=lcc +lat_1=40 +lat_2=70 +lat_0=-71.3 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
# target protection level in proportion (e.g. 0.2 is 20% protection)
MPA_coverage <- 0.05
# coastal:marine ratio (e.g. CtoM <- 0.4 is 60% marine and 40% coastal in terms of area)
CtoM <- 0.0009422693
# fixed distance parameters in km
fixdist <- 200

#### source custom functions ####
source("functions.R")

#### Spatial base layer ####
# Basic grid
source("basic_grid.R")
# Protection scenarios (Figure 1)
source("protection_scenarios.R")

############################# Begin time loops ###################################################
for(t in time){
    print(paste("Now calculating for year:",t))
    #### Growth and Reproduction ####
    # Individual growth model
    source("ind_growth_model.R")
    # Reproductive output
    source("reproduct_output.R")
    #### Dispersal ####
    # Larval
    source("larval_dispersal.R")
    # Adult
    source("adult_dispersal.R")
    #### Harvesting ####
    # Fishermen behaviour
    source("fisher_behaviour.R")
    # Fisheries management
    source("fisher_managment.R")
}
############################# End time loops ###################################################


#### Cost evaluation ####
# MPA implementation and enforcement
source("MPA_impl_enforcement.R")
# Value of fish catches over time
source("fish_value.R")
# Social discount rate
source("social_discount.R")

