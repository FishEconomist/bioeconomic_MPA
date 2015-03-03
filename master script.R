#### User input ####
# total model run time in years (e.g. 2001:2100 would be 100 years)
time <- 2001:2100
# time step
dt <- 1

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

