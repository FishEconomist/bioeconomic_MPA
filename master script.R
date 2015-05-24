rm(list=ls())
unlink("results/*")
if(length(dev.list()["RStudioGD"])>=1) dev.off(dev.list()["RStudioGD"])
par.ini <- par()
############################# Begin scenario loops ###################################################
for(rep in replicates){

    source("user_input.R")
    
    #### source custom functions ####
    source("functions.R")
    
    #### Spatial base layer ####
    # Basic grid
    source("basic_grid.R")
        
    # Protection scenarios (Figure 1)
    source("protection_scenarios.R")
    
    ############################# Begin scenario loops ###################################################
    for(scenario in protect_scen){  
        
        ############################# Begin time loops ###################################################
        for(t in tot_time){
            print(paste("Now calculating for year:",t,"for scenario",scenario))
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
            # Fisheries management
            source("fisher_managment.R")
            
            # Fishermen behaviour
            source("fisher_behaviour.R")
        }
        ############################# End time loops ###################################################
    }
    ############################# End scenario loops ###################################################
}
############################# End replicate loops ###################################################
par(par.ini)
#### Cost evaluation ####
# Value of fish catches over time
source("fish_value.R")

# MPA implementation and enforcement
source("MPA_impl_enforcement.R")

# Social discount rate
source("social_discount.R")

