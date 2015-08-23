# housekeeping
rm(list=ls())
if(length(dev.list()["RStudioGD"])>1) dev.off(dev.list()["RStudioGD"])
# specifiy folder for results
results_folder <- "G:/bioeconomic_results_test2"
if(!file.exists(results_folder)) dir.create(results_folder)

#### user inputs ####
source("user_input.R")

#### source custom functions ####
source("functions.R")

#### Spatial base layer ####
# Basic grid
source("basic_grid.R")

############################# Begin scenario loops ###################################################
if(full_model){
#     unlink("results/*")
    for(rep in replicates){        
            
        # Protection scenarios (Figure 1)
        source("protection_scenarios.R")
        
        ############################# Begin scenario loops ###################################################
        for(scenario in protect_scen){  
            
            ############################# Begin time loops ###################################################
            for(t in tot_time){
                print(paste("Now calculating for year:",t,"for scenario",scenario,"rep",rep))
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
                
                #### plotting ####
                if(time_loop_plot) source("time_loop_plots.R")
            }
            ############################# End time loops ###################################################
        }
        ############################ End scenario loops ###################################################
    }
    if(length(dev.list()["RStudioGD"])>=1) dev.off(dev.list()["RStudioGD"])
}
############################# End replicate loops ###################################################
#### Cost evaluation ####
# Value of fish catches over time
source("fish_value.R")

# MPA implementation and enforcement
source("MPA_impl_enforcement.R")

# Social discount rate
source("social_discount.R")

# Create figures for publication
source("publication_figures.R")
