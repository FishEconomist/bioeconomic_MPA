#### Harvesting ####
# Fisheries management
print("Now calculating: Fisheries management")
sampled <- sample(fish$polygon,round(length(Habitats)*sampling_pop))
biomass <- sum(fish$weight)*virtual_fish_ratio/1000
biomass_est <- sum(fish$weight[fish$polygon %in% sampled])*virtual_fish_ratio/1000/length(sampled)*length(Habitats)
biomass_error <- biomass_est/biomass

print(paste0("Actual biomass (t) = ",round(biomass)))
print(paste0("Estimated biomass (t) = ",round(biomass_est)))


# implementation of protection scenarios 
if(scenario=="Status_quo") fishable_mat <- !is.na(over(Habitats,MPAs))
if(scenario=="MPAs_random") fishable_mat <- !is.na(over(Habitats,MPAs_random))
if(scenario=="MPAs_maxdist") fishable_mat <- !is.na(over(Habitats,MPAs_maxdist))
if(scenario=="MPAs_fixed") fishable_mat <- !is.na(over(Habitats,MPAs_fixed))
if(scenario=="MPAs_targeted") fishable_mat <- !is.na(over(Habitats,MPAs_targeted))

print(paste("Enforcing",scenario))