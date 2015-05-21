#### Harvesting ####
# Fisheries management
print("Now calculating: Fisheries management")
if(t==min(time)){
    p_buffered <- gBuffer(p,byid = T,width=-0.1)
    # implementation of protection scenarios 
    fishable <- !apply(gCovers(get(scenario),p_buffered,byid=T),1,any)
    fishable <- as.numeric(substr(names(fishable),3,nchar(names(fishable)))[fishable])
}


print(paste("Enforcing",scenario))

sampled <- sample(fishable,round(length(Habitats)*sampling_pop))
biomass <- sum(fish$weight)*virtual_fish_ratio/1000
biomass_est <- sum(fish$weight[fish$polygon %in% sampled])*virtual_fish_ratio/1000/length(sampled)*length(Habitats)
biomass_error <- biomass_est/biomass

print(paste0("Actual biomass (t) = ",round(biomass)))
print(paste0("Estimated biomass (t) = ",round(biomass_est)))
