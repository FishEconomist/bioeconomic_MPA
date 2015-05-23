#### Harvesting ####
# Fisheries management
print("Now calculating: Fisheries management")
if(t==min(tot_time)){
    p_buffered <- gBuffer(p,byid = T,width=-0.1)
    # implementation of status quo for spin-up time
    fishable <- !apply(gCovers(Status_quo,p_buffered,byid=T),1,any)
    fishable <- as.numeric(substr(names(fishable),3,nchar(names(fishable)))[fishable])
    biomass_est_bank <- NULL
}
if(t==min(time)){
    # implementation of protection scenarios 
    fishable <- !apply(gCovers(get(scenario),p_buffered,byid=T),1,any)
    fishable <- as.numeric(substr(names(fishable),3,nchar(names(fishable)))[fishable])
}


print(paste("Enforcing",scenario))

sampled <- sample(fishable,round(length(Habitats)*sampling_pop))
biomass_tot <- sum(fish$weight)*virtual_fish_ratio/1000
biomass <- sum(fish$weight[fish$polygon %in% fishable])*virtual_fish_ratio/1000
biomass_est_t <- sum(fish$weight[(fish$polygon %in% sampled) %in% fishable])*virtual_fish_ratio/1000/length(sampled)*length(fishable)
biomass_est_bank <- c(biomass_est_bank,biomass_est_t)
while(length(biomass_est_bank)>biomass_est_n_years) biomass_est_bank <- biomass_est_bank[-1]
biomass_est <- mean(biomass_est_bank)

print(paste0("Actual biomass (t) = ",round(biomass_tot)))
print(paste0("Fishable biomass (t) = ",round(biomass)))
print(paste0("Estimated fishable biomass (t) = ",round(biomass_est)))
