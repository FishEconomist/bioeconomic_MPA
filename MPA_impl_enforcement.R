#### Cost evaluation ####
# MPA implementation and enforcement
print("Now calculating: MPA implementation and enforcement")
for(scenario in protect_scen){  
    fish_value$MPA_maintenance[fish_value$scenario==scenario] <- gArea(get(scenario))/10^6*MPA_maintenance_cost
}

fish_value$net_value <- fish_value$net_catch_value_USD-fish_value$MPA_maintenance

