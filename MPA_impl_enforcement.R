#### Cost evaluation ####
# MPA implementation and enforcement
print("Now calculating: MPA implementation and enforcement")
for(rep in replicates){
    for(scenario in protect_scen){ 
        assign(get(scenario), readOGR(dsn=paste0(getwd(),"/shapefiles"), layer = paste0(scenario,"_",rep)))
        assign(get(scenario), spTransform(get(scenario),CRS(proj)))
        fish_value$MPA_maintenance[fish_value$scenario==scenario,fish_value$rep==rep] <- gArea(get(scenario))/10^6*MPA_maintenance_cost
    }
}

fish_value$net_value <- fish_value$net_catch_value_USD-fish_value$MPA_maintenance

