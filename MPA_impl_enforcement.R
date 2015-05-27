#### Cost evaluation ####
# MPA implementation and enforcement
print("Now calculating: MPA implementation and enforcement")
fish_long$MPA_maintenance <- 0
fish_value$MPA_maintenance <- 0
for(scenario in protect_scen){
    for(rep in replicates){
        assign(paste(scenario), readOGR(dsn=paste0(getwd(),"/shapefiles"), layer = paste0(scenario,"_",rep)))
        assign(paste(scenario), spTransform(get(scenario),CRS(proj)))
        fish_long$MPA_maintenance[fish_long$scenario==scenario&fish_long$rep==rep] <- gArea(get(scenario))/10^6*MPA_maintenance_cost
    }
    fish_value$MPA_maintenance[fish_value$scenario==scenario] <- mean(fish_long$MPA_maintenance[fish_long$scenario==scenario])
}


 
fish_value$net_value <- fish_value$net_catch_value_USD-fish_value$MPA_maintenance

