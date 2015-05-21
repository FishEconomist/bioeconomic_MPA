#### Cost evaluation ####
# Value of fish catches over time
print("Now calculating: Value of fish catches over time")
###### re-load data ###########

catch <- NULL
fish <- NULL
for(scenario in protect_scen){    
    for(t in time){
        temp <- read.csv(paste0("results/",scenario,"_catch_",t,".csv"))
        temp$time <- t
        temp$scenario <- scenario
        catch <- rbind(temp,catch)
        temp <- read.csv(paste0("results/",scenario,"_fish_",t,".csv"))
        temp$time <- t
        temp$scenario <- scenario
        fish <- rbind(temp,fish)
    }
}
catch$distance <- apply(distance_from_shore,1,min)[catch$polygon]

###### summarize fish ###########

fish <- summarise(group_by(fish,scenario,time),tot_biomass=sum(weight))
fish$tot_biomass[is.na(fish$tot_biomass)] <- 0

#adjust from virtual fish (kg) to real fish (t)
fish$tot_biomass <- fish$tot_biomass/1000*virtual_fish_ratio
plot.new();box(bty="l")
plot.window(ylim=c(0,max(fish$tot_biomass)*1.1),xlim=range(time))
for(scenario in protect_scen){    
    lines(fish$tot_biomass[fish$scenario==scenario]~time,col=protect_scen_colour[protect_scen==scenario])
    points(fish$tot_biomass[fish$scenario==scenario]~time,col=protect_scen_colour[protect_scen==scenario])   
}
axis(1);axis(2)
title(xlab='Time',ylab='Total Stock Biomass (t)')


###### summarize catch ###########
fish_value <- summarise(group_by(catch,scenario,time),mean_dist=mean(distance),tot_catch=sum(weight))
fish_value$tot_catch[is.na(fish_value$tot_catch)] <- 0
fish_value$mean_dist[is.na(fish_value$mean_dist)] <- 0

#adjust from virtual fish (kg) to real fish (t)
fish_value$tot_catch <- fish_value$tot_catch/1000*virtual_fish_ratio

plot.new();box(bty="l")
plot.window(ylim=c(0,max(fish_value$tot_catch)*1.1),xlim=range(time))
for(scenario in protect_scen){    
    lines(fish_value$tot_catch[fish_value$scenario==scenario]~time,col=protect_scen_colour[protect_scen==scenario])
    points(fish_value$tot_catch[fish_value$scenario==scenario]~time,col=protect_scen_colour[protect_scen==scenario])   
}
axis(1);axis(2)
title(xlab='Time',ylab='Total catch (t)')


#calculate gross catch value
fish_value$gross_catch_value_USD <- fish_value$tot_catch*fish_landed_value


#calculate operating cost
# for 2001, the landing value should be ~1.6 times operating cost
fish_operating_cost_total <- fish_value$gross_catch_value_USD[fish_value$scenario=="Status_quo"]/Status_quo_profitability
fish_value$dist_corr_factor <- fish_value$mean_dist/fish_value$mean_dist[fish_value$scenario=="Status_quo"&fish_value$time==min(time)]
fish_value$operating_cost <- fish_value$dist_corr_factor*fish_operating_cost_total

#calculate net catch value
fish_value$net_catch_value_USD <- fish_value$gross_catch_value_USD-fish_value$operating_cost

plot.new();box(bty="l")
plot.window(ylim=c(min(fish_value$net_catch_value_USD)*1.1,max(fish_value$net_catch_value_USD)*1.1),xlim=range(time))
for(scenario in protect_scen){    
    lines(fish_value$net_catch_value_USD[fish_value$scenario==scenario]~time,col=protect_scen_colour[protect_scen==scenario])
    points(fish_value$net_catch_value_USD[fish_value$scenario==scenario]~time,col=protect_scen_colour[protect_scen==scenario])   
}
abline(a=1,b=0) #add break even line
axis(1);axis(2)
title(xlab='Time',ylab='Total catch net value (USD)')

