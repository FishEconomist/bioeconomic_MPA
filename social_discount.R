#### Cost evaluation ####
par(mfrow=c(1,1))
# Social discount rate
print("Now calculating: Social discount rate")
fish_value$net_value_SDRA <- SDR_value(min(fish_value$time),fish_value$time,fish_value$net_value,1)
fish_value$net_value_SDRB <- SDR_value(min(fish_value$time),fish_value$time,fish_value$net_value,2)
fish_value$net_value_SDRC <- SDR_value(min(fish_value$time),fish_value$time,fish_value$net_value,3)

fish_value$net_catch_value_SDRA <- SDR_value(min(fish_value$time),fish_value$time,fish_value$net_catch_value_USD,1)
fish_value$net_catch_value_SDRB <- SDR_value(min(fish_value$time),fish_value$time,fish_value$net_catch_value_USD,2)
fish_value$net_catch_value_SDRC <- SDR_value(min(fish_value$time),fish_value$time,fish_value$net_catch_value_USD,3)

fish_value_means$net_value_SDRA <- SDR_value(min(fish_value_means$time),fish_value_means$time,fish_value_means$net_value,1)
fish_value_means$net_value_SDRB <- SDR_value(min(fish_value_means$time),fish_value_means$time,fish_value_means$net_value,2)
fish_value_means$net_value_SDRC <- SDR_value(min(fish_value_means$time),fish_value_means$time,fish_value_means$net_value,3)

fish_value_means$net_catch_value_SDRA <- SDR_value(min(fish_value_means$time),fish_value_means$time,fish_value_means$net_catch_value_USD_mean,1)
fish_value_means$net_catch_value_SDRB <- SDR_value(min(fish_value_means$time),fish_value_means$time,fish_value_means$net_catch_value_USD_mean,2)
fish_value_means$net_catch_value_SDRC <- SDR_value(min(fish_value_means$time),fish_value_means$time,fish_value_means$net_catch_value_USD_mean,3)

### cumulative value curves ###
fish_value_means <- transform(fish_value_means,
                              net_catch_value_SDR0_cumsum=ave(net_catch_value_USD_mean,scenario,FUN=cumsum)/10^6,
                              net_catch_value_SDRA_cumsum=ave(net_catch_value_SDRA,scenario,FUN=cumsum)/10^6,
                              net_catch_value_SDRB_cumsum=ave(net_catch_value_SDRB,scenario,FUN=cumsum)/10^6,
                              net_catch_value_SDRC_cumsum=ave(net_catch_value_SDRC,scenario,FUN=cumsum)/10^6
                              )
    

fish_value_summary <- summarise(group_by(fish_value,scenario,rep),
                        net_value_SDR0=sum(net_value)/10^6,
                        net_value_SDRA=sum(net_value_SDRA)/10^6,
                        net_value_SDRB=sum(net_value_SDRB)/10^6,
                        net_value_SDRC=sum(net_value_SDRC)/10^6,
                        net_catch_value_SDR0=sum(net_catch_value_USD)/10^6,
                        net_catch_value_SDRA=sum(net_catch_value_SDRA)/10^6,
                        net_catch_value_SDRB=sum(net_catch_value_SDRB)/10^6,
                        net_catch_value_SDRC=sum(net_catch_value_SDRC)/10^6
)



