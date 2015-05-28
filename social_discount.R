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

par(mfrow=c(2,2))
for(i in 1:4){
    plot.new();box(bty="l")
    plot.window(ylim=c(min(fish_value_means$net_catch_value_SDRC_cumsum)*1.1,max(fish_value_means$net_catch_value_SDR0_cumsum)*1.1),xlim=range(time))
    for(scenario in protect_scen){    
        lines(fish_value_means[fish_value_means$scenario==scenario,16+i]~time,col=protect_scen_colour[protect_scen==scenario],type='b',lwd=2)
    }
    abline(a=1,b=0) #add break even line
    axis(1);axis(2)
    title(main=paste('SDR = ',c(0,SDR)[i]),xlab='Time',ylab='Present catch cumulative value (USD)')
}
    

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


par(mfrow=c(2,2))
xlim <- c(0.5, length(protect_scen)+0.5)
ylim <- c(min(fish_value_summary$net_catch_value_SDRC)-50,max(fish_value_summary$net_catch_value_SDR0)*1.2)
for(i in 1:4){
    fish_value_summary$plotting <- unlist(fish_value_summary[,(6+i)])
        boxplot(plotting ~ scenario, data=fish_value_summary, notches=TRUE, xaxt='n',
            col = protect_scen_colour,
            ylab = "Net Present Value (10^6 USD)",
            xlim = xlim, ylim = ylim, yaxs = "i")
    axis(1,at=1:length(protect_scen),labels=protect_scen)
    title(main=paste('SDR = ',c(0,SDR)[i]))
    abline(0,0,lty=3)
}

