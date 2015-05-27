#### Cost evaluation ####
# Social discount rate
print("Now calculating: Social discount rate")
fish_value$net_value_SDRA <- SDR_value(min(fish_value$time),fish_value$time,fish_value$net_value,1)
fish_value$net_value_SDRB <- SDR_value(min(fish_value$time),fish_value$time,fish_value$net_value,2)
fish_value$net_value_SDRC <- SDR_value(min(fish_value$time),fish_value$time,fish_value$net_value,3)

fish_value$net_catch_value_SDRA <- SDR_value(min(fish_value$time),fish_value$time,fish_value$net_catch_value_USD,1)
fish_value$net_catch_value_SDRB <- SDR_value(min(fish_value$time),fish_value$time,fish_value$net_catch_value_USD,2)
fish_value$net_catch_value_SDRC <- SDR_value(min(fish_value$time),fish_value$time,fish_value$net_catch_value_USD,3)


plot.new();box(bty="l")
plot.window(ylim=c(min(fish_value$net_catch_value_SDRA)*1.1,max(fish_value$net_catch_value_SDRA)*1.1),xlim=range(time))
for(scenario in protect_scen){    
    lines(fish_value$net_catch_value_SDRA[fish_value$scenario==scenario]~time,col=protect_scen_colour[protect_scen==scenario])
    points(fish_value$net_catch_value_SDRA[fish_value$scenario==scenario]~time,col=protect_scen_colour[protect_scen==scenario])   
}
abline(a=1,b=0) #add break even line
axis(1);axis(2)
title(xlab='Time',ylab='Present catch value (USD)')

fish_value_summary <- summarise(group_by(fish_value,scenario),
                        net_value_SDRA=sum(net_value_SDRA),
                        net_value_SDRB=sum(net_value_SDRB),
                        net_value_SDRC=sum(net_value_SDRC),
                        net_catch_value_SDRA=sum(net_catch_value_SDRA),
                        net_catch_value_SDRB=sum(net_catch_value_SDRB),
                        net_catch_value_SDRC=sum(net_catch_value_SDRC)
)

barplot(fish_value_summary$net_catch_value_SDRA,names=fish_value_summary$scenario,ylab="Net Present Value (USD)")
barplot(fish_value_summary$net_catch_value_SDRB,names=fish_value_summary$scenario,ylab="Net Present Value (USD)")
barplot(fish_value_summary$net_catch_value_SDRC,names=fish_value_summary$scenario,ylab="Net Present Value (USD)")
