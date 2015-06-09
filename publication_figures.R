res <- 400
qual <- 100

########## Figure 1 - map scenarios for rep 1 #################################
jpeg(paste('figures/f1_scenarios.jpg'),height=20,width=17,units="cm",res=res,qual=qual)
layout(matrix(c(1,2,3,4),nrow=2))
par(mar=c(1,1,1,1))
for(scenario in protect_scen){
    assign(paste(scenario), readOGR(dsn=paste0(getwd(),"/shapefiles"), layer = paste0(scenario,"_",1)))
    assign(paste(scenario), spTransform(get(scenario),CRS(proj)))
    plot(EEZ)
    plot(get(scenario),col=protect_scen_colour[protect_scen==scenario],add=T)
    title(gsub("_"," ",scenario))
}
dev.off()


########## Figure A1-49 - map scenarios for rep 1 #################################
for(rep in 2:max(replicates)){
    jpeg(paste0('figures/fA',rep,'_scenarios.jpg'),height=20,width=17,units="cm",res=res,qual=qual)
    layout(matrix(c(1,2,3,4),nrow=2))
    par(mar=c(1,1,1,1))
    for(scenario in protect_scen){
        assign(paste(scenario), readOGR(dsn=paste0(getwd(),"/shapefiles"), layer = paste0(scenario,"_",rep)))
        assign(paste(scenario), spTransform(get(scenario),CRS(proj)))
        plot(EEZ)
        plot(get(scenario),col=protect_scen_colour[protect_scen==scenario],add=T)
        title(gsub("_"," ",scenario))
    }
    dev.off()
}


########## Figure 3 - plot of biomass over time #################################
jpeg(paste('figures/f3_biomass.jpg'),height=20,width=17,units="cm",res=res,qual=qual)
layout(matrix(c(1,2,3,4),nrow=2))
for(scenario in protect_scen){   
    plot.new();box(bty="l")
    plot.window(ylim=c(min(fish_long$biomass),max(fish_long$biomass)*1.1)+1,xlim=range(time),log='y')
    lines((fish$tot_biomass[fish$scenario==scenario]+1)~time,col=protect_scen_colour[protect_scen==scenario],lwd=3)
    points((fish_long$biomass[fish_long$scenario==scenario]+1)~fish_long$time[fish_long$scenario==scenario],pch=4,cex=0.2) 
    abline(10000,0,lty=3)
    title(gsub("_"," ",scenario))
    title(xlab='Year',ylab='Total Stock Biomass (t)')
    axis(1);axis(2)
}
dev.off()

########## Figure 4 - plot of catch biomass over time #################################
jpeg(paste('figures/f4_catch.jpg'),height=20,width=17,units="cm",res=res,qual=qual)
layout(matrix(c(1,2,3,4),nrow=2))
for(scenario in protect_scen){   
    plot.new();box(bty="l")

    plot.window(ylim=c(min(fish_value$tot_catch),max(fish_value$tot_catch)*1.1)+1,xlim=range(time),log='y')
    lines((fish_value_means$tot_catch_mean[fish_value_means$scenario==scenario]+1)~time,col=protect_scen_colour[protect_scen==scenario],lwd=3)
    points((fish_value$tot_catch[fish_value$scenario==scenario]+1)~fish_value$time[fish_value$scenario==scenario],pch=4,cex=0.2) 
    title(gsub("_"," ",scenario))
    title(xlab='Time',ylab='Total catch (t)')
    axis(1);axis(2)
}
dev.off()


########## Figure 5 - distance from shore over time #################################
jpeg(paste('figures/f5_distance.jpg'),height=20,width=17,units="cm",res=res,qual=qual)
layout(matrix(c(1,2,3,4),nrow=2))
for(scenario in protect_scen){   
    plot.new();box(bty="l")
    plot.window(ylim=c(0,max(fish_value$dist/1000)*1.1),xlim=range(time))
    lines(fish_value_means$mean_dist[fish_value_means$scenario==scenario]/1000~time,col=protect_scen_colour[protect_scen==scenario],lwd=3)
    points(fish_value$dist[fish_value$scenario==scenario]/1000~fish_value$time[fish_value$scenario==scenario],pch=4,cex=0.2) 
    title(gsub("_"," ",scenario))
    title(xlab='Time',ylab='Mean distance from shore (km)')
    axis(1);axis(2)
}
dev.off()

########## Figure 6 - Social Discount Rate #################################
jpeg(paste('figures/f6_SDR.jpg'),height=22.5,width=17,units="cm",res=res,qual=qual)
layout(matrix(c(1,2,3,4,5,6),nrow=3))
# cumulative plots
for(i in 2:4){
    plot.new();box(bty="l")
    plot.window(ylim=c(min(fish_value_means$net_catch_value_SDRC_cumsum)*1.1,max(fish_value_means$net_catch_value_SDRA_cumsum)*1.1),xlim=range(time))
    for(scenario in protect_scen){    
        lines(fish_value_means[fish_value_means$scenario==scenario,16+i]~time,col=protect_scen_colour[protect_scen==scenario],lwd=3)
        lines(c(2005,2007),(c(200,200)-(17.5*which(scenario==protect_scen))),col=protect_scen_colour[protect_scen==scenario],lwd=3)
    }
    abline(a=1,b=0) #add break even line
    axis(1);axis(2)
    title(main=paste('SDR = ',c(0,SDR)[i]),xlab='Time',ylab='Present catch cumulative value (10^6 CAD)')
    legend(2005,200,gsub("_"," ",protect_scen),bty='n')
}

# boxplots
xlim <- c(0.5, length(protect_scen)+0.5)
ylim <- c(min(fish_value_summary$net_catch_value_SDRC)-100,max(fish_value_summary$net_catch_value_SDRA)*1.1)
for(i in 2:4){
    fish_value_summary$plotting <- unlist(fish_value_summary[,(6+i)])
    boxplot(plotting ~ scenario, data=fish_value_summary, notches=TRUE, xaxt='n',
            col = protect_scen_colour,
            ylab = "Net Present Value (10^6 CAD)",
            xlim = xlim, ylim = ylim, yaxs = "i")
    text(1:length(protect_scen),par("usr")[3]-175,labels=gsub("_"," ",protect_scen),srt=30,xpd=T)
    title(main=paste('SDR = ',c(0,SDR)[i]))
    abline(0,0,lty=3)
}
dev.off()

####### stats for value ################

### cumulative value curves ###
fish_value <- transform(fish_value,
                              net_catch_value_SDR0_cumsum=ave(net_catch_value_USD,scenario,rep,FUN=cumsum)/10^6,
                              net_catch_value_SDRA_cumsum=ave(net_catch_value_SDRA,scenario,rep,FUN=cumsum)/10^6,
                              net_catch_value_SDRB_cumsum=ave(net_catch_value_SDRB,scenario,rep,FUN=cumsum)/10^6,
                              net_catch_value_SDRC_cumsum=ave(net_catch_value_SDRC,scenario,rep,FUN=cumsum)/10^6
)

with(fish_value, interaction.plot(time, scenario, net_catch_value_SDRA_cumsum,
                              lwd = 3,
                             ylab = "10^6$", xlab = "time", trace.label = "group"))

fit <- aov(net_catch_value_SDRA_cumsum ~ scenario * time + Error(rep/(scenario*time)), data = fish_value)
summary(fit)

fit <- aov(net_catch_value_SDRB_cumsum ~ scenario * time + Error(rep/(scenario*time)), data = fish_value)
summary(fit)

fit <- aov(net_catch_value_SDRC_cumsum ~ scenario * time + Error(rep/(scenario*time)), data = fish_value)
summary(fit)
########### % protected table ############
#### make Canada (land) polygon for placement of coastal MPAs ####
data(wrld_simpl)
Canada <- wrld_simpl[wrld_simpl$NAME==country_name, ]
Canada <- spTransform(Canada,CRS(proj))
Habitats <- readOGR(dsn=paste0(getwd(),"/shapefiles"),layer="cod_habitat")
Breeding <- readOGR(dsn=paste0(getwd(),"/shapefiles"),layer="cod_breeding")
Breeding <- spTransform(Breeding,CRS(proj))
Habitats <- spTransform(Habitats,CRS(proj))

rm(wrld_simpl)
coastal <- as.vector(gDistance(p,Canada,byid=T)<cell_size)

per_prot <- data.frame(matrix(0,nrow=50,ncol=4))
per_prot_table <- data.frame(matrix(0,nrow=4,ncol=4))
names(per_prot) <- c('over','coast','breed','hab')
names(per_prot_table) <- c('over','coast','breed','hab')

percent_area <- function(a,b){
    if(is.null(gIntersection(a,b,byid=T,drop_lower_td=T))){
        x <- 0
    } else {
        x <- gArea(gIntersection(a,b,byid=T,drop_lower_td=T))/gArea(b)
    }
    return(x)
}


for(scenario in protect_scen){
    for(rep in replicates){
        assign(paste(scenario), readOGR(dsn=paste0(getwd(),"/shapefiles"), layer = paste0(scenario,"_",rep)))
        assign(paste(scenario), spTransform(get(scenario),CRS(proj)))
        per_prot$over[rep] <- percent_area(get(scenario),EEZ)
        per_prot$coast[rep] <- percent_area(get(scenario),p[coastal,])
        per_prot$breed[rep] <- percent_area(get(scenario),Breeding)
        per_prot$hab[rep] <- percent_area(get(scenario),Habitats)
    }
    per_prot_table[protect_scen==scenario,] <- paste0(round(apply(per_prot,2,mean)*100,2)," (",round(apply(per_prot,2,sd)*100/sqrt(50),2),")")
    per_prot <- data.frame(matrix(0,nrow=50,ncol=4))
    names(per_prot) <- c('over','coast','breed','hab')
}

################# catch and biomass summary table #########################
biomass_table <- summarise(group_by(fish_long,scenario), biomass=paste0(round(mean(biomass))," (",round(sd(biomass)/sqrt(2550)),")"))
catch_table <- summarise(group_by(fish_value,scenario), 
                         catch=paste0(round(mean(tot_catch))," (",round(sd(tot_catch)/sqrt(2550)),")"),
                         distance=paste0(round(mean(dist)/1000,2)," (",round(sd(dist/1000)/sqrt(2550),2),")"))
moratorium_table <- summarise(group_by(fish_value,scenario,rep), n=sum(tot_catch==0)/length(time)*100)
moratorium_table <- summarise(group_by(moratorium_table,scenario), nn=paste0(round(mean(n),2)," (",round(sd(n)/sqrt(2550),2),")"))
bcm_table <- cbind(biomass_table,catch_table[,-1],moratorium_table$nn)
