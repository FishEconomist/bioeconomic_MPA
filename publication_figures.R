require(ggplot2)
require(gridExtra)
require(grid)
require(tidyr)
if(!file.exists(paste0(results_folder,"/figures"))) dir.create(paste0(results_folder,"/figures"))

res <- 400
qual <- 100
textsize <- 10


########## Figure 1 - map scenarios for rep 1 #################################
jpeg(paste0(results_folder,'/figures/f1_scenarios.jpg'),height=20,width=17,units="cm",res=res,qual=qual)
layout(matrix(c(1,2,3,4),nrow=2))
par(mar=c(1,1,1,1))
for(scenario in protect_scen){
    assign(paste(scenario), readOGR(dsn=paste0(results_folder,"/shapefiles"), layer = paste0(scenario,"_",1)))
    assign(paste(scenario), spTransform(get(scenario),CRS(proj)))
    plot(EEZ)
    plot(get(scenario),col=protect_scen_colour[protect_scen==scenario],add=T)
    title(protect_scen_names[which(scenario==protect_scen)])
}
dev.off()


########## Figure A1-49 - map scenarios for rep 1 #################################
for(rep in 2:max(replicates)){
    jpeg(paste0(results_folder,'/figures/fA',rep,'_scenarios.jpg'),height=20,width=17,units="cm",res=res,qual=qual)
    layout(matrix(c(1,2,3,4),nrow=2))
    par(mar=c(1,1,1,1))
    for(scenario in protect_scen){
        assign(paste(scenario), readOGR(dsn=paste0(results_folder,"/shapefiles"), layer = paste0(scenario,"_",rep)))
        assign(paste(scenario), spTransform(get(scenario),CRS(proj)))
        plot(EEZ)
        plot(get(scenario),col=protect_scen_colour[protect_scen==scenario],add=T)
        title(protect_scen_names[which(scenario==protect_scen)])
    }
    dev.off()
}


########## Figure 3 - plot of biomass over time #################################
jpeg(paste0(results_folder,'/figures/f3_biomass.jpg'),height=8,width=17,units="cm",res=res,qual=qual)
ggplot(fish_long,aes(x=time,y=biomass,colour=scenario))+
    geom_smooth(cex=2)+
    theme_classic()+
    theme(legend.position="top",
          text=element_text(size=textsize))+
    labs(x="Year",y="Total Stock Biomass (t)")+
    scale_colour_manual(values=protect_scen_colour,
                        labels=protect_scen_names)
dev.off()
    
########## Figure 4 - plot of catch biomass over time #################################
jpeg(paste0(results_folder,'/figures/f4_catch.jpg'),height=8,width=17,units="cm",res=res,qual=qual)
ggplot(fish_value,aes(x=time,y=tot_catch,colour=scenario))+
    geom_smooth(cex=2)+
    theme_classic()+
    theme(legend.position="top",
          text=element_text(size=textsize))+
    labs(x="Year",y="Total catch (t)")+
    scale_colour_manual(values=protect_scen_colour,
                        labels=protect_scen_names)
dev.off()


########## Figure 5 - distance from shore over time #################################
jpeg(paste0(results_folder,'/figures/f5_distance.jpg'),height=8,width=17,units="cm",res=res,qual=qual)
ggplot(fish_value,aes(x=time,y=dist/1000,colour=scenario))+
    geom_smooth(cex=2)+
    theme_classic()+
    theme(legend.position="top",
          text=element_text(size=textsize))+
    labs(x="Year",y="Mean distance from shore (km)")+
    scale_colour_manual(values=protect_scen_colour,
                        labels=protect_scen_names)
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

########## Figure 6 - Social Discount Rate #################################
gathered <- gather(fish_value,SDR,cumsumvalue,net_catch_value_SDRA_cumsum,net_catch_value_SDRB_cumsum,net_catch_value_SDRC_cumsum)
levels(gathered$SDR) <- sub("^net_catch_value_SDRA_cumsum$","SDR = 0.015",levels(gathered$SDR))
levels(gathered$SDR) <- sub("^net_catch_value_SDRB_cumsum$","SDR = 0.03",levels(gathered$SDR))
levels(gathered$SDR) <- sub("^net_catch_value_SDRC_cumsum$","SDR = 0.06",levels(gathered$SDR))
levels(gathered$scenario) <- protect_scen_names


jpeg(paste0(results_folder,'/figures/f6_SDR.jpg'),height=20,width=17,units="cm",res=res,qual=qual)
p1 <- ggplot(gathered,aes(x=time,y=cumsumvalue,colour=scenario,xlab="test"))+
        geom_smooth(cex=1.5)+
        theme_classic()+
        scale_colour_manual(values=protect_scen_colour,name="",labels = protect_scen_names)+
        facet_wrap(~SDR,ncol=1,scale="free_x")+
        theme(strip.background=element_blank(),
              legend.position="none",
              strip.text=element_text(face="bold"),
              text=element_text(size=textsize))+
        labs(x="Year",y=expression(paste("Cumulative Net Present Value (10"^"6"," CAD)",sep="")))

p2 <- ggplot(gathered[gathered$time==2051,],aes(x=scenario,y=cumsumvalue,fill=scenario))+
    # geom_jitter(size=0.5)+
    # geom_violin(alpha=0.7)+
        geom_boxplot()+
        theme_classic()+
        scale_fill_manual(values=protect_scen_colour,name="",labels = protect_scen_names)+
        facet_wrap(~SDR,ncol=1,scale="free_x")+
        theme(strip.background=element_blank(),
              legend.position="none",
              strip.text=element_text(face="bold"),
              axis.text.x=element_text(angle=20,hjust=1),
              text=element_text(size=textsize))+
        labs(x="",y=expression(paste("Net Present Value (10"^"6"," CAD)",sep="")))
g <- ggplotGrob(p1 + theme(legend.position="bottom"))$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
lheight <- sum(legend$height)

grid.arrange(legend,arrangeGrob(p1,p2,ncol=2),heights=unit.c(lheight, unit(1, "npc") - lheight))
dev.off()

########### % protected table ############
#### make Canada (land) polygon for placement of coastal MPAs ####
data(wrld_simpl)
Canada <- wrld_simpl[wrld_simpl$NAME==country_name, ]
Canada <- spTransform(Canada,CRS(proj))
Habitats <- readOGR(dsn="shapefiles",layer="cod_habitat")
Breeding <- readOGR(dsn="shapefiles",layer="cod_breeding")
Breeding <- spTransform(Breeding,CRS(proj))
Habitats <- spTransform(Habitats,CRS(proj))

rm(wrld_simpl)
coastal <- as.vector(gDistance(p,Canada,byid=T)<cell_size)

per_prot <- data.frame(matrix(0,nrow=length(replicates),ncol=4))
per_prot_table <- data.frame(matrix(0,nrow=length(protect_scen),ncol=4))
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
        assign(paste(scenario), readOGR(dsn=paste0(results_folder,"/shapefiles"), layer = paste0(scenario,"_",rep)))
        assign(paste(scenario), spTransform(get(scenario),CRS(proj)))
        per_prot$over[which(replicates==rep)] <- percent_area(get(scenario),EEZ)
        per_prot$coast[which(replicates==rep)] <- percent_area(get(scenario),p[coastal,])
        per_prot$breed[which(replicates==rep)] <- percent_area(get(scenario),Breeding)
        per_prot$hab[which(replicates==rep)] <- percent_area(get(scenario),Habitats)
    }
    per_prot_table[protect_scen==scenario,] <- paste0(round(apply(per_prot,2,mean)*100,2)," (",round(apply(per_prot,2,sd)*100/sqrt(length(replicates)),2),")")
    per_prot <- data.frame(matrix(0,nrow=length(replicates),ncol=4))
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
