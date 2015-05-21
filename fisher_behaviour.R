#### Harvesting ####
# Fishermen behaviour
print("Now calculating: Fishermen behaviour")
require(rgeos)
require(rgdal)
require(dplyr)
if(t==min(time)){
    fish_communities2 <- spTransform(fish_communities2,CRS(proj))
    distance_from_shore <- gDistance(fish_communities2,p,byid=TRUE)
    distance_from_shore <- distance_from_shore/mean(distance_from_shore)
}
if(biomass>minimum_fishable_biomass){
    catch <- NULL
    
    fishers <- sample(rep(1:length(fish_communities),fish_licenses))
    
    while(sum(catch$weight,na.rm=TRUE)*virtual_fish_ratio/1000<FMSY*FMSY_buffer*biomass_est){
        fish_catchable <- fish[fish$length>min_size,]
        fish_catchable <- fish_catchable[fish_catchable$polygon %in% fishable,]
        biomass_mat <- summarise(group_by(fish_catchable,polygon),tot_weight=(sum(weight)/1000*virtual_fish_ratio))
        biomass_mat$tot_weight <- log(biomass_mat$tot_weight+1)/log(max(biomass_mat$tot_weight)+1)
        if(any(!(unique(as.vector(hab_mat[hab_mat>=1])) %in% biomass_mat$polygon))){
            biomass_mat <- rbind(biomass_mat,data.frame(polygon=which(!(unique(as.vector(hab_mat[hab_mat>=1])) %in% biomass_mat$polygon)),tot_weight=0))
        }
        index <- sample(fishers,length(fishers),replace=TRUE)
        for(i in unique(index)){
            effort <- (1-biomass_mat$tot_weight)*(distance_from_shore[biomass_mat$polygon,i]) #calculate relative effort
            effort <- round(effort,1)            #allow for 10% 'error'
            effort[biomass_mat$tot_weight==0] <- Inf
            catch_ID <- names(which(min(effort)==effort))
            catch_ID <- fish$polygon %in% as.numeric(substr(catch_ID,3,nchar(catch_ID)))
            net <- sum(index==i)
            if(sum(catch_ID)<net) net <- sum(catch_ID)
            catch_ID <- sample(which(catch_ID),net)
            catch_ID <- catch_ID[fish$length[catch_ID]>min_size]   # small fish escape the net
            if(length(catch_ID)>=1){
                catch <- rbind(fish[catch_ID,],catch)
                fish <- fish[!row.names(fish) %in% catch_ID,]
            }
        }  
        print(paste("catch =",round(sum(catch$weight,na.rm=TRUE)*virtual_fish_ratio/1000),"quota =",round(FMSY*FMSY_buffer*biomass_est)))
    }
    
    # write fish and catch to results folder
    if(length(catch)<1) catch <- data.frame(age=NA,polygon=NA,sex=NA,length=NA,weight=NA)
    write.csv(fish,paste0("results/",scenario,"_fish_",t,".csv"))
    write.csv(catch,paste0("results/",scenario,"_catch_",t,".csv"))
    
    layout(matrix(c(1,3,2,4),nrow=2),widths=c(4,1))
    par(mar=c(1,1,3,1))
    biomass_mat <- matrix(0,grd@cells.dim[1],grd@cells.dim[2])
    for(poly in unique(fish$polygon)){
        biomass_mat[which(hab_mat==poly,arr.ind=TRUE)] <- sum(fish$weight[fish$polygon==poly&fish$length>min_size])
    }
    zlim=c(-3,6)
    image(log(biomass_mat),axes=FALSE,zlim=zlim,col=rev(heat.colors(12)));box()
    title(paste('biomass',scenario,t))
    par(mar=c(1,1,3,1))
    image(matrix(c(zlim[1]:zlim[2]),nrow=1),axes=FALSE,zlim=zlim,col=rev(heat.colors(12)));box()
    mtext(c(zlim[1]:zlim[2]),2,at=(c(zlim[1]:zlim[2])-zlim[1])/(zlim[2]-zlim[1]),las=1)
    title("e^x tonnes")
    
    par(mar=c(1,1,3,1))
    catch_mat <- matrix(0,grd@cells.dim[1],grd@cells.dim[2])
    for(poly in unique(fish$polygon)){
        catch_mat[which(hab_mat==poly,arr.ind=TRUE)] <- sum(catch$weight[catch$polygon==poly])
    }
    image(log(catch_mat),axes=FALSE,zlim=zlim,col=rev(heat.colors(12)));box()
    title(paste('catch',scenario,t))
    par(mar=c(1,1,3,1))
    image(matrix(c(zlim[1]:zlim[2]),nrow=1),axes=FALSE,zlim=zlim,col=rev(heat.colors(12)));box()
    mtext(c(zlim[1]:zlim[2]),2,at=(c(zlim[1]:zlim[2])-zlim[1])/(zlim[2]-zlim[1]),las=1)
    title("e^x tonnes") 
} else {
    write.csv(fish,paste0("results/",scenario,"_fish_",t,".csv"))
    catch <- data.frame(age=NA,polygon=NA,sex=NA,length=NA,weight=NA)
    write.csv(catch,paste0("results/",scenario,"_catch_",t,".csv"))
}

# break for loop and write empty files if extinct
if(length(row.names(fish))==0){
    print("WARNING: TOTAL EXTINCTION!")
    for(i in t:max(time)){
        catch <- data.frame(age=NA,polygon=NA,sex=NA,length=NA,weight=NA)
        fish <- data.frame(age=NA,polygon=NA,sex=NA,length=NA,weight=NA)
        write.csv(fish,paste0("results/",scenario,"_fish_",i,".csv"))
        write.csv(catch,paste0("results/",scenario,"_catch_",i,".csv")) 
    }
    break
}

