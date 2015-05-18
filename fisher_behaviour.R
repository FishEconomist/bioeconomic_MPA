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
catch <- NULL

fishers <- sample(rep(1:length(fish_communities),fish_licenses))

while(sum(catch$weight,na.rm=TRUE)*virtual_fish_ratio/1000<FMSY*FMSY_buffer*biomass_est){
    biomass_mat <- summarise(group_by(fish,polygon),tot_weight=sum(weight))
    biomass_mat$tot_weight <- biomass_mat$tot_weight/CC
    if(any(!(unique(as.vector(hab_mat[hab_mat>=1])) %in% biomass_mat$polygon))){
        biomass_mat <- rbind(biomass_mat,data.frame(polygon=which(!(unique(as.vector(hab_mat[hab_mat>=1])) %in% biomass_mat$polygon)),tot_weight=0))
    }
    index <- sample(fishers,length(fishers),replace=TRUE)
    for(i in unique(index)){
        effort <- (1-biomass_mat$tot_weight)*(distance_from_shore[biomass_mat$polygon,i])
        effort <- round(effort,2)
        effort[fishable_mat] <- Inf
        catch_ID <- names(which(min(effort)==effort))
        catch_ID <- fish$polygon %in% as.numeric(substr(catch_ID,3,nchar(catch_ID)))
        net <- sum(index==i)
        if(sum(catch_ID)<net) net <- sum(catch_ID)
        catch_ID <- sample(which(catch_ID),net)
        catch_ID <- catch_ID[fish$length[catch_ID]>min_size]# small fish escape the net
        if(length(catch_ID)>=1){
            catch <- rbind(fish[catch_ID,],catch)
            fish <- fish[!row.names(fish)==catch_ID,]
        }
    }  
    print(paste("catch =",round(sum(catch$weight,na.rm=TRUE)*virtual_fish_ratio/1000),"quota =",round(FMSY*FMSY_buffer*biomass_est)))
}

# write fish and catch to results folder
write.csv(fish,paste0("results/",scenario,"_fish_",t,".csv"))
write.csv(catch,paste0("results/",scenario,"_catch_",t,".csv"))

par(mfrow = c(2,1))
biomass_mat <- matrix(0,grd@cells.dim[1],grd@cells.dim[2])
for(poly in unique(fish$polygon)){
    biomass_mat[which(hab_mat==poly,arr.ind=TRUE)] <- sum(fish$weight[fish$polygon==poly])
}
image(log(biomass_mat))
title(paste('biomass',scenario,t))

catch_mat <- matrix(0,grd@cells.dim[1],grd@cells.dim[2])
for(poly in unique(fish$polygon)){
    catch_mat[which(hab_mat==poly,arr.ind=TRUE)] <- sum(catch$weight[catch$polygon==poly])
}
image(log(catch_mat))
title(paste('catch',scenario,t))
