#### Harvesting ####
# Fishermen behaviour
print("Now calculating: Fishermen behaviour")
require(rgeos)
require(rgdal)
require(dplyr)
if(t==min(tot_time)){
    fish_communities2 <- spTransform(fish_communities2,CRS(proj))
    distance_from_shore <- gDistance(fish_communities2,p,byid=TRUE)
    distance_from_shore <- distance_from_shore/mean(distance_from_shore)
}

catch <- NULL

if(biomass>minimum_fishable_biomass){
    
    fishers <- sample(rep(1:length(fish_communities),fish_licenses))
    
    #fish while under the 
    quota <- round(FMSY*FMSY_buffer*biomass_est)
    catch_weight <- 0
    while(catch_weight<quota){
        # determine catchable biomass
        fish_catchable <- fish[fish$length>min_size,]
        fish_catchable <- fish_catchable[fish_catchable$polygon %in% fishable,]
        if(length(fish_catchable$polygon)==0){
            print("All the fish are too small!")
            break
        } 
        biomass_mat <- summarise(group_by(fish_catchable,polygon),tot_weight=(sum(weight)/1000*virtual_fish_ratio))
        biomass_mat$tot_weight <- log(biomass_mat$tot_weight+1)/log(max(biomass_mat$tot_weight)+1)
        if(any(!(unique(as.vector(hab_mat[hab_mat>=1])) %in% biomass_mat$polygon))){
            biomass_mat <- rbind(biomass_mat,data.frame(polygon=which(!(unique(as.vector(hab_mat[hab_mat>=1])) %in% biomass_mat$polygon)),tot_weight=0))
        }
        # orderfisherman from each community going to fish
        index <- sample(fishers,length(fishers),replace=TRUE)
        # loop for community
        for(i in unique(index)){
            #calculate community specific effort
            effort <- (1-biomass_mat$tot_weight)*(distance_from_shore[biomass_mat$polygon,i]) #calculate relative effort
            effort <- round(effort,1)            #allow for 10% 'error'
            effort[biomass_mat$tot_weight==0] <- Inf
            #identify fish caught
            catch_ID <- names(which(min(effort)==effort))
            catch_ID <- fish$polygon %in% as.numeric(substr(catch_ID,3,nchar(catch_ID)))
            net <- sum(index==i)                                  # number of nets to catch fish
            if(sum(catch_ID)<net) net <- sum(catch_ID)
            catch_ID <- sample(which(catch_ID),net)
            catch_ID <- catch_ID[fish$length[catch_ID]>min_size]   # small fish escape the net
            # if anyhing was caught, add it to this years catch (and remove biomass)
            if(length(catch_ID)>=1){
                while((sum(fish$weight[catch_ID])*virtual_fish_ratio/1000)>(quota*(fish_licenses[i]/sum(fish_licenses)))){
                    if(length(catch_ID)==1) break
                    catch_ID <- sample(catch_ID,length(catch_ID)-1)   # limits for equitable community quotas
                }
                while((sum(fish$weight[catch_ID])*virtual_fish_ratio/1000+catch_weight)>(1.1*quota)){
                    if(length(catch_ID)==1) break
                    catch_ID <- sample(catch_ID,length(catch_ID)-1)   # prevents overfishing
                }
                catch <- rbind(fish[catch_ID,],catch)
                catch_weight <- round(sum(catch$weight,na.rm=TRUE)*virtual_fish_ratio/1000)
                fish <- fish[!row.names(fish) %in% catch_ID,]
            }
        }  
        print(paste("catch =",catch_weight,"quota =",quota))
    }
    #quota reached
    
    
    # write fish and catch to results folder
    if(length(catch)<1) catch <- data.frame(age=NA,polygon=NA,sex=NA,length=NA,weight=NA)
    if(t>=min(time)){
        write.csv(fish,paste0("results/",scenario,"_fish_",t,"_rep_",rep,".csv"))
        write.csv(catch,paste0("results/",scenario,"_catch_",t,"_rep_",rep,".csv"))
    }
    
    
    
} else {
    # if in moratorium, then
    print("WARNING: Moratorium is activated")
    if(t>=min(time)){
        if(length(fish)<1) fish <- data.frame(age=NA,polygon=NA,sex=NA,length=NA,weight=NA)
        write.csv(fish,paste0("results/",scenario,"_fish_",t,"_rep_",rep,".csv"))
        catch <- data.frame(age=NA,polygon=NA,sex=NA,length=NA,weight=NA)
        write.csv(catch,paste0("results/",scenario,"_catch_",t,"_rep_",rep,".csv"))
    }
}

# break for loop and write empty files if extinct
if(length(row.names(fish))==0){
    print("WARNING: TOTAL EXTINCTION!")
    for(i in t:max(time)){
        catch <- data.frame(age=NA,polygon=NA,sex=NA,length=NA,weight=NA)
        fish <- data.frame(age=NA,polygon=NA,sex=NA,length=NA,weight=NA)
        if(t>=min(time)){
            write.csv(fish,paste0("results/",scenario,"_fish_",i,"_rep_",rep,".csv"))
            write.csv(catch,paste0("results/",scenario,"_catch_",i,"_rep_",rep,".csv"))
        }
    }
    break
}