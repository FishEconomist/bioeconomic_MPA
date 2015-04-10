#### functions for MPA size ####
#cum_prob and breaks are from MPA_size_dist.csv which is created in MPA_size_dist
find_bin <- function(x,cum_prob){
    which.min(abs(cum_prob-x))-1
}

random_between_breaks <- function(x){
    runif(1,x,x+0.1)
}

generate_MPA_size <- function(n,cum_prob,breaks){
    x <- runif(n,0,1)
    bin <- sapply(x,find_bin,cum_prob)
    low_break <- breaks[bin]
    log_size <- sapply(low_break,random_between_breaks)
    (10^6)*(10^log_size)
}


# protection scenario type "Random", "MaxDist", or numeric value for set distance
generate_MPAs <- function(preexist_polygons,seed_polygons,sprout_polygons,cover,EEZ,type){
    tot_area <- gArea(EEZ)
    MPA_cov_new <- 0
    MPA_cov_current <- gArea(gIntersection(preexist_polygons,EEZ,byid = T))/tot_area
    MPA_count <- 0
    while((MPA_cov_current+MPA_cov_new)<cover){
        size <- generate_MPA_size(1,MPA_size_dist_coast$cum_prob,MPA_size_dist_coast$breaks)
        MPA_count <- MPA_count+1
        print(paste0("Seeding new MPA #",MPA_count))
        # seed for random
        if(type=="Random"){
            seed <- round(runif(1,1,length(seed_polygons)))
        }
        # seed for max-dist
        if(type=="MaxDist"){
            pdist <- gDistance(seed_polygons,preexist_polygons,byid=T)
            pdist <- log10(pdist)
            pdist2 <- apply(pdist,2,mean)          
            seed <- pdist2==max(pdist2)
            while(sum(seed)>1) seed[seed==T][round(runif(1,1,length(seed[seed==T])))] <- F
            seed <- which(seed)
        }
        # seed for set distance
        if(is.numeric(type)){
            pdist <- gDistance(seed_polygons,preexist_polygons,byid=T)
            pdist <- pdist>type/2|pdist<type*2
            pdist2 <- apply(pdist,2,sum)         
            seed <- pdist2==min(pdist2)
            while(sum(seed)>1) seed[seed==T][round(runif(1,1,length(seed[seed==T])))] <- F
            seed <- which(seed)
        }
        
        new_MPA <- seed_polygons[seed,]
        seed_area <- gArea(seed_polygons[seed,])
        seed_polygons <- seed_polygons[-seed,]
        if(sum(gContains(sprout_polygons,new_MPA,byid=T))>0){
            sprout_polygons <- sprout_polygons[-which(gContains(sprout_polygons,new_MPA,byid=T)),]
        }
#         plot(sprout_polygons)
#         plot(new_MPA,col="blue",add=T)
        while(seed_area<size){
            pdist <- gDistance(new_MPA,sprout_polygons,byid=T)
            pdist <- apply(pdist,1,mean)
            sprout <- pdist==min(pdist)
            while(sum(sprout)>1) sprout[sprout==T][round(runif(1,1,length(sprout[sprout==T])))] <- F
            sprout_MPA <- sprout_polygons[sprout,]
#             plot(p)
#             plot(new_MPA,col="blue",add=T)
#             plot(sprout_MPA,col="red",add=T)
            sprout_polygons <- sprout_polygons[-which(sprout),]
            if(sum(gContains(seed_polygons,new_MPA,byid=T))>0){
                seed_polygons <- seed_polygons[-which(gContains(seed_polygons,new_MPA,byid=T)),]
            }
            while(gContains(new_MPA,sprout_MPA)==F){
                print("Sprouting - Increasing MPA size")
                new_MPA <- rbind(new_MPA,sprout_MPA)
                seed_area <- gArea(new_MPA)
            }
        }
        while(sum(gContains(preexist_polygons,new_MPA,byid = T))<1){
            seed_area <- gArea(new_MPA)
            MPA_cov_new <- MPA_cov_new + (seed_area/tot_area)
            preexist_polygons <- rbind(preexist_polygons,new_MPA)
            plot(p)
            plot(preexist_polygons,col="red",add=T)
            plot(new_MPA,col="blue",add=T)
            print(paste0("Added new MPA, new percent cover = ",round(MPA_cov_current+MPA_cov_new,4)))
        }
    }
    return(preexist_polygons)
}
    