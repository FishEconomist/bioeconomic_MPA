#### functions for MPA size ####
#cum_prob and breaks are from MPA_size_dist.csv which is created in MPA_size_dist
find_bin <- function(x,cum_prob){
    (which.min(abs(cum_prob-x)))[1]
}

random_between_breaks <- function(x){
    runif(1,x,x+0.1)
}

generate_MPA_size <- function(n,cum_prob,breaks){
    x <- runif(n,min(cum_prob),1)
    bin <- sapply(x,find_bin,cum_prob)
    breaks <- c(min(breaks)-0.01,breaks)
    low_break <- breaks[bin]
    log_size <- (sapply(low_break,random_between_breaks))
    (10^6)*(10^log_size)
}

getnearest4 <- function(a) {
    if(length(a)>4){
        tail(sort(a,decreasing=T),4)
    } else {
        tail(sort(a,decreasing=T),length(a))
    }
}


# protection scenario type "Random", "MaxDist", or numeric value for set distance
generate_MPAs <- function(sizes,preexist_polygons,seed_polygons,sprout_polygons,cover,EEZ,type,cum_prob,breaks){
    tot_area <- gArea(EEZ)
    MPA_cov_new <- 0
    MPA_cov_current <- gArea(gIntersection(preexist_polygons,EEZ,byid = TRUE))/tot_area
    MPA_count <- 0
    while((MPA_cov_current+MPA_cov_new)<cover){
        size <- sizes[MPA_count+1]
        print(paste0("Seeding new MPA #",MPA_count+1))
        # seed for random
        if(type=="Random"){
            seed <- round(runif(1,1,length(seed_polygons)))
        }
        # seed for max-dist
        if(type=="MaxDist"){
            pdist <- gDistance(seed_polygons,preexist_polygons,byid=TRUE)
            pdist <- apply(pdist,2,getnearest4)
            if(length(preexist_polygons)>1) pdist <- apply(pdist,2,mean)          
            seed <- pdist==max(pdist)
            while(sum(seed)>1) seed[seed==T][round(runif(1,1,length(seed[seed==TRUE])))] <- FALSE
            seed <- which(seed)
        }
        # seed for set distance
        if(is.numeric(type)){
            pdist <- gDistance(seed_polygons,preexist_polygons,byid=TRUE)
            pdist <- pdist>type*0.75|pdist<type*1.25
            pdist2 <- apply(pdist,2,sum)         
            seed <- pdist2==min(pdist2)
            while(sum(seed)>1) seed[seed==TRUE][round(runif(1,1,length(seed[seed==T])))] <- FALSE
            seed <- which(seed)
        }
        
        new_MPA <- seed_polygons[seed,]
        seed_area <- gArea(seed_polygons[seed,])
        seed_polygons <- seed_polygons[-seed,]
        if(sum(gContains(sprout_polygons,new_MPA,byid=T))>0){
            sprout_polygons <- sprout_polygons[-which(gContains(sprout_polygons,new_MPA,byid=TRUE)),]
        }
#         plot(sprout_polygons)
#         plot(new_MPA,col="blue",add=T)
        while(seed_area<size){
            pdist <- gDistance(new_MPA,sprout_polygons,byid=TTRUE)
            pdist <- apply(pdist,1,mean)
            sprout <- pdist==min(pdist)
            while(sum(sprout)>1) sprout[sprout==TRUE][round(runif(1,1,length(sprout[sprout==TRUE])))] <- FALSE
            sprout_MPA <- sprout_polygons[sprout,]
            plot(EEZ)
            plot(preexist_polygons,col="red",add=TRUE)
            plot(new_MPA,col="blue",add=TRUE)
            plot(sprout_MPA,col="green",add=TRUE)
            sprout_polygons <- sprout_polygons[-which(sprout),]
            if(sum(gContains(seed_polygons,new_MPA,byid=T))>0){
                seed_polygons <- seed_polygons[-which(gContains(seed_polygons,new_MPA,byid=TRUE)),]
            }
            while(gContains(new_MPA,sprout_MPA)==FALSE){
                print("Sprouting - Increasing MPA size")
                new_MPA <- rbind(new_MPA,sprout_MPA)
                seed_area <- gArea(new_MPA)
            }
        }
        while(sum(gContains(preexist_polygons,new_MPA,byid = TRUE))<1){
            MPA_count <- MPA_count+1
            df <- new_MPA@data[1,]
            new_MPA <- unionSpatialPolygons(new_MPA,rep(1,length(new_MPA)))
            new_MPA <- SpatialPolygonsDataFrame(new_MPA,df,match.ID = FALSE)
            new_MPA <- spChFIDs(new_MPA,paste("new",MPA_count))
            seed_area <- gArea(new_MPA)
            MPA_cov_new <- MPA_cov_new + (seed_area/tot_area)
            preexist_polygons <- rbind(preexist_polygons,new_MPA)
            plot(EEZ)
            plot(preexist_polygons,col="red",add=TRUE)
            plot(new_MPA,col="blue",add=TRUE)
            print(paste0("Added new MPA, new percent cover = ",round(MPA_cov_current+MPA_cov_new,4)))
        }
    }
    return(preexist_polygons)
}

#Von Bertalanffy growth model parameters (Knickle and Rose 2013)
#Lt = Linf * {1-exp(-k*(t-t0))}, where Lt is length (cm) at age t (year), Linf is the asymptotic length (cm), k is the VB growth coefficient (1/year), and t0 is the x intercept (year). Linf = 112.03 (95% CI = 10.46). k = 0.13 (95% CI = 0.021). t0 = 0.18).

VB_length <- function(t,Linf_mean,Linf_SD,k_mean,k_SD,t0){
    if(t<t0) {
        return(0)
    } else{
        Linf <- rnorm(1,Linf_mean,Linf_SD)
        k <- rnorm(1,k_mean,k_SD)
        Lt  <-  Linf * (1-exp((-k)*(t-t0)))
        return(Lt) 
    }
}

# disperse_to_polygon <- function(dispersal_distances,polygon_distance_matrix){
#     disp_mat <- as.matrix(dispersal_distances)%*%rep(1,length(polygon_distance_matrix))
#     pol_mat <- as.matrix(rep(1,length(dispersal_distances)))%*%polygon_distance_matrix
#     diff_mat <- abs(disp_mat-pol_mat)
#     dispersed <- apply(diff_mat,1,function(x) sample(which(x==min(x)),1))
#     return(table(dispersed))
# }

## General function to take in a lattice and disperse from (http://www.r-bloggers.com/continuous-dispersal-on-a-discrete-lattice/)
## according to a user provided dispersal kernel
## Author: Corey Chivers
## Modified by: Remi Daigle
disperse_to_polygon <- function(domain_boundaries,pop,kernel,...){
    lattice_size<-dim(pop)
    new_pop<-array(0,dim=lattice_size)
    
    for(i in 1:lattice_size[1]){
        for(j in 1:lattice_size[2]){
            N<-pop[i,j]
            while(N>0){
                dist<-kernel(N,...)
                theta<-runif(N,0,2*pi)
                x<-cos(theta)*dist
                y<-sin(theta)*dist
                for(k in 1:N){
                    x_ind<-round(i+x[k]) %% lattice_size[1]
                    y_ind<-round(j+y[k]) %% lattice_size[2]
                    if(x_ind==0) x_ind <- lattice_size[1]
                    if(y_ind==0) y_ind <- lattice_size[2]
                    while(domain_boundaries[x_ind,y_ind]==0){
                        dist2<-kernel(1,...)
                        theta2<-runif(1,0,2*pi)
                        x2<-cos(theta2)*dist2
                        y2<-sin(theta2)*dist2
                        x_ind<-round(i+x2) %% lattice_size[1]
                        y_ind<-round(j+y2) %% lattice_size[2]
                        if(x_ind==0) x_ind <- lattice_size[1]
                        if(y_ind==0) y_ind <- lattice_size[2]
                    }
                    new_pop[x_ind,y_ind]<-new_pop[x_ind,y_ind]+1
                    N <- 0
                } 
            }
            
        }
    }
    return(new_pop)
}

#Beverton-Holt model for carrying capacity based recruitment mortality, carrying capacity is the mean of North American carrying capacities in Table 3 of Myers et al. 2001 (CC=0.431088 tonnes/km^2 )
# input larvae, get recruits
BH_CC_mortality <- function(larvae,fish,CC,CC_sd){
    R <- sum(larvae$recruit)/(dim(fish)[1])
    CCs <- rnorm(length(unique(larvae$polygon)),CC,CC_sd)
    CCs[CCs<=0] <- 0.1
    fish_table <- as.data.frame(table(fish$polygon))
    fish_table$Var1 <- as.numeric(fish_table$Var1)
    larvae$polygon <- as.numeric(larvae$polygon)
    new_recruits <- sapply(unique(larvae$polygon), function(i) if(length(round(R*fish_table$Freq[fish_table$Var1==i]/(1+fish_table$Freq[fish_table$Var1==i]/CCs[i])))==1){
        round(R*fish_table$Freq[fish_table$Var1==i]/(1+fish_table$Freq[fish_table$Var1==i]/CCs[i]))
                                                                        } else {
                                                                            0
                                                                        }
                                                                  )
    new_recruits <- unlist(new_recruits)
    new_recruits[is.na(new_recruits)] <- 0
    return(new_recruits)
}

getBigPolys <- function(poly, minarea=0.01) {
    # Get the areas
    areas <- lapply(poly@polygons, 
                    function(x) sapply(x@Polygons, function(y) y@area))
    
    # Quick summary of the areas
    print(quantile(unlist(areas)))
    
    # Which are the big polygons?
    bigpolys <- lapply(areas, function(x) which(x > minarea))
    length(unlist(bigpolys))
    
    # Get only the big polygons
    for(i in 1:length(bigpolys)){
        if(length(bigpolys[[i]]) >= 1){
            poly@polygons[[i]]@Polygons <- poly@polygons[[i]]@Polygons[bigpolys[[i]]]
            poly@polygons[[i]]@plotOrder <- 1:length(poly@polygons[[i]]@Polygons)
        }
    }
    return(poly)
}
