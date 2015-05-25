#### functions for MPA size ####
#cum_prob and breaks are from MPA_size_dist.csv which is created in MPA_size_dist
find_bin <- function(x,cum_prob){
    (which.min(abs(cum_prob-x)))[1]
}

#picks a random number between the breaks
random_between_breaks <- function(x){
    runif(1,x,x+0.1)
}

# generates a vector of randomly selected sizes
generate_MPA_size <- function(n,cum_prob,breaks){
    x <- runif(n,min(cum_prob),1)
    bin <- sapply(x,find_bin,cum_prob)
    breaks <- c(min(breaks)-0.01,breaks)
    low_break <- breaks[bin]
    log_size <- (sapply(low_break,random_between_breaks))
    (10^6)*(10^log_size)
}

# gets the nearest 4 neighbours
getnearest4 <- function(a) {
    if(length(a)>4){
        tail(sort(a,decreasing=T),4)
    } else {
        tail(sort(a,decreasing=T),length(a))
    }
}


# generates SpatialPolygonsDataframe object that represents protection scenario type "Random", "Status_quo","MPAs_maxdist","MPAs_fixed","MPAs_targeted"
generate_MPAs <- function(sizes,preexist_polygons,seed_polygons,sprout_polygons,cover,EEZ,type){
    tot_area <- gArea(EEZ)
    MPA_cov_new <- 0
    MPA_cov_current <- gArea(gIntersection(preexist_polygons,EEZ,byid = TRUE))/tot_area
    MPA_count <- 0
    
    # fit preexisting polygons to grid
    df <- sprout_polygons@data[1,]
    preexist_polygons <- sprout_polygons[apply(gIntersects(preexist_polygons,sprout_polygons,byid=TRUE),1,any),]
    preexist_polygons <- unionSpatialPolygons(preexist_polygons,rep(1,length(preexist_polygons)))
    preexist_polygons <- SpatialPolygonsDataFrame(preexist_polygons,df,match.ID = FALSE)
    preexist_polygons <- spChFIDs(preexist_polygons,paste("old",1:length(preexist_polygons)))
    
    # remove preexisting from seed and sprout
    seed_polygons <- seed_polygons[apply(gContains(preexist_polygons,seed_polygons,byid=TRUE),1,any)==FALSE,]
    sprout_polygons <- sprout_polygons[apply(gContains(preexist_polygons,sprout_polygons,byid=TRUE),1,any)==FALSE,]
    
    while((MPA_cov_current+MPA_cov_new)<cover){
        while((MPA_cov_current+MPA_cov_new+(sizes[MPA_count+1]/tot_area))>(cover+0.01)) sizes <- sizes[-(MPA_count+1)]
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
            seed <- sample(as.numeric(which(seed)),1)
        }
        # seed for set distance
        if(is.numeric(type)){
            pdist <- gDistance(seed_polygons,preexist_polygons,byid=TRUE)
            pdist <- pdist>type*0.75|pdist<type*1.25
            pdist2 <- apply(pdist,2,sum)         
            seed <- pdist2==min(pdist2)
            seed <- sample(as.numeric(which(seed)),1)
        }
        
        new_MPA <- seed_polygons[seed,]
        seed_area <- gArea(seed_polygons[seed,])
        seed_polygons_temp <- seed_polygons[-seed,]
        sprout_polygons_temp <- sprout_polygons
        if(any(gContains(sprout_polygons_temp,new_MPA,byid=T))){
            sprout_polygons_temp <- sprout_polygons_temp[-which(gContains(sprout_polygons_temp,new_MPA,byid=TRUE)),]
        }
        
        # grow the MPA (sprouting)
        sprout_count <- 0
        while(seed_area<size){
            
            pdist <- gDistance(new_MPA,sprout_polygons_temp,byid=TRUE)
            pdist <- apply(pdist,1,mean)
            sprout <- pdist==min(pdist)
            while(sum(sprout)>1) sprout[sprout==TRUE][round(runif(1,1,length(sprout[sprout==TRUE])))] <- FALSE
            #             sprout <- sample(which(sprout),1)
            sprout_MPA <- sprout_polygons_temp[sprout,]
            plot(EEZ)
            plot(preexist_polygons,col="red",add=TRUE)
            plot(new_MPA,col="blue",add=TRUE)
            plot(sprout_MPA,col="green",add=TRUE)
            title(paste(type,rep))
            sprout_polygons_temp <- sprout_polygons_temp[-which(sprout),]
            
            while(gContains(new_MPA,sprout_MPA)==FALSE){
                print("Sprouting - Increasing MPA size")
                new_MPA <- rbind(new_MPA,sprout_MPA)
                seed_area <- gArea(new_MPA)
                
            }
            if(length(new_MPA)>10+sprout_count){
                sprout_count <- sprout_count+1
                if(sprout_count==1) new_MPA <- unionSpatialPolygons(new_MPA,rep(sprout_count,length(new_MPA)))
                if(sprout_count!=1) new_MPA <- unionSpatialPolygons(new_MPA,c(1:(sprout_count-1),rep(sprout_count,length(new_MPA)-sprout_count+1)))
                new_MPA <- SpatialPolygonsDataFrame(new_MPA,df[rep(seq_len(nrow(df)), each=sprout_count),],match.ID = FALSE)
                new_MPA <- spChFIDs(new_MPA,paste("new",1:length(new_MPA)))
            }
            
        }
        
        while(any(gContains(preexist_polygons,new_MPA,byid = TRUE))==FALSE){
            if(any(gContains(seed_polygons_temp,new_MPA,byid=TRUE))){
                seed_polygons_temp <- seed_polygons_temp[-which(gContains(seed_polygons_temp,new_MPA,byid=TRUE)),]
            }
            seed_polygons <- seed_polygons_temp
            sprout_polygons <- sprout_polygons_temp
            MPA_count <- MPA_count+1
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

# sexual maturity based on logistic probability, output is logical
egg_producer <- function(ages,sex,steepness,sigmoid){
    female <- sex==1
    sexually_mature <- 1/(1+exp(-steepness*(ages-sigmoid)))>runif(length(ages),0,1)
    return(female&sexually_mature)
}


## General function to take in a lattice and disperse from (http://www.r-bloggers.com/continuous-dispersal-on-a-discrete-lattice/)
## according to a user provided dispersal kernel
## Author: Corey Chivers
## Modified by: Remi Daigle
# used for larvae as is, wrapper function below for adults (adult_disperse)
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

# adult dispersal wrapper function
adult_disperse <- function(fish,poly,min_size_migration,hab_mat,pop_mat){
    temp_fish <- fish[fish$length>min_size_migration,]
    pop_mat <- matrix(0,grd@cells.dim[1],grd@cells.dim[2])
    pop_mat[which(hab_mat==poly,arr.ind=TRUE)] <- length(temp_fish$polygon)
    pop_mat <- disperse_to_polygon(hab_mat,pop_mat,rexp,rate=1/e_fold_adult)
    if(dim(temp_fish)[1]==1){
        temp_fish$polygon <- hab_mat[which(pop_mat>0,arr.ind=TRUE)]
    } else {
        temp_fish$polygon <- sample(rep(hab_mat[which(pop_mat>0,arr.ind=TRUE)],pop_mat[which(pop_mat>0,arr.ind=TRUE)]))
    }
    fish[fish$length>min_size_migration,] <- temp_fish
    return(fish)
}


#Beverton-Holt model for carrying capacity based recruitment mortality, carrying capacity is the mean of North American carrying capacities in Table 3 of Myers et al. 2001 (CC=0.431088 tonnes/km^2 )
# input larvae, get recruits
BH_CC_mortality <- function(larvae,fish,CCs){
    fish_table <- summarise(group_by(fish,polygon),num=sum(polygon==polygon),weight=sum(weight))
    fish_table <- fish_table[fish_table$polygon %in% larvae$polygon,]
    if(dim(fish_table)[1]<dim(larvae)[1]) fish_table <- rbind(fish_table,cbind(polygon=larvae$polygon[!(larvae$polygon %in% fish_table$polygon)],num=1,weight=0))    
    larvae$new_recruits <- 0
    fish_avg_weighti <- mean(fish$weight)
    for(i in larvae$polygon){
        fishi <- fish_table$num[fish_table$polygon==i]
        larvaei <- larvae$recruit[larvae$polygon==i]
        R <- (larvaei+fishi)/fishi
        CC_n <- ceiling(CCs[i]/fish_avg_weighti)
        new_total <- R*fishi/(1+fishi/CC_n)
        new_recruits <- new_total-fishi
        larvae$new_recruits[larvae$polygon==i] <- round(new_recruits)
    } 
    larvae$new_recruits[larvae$new_recruits<0] <- 0
    larvae$new_recruits[is.na(larvae$new_recruits)] <- 0
    new_recruits <- rep(larvae$polygon,larvae$new_recruits)
    return(new_recruits)
}

# returns only the biggest polygons
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

# calculates socially discounted values
SDR_value <- function(t0,t,value,i){
    value*(1/(1+SDR[i])^(t-t0))
}
