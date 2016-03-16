#### Dispersal ####
# Larval
print("Now calculating: Larval Dispersal")
if(t==min(tot_time)){
    distance_breeding <- gDistance(Breeding,p,byid = TRUE)
    closest_breeding <- apply(distance_breeding,1,which.min)
}

#### reallocate eggs to spawning ground ####
# print("reallocate eggs to spawning ground")
require(dplyr)
larvae$polygon <- closest_breeding[larvae$polygon]
larvae$polygon[is.na(larvae$polygon)] <- round(runif(1,1,length(Breeding))) #remove NAs
larvae <- summarise(group_by(larvae,polygon),eggs=sum(eggs,na.rm=TRUE))
larvae$eggs[is.na(larvae$eggs)] <- 1                                        #remove NAs


#### disperse eggs/larvae ####
if(larvae_con_mat){
    # use connectivity matrices
    require(readr)
    require(tidyr)
    #re-assign polygon index to main p grid
    if(t==min(tot_time)) overlaps <- which(gIntersects(p,Breeding,byid = TRUE),arr.ind = TRUE)
    larvae <- bind_rows(lapply(larvae$polygon, function(x) data.frame(polygon=overlaps[overlaps[,1]==x,2],eggs=larvae$eggs[larvae$polygon==x]%/%length(overlaps[overlaps[,1]==x,2]))))
    # load the con_mat for the current year
    x <- ncol(read.table(paste0("con_mat/con_mat_larvae_",t,".csv"),sep=",",nrows=1))
    con_mat <- read_csv(paste0("con_mat/con_mat_larvae_",t,".csv"),progress = FALSE,col_type=paste0(rep("d",x),collapse=""))
    row.names(con_mat) <- as.numeric(unlist(con_mat[, 1]))
    con_mat <- con_mat %>% select(-1)
    
    while("eggs" %in% names(larvae)){
        try(
            larvae <- con_mat_disperse(larvae$polygon,larvae$eggs,con_mat) %>% 
                select(settlement_site) %>% 
                table(dnn="polygon") %>% 
                as.data.frame() %>% 
                filter(Freq>0) %>% 
                group_by(polygon) %>% 
                summarize(recruit=sum(Freq)) %>% 
                ungroup() %>% 
                mutate(polygon=as.numeric(as.character(polygon)))
        )
    }

} else {
    # completely random dispersal
    pop_mat <- matrix(0,grd@cells.dim[1],grd@cells.dim[2])
    for(i in larvae$polygon){
        pop_mat[loc_breeding$X[i],loc_breeding$Y[i]] = larvae$eggs[larvae$polygon==i]
    }
    
    pop_mat <- disperse_to_polygon(EEZ_mat,pop_mat,rexp,rate=1/e_fold_larvae)
    larvae <- sapply(unique(as.vector(EEZ_mat)), function(i) data.frame(polygon=i,recruit=sum(pop_mat[which(EEZ_mat==i, arr.ind=TRUE)])))
    rm(pop_mat)
    larvae <- data.frame(polygon=unlist(larvae[1,]),recruit=unlist(larvae[2,]))
    names(larvae) <- c("polygon","recruit")
    larvae <- larvae[larvae$recruit>0,]
    larvae <- larvae[larvae$polygon>0,]
}

#### larvae that don't settle on suitable habitat will die ####
larvae <- larvae[larvae$polygon %in% hab_mat,]
