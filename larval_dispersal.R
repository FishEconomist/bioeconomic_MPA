#### Dispersal ####
# Larval
print("Now calculating: Larval Dispersal")
if(t==min(time)){
    distance_breeding <- gDistance(Breeding,p,byid = TRUE)
    closest_breeding <- apply(distance_breeding,1,which.min)
}

#### reallocate eggs to spawning ground ####
# print("reallocate eggs to spawning ground")
require(dplyr)
larvae$polygon <- closest_breeding[larvae$polygon]
larvae$polygon[is.na(larvae$polygon)] <- round(runif(1,1,length(Breeding))) #remove NAs
larvae <- summarise(group_by(larvae,polygon),eggs=sum(eggs))
larvae$eggs[is.na(larvae$eggs)] <- 1                                        #remove NAs


#### disperse eggs/larvae ####
# print("disperse eggs/larvae")
pop_mat <- matrix(0,grd@cells.dim[1],grd@cells.dim[2])
for(i in larvae$polygon){
    pop_mat[loc_breeding$X[i],loc_breeding$Y[i]] = larvae$eggs[larvae$polygon==i]
}

pop_mat <- disperse_to_polygon(hab_mat,pop_mat,rexp,rate=1/e_fold_larvae)
larvae <- sapply(unique(as.vector(hab_mat)), function(i) data.frame(polygon=i,recruit=sum(pop_mat[which(hab_mat==i, arr.ind=TRUE)])))
rm(pop_mat)
larvae <- data.frame(polygon=unlist(larvae[1,]),recruit=unlist(larvae[2,]))
names(larvae) <- c("polygon","recruit")
larvae <- larvae[larvae$recruit>0,]

### recruits not near habitat will die ###
require(dplyr)
larvae <- larvae[(larvae$polygon %in% unique(as.vector(hab_mat))),]


#### recruitment mortality due to carrying capacity ####
# print("recruitment mortality due to carrying capacity")
BH_CC_mortality(larvae,fish,CC,CC_sd)
larvae$recruit <- BH_CC_mortality(larvae,fish,CC,CC_sd)
larvae <- larvae[larvae$recruit>0,]

recruits <- as.vector(unlist(sapply(unique(larvae$polygon), function(i) rep(i,larvae$recruit[larvae$polygon==i]) )))

print(paste(length(recruits),"new recruits to be added to population in",t+1))
