#### Dispersal ####
# Larval
print("Now calculating: Larval Dispersal")
if(t==min(time)){
    distance_matrix <- gDistance(p,p,byid = TRUE)
    distance_breeding <- gDistance(Breeding,p,byid = TRUE)
    closest_breeding <- apply(distance_breeding,1,which.min)
    not_breeding <- c(1:length(p))[!apply(gIntersects(p,Breeding,byid=TRUE),2,any)]
}

### reallocate eggs to spawning ground
larvae$polygon <- closest_breeding[larvae$polygon]
larvae$polygon[is.na(larvae$polygon)] <- round(runif(1,1,length(Breeding))) #remove NAs
larvae <- summarise(group_by(larvae,polygon),eggs=sum(eggs))
larvae$eggs[is.na(larvae$eggs)] <- 1                                        #remove NAs


### disperse eggs/larvae
recruits <- (sapply(unique(larvae$polygon), function(i) disperse_to_polygon(rexp(larvae$eggs[larvae$polygon==i],rate=1/e_fold_larvae),distance_breeding[,i])))
#        plot(EEZ)
#        plot(Breeding,add=T,col='blue')
#        plot(p[unique(recruits[[13]]),],add=T)
