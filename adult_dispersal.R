#### Dispersal ####
# Adult
print("Now calculating: Adult Dispersal")
for(poly in unique(fish$polygon)){
    temp_fish <- fish[fish$polygon==poly,]
    pop_mat <- matrix(0,grd@cells.dim[1],grd@cells.dim[2])
    pop_mat[which(hab_mat==poly,arr.ind=TRUE)] <- length(temp_fish$polygon)
    pop_mat <- disperse_to_polygon(hab_mat,pop_mat,rexp,rate=1/e_fold_adult)
    if(dim(temp_fish)[1]==1){
        temp_fish$polygon <- hab_mat[which(pop_mat>0,arr.ind=TRUE)]
    } else {
        temp_fish$polygon <- sample(rep(hab_mat[which(pop_mat>0,arr.ind=TRUE)],pop_mat[which(pop_mat>0,arr.ind=TRUE)]))
    }
    fish[fish$polygon==poly,] <- temp_fish
}