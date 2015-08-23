require(dplyr)
### Generate random connectivity matrix

#### user inputs ####
source("user_input.R")
protect_scen_new <- FALSE

#### source custom functions ####
source("functions.R")

#### Spatial base layer ####
# Basic grid
source("basic_grid.R")

# Protection scenarios (Figure 1)
rep=1
source("protection_scenarios.R")

# set n
n=1000

#### con_mat for adults ####
for(t in tot_time[1:5]){
        
    fish <- data.frame(age=5,
                       polygon=rep(1:length(p),each=n),
                       sex=round(runif(n*length(p),0,1)),
                       length=10
    )
    fish <- fish[(fish$polygon %in% unique(as.vector(hab_mat))),]
    
    fish$old_poly <- fish$polygon
    
    fish <- bind_rows(lapply(unique(fish$polygon), function(x) adult_disperse(fish[fish$polygon==x,],x,0,hab_mat,pop_mat)))
    
    con_mat_adult <- with(fish,table(old_poly,polygon))/n
    image(log(as.matrix(con_mat_adult)))

    write.csv(con_mat_adult,paste0("con_mat/con_mat_adult_",t,".csv"))
}

for(t in tot_time[6:61]){
    original <- t%%5
    original[original==0] <- 5
    print(paste(t,original))
    file.copy(paste0("con_mat/con_mat_adult_",tot_time[original],".csv"),paste0("con_mat/con_mat_adult_",t,".csv"),overwrite = T)
}

#### con_mat for larvae ####
n <- 5000
for(t in tot_time[1:5]){
    
    larvae <- data.frame(polygon=rep(which(gIntersects(p,Breeding,byid = TRUE),arr.ind = TRUE)[,2],each=n))
    larvae <- larvae %>% filter(polygon!=0)
    
    larvae$old_poly <- larvae$polygon
    
    larval_disperse <- function(larvae,poly,hab_mat,pop_mat){
        pop_mat <- matrix(0,grd@cells.dim[1],grd@cells.dim[2])
        pop_mat[which(hab_mat==poly,arr.ind=TRUE)] <- length(larvae$polygon)
        pop_mat <- disperse_to_polygon(hab_mat,pop_mat,rexp,rate=1/e_fold_larvae)
        if(dim(larvae)[1]==1){
            larvae$polygon <- hab_mat[which(pop_mat>0,arr.ind=TRUE)]
        } else {
            larvae$polygon <- sample(rep(hab_mat[which(pop_mat>0,arr.ind=TRUE)],pop_mat[which(pop_mat>0,arr.ind=TRUE)]))
        }
        return(larvae)
    }
    
    larvae <- bind_rows(lapply(unique(larvae$polygon), function(x) larval_disperse(larvae[larvae$polygon==x,],x,EEZ_mat,pop_mat)))
    
    con_mat_larvae <- with(larvae,table(old_poly,polygon))/n
    image(log(as.matrix(con_mat_larvae)))

    write.csv(con_mat_larvae,paste0("con_mat/con_mat_larvae_",t,".csv"))
}

for(t in tot_time[6:61]){
    original <- t%%5
    original[original==0] <- 5
    print(paste(t,original))
    file.copy(paste0("con_mat/con_mat_larvae_",tot_time[original],".csv"),paste0("con_mat/con_mat_larvae_",t,".csv"),overwrite = T)
}