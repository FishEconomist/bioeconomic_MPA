#### Spatial base layer ####
# Protection scenarios (Figure 1)
print("Now calculating: Protection scenarios")
if(!file.exists(paste0(results_folder,"/shapefiles"))) dir.create(paste0(results_folder,"/shapefiles"))

if(rep==min(replicates)){
    # MPA size frequency obtained from WDPA database downloaded from http://www.protectedplanet.net/search?marine=1 in March 2015
    # calculations of MPA size frequency are done in MPA_size.R
    MPA_size_dist_coast <- read.csv("MPA_size_dist_coast.csv")
    MPA_size_dist_mar <- read.csv("MPA_size_dist_mar.csv")
    
    # remove size distribution below grid size
    MPA_size_dist_coast <- MPA_size_dist_coast[MPA_size_dist_coast$breaks>=log10(cell_size^2),]
    MPA_size_dist_mar <- MPA_size_dist_mar[MPA_size_dist_mar$breaks>=log10(cell_size^2),]
    
    
    coords_bbox <- bbox(EEZ)
    coords_bbox <- data.frame(long=as.vector(coords_bbox[1,]),lat=as.vector(coords_bbox[2,]), stringsAsFactors=F)
    coords_bbox <- data.frame(long=coords_bbox$long[c(1,2,2,1)],lat=coords_bbox$lat[c(1,1,2,2)], stringsAsFactors=F)
    coordinates(coords_bbox) <- ~ long + lat
    coords_bbox <- SpatialPolygons(list(Polygons(list(Polygon(coords_bbox)),ID="CB")))
    
    # standard projection
    require(sp)
    MPAs_coast <- spTransform(MPAs_coast,CRS(proj))
    MPAs_mar <- spTransform(MPAs_mar,CRS(proj))
    EEZ <- spTransform(EEZ,CRS(proj))
    EEZ <- unionSpatialPolygons(EEZ,rep(1,length(EEZ)))
    
    
    
    Habitats <- spTransform(Habitats,CRS(proj))
    Breeding <- spTransform(Breeding,CRS(proj))
    Breeding <- gIntersection(EEZ,Breeding,byid=T, drop_lower_td=TRUE)
    # Habitats <- gIntersection(p,Habitats,byid=T, drop_lower_td=TRUE)
    Habitats <- p[apply(gIntersects(p,Habitats,byid=T),2,any),]
    plot(Habitats)
    
    #### plot MPAs ####
    require(rgeos)
    plot(EEZ)
    plot(MPAs_coast,add=T,col='blue')
    plot(MPAs_mar,add=T,col='red')
    # plot(MPAs_AOI,add=T,col='green')
    
    #### unique IDs ####
    # MPAs_AOI <- spChFIDs(MPAs_AOI,paste("AOI",c(1:length(MPAs_AOI))))
    MPAs_coast <- spChFIDs(MPAs_coast,paste("coast",c(1:length(MPAs_coast))))
    MPAs_mar <- spChFIDs(MPAs_mar,paste("mar",c(1:length(MPAs_mar))))
    
    #### rbind MPAs ####
    MPAs <- rbind(MPAs_mar,MPAs_coast)
    rm(MPAs_coast)
    rm(MPAs_mar)
    
    # MPAs <- rbind(MPAs,MPAs_AOI)
    
    # remove Canadian part of the Davis Strait for MPA
    MPAs <- MPAs[as.logical(gContains(EEZ,MPAs,byid=T)),]
    
    #### ignore any MPAs smaller than 10% of grid size ####
    MPAs <- MPAs[gArea(MPAs,byid=T)/(10^6)>(0.1*(cell_size^2)),]
    Status_quo <- MPAs
    
    #### match dataframe for p ####
    df <- MPAs@data[1,]
    df[,sapply(df, is.numeric)] <- 0
    df[,sapply(df, is.numeric)==F] <- NA
    df <- df[rep(1,length(p)),]
    p <- SpatialPolygonsDataFrame(p,df,match.ID = F)
    p <- spChFIDs(p,paste("p",c(1:length(p))))
    
    #### match dataframe for Habitats ####
    df <- MPAs@data[1,]
    df[,sapply(df, is.numeric)] <- 0
    df[,sapply(df, is.numeric)==F] <- NA
    df <- df[rep(1,length(Habitats)),]
    Habitats <- SpatialPolygonsDataFrame(Habitats,df,match.ID = F)
    Habitats <- spChFIDs(Habitats,paste("hab",c(1:length(Habitats))))
    
    #### match dataframe for Breeding ####
    df <- MPAs@data[1,]
    df[,sapply(df, is.numeric)] <- 0
    df[,sapply(df, is.numeric)==F] <- NA
    df <- df[rep(1,length(Breeding)),]
    Breeding <- SpatialPolygonsDataFrame(Breeding,df,match.ID = F)
    Breeding <- spChFIDs(Breeding,paste("hab",c(1:length(Breeding))))
    
    #### make Canada (land) polygon for placement of coastal MPAs ####
    data(wrld_simpl)
    Canada <- wrld_simpl[wrld_simpl$NAME==country_name, ]
    Canada <- spTransform(Canada,CRS(proj))
    rm(wrld_simpl)
    plot(Canada)
    
    coastal <- as.vector(gDistance(p,Canada,byid=T)<cell_size)
    plot(p[coastal,])
}

if(protect_scen_new){
    
    #### place coastal MPAs ####
    #generate consistent sizing
    sizes <- generate_MPA_size(1000,MPA_size_dist_coast$cum_prob,MPA_size_dist_coast$breaks)
    tot_area <- gArea(EEZ)
    while(max(sizes)>MPA_coverage*tot_area){
        sizes[sizes>MPA_coverage*tot_area] <- generate_MPA_size(length(sizes[sizes>MPA_coverage*tot_area]),MPA_size_dist_coast$cum_prob,MPA_size_dist_coast$breaks)
    }
    if("MPAs_random" %in% protect_scen){
        print("Establishing new coastal randomly placed MPAs")
        MPAs_random <- generate_MPAs(sizes,MPAs,p[coastal,],p,MPA_coverage*CtoM,EEZ,"Random")
    }
    
    if("MPAs_maxdist" %in% protect_scen){
        print("Establishing new coastal maximum distance MPAs")
        MPAs_maxdist <- generate_MPAs(sizes,MPAs,p[coastal,],p,MPA_coverage*CtoM,EEZ,"MaxDist") 
    }
    
    if("MPAs_fixed" %in% protect_scen){
        print(paste0("Establishing new coastal fixed distance (",fixdist," km) MPAs"))
        MPAs_fixed <- generate_MPAs(sizes,MPAs,p[coastal,],p,MPA_coverage*CtoM,EEZ,fixdist)
    }
    
    if("MPAs_targeted" %in% protect_scen){
        print(paste0("Establishing new coastal targeted with fixed distance (",fixdist," km) MPAs"))
        MPAs_targeted <- generate_MPAs(sizes,rbind(MPAs,Breeding),Habitats,Habitats,MPA_coverage*CtoM,EEZ,fixdist)
    }
    

    #### place marine MPAs ####
    #generate consistent sizing
    sizes <- generate_MPA_size(1000,MPA_size_dist_mar$cum_prob,MPA_size_dist_mar$breaks)
    tot_area <- gArea(EEZ)
    while(max(sizes)>MPA_coverage*tot_area){
        sizes[sizes>MPA_coverage*tot_area] <- generate_MPA_size(length(sizes[sizes>MPA_coverage*tot_area]),MPA_size_dist_mar$cum_prob,MPA_size_dist_mar$breaks)
    }
    if("MPAs_random" %in% protect_scen){
        print("Establishing new marine randomly placed MPAs")
        MPAs_random <- generate_MPAs(sizes,MPAs_random,p,p,(MPA_coverage-(MPA_coverage*CtoM)),EEZ,"Random")
    }
    
    if("MPAs_maxdist" %in% protect_scen){
        print("Establishing new marine maximum distance MPAs")
        MPAs_maxdist <- generate_MPAs(sizes,MPAs_maxdist,p,p,(MPA_coverage-(MPA_coverage*CtoM)),EEZ,"MaxDist")
    }
    
    if("MPAs_fixed" %in% protect_scen){
        print(paste0("Establishing new marine fixed distance (",fixdist," km) MPAs"))
        MPAs_fixed <- generate_MPAs(sizes,MPAs_fixed,p,p,(MPA_coverage-(MPA_coverage*CtoM)),EEZ,fixdist)
    }
    
    if("MPAs_targeted" %in% protect_scen){
        print(paste0("Establishing new marine targeted with fixed distance (",fixdist," km) MPAs"))
        MPAs_targeted <- generate_MPAs(sizes,MPAs_targeted,Habitats,Habitats,(MPA_coverage-(MPA_coverage*CtoM)),EEZ,fixdist)
    }
    
    
    #### save scenarios ####
    for(scenario in protect_scen){
        writeOGR(get(scenario),dsn=paste0(results_folder,"/shapefiles"), layer = paste0(scenario,"_",rep), driver = "ESRI Shapefile",overwrite_layer=T)
    }
    
    
} else {
    #### read scenarios
    for(scenario in protect_scen){
        assign(paste(scenario), readOGR(dsn=paste0(results_folder,"/shapefiles"), layer = paste0(scenario,"_",rep)))
        assign(paste(scenario), spTransform(get(scenario),CRS(proj)))
    }
}

#### plots ####
if("MPAs_random" %in% protect_scen){
    plot(EEZ)
    plot(MPAs_random,col=protect_scen_colour[protect_scen=="MPAs_random"],add=T)
    title("Random")
}
if("Status_quo" %in% protect_scen){
   plot(EEZ)
    plot(MPAs,col=protect_scen_colour[protect_scen=="Status_quo"],add=T)
    title("Status-quo")
}
if("MPAs_maxdist" %in% protect_scen){
    plot(EEZ)
    plot(MPAs_maxdist,col=protect_scen_colour[protect_scen=="MPAs_maxdist"],add=T)
    title("Max-Dist")
}
if("MPAs_fixed" %in% protect_scen){
    plot(EEZ)
    plot(MPAs_fixed,col=protect_scen_colour[protect_scen=="MPAs_fixed"],add=T)
    title("Fixed-Dist")
}
if("MPAs_targeted" %in% protect_scen){
    plot(EEZ)
    plot(MPAs_targeted,col=protect_scen_colour[protect_scen=="MPAs_targeted"],add=T)
    title("Targeted MPAs")
}



#### determine polygon locations for dispersal process ####
loc_breeding <- data.frame(matrix(sapply(1:length(Breeding),function(i) slot(Breeding@polygons[[i]],"labpt")),ncol=2,byrow=TRUE))
names(loc_breeding) <- c("X","Y")
loc_breeding$X <- round((loc_breeding$X-grd@cellcentre.offset[1])/grd@cellsize[1])+1
loc_breeding$Y <- round((loc_breeding$Y-grd@cellcentre.offset[2])/grd@cellsize[2])+1

loc_p <- data.frame(matrix(sapply(1:length(p),function(i) slot(p@polygons[[i]],"labpt")),ncol=2,byrow=TRUE))
names(loc_p) <- c("X","Y")
loc_p$X <- round((loc_p$X-grd@cellcentre.offset[1])/grd@cellsize[1])+1
loc_p$Y <- round((loc_p$Y-grd@cellcentre.offset[2])/grd@cellsize[2])+1

loc_hab <- data.frame(matrix(sapply(1:length(Habitats),function(i) slot(Habitats@polygons[[i]],"labpt")),ncol=2,byrow=TRUE))
names(loc_hab) <- c("X","Y")
loc_hab$X <- round((loc_hab$X-grd@cellcentre.offset[1])/grd@cellsize[1])+1
loc_hab$Y <- round((loc_hab$Y-grd@cellcentre.offset[2])/grd@cellsize[2])+1

EEZ_mat <- matrix(0,grd@cells.dim[1],grd@cells.dim[2])
for(i in 1:grd@cells.dim[1]){
    for(j in 1:grd@cells.dim[2]){
        if(any(loc_p$X==i&loc_p$Y==j)){
            EEZ_mat[i,j] <- which(loc_p$X==i&loc_p$Y==j)
        }
    }
}

hab_mat <- matrix(0,grd@cells.dim[1],grd@cells.dim[2])
for(i in 1:grd@cells.dim[1]){
    for(j in 1:grd@cells.dim[2]){
        if(any(loc_hab$X==i&loc_hab$Y==j)){
            hab_mat[i,j] <- which(loc_p$X==i&loc_p$Y==j)
        }
    }
}
