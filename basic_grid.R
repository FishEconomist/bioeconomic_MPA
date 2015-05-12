#### Spatial base layer ####
# Basic grid
print("Now calculating: Basic grid")
print(paste0("cell size = ",cell_size," km by ",cell_size," km"))

# EEZ shapefile obtained from http://www.marineregions.org/gazetteer.php?p=details&id=8777 in March 2015
require(rgdal)
require(maptools)


EEZ <- readOGR(dsn="D:/WDPA_Mar2015_Public",layer="eez_iho_union_v2")
EEZ <- spTransform(EEZ,CRS(proj))
EEZ <- EEZ[EEZ$marregion!="Canadian part of the Davis Strait",]
EEZ <- unionSpatialPolygons(EEZ,rep(1,length(EEZ)))
bb <- bbox(EEZ)
plot(EEZ)

#### create habitat grid ####
bb <- bbox(EEZ)
cs <- c(cell_size,cell_size)*1000  # cell size
cc <- bb[, 1] + (cs/2)  # cell offset
cd <- ceiling(diff(t(bb))/cs)  # number of cells per direction
grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)

sp_grd <- SpatialGridDataFrame(grd,
                               data=data.frame(id=1:prod(cd)),
                               proj4string=CRS(proj4string(EEZ)))

#### make grid into polygon ####
library(Grid2Polygons)
p <- Grid2Polygons(sp_grd)
plot(p,add=T)

#### trim grid polygon to EEZ
library(rgeos)
p <- gIntersection(EEZ,p,byid=T, drop_lower_td=TRUE)
plot(p)

#### determine polygon locations for dispersal process ####
loc_breeding <- data.frame(matrix(sapply(unique(larvae$polygon),function(i) slot(Breeding@polygons[[i]],"labpt")),ncol=2,byrow=TRUE))
names(loc_breeding) <- c("X","Y")
loc_breeding$X <- round((loc_breeding$X-grd@cellcentre.offset[1])/grd@cellsize[1])
loc_breeding$Y <- round((loc_breeding$Y-grd@cellcentre.offset[2])/grd@cellsize[2])

loc_p <- data.frame(matrix(sapply(1:length(p),function(i) slot(p@polygons[[i]],"labpt")),ncol=2,byrow=TRUE))
names(loc_p) <- c("X","Y")
loc_p$X <- round((loc_p$X-grd@cellcentre.offset[1])/grd@cellsize[1])
loc_p$Y <- round((loc_p$Y-grd@cellcentre.offset[2])/grd@cellsize[2])

EEZ_mat <- matrix(0,grd@cells.dim[1],grd@cells.dim[2])
for(i in 1:grd@cells.dim[1]){
    for(j in 1:grd@cells.dim[2]){
        if(any(loc_p$X==i&loc_p$Y==j)){
            EEZ_mat[i,j] <- which(loc_p$X==i&loc_p$Y==j)
        }
    }
}
