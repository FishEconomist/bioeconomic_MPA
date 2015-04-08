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
grd

sp_grd <- SpatialGridDataFrame(grd,
                               data=data.frame(id=1:prod(cd)),
                               proj4string=CRS(proj4string(EEZ)))

#### make grid into polygon ####
library(Grid2Polygons)
p <- Grid2Polygons(sp_grd)
plot(p,add=T)

#### trim grid polygon to EEZ
library(rgeos)
p <- gIntersection(EEZ,p,byid=T, drop_not_poly=TRUE)
plot(p)
