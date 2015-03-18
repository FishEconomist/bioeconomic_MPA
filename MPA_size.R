#### MPA size frequency ####
# MPA size frequency obtained from WDPA database downloaded from http://www.protectedplanet.net/search?marine=1 in March 2015
require(rgdal)

MPAs_coast <- readOGR(dsn="D:/WDPA_Mar2015_Public",layer="WDPA_poly_Mar2015_MARINE1")
MPAs_mar <- readOGR(dsn="D:/WDPA_Mar2015_Public",layer="WDPA_poly_Mar2015_MARINE2_clip")
MPAs_coast <- MPAs_coast[MPAs_coast$REP_M_AREA>0,]  #filter out marine area=0
MPAs_mar <- MPAs_mar[MPAs_mar$REP_M_AREA>0,]  #filter out marine area=0


# plot(MPAs)
hist(MPAs_coast$REP_M_AREA,breaks=200)
h_coast <- hist(log10(MPAs_coast$REP_M_AREA),breaks=200,xlim=c(-10,10))

hist(MPAs_mar$REP_M_AREA,breaks=200)
h_mar <- hist(log10(MPAs_mar$REP_M_AREA),breaks=200,xlim=c(-10,10))

MPA_size_dist_coast <- data.frame(cbind(breaks=h_coast$breaks,counts=c(h_coast$counts,0)))
MPA_size_dist_coast$prob <- MPA_size_dist_coast$counts/sum(MPA_size_dist_coast$counts)
MPA_size_dist_coast$cum_prob <- cumsum(MPA_size_dist_coast$prob)
write.csv(MPA_size_dist_coast,"MPA_size_dist_coast.csv")

MPA_size_dist_mar <- data.frame(cbind(breaks=h_mar$breaks,counts=c(h_mar$counts,0)))
MPA_size_dist_mar$prob <- MPA_size_dist_mar$counts/sum(MPA_size_dist_mar$counts)
MPA_size_dist_mar$cum_prob <- cumsum(MPA_size_dist_mar$prob)
write.csv(MPA_size_dist_mar,"MPA_size_dist_mar.csv")

#### proof of concept ####
x <- runif(30000,0,1)

find_bin <- function(x,cum_prob){
    which.min(abs(cum_prob-x))-1
}

random_between_breaks <- function(x){
    runif(1,x,x+0.1)
}
generate_MPAs <- function(n,cum_prob,breaks){
    x <- runif(n,0,1)
    bin <- sapply(x,find_bin,cum_prob)
    low_break <- breaks[bin]
    log_size <- sapply(low_break,random_between_breaks)
    10^log_size
}
size <- generate_MPAs(3000,MPA_size_dist_coast$cum_prob,MPA_size_dist_coast$breaks)
hist(log10(size),breaks=200,xlim=c(-10,10))
h <- hist(log10(MPAs_coast$REP_M_AREA),breaks=200,xlim=c(-10,10))


#### trim MPA to EEZ ####
# EEZ shapefile obtained from http://www.marineregions.org/gazetteer.php?p=details&id=8777 in March 2015
EEZ <- readOGR(dsn="D:/WDPA_Mar2015_Public",layer="eez_iho_union_v2")
library(sp)
EEZ <- spTransform(EEZ,CRS(proj4string(MPAs_coast)))

library(rgeos)

index <- gIntersects(MPAs_coast,EEZ, byid=TRUE)
index <- apply(index,2,sum)
index <- c(1:length(index))[index>0]
MPAs_coast <- MPAs_coast[index,]
writeOGR(MPAs_coast,dsn=getwd(), layer = "MPAs_coast", driver = "ESRI Shapefile",overwrite_layer=T)

index <- gIntersects(MPAs_mar,EEZ, byid=TRUE)
index <- apply(index,2,sum)
index <- c(1:length(index))[index>0]
MPAs_mar <- MPAs_mar[index,]
writeOGR(MPAs_mar,dsn = getwd(),layer = "MPAs_mar", driver = "ESRI Shapefile",overwrite_layer=T)
