#### MPA size frequency ####
# MPA size frequency obtained from WDPA database downloaded from http://www.protectedplanet.net/search?marine=1 in March 2015
require(rgdal)
require(fitdistrplus)
MPAs <- readOGR(dsn="D:/WDPA_Mar2015_Public",layer="WDPA_poly_Mar2015_MARINE1or2")
MPAs <- MPAs[MPAs$REP_M_AREA>0,]

# plot(MPAs)
hist(MPAs$REP_M_AREA)
h <- hist(log10(MPAs$REP_M_AREA),breaks=140,xlim=c(-7,7))

MPA_size_dist <- data.frame(cbind(breaks=h$breaks,counts=c(h$counts,0)))
MPA_size_dist$prob <- MPA_size_dist$counts/sum(MPA_size_dist$counts)
MPA_size_dist$cum_prob <- cumsum(MPA_size_dist$prob)
write.csv(MPA_size_dist,"MPA_size_dist.csv")

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
size <- generate_MPAs(3000,MPA_size_dist$cum_prob,MPA_size_dist$breaks)
hist(log10(size),breaks=140,xlim=c(-7,7))
h <- hist(log10(MPAs$REP_M_AREA),breaks=140,xlim=c(-7,7))
