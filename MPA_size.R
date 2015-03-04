#### MPA size frequency ####
# MPA size frequency obtained from WDPA database downloaded from http://www.protectedplanet.net/search?marine=1 in March 2015
require(rgdal)
MPAs <- readOGR(dsn="D:/WDPA_Mar2015_Public",layer="WDPA_poly_Mar2015_MARINE1or2")
MPAs <- MPAs[MPAs$REP_M_AREA>0,]

# plot(MPAs)
hist(log10(MPAs$REP_M_AREA))
hist(MPAs$REP_M_AREA)

fit <- fitdist(log10(MPAs$REP_M_AREA),"norm",method="mme")
fit <- fitdist(MPAs$REP_M_AREA,"lnorm",method="mme")
summary(fit)
b <- c(-8:8)
h1 <- hist(rnorm(3292,fit$estimate[1],fit$estimate[2]*1.1),b)
h2 <- hist(log10(MPAs$REP_M_AREA),b)
h1$counts-h2$counts
