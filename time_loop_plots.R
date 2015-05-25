print("plotting...")
#### plot biomass ####
layout(matrix(c(1,1,1,2,2,2,3,4,4,4,4,5,5,5),nrow=2,byrow = TRUE),widths=c(1,1,1,1,1,1,1))
par(mar=c(1,1,3,1))
biomass_mat <- matrix(0,grd@cells.dim[1],grd@cells.dim[2])
for(poly in unique(fish$polygon)){
    biomass_mat[which(hab_mat==poly,arr.ind=TRUE)] <- sum(fish$weight[fish$polygon==poly])/1000*virtual_fish_ratio
}
zlim=c(0,8)
#print(paste("zlim for biomass",ceiling(log(max(as.vector(biomass_mat))))))
image(log(biomass_mat),axes=FALSE,zlim=zlim,col=rev(heat.colors(12)));box()
title(paste0('Biomass for ',t," (",scenario," - Rep ",rep,")"))
#     par(mar=c(1,1,3,1))
#     image(matrix(c(zlim[1]:zlim[2]),nrow=1),axes=FALSE,zlim=zlim,col=rev(heat.colors(12)));box()
#     mtext(c(zlim[1]:zlim[2]),2,at=(c(zlim[1]:zlim[2])-zlim[1])/(zlim[2]-zlim[1]),las=1)
#     title("e^x tonnes")

#### plot catch ####
par(mar=c(1,1,3,1))
catch_mat <- matrix(0,grd@cells.dim[1],grd@cells.dim[2])
for(poly in unique(fish$polygon)){
    catch_mat[which(hab_mat==poly,arr.ind=TRUE)] <- sum(catch$weight[catch$polygon==poly])/1000*virtual_fish_ratio
}
#print(paste("zlim for catch",ceiling(log(max(as.vector(biomass_mat))))))
image(log(catch_mat),axes=FALSE,zlim=zlim,col=rev(heat.colors(12)));box()
title(paste0('Catch for ',t," (",scenario," - Rep ",rep,")"))

#plot legend
par(mar=c(1,1,3,1))
image(matrix(c(zlim[1]:zlim[2]),nrow=1),axes=FALSE,zlim=zlim,col=rev(heat.colors(12)));box()
mtext(c(zlim[1]:zlim[2]),2,at=(c(zlim[1]:zlim[2])-zlim[1])/(zlim[2]-zlim[1]),las=1)
title("ln(tonnes)")

#### plot age distribution histogram ####
par(mar=c(4,4,1,1))
age_distribution <- hist(fish$age,breaks=c(0:max(fish$age)),plot=FALSE,labels=FALSE)
age_distribution$counts <- log(age_distribution$counts*virtual_fish_ratio+1)
plot(age_distribution,main="",xlab="",ylab="")
title(xlab="Age",ylab="log(Frequency)")

#### plot recruitment ####
pop_mat <- matrix(0,grd@cells.dim[1],grd@cells.dim[2])
for(i in unique(larvae$polygon)){
    pop_mat[loc_p$X[i],loc_p$Y[i]] = larvae$recruit[larvae$polygon==i]
}

par(mar=c(1,1,1,1))
image(log(pop_mat),axes=FALSE,zlim=c(0,10),col=rev(heat.colors(12)));box()
title(paste("Potential New Recruits for ",t+1))
#print(paste("zlim for recruits",ceiling(log(min(as.vector(pop_mat)))),ceiling(log(max(as.vector(pop_mat))))))