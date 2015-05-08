#### Growth and Reproduction ####
# Individual growth model
print("Now calculating: Individual growth model")
#create initial fish distribution at time 0
if(t==min(time)){
    fish <- data.frame(age=round(rexp(n*length(p), rate = M)),
                       polygon=rep(1:length(p),each=n),
                       sex=round(runif(n*length(p),0,1)),
                       length=0
                       )
    not_habitat <- c(1:length(p))[!apply(gIntersects(p,Habitats,byid=TRUE),2,any)]
    unlink("results/*")
} else {
    fish$age <- fish$age+dt
    fish <- rbind(fish,data.frame(age=0,
                                  polygon=recruit,
                                  sex=round(runif(length(recruit),0,1)),
                                  length=0
                                  ))
}
# note: for SEX males=0, females=1
# calculate new length
fish$length <- as.numeric(lapply(fish$age,VB_length,Linf_mean,Linf_SD,k_mean,k_SD,t0))
# calculate new weight - Length-weight relationship (Knickle and Rose 2013)
fish$weight <- 0.000011 * fish$length^2.91


# write fish to results folder
write.csv(fish,paste0("results/fish_",t))

### fish not near habitat will die ###
require(dplyr)
fish <- fish[!(fish$polygon %in% not_habitat),]


### natural mortality in habitat ###
fish <- fish[runif(length(fish$age),0,1)>=M,]