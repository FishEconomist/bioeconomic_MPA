#### Growth and Reproduction ####
# Individual growth model
print("Now calculating: Individual growth model")
require(rgeos)
#create initial fish distribution at time 0
if(t==min(time)){
    fish <- data.frame(age=round(rexp(n*length(p), rate = M)),
                       polygon=rep(1:length(p),each=n),
                       sex=round(runif(n*length(p),0,1)),
                       length=0
                       )
} else {
    fish$age <- fish$age+dt
    fish <- rbind(fish,data.frame(age=0,
                                  polygon=recruits,
                                  sex=round(runif(length(recruits),0,1)),
                                  length=0,
                                  weight=0
                                  ))
}
# note: for SEX males=0, females=1
# calculate new length
fish$length <- as.numeric(lapply(fish$age,VB_length,Linf_mean,Linf_SD,k_mean,k_SD,t0))
# calculate new weight - Length-weight relationship (Knickle and Rose 2013)
fish$weight <- l_to_w_int * fish$length^l_to_w_power


### fish not near habitat will die ###
require(dplyr)
fish <- fish[(fish$polygon %in% unique(as.vector(hab_mat))),]


### natural mortality in habitat ###
fish <- fish[runif(length(fish$age),0,1)>=M,]

row.names(fish) <- 1:length(row.names(fish))
print(paste("Model domain contains",length(row.names(fish)),"virtual fish"))
