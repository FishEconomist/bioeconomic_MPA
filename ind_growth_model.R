#### Growth and Reproduction ####
# Individual growth model
print("Now calculating: Individual growth model")
require(rgeos)
#create initial fish distribution at time 0
# rate = 0.7 in rexp is a good approximation of the population at equilibrium (after 50 yrs) in the status quo scenario
if(t==min(tot_time)){
    fish <- data.frame(age=round(rexp(n*length(p), rate = 0.7)),
                       polygon=rep(1:length(p),each=n),
                       sex=round(runif(n*length(p),0,1)),
                       length=0
                       )
    CCs <- rnorm(max(fish$polygon),CC,CC_SD)*(cell_size^2)/virtual_fish_ratio #kg of virtual fish per cell
    CCs[CCs<=0] <- 0.1
    recruits <- NULL
    # note: for SEX males=0, females=1
    # calculate new length
    fish$length <- as.numeric(lapply(fish$age,VB_length,Linf_mean,Linf_SD,k_mean,k_SD,t0))
    # calculate new weight - Length-weight relationship (Knickle and Rose 2013)
    fish$weight <- l_to_w_int * fish$length^l_to_w_power
} else {
    fish$age <- fish$age+dt
    # note: for SEX males=0, females=1
    # calculate new length
    fish$length <- as.numeric(lapply(fish$age,VB_length,Linf_mean,Linf_SD,k_mean,k_SD,t0))
    # calculate new weight - Length-weight relationship (Knickle and Rose 2013)
    fish$weight <- l_to_w_int * fish$length^l_to_w_power
    
    #### recruitment mortality due to carrying capacity ####
    # print("recruitment mortality due to carrying capacity")
    if(length(larvae$polygon)>=1) recruits <- BH_CC_mortality(larvae,fish,CCs)
    
    if(length(recruits)>=1){
        
        print(paste(length(recruits),"new recruits to be added to population"))
        # pop_mat <- matrix(0,grd@cells.dim[1],grd@cells.dim[2])
        # for(i in unique(recruits)){
        #     pop_mat[loc_p$X[i],loc_p$Y[i]] = sum(recruits==i)
        # }
        # image(log(pop_mat))
        
        #bind new recruits to fish dataframe
        fish <- rbind(fish,data.frame(age=0,
                                      polygon=recruits,
                                      sex=round(runif(length(recruits),0,1)),
                                      length=5,
                                      weight=l_to_w_int * 5^l_to_w_power
        ))
    }
}


### fish not near habitat will die ###
require(dplyr)
fish <- fish[(fish$polygon %in% unique(as.vector(hab_mat))),]


### natural mortality in habitat ###
fish <- fish[runif(length(fish$age),0,1)>=sample(M,1),]

# break for loop and write empty files if extinct
if(length(row.names(fish))==0){
    print("WARNING: TOTAL EXTINCTION!")
    for(i in t:max(time)){
        catch <- data.frame(age=NA,polygon=NA,sex=NA,length=NA,weight=NA)
        fish <- data.frame(age=NA,polygon=NA,sex=NA,length=NA,weight=NA)
        write.csv(fish,paste0("results/",scenario,"_fish_",i,".csv"))
        write.csv(catch,paste0("results/",scenario,"_catch_",i,".csv")) 
    }
    break
}

row.names(fish) <- 1:length(row.names(fish))
print(paste("Model domain contains",length(row.names(fish)),"virtual fish"))



