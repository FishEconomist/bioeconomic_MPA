#### Cost evaluation ####
# Value of fish catches over time
print("Now calculating: Value of fish catches over time")
###### re-load data ###########
require(readr)
require(dplyr)
catch <- NULL
fish <- NULL

fish_communities2 <- spTransform(fish_communities2,CRS(proj))
distance_from_shore <- gDistance(fish_communities2,p,byid=TRUE)

files_df <- expand.grid(replicates,protect_scen,time)
names(files_df) <- c('rep','scenario','time')


files <- paste0(results_folder,"/",files_df$scenario,"_fish_",files_df$time,"_rep_",files_df$rep,".csv")
fish <- lapply(files,read_csv,col_type=list(col_skip() ,col_skip(),col_skip(),col_skip(),col_skip(),col_double()))
fish_long <- data.frame(cbind(files_df,biomass=unlist(lapply(fish,sum))))
###### summarize fish ###########

fish <- summarise(group_by(fish_long,scenario,time),tot_biomass=mean(biomass),biomass_SD=sd(biomass))

fish$tot_biomass[is.na(fish$tot_biomass)] <- 0

#adjust from virtual fish (kg) to real fish (t)
fish$tot_biomass <- fish$tot_biomass/1000*virtual_fish_ratio
fish_long$biomass <-fish_long$biomass/1000*virtual_fish_ratio


########### catch ################
files <- paste0(results_folder,"/",files_df$scenario,"_catch_",files_df$time,"_rep_",files_df$rep,".csv")
catch <- lapply(files,read_csv,col_type=list(col_skip() ,col_double(),col_double(),col_double(),col_double(),col_double()))
catch <- bind_rows(lapply(1:length(files),function(i) cbind(catch[[i]],files_df[i,])))

catch$distance <- apply(distance_from_shore,1,min)[catch$polygon]
###### summarize catch ###########
fish_value <- summarise(group_by(catch,scenario,time,rep),dist=mean(distance,na.rm=TRUE),tot_catch=sum(weight,na.rm=TRUE))


fish_value$tot_catch[is.na(fish_value$tot_catch)] <- 0
fish_value$dist[is.na(fish_value$dist)] <- 0

#adjust from virtual fish (kg) to real fish (t)
fish_value$tot_catch <- fish_value$tot_catch/1000*virtual_fish_ratio

#calculate means
fish_value_means <- summarise(group_by(fish_value,scenario,time),mean_dist=mean(dist,na.rm=TRUE),SD_dist=sd(dist,na.rm=TRUE),tot_catch_mean=mean(tot_catch,na.rm=TRUE),tot_catch_SD=sd(tot_catch,na.rm=TRUE))


#calculate gross catch value
fish_value$gross_catch_value_USD <- fish_value$tot_catch*fish_landed_value


#calculate operating cost
# for 2001, the landing value should be ~1.6 times operating cost
fish_operating_cost_total <- mean(fish_value$gross_catch_value_USD[fish_value$scenario=="Status_quo"]/Status_quo_profitability)
fish_value$dist_corr_factor <- fish_value$dist/mean(fish_value$dist[fish_value$scenario=="Status_quo"])
fish_value$operating_cost <- fish_value$dist_corr_factor*fish_operating_cost_total

#calculate net catch value
fish_value$net_catch_value_USD <- fish_value$gross_catch_value_USD-fish_value$operating_cost


#calculate means again
fish_value_means <- summarise(group_by(fish_value,scenario,time),
                              mean_dist=mean(dist,na.rm=TRUE),
                              SD_dist=sd(dist,na.rm=TRUE),
                              tot_catch_mean=mean(tot_catch,na.rm=TRUE),
                              tot_catch_SD=sd(tot_catch,na.rm=TRUE),
                              net_catch_value_USD_mean=mean(net_catch_value_USD,na.rm=TRUE),
                              net_catch_value_USD_SD=sd(net_catch_value_USD,na.rm=TRUE))


