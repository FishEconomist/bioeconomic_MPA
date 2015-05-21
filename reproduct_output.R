#### Growth and Reproduction ####
# Reproductive output
print("Now calculating: Reproductive output")
eggers <- egg_producer(fish$age,fish$sex,age_mat_steepness,age_mat_sigmoid)
larvae <- data.frame(eggs=round(fish$weight[eggers]*fecundity),
                     polygon=fish$polygon[eggers]
                     )
# note: for SEX males=0, females=1
# summarize per polygon
require(dplyr)
larvae <- summarise(group_by(larvae,polygon),eggs=sum(eggs))

# larval mortality
larvae$eggs <- round(larvae$eggs*(1-lM))

print(paste(sum(larvae$eggs),"eggs survive to become competent larvae"))
egg_producer(fish$age,fish$sex,age_mat_steepness,age_mat_sigmoid)
