#### Growth and Reproduction ####
# Reproductive output
print("Now calculating: Reproductive output")
larvae <- data.frame(eggs=round(fish$weight[fish$age>=min_age_mat & fish$sex==1]*fecundity),
                     polygon=fish$polygon[fish$age>=min_age_mat & fish$sex==1]
                     )
# note: for SEX males=0, females=1
# summarize per polygon
larvae <- summarise(group_by(larvae,polygon),eggs=sum(eggs))

# larval mortality
larvae$eggs <- round(larvae$eggs*(1-lM))
