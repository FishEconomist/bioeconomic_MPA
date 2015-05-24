#### Dispersal ####
# Adult
print("Now calculating: Adult Dispersal")
fish <- bind_rows(lapply(unique(fish$polygon), function(x) adult_disperse(fish[fish$polygon==x,],x,min_size_migration,hab_mat,pop_mat)))

# enforce carrying capacity for adults
while(sum(fish$weight)>sum(CCs[unique(fish$polygon)])){
    fish_table <- summarise(group_by(fish,polygon),num=sum(polygon==polygon),weight=sum(weight))
    fish_table$CC <- CCs[fish_table$polygon]
    fish_table$CCn  <- fish_table$CC/(fish_table$weight/fish_table$num)
    fish_table$CC_M  <- 1-fish_table$CCn/fish_table$num
    CC_M <- rep(fish_table$CC_M,fish_table$num)
    CC_M[CC_M<0] <- 0
    CC_M <- CC_M^fish$age  #because bigger fish are competitively superior
    fish <- fish[runif(length(fish$age),0,1)>=CC_M,]
}