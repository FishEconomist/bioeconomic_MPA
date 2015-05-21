#### Dispersal ####
# Adult
print("Now calculating: Adult Dispersal")
fish <- bind_rows(lapply(unique(fish$polygon), function(x) adult_disperse(fish[fish$polygon==x,],x,min_size_migration,hab_mat,pop_mat)))