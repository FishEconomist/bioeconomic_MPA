#### Dispersal ####
# Adult
print("Now calculating: Adult Dispersal")

if(adult_con_mat){
    # use connectivity matrices
    require(readr)
    require(tidyr)
    print(paste0("using con_mat_adult_",t,".csv"))
    # load the con_mat for the current year
    x <- ncol(read.table(paste0("con_mat/con_mat_adult_",t,".csv"),sep=",",nrows=1))
    con_mat <- read_csv(paste0("con_mat/con_mat_adult_",t,".csv"),progress = FALSE,col_type=paste0(rep("d",x),collapse=""))
    row.names(con_mat) <- as.numeric(unlist(con_mat[, 1]))
    con_mat <- con_mat %>% select(-1)
    
    if(any(fish$length>=min_size_migration)){
        fish_table <- data.frame(table(fish$polygon[fish$length>=min_size_migration]),stringsAsFactors=FALSE)
        while("Var1" %in% names(fish_table)){
            try(
                fish_table <- con_mat_disperse(fish_table$Var1,fish_table$Freq,con_mat)
            )
        }
        fish_table <- fish_table[with(fish_table, order(as.numeric(release_site))), ]
        fish$polygon[fish$length>=min_size_migration] <- as.numeric(fish_table$settlement_site)
    }
    

} else {
    # completely random dispersal
    fish <- bind_rows(lapply(unique(fish$polygon), function(x) adult_disperse(fish[fish$polygon==x,],x,min_size_migration,hab_mat,pop_mat)))
}