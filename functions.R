#### functions for MPA size ####
#cum_prob and breaks are from MPA_size_dist.csv which is created in MPA_size_dist
find_bin <- function(x,cum_prob){
    which.min(abs(cum_prob-x))-1
}

random_between_breaks <- function(x){
    runif(1,x,x+0.1)
}

generate_MPAs <- function(n,cum_prob,breaks){
    x <- runif(n,0,1)
    bin <- sapply(x,find_bin,cum_prob)
    low_break <- breaks[bin]
    log_size <- sapply(low_break,random_between_breaks)
    10^log_size
}