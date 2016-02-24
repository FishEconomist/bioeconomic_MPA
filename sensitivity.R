#### Sensitivity Analysis Setup ####
# choose % to alter variable in question
sens_per <- 0.10

# run "regular" run? (TRUE if you want the "regular" run to be calculated)
reg_run <- TRUE
# copy protection scenario shapefiles from regular
reg_shp_copy <- TRUE

# choose variable to alter and test sensitivity (only 1 should be true at a time)
sens_var <- rbind(
    time = FALSE,
    spinup = FALSE, 
    dt = FALSE,
    cell_size = FALSE,
    n = FALSE,
    virtual_fish_ratio = FALSE,
    Linf_mean = FALSE,
    Linf_SD = FALSE,
    k_mean = FALSE,
    k_SD = FALSE,
    t0 = FALSE,
    l_to_w_int = FALSE,
    l_to_w_power = FALSE,
    age_mat_steepness = FALSE,
    age_mat_sigmoid = FALSE,
    fecundity = TRUE,
    M = FALSE,
    lM = FALSE,
    CC = FALSE, 
    CC_SD = FALSE,
    e_fold_larvae = FALSE,
    e_fold_adult  = FALSE,
    min_size_migration = FALSE,
    fish_licenses = FALSE,
    FMSY = FALSE,
    FMSY_buffer = FALSE,
    sampling_pop = FALSE,
    biomass_est_n_years = FALSE,
    min_size = FALSE, 
    MPA_coverage = FALSE,
    CtoM = FALSE,
    fixdist = FALSE,
    minimum_fishable_biomass = FALSE,
    fish_operating_cost_ratio = FALSE,
    Status_quo_profitability = FALSE,
    fish_landed_value = FALSE,
    SDR = FALSE
)

#### Sensitivity Analysis Prep ####
sens_var <- row.names(sens_var)[sens_var]

# create new folders
master <- scan("master_script.R","character",sep="\n")
new_dir <- c(master[grep("results_folder <-",master)],
             "if(!file.exists(results_folder)) dir.create(results_folder)",
             "if(!file.exists(paste0(results_folder,\"/shapefiles\"))) dir.create(paste0(results_folder,\"/shapefiles\"))")

write(new_dir,"new_dir.R")
source("new_dir.R")

# edit plus and minus versions of master_script.R
# plus
master <- scan("master_script.R","character",sep="\n")
temp <- master[grep("results_folder <-",master)]
master[grep("results_folder <-",master)] <- paste0(substr(temp,1,nchar(temp)-1),"_plus_",sens_var,"_",sens_per,substr(temp,nchar(temp),1000))
master[grep("user_input.R",master)] <- gsub("user_input.R","user_input_plus.R",master[grep("user_input.R",master)] )
write(master,"master_script_plus.R")

# create new folders
new_dir <- c(master[grep("results_folder <-",master)],
             "if(!file.exists(results_folder)) dir.create(results_folder)",
             "if(!file.exists(paste0(results_folder,\"/shapefiles\"))) dir.create(paste0(results_folder,\"/shapefiles\"))")

write(new_dir,"new_dir_plus.R")
source("new_dir_plus.R")

# minus
master <- scan("master_script.R","character",sep="\n")
temp <- master[grep("results_folder <-",master)]
master[grep("results_folder <-",master)] <- paste0(substr(temp,1,nchar(temp)-1),"_minus_",sens_var,"_",sens_per,substr(temp,nchar(temp),1000))
master[grep("user_input.R",master)] <- gsub("user_input.R","user_input_minus.R",master[grep("user_input.R",master)] )
write(master,"master_script_minus.R")

# create new folders
new_dir <- c(master[grep("results_folder <-",master)],
             "if(!file.exists(results_folder)) dir.create(results_folder)",
             "if(!file.exists(paste0(results_folder,\"/shapefiles\"))) dir.create(paste0(results_folder,\"/shapefiles\"))")

write(new_dir,"new_dir_minus.R")
source("new_dir_minus.R")

# new protection shapefiles or copy?
# changes in cell_size or MPA_coverage will REQUIRE new protection scenario shapefiles
if(sens_var=="cell_size"|sens_var=="MPA_coverage"){
    master <- scan("user_input.R","character",sep="\n")
    master[grep("^protect_scen_new <-",master)] <- "protect_scen_new <- TRUE"
}

if(reg_shp_copy){
    source("new_dir.R")
    shps <- list.files(paste0(results_folder,"/shapefiles"),".")
    shps_dir <- list.files(paste0(results_folder,"/shapefiles"),".",full.names = TRUE)
    
    source("new_dir_plus.R")
    file.copy(shps_dir,paste0(results_folder,"/shapefiles/",shps))
    
    source("new_dir_minus.R")
    file.copy(shps_dir,paste0(results_folder,"/shapefiles/",shps))
}


file.remove("new_dir.R")
file.remove("new_dir_plus.R")
file.remove("new_dir_minus.R")

# edit plus and minus versions of user_input.R
# plus
master <- scan("user_input.R","character",sep="\n")
temp <- master[grep(paste0("^",sens_var),master)]
master[grep(paste0("^",sens_var),master)] <- paste0(temp,"*",1+sens_per)
write(master,"user_input_plus.R")

# minus
master <- scan("user_input.R","character",sep="\n")
temp <- master[grep(paste0("^",sens_var),master)]
master[grep(paste0("^",sens_var),master)] <- paste0(temp,"*",1-sens_per)
write(master,"user_input_minus.R")



#### Sensitivity Analysis Calculation ####
system("Rscript master_script.R",wait = FALSE,invisible=FALSE)
system("Rscript master_script_plus.R",wait = FALSE,invisible=FALSE)
system("Rscript master_script_minus.R",wait = FALSE,invisible=FALSE)
