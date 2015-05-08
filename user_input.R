#### User input ####
# total model run time in years (e.g. 2001:2100 would be 100 years)
time <- 2001:2003
# time step in years
dt <- 1
# cell size in km
cell_size <- 20
# default projection
proj  <- "+proj=lcc +lat_1=40 +lat_2=70 +lat_0=-71.3 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
# target protection level in proportion (e.g. 0.2 is 20% protection)
MPA_coverage <- 0.20
# coastal:marine ratio (e.g. CtoM <- 0.4 is 60% marine and 40% coastal in terms of area)
CtoM <- 0.0009422693
# fixed distance for setting MPA distance in km
fixdist <- 200
# create new protection scenarios? (TRUE creates new maps, but is slow. FALSE uses previously saved maps)
protect_scen_new <- FALSE
# initial number of fish per cell
n <- 100
# virtual fish:real fish ratio (e.g. if virtual_fish_ratio=10^6, then 1 virtual fish is 'worth' 10^6 real fish)
virtual_fish_ratio <- 10^6
# #Von Bertalanffy growth model parameters (Knickle and Rose 2013)
#Lt = Linf * {1-exp(-k*(t-t0))}, where Lt is length (cm) at age t (year), Linf is the asymptotic length (cm), k is the VB growth coefficient (1/year), and t0 is the x intercept (year). Linf = 112.03 (95% CI = 10.46). k = 0.13 (95% CI = 0.021). t0 = 0.18).
Linf_mean <- 112.03
Linf_SD <- 10.46/1.96
k_mean <- 0.13
k_SD <- 0.021/1.96
t0 <- 0.18

# minimum age at maturity
min_age_mat <- 2
# Fecundity (size dependent). 0.5 million eggs per kg of female
fecundity <- 0.5*10^6


# natural mortality
M <- 0.4
# larval mortality
lM <- 0.999999
# larval dispersal kernels are assumed to be exponential, e_fold_larvae is the e folding scale in km (the distance at which there will be fewer settlers by a factor of e). We assume that scale to be 2cm/s*90d (avg current velocity * PLD)
e_fold_larvae <- 2/100000*60*60*24*90
# adult dispersal kernels are also assumed to be exponential, e_fold_adult (in km) was calculated from data in Lawson & Rose 2000
e_fold_adult  <- 74.139
