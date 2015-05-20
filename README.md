# Bioeconomic MPA network design - R toolkit
Thank you for your interest in the R toolkit for the bioeconomic evaluation of MPA network design! If you haven't already done so, please refer to the manuscript describing the model results (insert link to manuscript once published).

## Usage
If you would like to apply this toolkit to your own scenario, please fork this repository and modify as needed. We kindly ask that you cite both the original manuscript and this repository in any resulting publication. We recognize that there are some aspects of the model the could be closer to reality and we look forward to seeing how you would improve it. To make changes, we recommend starting with user_input.R, functions.R, or changing one or more of the sub-modules.

If you would like to replicate our results, download the repository as is and run the simulations by running the code in master.R. This script will source the user defined parameters (user_input.R), the custom MPA toolkit functions (functions.R), and the various MPA toolkit sub-modules.

**Please note:** This toolkit requires the installation of R packages before use (dplyr, Grid2Polygons, maptools, raster, rgdal*, rgeos, sp)
*installation of the rgdal package is notoriously difficult, please see [here](http://stackoverflow.com/questions/15248815/rgdal-package-installation) and [here](http://cran.r-project.org/web/packages/rgdal/index.html) for further information should you have any problems.

## Parameterization
Such a complex model requires many parameters to function properly. Below we have included a list of parameters and user inputs contained in the model.

Variable | Value | Definition | Source
:----------:|:----:|:--------------:|:----------:
_time_| 2001:2100| The time over which the model is run (e.g. 2001 to 2100)| user defined
_dt_| 1| The time step in years| user defined
_cell_size_| 20| The height and width of the model cell size in km| user defined
_proj_| "+proj=lcc ... +units=m +no_defs"|The map projection to be used, must comply with [PROJ.4 CRS](http://www.inside-r.org/packages/cran/sp/docs/CRS)| user defined
_n_| 10| The number of virtual fish per cell at initialization | user defined
_virtual_fish_ratio_| 5000| The virtual fish:real fish ratio (e.g. if virtual_fish_ratio=10^6, then 1 virtual fish is 'worth' 10^6 real fish)| user defined
_Linf_mean_|112.03| Von Bertalanffy growth model parameter - mean asymptotic length (cm) | Knickle and Rose 2013
_Linf_SD_ |5.34 |Von Bertalanffy growth model parameter - standard deviation asymptotic length (cm) | Knickle and Rose 2013
_k_mean_| 0.13|Von Bertalanffy growth model parameters - mean growth coefficient (1/year)| Knickle and Rose 2013
_k_SD_|0.1 |Von Bertalanffy growth model parameters - standard deviation growth coefficient (1/year)| Knickle and Rose 2013
_t0_|0.18|Von Bertalanffy growth model parameters - x intercept (year) | Knickle and Rose 2013
_l_to_w_int_| 1| The time step in years| Knickle and Rose 2013
_l_to_w_power_| 1| The time step in years| Knickle and Rose 2013
_min_age_mat_| 5 | Age at sexual maturity | REF
_fecundity_| 0.5*10^6 | Size dependent fecundity (eggs per kg of female)| REF
_M_| 0.4| Natural adult mortality | Mountain et al. 2008
_lM_| 0.9999| Natural larval mortality | Mountain et al. 2008
_CC_| 34.49 | The mean carrying capacity for Canadian cod stocks (t of virtual fish/cell)| Myers et al. 2001
_CC_sd_| 30.94 | The standard deviation of carrying capacity for Canadian cod stocks (t of virtual fish/cell)| Myers et al. 2001
_e_fold_larvae_| 155.52 | The e-folding scale for larvae in km (the distance at which there will be fewer settlers by a factor of e). Estimated from linear extrapolation of 2 cm/s over 90 d planktonic larval duration| Brander and Hurley 1992
_e_fold_adult_| 74.139| The e-folding scale for larvae in km (the distance at which there will be fewer settlers by a factor of e). Estimated from dispersal data | Lawson & Rose 2000
_fish_communities_| SpatialPolygonsDataFrame | The spatial distribution of fish_licenses for shore distance calculation in effort calculation. It can be spatial points or spatial polygons data frame (sp package)| raster package
_fish_communities2_| SpatialPolygonsDataFrame | Same as above, but polygons are simplified to speed up calculation| user defined
_fish_licenses_| c(866, 4714, 3002, 879, 963)| Number of licenses per region in fish_communities | [DFO](http://www.dfo-mpo.gc.ca/stats/commercial/licences-permis/licences-permis-atl-eng.htm)
_FMSY_| 0.28| Fisheries mortality at Maximum Sustainable Yield| Mountain et al. 2008
_FMSY_buffer_| 2/3 | quota set to fraction of FMSY as per precautionary principle | user defined
_sampling_pop_| 0.0005| percent of population measured for biomass estimation | user defined
_min_size_| 38| Minimum size fish caught by nets (cm)| Feekings et al. 2013
_MPA_coverage_| 0.2 | Target protection level in proportion (e.g. 0.2 is 20% protection)| REF
_CtoM_| 0.0009 | Coastal:marine ratio for MPAs (e.g. CtoM <- 0.4 is 60% marine and 40% coastal in terms of area) | IUCN and UNEP-WCMC, 2015
_fixdist_| 75 | The fixed distance for setting MPA distance in km in fixed distance scenario. Set to approximately the mean adult dispersal distance | Lawson & Rose 2000
_protect_scen_new_| LOGICAL | Create new protection scenarios? (TRUE creates new maps, but is slow. FALSE uses previously saved maps). Very computationally expensive if TRUE| user defined
_dt_| 1| The time step in years| user defined
_dt_| 1| The time step in years| user defined
_dt_| 1| The time step in years| user defined
_dt_| 1| The time step in years| user defined
_dt_| 1| The time step in years| user defined
_dt_| 1| The time step in years| user defined
_dt_| 1| The time step in years| user defined
_dt_| 1| The time step in years| user defined
_dt_| 1| The time step in years| user defined
_dt_| 1| The time step in years| user defined

##Equations
#### Von Bertalanffy growth model

    Lt  <-  Linf * (1-exp((-k)*(t-t0)))
where `Lt` is length (cm) at age `t` (year), `Linf` is the asymptotic length (cm), `k` is the Von Bertalanffy growth coefficient (1/year), and t0 is the x intercept (year). Linf and k are randomly generated from a normal distribution (see table above). 
#### Length-weight relationship
    fish$weight <- l_to_w_int * fish$length^l_to_w_power
 where `fish$length` is length (cm) and `fish$weight` is the weight (kg)