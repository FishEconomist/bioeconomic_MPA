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
_spinup_| 10| The number of years the model runs to generate a realistic starting population before "time". Results are not saved during spin-up time| user defined
_tot_time_| 1991:2100| The time over which the model is run including spin-up time | user defined
_dt_| 1| The time step in years| user defined
_cell_size_| 20| The height and width of the model cell size in km| user defined
_proj_| "+proj=lcc ... +units=m +no_defs"|The map projection to be used, must comply with [PROJ.4 CRS](http://www.inside-r.org/packages/cran/sp/docs/CRS)| user defined
_n_| 20 | The number of virtual fish per cell at initialization | user defined
_virtual_fish_ratio_| 10000| The virtual fish:real fish ratio (e.g. if virtual_fish_ratio=10^6, then 1 virtual fish is 'worth' 10^6 real fish)| user defined
_Linf_mean_|112.03| Von Bertalanffy growth model parameter - mean asymptotic length (cm) | Knickle and Rose 2013
_Linf_SD_ |5.34 |Von Bertalanffy growth model parameter - standard deviation asymptotic length (cm) | Knickle and Rose 2013
_k_mean_| 0.13|Von Bertalanffy growth model parameters - mean growth coefficient (1/year)| Knickle and Rose 2013
_k_SD_|0.1 |Von Bertalanffy growth model parameters - standard deviation growth coefficient (1/year)| Knickle and Rose 2013
_t0_|0.18|Von Bertalanffy growth model parameters - x intercept (year) | Knickle and Rose 2013
_l_to_w_int_| 1| The time step in years| Knickle and Rose 2013
_l_to_w_power_| 1| The time step in years| Knickle and Rose 2013
_age_mat_steepness_| 2.5 | Steepness of the logitic curve for age at sexual maturity. Fish begin maturing at 2 y, 50% at 4 y and all are mature at 6y | user defined
_age_mat_sigmoid_| 4 | Sigmoid of the logitic curve for age at sexual maturity. Fish begin maturing at 2 y, 50% at 4 y and all are mature at 6y | user defined
_fecundity_| 0.5*10^6 | Size dependent fecundity (eggs per kg of female)| REF
_M_| 0.4| Natural adult mortality | Mountain et al. 2008
_lM_| rbeta(10000,1000,1.2) | Natural larval mortality, the model selects a random mortality rate from a beta distribution with α=1000 and β=1.2. This is designed to match a mean larval mortality of 99.88% with a range of 98.98-99.99% | Mountain et al. 2008
_CC_| 431 | The mean carrying capacity for Canadian cod stocks (kg of fish/km^2)| Myers et al. 2001
_CC_sd_| 387 | The standard deviation of carrying capacity for Canadian cod stocks (kg of fish/km^2)| Myers et al. 2001
_e_fold_larvae_| 155.52 | The e-folding scale for larvae in km (the distance at which there will be fewer settlers by a factor of e). Estimated from linear extrapolation of 2 cm/s over 90 d planktonic larval duration| Brander and Hurley 1992
_e_fold_adult_| 74.139| The e-folding scale for larvae in km (the distance at which there will be fewer settlers by a factor of e). Estimated from dispersal data | Lawson & Rose 2000
_min_size_migration_| cm| Minimium size for adult migration (cm) | Lawson & Rose 2000
_fish_communities_| SpatialPolygonsDataFrame | The spatial distribution of fish_licenses for shore distance calculation in effort calculation. It can be spatial points or spatial polygons data frame (sp package)| raster package
_fish_communities2_| SpatialPolygonsDataFrame | Same as above, but polygons are simplified to speed up calculation| user defined
_fish_licenses_| c(866, 4714, 3002, 879, 963)| Number of licenses per region in fish_communities | [DFO](http://www.dfo-mpo.gc.ca/stats/commercial/licences-permis/licences-permis-atl-eng.htm)
_FMSY_| 0.28| Fisheries mortality at Maximum Sustainable Yield| Mountain et al. 2008
_FMSY_buffer_| 2/3 | quota set to fraction of FMSY as per precautionary principle | user defined
_sampling_pop_| 0.001| percent of population measured for biomass estimation | user defined
_biomass_est_n_years_| 5| number if years averaged for biomass estimation | user defined
_min_size_| 38| Minimum size fish caught by nets (cm)| Feekings et al. 2013
_MPA_coverage_| 0.2 | Target protection level in proportion (e.g. 0.2 is 20% protection)| REF
_CtoM_| 0.0009 | Coastal:marine ratio for MPAs (e.g. CtoM <- 0.4 is 60% marine and 40% coastal in terms of area) | IUCN and UNEP-WCMC, 2015
_fixdist_| 75 | The fixed distance for setting MPA distance in km in fixed distance scenario. Set to approximately the mean adult dispersal distance | Lawson & Rose 2000
_protect_scen_new_| LOGICAL | Create new protection scenarios? (TRUE creates new maps, but is slow. FALSE uses previously saved maps). Very computationally expensive if TRUE| user defined
_protect_scen_| 1| The time step in years| user defined
_protect_scen_colour_| 1| The time step in years| user defined
_country_name_| 1| The time step in years| user defined
_MPAs_coast_|SpatialPolygonsDataFrame | COASTAL Marine Protected Areas in Atlantic Canada| IUCN and UNEP-WCMC, 2015
_MPAs_mar_|SpatialPolygonsDataFrame | MARINE Marine Protected Areas in Atlantic Canada| IUCN and UNEP-WCMC, 2015
_MPAs_AOI_|SpatialPolygonsDataFrame | Areas of Interest identified by DFO as potential areas for future MPAs. Not used in the latest version of the toolkit| [DFO](http://www.dfo-mpo.gc.ca/oceans/marineareas-zonesmarines/mpa-zpm/index-eng.htm)
_EEZ_|SpatialPolygonsDataFrame | Canadian Exclusive Economic Zone [Atlantic part]. The toolkit removes the "Canadian part of the Davis Strait" because it is not important for cod| [Maritime Boundaries Geodatabase](http://www.marineregions.org/gazetteer.php?p=details&id=8777)
_Habitats_| SpatialPolygonsDataFrame | Spatial boundaries of the suitable cod habitat. Shapefile generated by georeferencing figure 2 | Lough 2004
_Breeding_| SpatialPolygonsDataFrame | Spatial boundaries of the cod breeding habitat. Shapefile generated by georeferencing figure 2| Lough 2004
_minimum_fishable_biomass_| 1000 | Prevent computational errors from occurring when fisherman are trying to fish with insufficient fish. Biomass below this threshold will impose a moratorium on the fishery until the fishery recovers| user defined
_fish_operating_cost_ratio_| 0.53| The ratio of operating cost that are variable with distance (labour and fuel) to the total operating costs for the Mixed Fishery | [DFO 2007](http://publications.gc.ca/site/eng/429700/publication.html)
_Status_quo_profitability_| 1.58| The ratio of the landed value of the catch to the total operating costs for the Mixed Fishery| [DFO 2007](http://publications.gc.ca/site/eng/429700/publication.html)
_fish_landed_value_| 1240 | The landed value of cod USD per t| [DFO](http://www.dfo-mpo.gc.ca/stats/commercial/sea-maritimes-eng.htm)
_MPA_maintenance_cost_| 2698| The median maintenance cost for MPAs (USD/km^2/year) | Balmford 2004
_SDR_| c(0.015,0.03,0.06) | Values for the social discount rate | Keller et al. 2008

##Equations
#### Von Bertalanffy growth model
    Lt  <-  Linf * (1-exp((-k)*(t-t0)))
where `Lt` is length (cm) at age `t` (year), `Linf` is the asymptotic length (cm), `k` is the Von Bertalanffy growth coefficient (1/year), and t0 is the x intercept (year). Linf and k are randomly generated from a normal distribution (see table above). 
#### Length-weight relationship
    fish$weight <- l_to_w_int * fish$length^l_to_w_power
 where `fish$length` is length (cm) and `fish$weight` is the weight (kg).

#### Beverton-Holt recruitment model
    R[i] <- (larvae[i]+fish[i])/fish[i]
    new_total[i] <- R*fish[i]/(1+fish[i]/CC[i])
    new_recruits[i] <- new_total[i]-fish[i]
where `R` is the population growth rate for cell `i`, `larvae[i]` is the number of competent larvae delivered to cell `i`, `fish[i]` is the number of adult fish living in cell `i`, `new_total[i]` is the population size for next year (if no adults were to die), `new_total[i]` is the number if new recruits to be added to the population next year.
#### Effort
    effort <- (1-relative_biomass[i])*relative_distance[i]
where the `relative_biomass` is the biomass in cell `i` divided by the maximum biomass in the model domain and `relative_distance` is the actual distance to shore for cell `i` divided by the mean distance to shore for all the cells in the model domain.