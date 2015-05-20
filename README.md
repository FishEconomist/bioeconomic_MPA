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
_dt_| The time step in years| user defined
_cell_size_| The height and width of the model cell size in km| user defined
_proj_| The map projection to be used, must comply with [PROJ.4 CRS](http://www.inside-r.org/packages/cran/sp/docs/CRS)| user defined
_n_| The number of virtual fish per cell at initialization | user defined
_virtual_fish_ratio_| The virtual fish:real fish ratio (e.g. if virtual_fish_ratio=10^6, then 1 virtual fish is 'worth' 10^6 real fish)| user defined
_Linf_mean_| Von Bertalanffy growth model parameter | (Knickle and Rose 2013)
_Linf_SD_ | Von Bertalanffy growth model parameter | (Knickle and Rose 2013)
_k_mean_| Von Bertalanffy growth model parameters| (Knickle and Rose 2013)
_k_SD_| Von Bertalanffy growth model parameters| (Knickle and Rose 2013)
_t0_| Von Bertalanffy growth model parameters | (Knickle and Rose 2013)

