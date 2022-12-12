## METADATA ===============================================================
## Description: This script sources the working directory and any aditional
## variables you decide to declare during multiple script.
## 
## R version: 4.0.2 for Windows
## Date: 2022-11-10 16:34:44
## License: GPL3
## Author: Hagen (oskar@hagen.bio), Skeels and Rosenbaum 
##=======================================================================##

# Here we have a list of packages we want to install
lop <- c("gen3sis", "raster") #list.of.packages
# And finally we install the missing packages, including their dependency.
new.packages <- lop[!(lop %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# After the installation process completes, we load all packages.
sapply(lop,require,character=TRUE)

# set working directory -----------
setwd(getwd())

### [] create variables for directory relative location -------
# dd= data directory
dd <- "../data"
# dd <- tempdir()
# od= output directory
od <- "../../outputs"

# some things we want to see the code, but avoid running by mistake
cat(
"# During this prac we won't run the code in the run_slow sections (will take too long!) \n
# but so you get an idea of the code, please take a look and see whats going on in these sections \n
# keep this as FALSE  \n")
run_slow <- FALSE




# # dp_wc = datapath World Center
# dp_wc <- system.file(file.path("extdata", "WorldCenter"), package="gen3sis")
# 
# conf_loc <- file.path(dp_wc, "config/config_worldcenter.R")
# land_loc <- file.path(dp_wc, "landscape")
# out_loc <- tempdir()
# 
# sim <- run_simulation(config = conf_loc,
#                       landscape = land_loc, 
#                       output_directory = out_loc)


# if running as source as local Job
# simR <- get("sim", envir = start_results)


# library(callr)
# 
# # example
# rp <- callr::r_bg(function() "x")
# (rp)

# ###################################################################
# 
# simb <- callr::r_bg(function(){gen3sis::run_simulation(config = conf_loc,
#                                                 landscape = land_loc, 
#                                                 output_directory = out_loc)})



# less prints and outputs....
# sim <- run_simulation(config = conf_loc,
#                       landscape = land_loc, 
#                       output_directory = out_loc, 
#                       call_observer = 1,
#                       verbose = 0)



### Example South America higher resolution
# 
# wd <- "C:/Users/am92guke/Documents/iDiv/Teaching/Macroecology & Macroevolution course/ge3sis"
# 
# sim <- run_simulation(config = file.path(wd, "SA/config/config_southamerica.R"), 
#                       landscape = file.path(wd, "SA/landscape"), output_directory = out_loc, call_observer = "all", 
#                       verbose = 1)
