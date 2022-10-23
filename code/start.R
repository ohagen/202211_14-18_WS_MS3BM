library(gen3sis)

# example course

datapath <- system.file(file.path("extdata", "WorldCenter"), package="gen3sis")

conf_loc <- file.path(datapath, "config/config_worldcenter.R")
land_loc <- file.path(datapath, "landscape")
out_loc <- tempdir()

sim <- run_simulation(config = conf_loc,
                      landscape = land_loc, 
                      output_directory = out_loc)

# less prints and outputs....
# sim <- run_simulation(config = conf_loc,
#                       landscape = land_loc, 
#                       output_directory = out_loc, 
#                       call_observer = 1,
#                       verbose = 0)



### Example South America higher resolution

wd <- "C:/Users/am92guke/Documents/iDiv/Teaching/Macroecology & Macroevolution course/ge3sis"

sim <- run_simulation(config = file.path(wd, "SA/config/config_southamerica.R"),
                      landscape = file.path(wd, "SA/landscape"), output_directory = out_loc, call_observer = "all",
                      verbose = 1)
