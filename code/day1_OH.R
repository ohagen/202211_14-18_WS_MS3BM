## METADATA ===============================================================
## Description: MS3BM course Day 1. Setting up files  and system.
## In this script we will learn how to:
## 1) set up a machine to run gen3sis simulations
## 2) get comfortable with gen3sis objects (i.e. configs and landscapes)
## 3) perform simulations
## 4) perform simple analysis of gen3sis outputs
## 5) modify and creatr configs and landscapes
## 6) troubleshoot gen3sis
##
## R version: 4.0.2 for Windows
## Date: 2022-10-20 16:59:30
## License: GPL3
## Author: Hagen (oskar@hagen.bio), Skeels and Rosenbaum 
##=======================================================================##

# .  ------------------
# PREAMBLE ------------

# Before you dive in, make sure you have R installed. If you use Rstudio, you 
# can make use of the sections on the side. 
# To show document outline use [Ctrl+Shift+O], make use of the help function in R. 
# This scrip cover all of Day 1. This means it includes the parts:
# 
# 1. THE GEN3SIS R-PACKAGE
# 2. MODIFY LANDSCAPES AND ECO-EVOLUTIONARY RULES
# 3. LEARNING HOW TO TROUBLESHOOT
# 4. EXERCISE YOUR SKILLS

# .  ----------------------------------------
#  [ 1. THE GEN3SIS R-PACKAGE ]      ########
# .  ----------------------------------------

### Setup ------------

# install gen3sis

install.packages("gen3sis")

### [] download data --------


### [] store it into working directory ------------

### [] source.R ------------
source("./source.R")
### [] Reflection --------
### [] Questions: Sourcing, WD, relative and absolute paths clear?
# What does it means, if the bellow does not work? ------------
source(file.path(getwd(), "source.R"))
### [] Questions: Did you noticed the variables in the global environment?

#get version of gen3sis. Who is running the dev. version?
print(paste("gen3sis version:", packageVersion("gen3sis")))

### attention! Download the data If using South America!!!
# datapath <- system.file(file.path("extdata", "SouthAmerica"), package = "gen3sis")
# print(datapath)
# list.dirs(datapath, recursive = F)
# note that dist distances are not calculated from the data of the package

### Input data ------------

list.dirs(file.path(dd), recursive = F)

### [] open explorer --------
### [] look at the input data. Output Later!  --------

#### landscape ---------

#### [] look at metadata -----

#### [] example of a good metadata ----

# lp = landscapes path
lc <- readRDS(file.path(dd,"landscapes", "SA_1d", "landscapes.rds"))
class(lc)
names(lc)
dim(lc$temp)
lc$temp[1:10, 1:10]
colnames(lc$temp)


plot(rasterFromXYZ(lc$temp[ ,c("x", "y", "0")]))
plot(rasterFromXYZ(lc$temp[,c("x", "y", "65")]))

# overlay
rl0 <-rasterFromXYZ(lc$temp[,c("x", "y", "0")])
rl65 <- rasterFromXYZ(lc$temp[,c("x", "y", "65")])

plot(rl65, col=rgb(1,0,0))
plot(rl0, col=rgb(0,0,1,0.5,1), add=T)


### [] Exercise (10min) -----------------
# create a way to visualize a gen3sis input.
# If possible think of abstracting for any x,y,z1,z2,z3... temporal data-frame.
# Hint: Make a function! ;) 

# ANSWER 
{
  # animation
  for (ti in as.character(65:0)) {
    # ti <- "65"
    ri <- rasterFromXYZ(lc$temp[,c("x", "y", ti)])
    plot(ri, main=paste(ti, "Ma"))
    Sys.sleep(1)
  }
  
  # function
  # now as a function with temporal ordering
  plots <-  function(df, reverse=T){
    #df <- lc$temp
    times <- names(df)[!names(df)%in%c("x", "y")]
    if (reverse){
      times <- rev(times)
    }
    for (ti in times){
      ri <- rasterFromXYZ(df[,c("x", "y", ti)])
      plot(ri, main=paste(ti, "Ma"))
      Sys.sleep(1)
    }
  }
  
  plots(lc$arid)
}



#### config -------------

#### [] open config file --------

?create_input_config

empt_conf <- create_input_config()

empt_conf

##### [] have a look at the structure -----
names(empt_conf$gen3sis)
names(empt_conf$gen3sis$general)
#### [] open config file --------

temp <- create_input_config(config_file = file.path(dd,"/configs/SA_1d/config_southamerica.R"))
names(temp$gen3sis)
names(temp$gen3sis$general)
temp$gen3sis$general$start_time
empt_conf$gen3sis$general$start_time
# remove this from memory call
rm(temp)




### Exercise (5min) -----------------
# think of 3 ways to impact on extinction rates?



##### break ##### 
#Everybody good? Found files?


### Run --------------- 
# one simple example simulation
# do not run!
sim <- run_simulation(config = file.path(dd, "configs/SA_1d/config_southamerica.R"), 
                      landscape = file.path(dd, "landscapes/SA_1d"), output_directory = od, call_observer = "all", 
                      verbose = 1)
  
names(sim)
# sim <- readRDS(file.choose())


#### Visualize --------------- 
#the outputs
plot_summary(sim)

# using raster package
plot(rasterFromXYZ(sim$summary$`richness-final`))

plot(rasterFromXYZ(sim$summary$`richness-final`), col=gen3sis::color_richness(10))




##### species and landscapes objects --------------- 

# [] look at the output directory  -----
list.files(od)
# note that directory name is registered according to config name
list.dirs(file.path(od,"config_southamerica"))

# plot diversity at 3 time slices
timesteps <- c(40, 20, 0)
par(mfrow = c(1, 3))
for (i in timesteps) {
  landscape_i <- readRDS(file.path(od, paste0("config_southamerica/landscapes/landscape_t_", 
                                                    i, ".rds")))
  species_i <- readRDS(file.path(od, paste0("config_southamerica/species/species_t_", 
                                                  i, ".rds")))
  plot_richness(species_i, landscape_i)
}
dev.off()

### Analyze --------------- 

#the outputs
#get the input landscape
landscapes <- readRDS(file.path(dd, "landscapes/SA_1d/landscapes.rds"))  
#get landscape at last time-step
landscape_t0 <- as.data.frame(cbind(landscapes$temp[, 1:2], 
                                    temp = landscapes$temp[,3], 
                                    arid = landscapes$arid[, 3], 
                                    area = landscapes$area[, 3]))  
#add richness to the dataframe
landscape_t0 <- cbind(landscape_t0, rich = sim$summary$`richness-final`[, 3])  
landscape_t0 <- na.omit(landscape_t0)


#For this, we first fit a univariate generalized linear model between richness and temperature.
glm.uni <- glm(rich ~ poly(temp, 2), data = landscape_t0, family = poisson)
cor(landscape_t0$temp, landscape_t0$rich)


# prepare data with temperature and predicted richness from our model
data_plot <- data.frame(cbind(landscape_t0$temp, predict(glm.uni, type = "response")))
# sort data for plotting and ommit NA's
data_plot <- na.omit(data_plot[order(data_plot[, 1], decreasing = FALSE), ])
# get the number of observations
n <- paste0("observations (n = ", length(landscape_t0$rich), ")")
# plot model curve
plot(data_plot[, 1], data_plot[, 2], xlab = "Temperature [?C]", 
     ylab = expression(paste(alpha," richness")), frame.plot = F, type = "l", 
     col = "red", lwd = 2, xlim = c(min(landscape_t0$temp), 
                                                                                                                                                                  max(landscape_t0$temp)), ylim = c(min(landscape_t0$rich), max(landscape_t0$rich)))
# add observed points
points(landscape_t0$temp, landscape_t0$rich, col = rgb(0.5, 0.5, 0.5, alpha = 0.4), 
       pch = 16)
# add legend
legend(-20, 30, col = c(rgb(0.5, 0.5, 0.5, 0.4), "red"), legend = c(n, "model fit"), 
       pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2), bty = "n")



# phylogenetic diversity 
library(ape)
phy <- try(read.nexus(file.path(od, "config_southamerica","phy.nex")))
#visualize
plot(phy)

# .  ----------------------------------------------------------------------------
####### [ 2. MODIFY LANDSCAPES AND ECO-EVOLUTIONARY RULES I and II ]   ##########
# .  ----------------------------------------------------------------------------

# [] modify landscape -----

lpmod <- lc

for (ti in as.character(0:65)){
  slice <- lpmod$temp[,ti]
  lpmod$temp[,ti] <- slice*rnorm(n=length(slice))
}

#see all times...
plots(lpmod$temp)

#compare...
# get vector time 0 ...
# dt = diff. temp
dt_t0 <- data.frame(mod=lpmod$temp[,3], lc=lc$temp[,3])
plot(dt_t0$lc)
plot(dt_t0$mod)
diff <- abs(dt_t0$lc-dt_t0$mod)
plot(rasterFromXYZ(cbind(lc$temp[,c(1,2)],diff)))

# [] Reflection -----
# If we would modify the habitable sites, do you need to compile a new landscape? 

# Creating landscapes (gen3sis input) -----

?create_input_landscape


??gen3sis

# [] the gen3sis vignette ----

# [] examples landscapes  ----


###  Paleoclim NA ----
if(run_slow==T){
  # do as admin
  install.packages("remotes")
  remotes::install_github("EvolEcolGroup/pastclim")
  
  library(pastclim)
  library(terra)
  
  # making name as variable, for wrapping as a future function....
  lcname <- "NA_790k_10k"
  
  
  help("Krapp2021")
  pastclim:::get_dataset_info(dataset="Krapp2021")
  get_vars_for_dataset(dataset = "Krapp2021")
  
  # download
  ### THIS IS SLOW !
  download_dataset(dataset = "Krapp2021", bio_variables = c("bio01", "bio04", "bio12", "npp", "biome", "altitude", "rugosity"))
  # see https://www.nature.com/articles/s41597-021-01009-3/tables/2 for reference
  # time series is constant!
  t_steps <- get_time_steps(dataset = "Krapp2021")
  
  plot(t_steps) # [] what for this plot? -----
  
  # make timesteps at 10k 
  selected_t_steps <- seq(-790000, 0, 10000)
  
  ## this is slow
  ?region_series
  srd <- region_series(
    bio_variables = c("bio01", "bio04", "bio12", "npp", "biome", "altitude", "rugosity"),
    time_bp = selected_t_steps, #getting for every time_step
    dataset = "Krapp2021",
    ext = region_extent$N_America
  )
  class(srd)
  nlyr(srd)
  
  #plot time series of a given variable:
  terra::plot(srd$bio01)
  
  ###### now we go from the terra format to raster and then gen3sis..... 
  
  # create raster bricks---
  
  #create temp dir
  temp_dir <- "c:/temp/temp_rasters"
  dir.create(temp_dir)
  
  #loop over variables names..
  for (var_i in names(srd)){
    #var_i <- names(srd)[1]
    nts <- nlyr(srd[var_i])
    rstack <- NULL
    for (c_i in 1:nts){
      # [] Note aggregation -----
      rstack[[c_i]] <-aggregate(raster(srd[var_i][[(nts+1)-c_i]]), fact=2)
    }
    rstack <- brick(rstack)
    writeRaster(rstack, filename = file.path(temp_dir, paste0(var_i, ".grd")), overwrite=TRUE)
  }
  
  # load raster bricks
  #list all temp raster bricks
  bf <- list.files(temp_dir, pattern=".grd")
  #load all temp raster bricks
  b <- NULL
  for (i in 1:length(bf)){
    b[[i]] <- brick(file.path(temp_dir, bf[i]))
  }
  #prepare list
  lsn <- lapply(ls, function(x){x <- NULL})
  # set names
  names(lsn) <- gsub(".grd", "", bf)
  #attribute to list
  for (i in 1:length(lsn)){
    # i <- 1
    for (il in 1:nlayers(b[[i]])) { #over time-steps
      lsn[[i]] <- c(lsn[[i]], b[[i]][[il]]) # b[[>ENV<]][[>TIME<]]
    }
  }
  
  # set names
  names(lsn) <- gsub(".grd", "", bf)
  
  
  #### FUNCTIONS ----------
  cost_function_water <- function(source, habitable_src, dest, habitable_dest) {
    if (!all(habitable_src, habitable_dest)) {
      return(2/1000)
    } else {
      return(1/1000)
    }
  }
  
  string_time_step <- paste0(formatC(abs(rev(selected_t_steps))/1000, width=3, flag="0", digits=0, format="f"),"kya" )
  
  create_input_landscape(landscapes = lsn, cost_function = cost_function_water, directions=8,
                         output_directory = file.path(temp_dir, lcname), timesteps = string_time_step, 
                         calculate_full_distance_matrices = T, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84", verbose=T)
  
  # load landscapes
  lc <- readRDS(file.path(temp_dir, lcname, "landscapes.rds"))
  # load distance
  dmf0 <- readRDS(file.path(temp_dir, lcname, "distances_full", "distances_full_0.rds"))
  # check distribution
  hist(dmf0[])
  # check on landscape
  randompoints <- sample(colnames(dmf0), 2)
  # randompoints <- c("2247","5863")
  # distance
  dmf0[randompoints[1],randompoints[2]]
  plot(rasterFromXYZ(lc$altitude[,c(1,2,3)]))
  points(lc$altitude[randompoints,"x"], lc$altitude[randompoints,"y"], cex=2)

  # [] show check in real life
}


###  Functional Islands ----
{
  ## [] Go to ex_island.R
}

# Configs -----

# [] modify config -----

conf <- create_input_config()

conf <- create_input_config(file.path(dd, "configs/SA_1d/config_southamerica.R"))

#before we run
sim <- run_simulation(config = file.path(dd, "configs/SA_1d/config_southamerica.R"), 
                      landscape = file.path(dd, "landscapes/SA_1d"), output_directory = od, call_observer = 1, 
                      verbose = 0)
# Try to cancel the run. Try to stop using [Esc]


#same as running. Lets add verbose
sim <- run_simulation(config = conf, 
                      landscape = file.path(dd, "landscapes/SA_1d"), output_directory = od, call_observer = 1, 
                      verbose = 1)
# press [Esc to cancel]

#lets make it faster, only for exemplification
conf$gen3sis$initialization$create_ancestor_species

conf_n <- conf

# spread ancestor over around the world
conf_n$gen3sis$initialization$create_ancestor_species <- function(landscape, config) {
  range <- c(-180, 180, -90, 90)
  co <- landscape$coordinates
  selection <- co[, "x"] >= range[1] &
    co[, "x"] <= range[2] &
    co[, "y"] >= range[3] &
    co[, "y"] <= range[4]
  initial_cells <- rownames(co)[selection]
  new_species <- create_species(initial_cells, config)
  #set local adaptation to max optimal temp equals local temp
  new_species$traits[ , "temp"] <- landscape$environment[,"temp"]
  new_species$traits[ , "dispersal"] <- 1
  return(list(new_species))
}


sim <- run_simulation(config = conf_n, 
                      landscape = file.path(dd, "landscapes/SA_1d"), output_directory = od, 
                      verbose = 1)

# [] Look at the starting_ranges.pdf and the respective saving location  ----------



# .  --------------------------------------------
# [ 3. LEARNING HOW TO TROUBLESHOOT ] ###########
# .  --------------------------------------------

# Lets say I want it to start in a specific region

conf_n$gen3sis$initialization$create_ancestor_species <- function(landscape, config) {
  range <- c(-50, -45, -30, 20)
  co <- landscape$coordinates
  selection <- co[, "x"] >= range[1] &
    co[, "x"] <= range[2] &
    co[, "y"] >= range[3] &
    co[, "y"] <= range[4]
  initial_cells <- rownames(co)[selection]
  new_species <- create_species(initial_cells, config)
  #set local adaptation to max optimal temp equals local temp
  new_species$traits[ , "temp"] <- landscape$environment[,"temp"]
  new_species$traits[ , "dispersal"] <- 1
  return(list(new_species))
}


sim <- run_simulation(config = conf_n, 
                      landscape = file.path(dd, "landscapes/SA_1d"), output_directory = od,
                      verbose = 1)

# OH NO! Somethings is not working....

#resolution...
conf_n$gen3sis$initialization$create_ancestor_species <- function(landscape, config) {
  # browser()
  range <- c(-50, -45, -30, 20)
  co <- landscape$coordinates
  selection <- co[, "x"] >= range[1] &
    co[, "x"] <= range[2] &
    co[, "y"] >= range[3] &
    co[, "y"] <= range[4]
  initial_cells <- rownames(co)[selection]
  new_species <- create_species(initial_cells, config)
  #set local adaptation to max optimal temp equals local temp
  new_species$traits[ , "temp"] <- landscape$environment[selection,"temp"]
  new_species$traits[ , "dispersal"] <- 1
  return(list(new_species))
}


# you can see what objects are in the internal environment with ls().

# timestep at the landscape object can help you stop at your convenience:
stop_time <- "64"
get_dispersal_values <- function(n, species, landscape, config) {
if(landscape$timestep== stop_time){browser()}
# also found on vars:
vars <- dynGet("vars", inherits = TRUE)
if(vars$ti== stop_time){browser()}

# get your error stats
# options()$error
# set the global options of your R session to enter the browser mode anytime you run into an error.
# options(error=recover)

# re_run
sim <- run_simulation(config = conf_n, 
                      landscape = file.path(dd, "landscapes/SA_1d"), output_directory = od, call_observer = "all", 
                      verbose = 1)

# *\o/* Discover the gen3sis calls ------------
# This call has a lot of browsers inside the config. This will help us refresh the gen3sis calls.
sb <- run_simulation(config = file.path(dd, "configs/SA_1d/config_browser.R"), 
                      landscape = file.path(dd, "landscapes/SA_1d"), output_directory = od, call_observer = "all", 
                      verbose = 3)

# second stop! stop_time="60"

# Question [] What would be the consequences of changing this config seed?------ 
rm(sb)

### Changing worlds practical ----

# try yourself with the simple and World center data-set
cdp <- system.file(file.path("extdata", "CaseStudy1"), package="gen3sis")

list.files(cdp)

# run simulation and store summary object to fast object
fast <- run_simulation(config = file.path(cdp,"config/config_fast.R"), 
                      landscape = file.path(cdp,"landscape"),
                      output_directory = od)

# plot summary object
plot_summary(fast)

## 
cdp <- system.file(file.path("extdata", "WorldCenter"), package="gen3sis")

# run simulation and store summary obejct to sim
africa <- run_simulation(config = file.path(cdp,"config/config_worldcenter.R"), 
                       landscape = file.path(cdp,"landscape"),
                       output_directory = od)

# plot summary object
plot_summary(africa)


### Exercise (15min) -----------------
# copy world_center config to a folder, modify it to stop after 30 time-steps 
# and plot the total richness plus the range of species 1. Either though time 
# or at final time-step

# [] create a folder mod_config ---------

# [] move world center config in there -------

# [] modify world center there -------

# modsim <- run_simulation(config = file.path(dd,"/configs/XZY_config/XZYified_config_worldcenter.R"),
#                        landscape = file.path(cdp,"landscape"),
#                        output_directory = od)




# [] All GOOD? --------------

# .  ----------------------------------------
#  [ 4. EXERCISE YOUR SKILLS ]       ########
# .  ----------------------------------------

### Exercise (5min) -----------------
# What would you like to try out with gen3sis? -----------
# [] Look at different config files and try to explore them-------
# [] what are they doing? -------
# [] What for do we have a set.seed variable here -------

### Exercise (45 min) -----------------
# e1[] Create a config ------- 
# The config should have: No trait evolution,  starting with at least 6 species 
# that have different temperature niche width and height.


### Exercise (45 min) -----------------
# e2[] Create a No Evol model ------- 
# The config should have: No trait evolution,  starting with at least 6 species 
# that have different temperature niche width and height.

# e3[] Create a Complex Fossilization model ------- 
# Use these: trait_names = c("temp",  "dispersal", "foss")
# let traits evolve
# make dispersal and foss interact or not
# 


###### Project definition for next block --------------

### Exercise (5min) -----------------
# What is still unclear to you? What would you like to try out with gen3sis?
