## METADATA ===============================================================
## Description: MS3BM course
## 
## R version: 4.0.2 for Windows
## Date: 2022-10-20 16:59:30
## License: GPL3
## Author: Oskar Hagen (oskar@hagen.bio), Skeels and Rosenbaum 
##=======================================================================##


# install gen3sis

install.packages("gen3sis")

library(raster)
library(gen3sis)

#### SETUP ------------

### [] download data --------

#### TODOUPDATEBELOW
# DropboxFolders\MacroEcoEvo_course_2021\1 Tutorials week1\Day5_EcoEvoModel\SA

### [] store it to a working directory ------------


### [] create wd -------
wd <- "../data"
od <- "../../outputs"

# wd <- tempdir()


print(paste("gen3sis version:", packageVersion("gen3sis")))

### attention! Download the data If using South America!!!
# datapath <- system.file(file.path("extdata", "SouthAmerica"), package = "gen3sis")
#
# print(datapath)
# list.dirs(datapath, recursive = F)
# note that dist distances are not calculated from the data of the package


list.dirs(file.path(wd, "SA"), recursive = F)

### [] open explorer --------
### [] look at the input data. Output Later!  --------



#### Landscape ---------

#### [] look at metadata -----

lp <- readRDS(file.path(wd,"SA", "landscape", "landscapes.rds"))
class(lp)
names(lp)
dim(lp$temp)
lp$temp[1:10, 1:10]
colnames(lp$temp)


plot(rasterFromXYZ(lp$temp[,c("x", "y", "0")]))
plot(rasterFromXYZ(lp$temp[,c("x", "y", "65")]))

# overlay
rl0 <-rasterFromXYZ(lp$temp[,c("x", "y", "0")])
rl65 <- rasterFromXYZ(lp$temp[,c("x", "y", "65")])

plot(rl65, col=rgb(1,0,0))
plot(rl0, col=rgb(0,0,1,0.5,1), add=T)


### !!!!!! EXERCISE (10min) -----------------
# create a way to visualize a gen3sis input.
# If possible think of abstracting for any x,y,z1,z2,z3... temporal data-frame.
# Hint: Make a function! ;) 

# ANSWER 
{
  # animation -----------
  for (ti in as.character(65:0)) {
    # ti <- "65"
    ri <- rasterFromXYZ(lp$temp[,c("x", "y", ti)])
    plot(ri, main=paste(ti, "Ma"))
    Sys.sleep(1)
  }
  
  plots <-  function(df, reverse=T){
    #df <- lp$temp
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
  
  plots(lp$temp)
}






#### Config -------------

#### [] open config file --------

?create_input_config

empt_conf <- create_input_config()

empt_conf


### !!!!!! EXERCISE (5min) -----------------
# think of 3 ways to impact on extinction rates?



##### break ##### 
#Everybody good? found files?

#### > RUN --------------- 
# one simple example simulation
# do not run!
sim <- run_simulation(config = file.path(wd, "SA/config/config_southamerica.R"), 
                      landscape = file.path(wd, "SA/landscape"), output_directory = od, call_observer = "all", 
                      verbose = 1)
  
names(sim)

# sim <- readRDS(file.choose())


#### > Visualize --------------- 
#the outputs
plot_summary(sim)

# using raster package
plot(rasterFromXYZ(sim$summary$`richness-final`))

plot(rasterFromXYZ(sim$summary$`richness-final`), col=gen3sis::color_richness(10))




# species and landscapes
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

#### > Analyze --------------- 

#the outputs
landscapes <- readRDS(file.path(wd, "SA/landscape/landscapes.rds"))  #get the input landscape
landscape_t0 <- as.data.frame(cbind(landscapes$temp[, 1:2], temp = landscapes$temp[,3], arid = landscapes$arid[, 3], area = landscapes$area[, 3]))  #get landscape at last time-step
landscape_t0 <- cbind(landscape_t0, rich = sim$summary$`richness-final`[, 3])  #add richness to the dataframe
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
plot(data_plot[, 1], data_plot[, 2], xlab = "Temperature [°C]", ylab = expression(paste(alpha," richness")), frame.plot = F, type = "l", col = "red", lwd = 2, xlim = c(min(landscape_t0$temp), 
                                                                                                                                                                  max(landscape_t0$temp)), ylim = c(min(landscape_t0$rich), max(landscape_t0$rich)))
# add observed points
points(landscape_t0$temp, landscape_t0$rich, col = rgb(0.5, 0.5, 0.5, alpha = 0.4), 
       pch = 16)
# add legend
legend(-20, 30, col = c(rgb(0.5, 0.5, 0.5, 0.4), "red"), legend = c(n, "model fit"), 
       pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2), bty = "n")


###### > Modify landscapes and eco-evolutionary rules  ------------------------

# [] modify landscape -----

lpmod <- lp

for (ti in as.character(0:65)){
  slice <- lpmod$temp[,ti]
  lpmod$temp[,ti] <- slice*rnorm(n=length(slice))
}
plots(lpmod$temp)


# [] if we would modify the habitable sites.. we need to compile a new landscape

?create_input_landscape


??gen3sis

# show Creating landscapes (gen3sis input)

# [] modify config -----

conf <- create_input_config()

conf <- create_input_config(file.path(wd, "SA/config/config_southamerica.R"))

#before we run
sim <- run_simulation(config = file.path(wd, "SA/config/config_southamerica.R"), 
                      landscape = file.path(wd, "SA/landscape"), output_directory = wd, call_observer = 1, 
                      verbose = 0)

#same as running. Lets add verbose
sim <- run_simulation(config = conf, 
                      landscape = file.path(wd, "SA/landscape"), output_directory = wd, call_observer = 1, 
                      verbose = 1)


#lets make it faster
conf$gen3sis$initialization$create_ancestor_species

conf_n <- conf


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
                      landscape = file.path(wd, "SA/landscape"), output_directory = wd, 
                      verbose = 1)





###### > Troubleshooting -----------------

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
                      landscape = file.path(wd, "SA/landscape"), output_directory = wd,
                      verbose = 1)

# OH NO! Somethings is not working....

#resolution
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

# re_run
sim <- run_simulation(config = conf_n, 
                      landscape = file.path(wd, "SA/landscape"), output_directory = wd, call_observer = "all", 
                      verbose = 1)

#### CHANGING WORLDS ####

# try yourself with the simple and World center data-set
datapath <- system.file(file.path("extdata", "CaseStudy1"), package="gen3sis")

list.files(datapath)

# run simulation and store summary object to fast object
fast <- run_simulation(config = file.path(datapath,"config/config_fast.R"), 
                      landscape = file.path(datapath,"landscape"),
                      output_directory = wd)

# plot summary object
plot_summary(fast)

## 
datapath <- system.file(file.path("extdata", "WorldCenter"), package="gen3sis")

# run simulation and store summary obejct to sim
africa <- run_simulation(config = file.path(datapath,"config/config_worldcenter.R"), 
                       landscape = file.path(datapath,"landscape"),
                       output_directory = wd)

# plot summary object
plot_summary(africa)


### !!!!!! EXERCISE (5min) -----------------
# copy world_center config to a folder, modify it to stop after 30 time-steps 
# and plot the total richness plus the range of species 1. Either though time 
# or at final time step

# [] create a folder mod_config ---------

# [] move world center confige in there -------

# [] modify world center there -------

modsim <- run_simulation(config = file.path(wd,"mod_config/modified_config_worldcenter.R"), 
                       landscape = file.path(datapath,"landscape"),
                       output_directory = wd)


###### > Project definition for next block --------------

### !!!!!! EXERCISE (5min) -----------------
# What is still unclear to you? What would you like to try out with gen3sis?
