## METADATA ===============================================================
## Description: MS3BM course work though Day 2
## In this script we will be:
## 1) generating a batch of configs
## 2) running a batch of simulation model
## 3) extracting summary statistics
## 4) performing sensitivity analysis
## 5) performing model discrimination analysis
## 6) performing model selection
## 
## R version: 4.0.2 for Windows
## Date: 2022-10-28 16:59:30
## License: GPL3
## Author: Skeels (alexander.skeels@gmail.com), Hagen and Rosenbaum 
##=======================================================================##

#### PREAMBLE ------------

# before you dive in, check out the configs templates we use to simulate the underlying data:
# 1) config_template_M1_SA.R
# 2) config_template_M2_SA.R

# Pay attention to the speciation function and evolution function
# What's different between the two models?
# Do you have any predictions about how biodiversity patterns might differ between the models?


########## PART 1: GENERATING CONFIGS ############

#### SETUP ------------
### [] source.R ------------
source(file.path(getwd(), "source.R"))

### [] Paths ---------------
conf_d <- file.path(dd, "configs/SA_coarse")
land_d <- file.path(dd, "landscapes/SA_coarse")
exp_d <- file.path(dd, "examples/day2")
out_d <- file.path(od, "day2")

# install new packages
install.packages("randtoolbox") 
install.packages("scatterplot3d") 

# libraries

library(randtoolbox)
library(scatterplot3d)

#### FUNCTIONS ---------
linmap <- function(x, from, to) {(x - min(x)) / max(x - min(x)) * (to - from) + from}

#### GENERATE PARAMS ---------

# number of configs to generate per model
# we'll start small because the model can take a while to run
n <- 200

# number of variable parameters to generate 
# we'll follow the example from the lecture - dispersal, niche evolution, and divergence threshold
p <- 3

# create a table of Sobol sequences to evenly sample parameter space
params_table <- data.frame(sobol(n, p, init = T))
colnames(params_table) <- c("dispersal_scale", "sigma", "divergence_threshold")

# look at Sobol sequences
head(params_table)

# see how they relate to each other - notice how they evenly fill the cube?
scatterplot3d(params_table, xlab="dispersal_scale", ylab="sigma", zlab="divergence_threshold")

# however out values are all scaled between 0-1, we want to change this to match the units of each parameter
# so here will be linearly map the Sobol sequences to the parameter range we are interested in
# lets vary dispersal scale bewteen 222 and 666 (in units of Km)
params_table[,"dispersal_scale"] <- linmap(params_table[,"dispersal_scale"], 222, 666)

# lets vary sigma_e bewteen 0.001 and 0.1 (in standardised units of temperature)
params_table[,"sigma"] <- linmap(params_table[,"sigma"], 0.001, 0.01)

# lets vary divergence_threshold bewteen 2 and 10 (in number of timesteps)
params_table[,"divergence_threshold"] <- linmap(params_table[,"divergence_threshold"], 1, 3)

# can plot out the parameters again and see how the units have changed
scatterplot3d(params_table, xlab="dispersal_scale", ylab="sigma", zlab="divergence_threshold")

# add model names to the parameter table
params_table$model <- sapply(seq(1, n, 1), FUN=function(x){paste("SA_model_", x, sep="")})

# save the table
write.table(params_table, file=file.path(exp_d, "SA_param_table.txt"), row.names = F, col.names = T)

# Questions to think about...
# can you think of other ways to sample parameters?
# here we use uniform distributions, can you think of reasons to prefer other distributions?

#### GENERATE CONFIGS ---------

# now that we have sampled our parameters, we want to generate the configs for gen3sis to read

# create a directory to place them
if(dir.exists(file.path(dd, "configs", "SA_coarse"))==F){
  dir.create(file.path(dd, "configs", "SA_coarse"))
}
# create dir for outputs in
if(dir.exists(file.path(od, "day2"))==F){
  dir.create(file.path(od, "day2"))
}
if(dir.exists(file.path(od, "day2", "SA_configs"))==F){
  dir.create(file.path(od, "day2", "SA_configs"))
}

# write a loop which reads in the config template and replaces the parameters with those drawn from each row in the table
# M1
for(i in 1:nrow(params_table)){ 
  
  # read in row i of the parameter table
  params <- params_table[i,]
  
  # read in the config template
  config_i <- readLines(file.path(conf_d, "config_template_M1_SA.R"))
  
  # replace the lines in the template with the parameter values from row i
  config_i <- gsub('*.params\\$sigma', params$sigma, config_i)
  config_i <- gsub('*.params\\$dispersal_scale', params$dispersal_scale, config_i)
  config_i <- gsub('*.params\\$divergence_threshold', params$divergence_threshold, config_i)
  
  # save the config with a unique name
  writeLines(config_i, file.path(od, "day2", "SA_configs", paste0('SA_config_M1_', i, '.R')))
  
}


# repeat for M2
for(i in 1:nrow(params_table)){ 
  
  # read in row i of the parameter table
  
  params <- params_table[i,]
  # read in the config template
  config_i <- readLines(file.path(conf_d, "config_template_M2_SA.R"))
  
  # replace the lines in the template with the parameter values from row i
  config_i <- gsub('*.params\\$sigma', params$sigma, config_i)
  config_i <- gsub('*.params\\$dispersal_scale', params$dispersal_scale, config_i)
  config_i <- gsub('*.params\\$divergence_threshold', params$divergence_threshold, config_i)
  
  # save the config with a unique name
  writeLines(config_i, file.path(od, "day2", "SA_configs", paste0('SA_config_M2_', i, '.R')))
}


########## PART 2: RUNNING SIMULATION BATCH ############

#### SETUP ------------

# install new packages
install.packages("foreach")
install.packages("doParallel")

# libraries
library(doParallel)
library(foreach)

#### RUN SINGLE SIMULATION ------------

# get config file names and order them numerically
configs <- list.files(file.path(exp_d,"SA_configs"))
#### ALEX.. is this fix correct?
# configs <- list.files(file.path(od, "day2", "SA_configs"))




# subset M1 configs
configs_m1 <- configs[grepl("SA_config_M1", configs)]
configs_m1 <- configs_m1[order(as.numeric(sapply(sapply(configs_m1, FUN=function(x)(strsplit(x, "_")[[1]][4])), FUN=function(y){strsplit(y, ".R")[[1]][1]})))]

# subset M2 configs
configs_m2 <- configs[grepl("SA_config_M2", configs)]
configs_m2 <- configs_m2[order(as.numeric(sapply(sapply(configs_m2, FUN=function(x)(strsplit(x, "_")[[1]][4])), FUN=function(y){strsplit(y, ".R")[[1]][1]})))]

# run a single simulation under M1!
sim_1 <- run_simulation(config = file.path(od, "day2", "SA_configs", configs_m1[1]),
                        landscape = land_d,
                        verbose=0, # no progress printed
                        output_directory=file.path(out_d))

# run a single simulation under M2!
sim_2 <- run_simulation(config = file.path(od, "day2", "SA_configs", configs_m2[1]),
                        landscape = land_d,
                        verbose=0, # no progress printed
                        output_directory=file.path(out_d))


# this next bit shows one way of running a batch of simulations
# here we are using parallel processing to run a bunch of simulation at the same time
# alternatively, and what we've done for larger studies, we use large clusters to run hundreds or thousands of simulations...
#... but configuring this will be specific to the cluster/HPC
if(run_slow==T){


  # M1
  
  # determine number of cores of your machines to use
  cl <- makeCluster(detectCores()-1)
  
  # register the cluster
  registerDoParallel(cl)
  
  # use foreach loop to run simulations in parallel
  foreach(i=1:n) %dopar% {
    library(gen3sis)
    run_simulation(config = file.path(od, "day2", "SA_configs", configs_m1[i]),
                   landscape = land_d,
                   verbose=0, # no progress printed
                   output_directory=file.path(out_d),
                   call_observer = NA)
  }
  
  stopCluster()
  
  # rep[eat for M2
  cl <- makeCluster(detectCores()-2)
  registerDoParallel(cl)
  foreach(i=1:n) %dopar% {
    library(gen3sis)
    run_simulation(config = file.path(od, "day2", "SA_configs", configs_m2[i]),
                   landscape = land_d,
                   verbose=0, # no progress printed
                   output_directory=file.path(out_d),
                   call_observer = NA)
  }
  
  stopCluster()
}

########## PART 3: EXTRACT SUMMARY STATISTICS ############

# install new packages
install.packages("ape")
install.packages("treebalance")
install.packages("PhyloMeasures")
install.packages("ggplot2")

# libraries
library(ape)
library(treebalance)
library(PhyloMeasures)
library(ggplot2)


#### PARAMETER TABLE ------------

# lets set up new columns for 10 biodiversity metrics of interest
params_table$NSP <- NA # total diversity of clade
params_table$LDG <- NA # Latitudinal diversity gradient
params_table$Occ <- NA # Occupancy of the domain
params_table$Gamma <- NA   # Gamma statitic of phylogenetic slowdown
params_table$PD <- NA # phylogenetic diversity
params_table$MPD <- NA # mean pairwise phylogenetic distance
params_table$MNTD <- NA # mean nearest taxon phylogenetic distance
params_table$MBS <- NA  # Mean body size
params_table$SDBS <- NA # Standard deviation of body size 
params_table$MT <- NA  # Mean temp
params_table$SDT <- NA # Standard deviation of temp 
params_table$TBS <- NA # Correlation of temperature trait and body size trait

# we'll start with a seperate table for each model
params_m1 <- params_table
params_m2 <- params_table

#### SUMMARY STATISTICS ------------

# lets work through a single simulation, then we can loop it over them all
# okey dokey, lets start by looking at the sgen3sis summary file to start
summary_1 <- readRDS(file.path(out_d, paste0("SA_config_M1_", 1),  "sgen3sis.rds"))

# we're interested here in the phylo summary, richness final, and occupancy
head(summary_1$summary$phylo_summary)
head(summary_1$summary$`richness-final`)
head(summary_1$summary$occupancy)
# The number of species that emerged during the model is in the last line of the phylo summary
NSP <- tail(summary_1$summary$phylo_summary[, "alive"], 1) # 34 species in total

# richness-final has the number of species in each grid cell, and their correpsonding xy coordinates
richness_1 <- summary_1$summary$`richness-final`

# this is great because now we can do a correlation to get a measure of the strength of the latitudinal diversity gradient
# Spearman's correlation between absolute latitude and species richness
LDG <- cor(richness_1[,3], abs(richness_1[,2]), method="spearman", use="complete.obs")

# we can also look at the occupancy of the domain 
# this is the proportion of grid cells that are occupied
# this is a measure of the geographic range size of the whole clade
# lets take the final time step
Occ <- tail(summary_1$summary$occupancy,1)

# now lets move onto some phylogenetic tree shape metrics
# lets load in the phylogentic tree nexus file
phy_1 <- read.nexus(file.path(out_d, paste0("SA_config_M1_", 1),  "phy.nex"))

# lets resolve the root polytomy so the tree is completely bifuricating
phy_1 <- multi2di(phy_1)

# double check
is.binary(phy_1)

# the Gamma statistic is a measure of phylogenetic node height distribution
# positive values indicate nodes are more densly distributed towards the present - suggesting diversification speed up
# negative values indicate nodes are more densly distributed towards the root - suggesting diversification slowdown
Gamma <- gammaStat(phy_1)

# load the landscape object 
landscape_1 <- readRDS(file.path(out_d, paste0("SA_config_M1_", 1),"landscapes","landscape_t_0.rds"))

# Lets now look at some metrics from the species object
# load species object
species_1 <- readRDS(file.path(out_d, paste0("SA_config_M1_", 1), "species", "species_t_0.rds"))

# check out the species object for species 1
# abundance in each grid cells
head(species_1[[1]]$abundance)

# trait table
head(species_1[[1]]$traits)

# population divergence data
species_1[[1]]$divergence

# From the landscpae + species objects we can create a presence/absence matrix - a commonly used data format in ecology
# grid cell names
all_cells <- rownames(landscape_1$coordinates)

# get 0 for absence and 1 for presence in each grid cell
all_species_presence <- do.call( cbind, lapply(species_1, FUN = function(x) {ifelse(all_cells %in% names(x$abundance), 1, 0)}))

# colnames are species names
# need them to match the phylo
colnames(all_species_presence ) <- paste0("species",unlist(lapply(species_1, function(x){x$id})))

# column bind with x/y coordinates 
presence_absence_matrix <- cbind(landscape_1$coordinates, all_species_presence)

# lets get 3 community phylogenetic metrics!
pd_estimate <- pd.query(phy_1, presence_absence_matrix[, 3:ncol(presence_absence_matrix)], standardize = T)
mpd_estimate <- mpd.query(phy_1, presence_absence_matrix[, 3:ncol(presence_absence_matrix)], standardize = T)
mntd_estimate <- mpd.query(phy_1, presence_absence_matrix[, 3:ncol(presence_absence_matrix)], standardize = T)

PD <- mean(pd_estimate, na.rm=T)
MNTD <- mean(mntd_estimate, na.rm=T)
MPD <- mean(mpd_estimate, na.rm=T)

# from the distribution of trait values for each species, summarise the mean trait values for each species
traits_1 <- do.call(rbind, lapply(species_1, FUN=function(x){if(!is.null(x)){colMeans(x$traits)}else {data.frame( temp=NA, body_size=NA, dispersal=NA) }}))

# mean body size of all species
MBS <- mean(traits_1[,"body_size"], na.rm=T)

# sd of body size across all species
SDBS <- sd(traits_1[,"body_size"])

# mean temperature trait of all species
MT <- mean(traits_1[,"temp"], na.rm=T)

# sd of temperature trait across all species
SDT <- sd(traits_1[,"temp"])

# temperature trait and body size trait correlation
TBS <- cor(traits_1[,"body_size"], traits_1[, "temp"], method="spearman", use='complete.obs')

# This next bit loops over each simulation, extracts the metrics, and puts them in the parameter table
# need to have run the simulations for this
# so we'll just load in the precalulated metrics
if(run_slow==TRUE){
  landscape_i <- readRDS(file.path(out_d, paste0("SA_config_M1_", 1),"landscapes","landscape_t_0.rds"))
  
  for(i in 1:n){
    
    summary_1 <- readRDS(file.path(out_d, paste0("SA_config_M1_", i),  "sgen3sis.rds"))
    
    
    richness_1 <- summary_1$summary$`richness-final`
    phy_1 <- read.nexus(file.path(out_d, paste0("SA_config_M1_", i),  "phy.nex"))
    phy_1 <- multi2di(phy_1)
    
    # in case of simulation not finishing, use try
    species_1 <- try(readRDS(file.path(out_d, paste0("SA_config_M1_", i), "species", "species_t_0.rds")))
    if(class(species_1)=="try-error"){next}
    all_cells <- rownames(landscape_1$coordinates)
    all_species_presence <- do.call( cbind, lapply(species_1, FUN = function(x) {ifelse(all_cells %in% names(x$abundance), 1, 0)}))
    colnames(all_species_presence ) <- paste0("species",unlist(lapply(species_1, function(x){x$id})))
    presence_absence_matrix <- cbind(landscape_1$coordinates, all_species_presence)
    
    pd_estimate <- pd.query(phy_1, presence_absence_matrix[, 3:ncol(presence_absence_matrix)], standardize = T)
    params_m1$PD[i] <- mean(pd_estimate, na.rm=T)
    rm(pd_estimate)
    
    mpd_estimate <- mpd.query(phy_1, presence_absence_matrix[, 3:ncol(presence_absence_matrix)], standardize = T)
    params_m1$MPD[i] <- mean(mpd_estimate, na.rm=T)
    rm(mpd_estimate)
    
    mntd_estimate <- mpd.query(phy_1, presence_absence_matrix[, 3:ncol(presence_absence_matrix)], standardize = T)
    params_m1$MNTD[i] <- mean(mntd_estimate, na.rm=T)
    rm(mntd_estimate)
    
    traits_1 <- do.call(rbind, lapply(species_1, FUN=function(x){if(!is.null(x)){colMeans(x$traits)}else {data.frame( temp=NA, body_size=NA, dispersal=NA) }}))
    
    params_m1$NSP[i] <- tail(summary_1$summary$phylo_summary[, "alive"])[1]
    params_m1$LDG[i] <- cor(richness_1[,3], abs(richness_1[,2]), method="spearman", use="complete.obs")
    params_m1$Occ[i] <- tail(summary_1$summary$occupancy,1)
    params_m1$Gamma[i] <- gammaStat(phy_1)
    params_m1$MBS[i] <- mean(traits_1[,"body_size"], na.rm=T)
    params_m1$SDBS[i] <- sd(traits_1[,"body_size"], na.rm=T)
    params_m1$MT[i] <- mean(traits_1[,"temp"], na.rm=T)
    params_m1$SDT[i] <- sd(traits_1[,"temp"], na.rm=T)
    params_m1$TBS[i] <- cor(traits_1[,"body_size"], traits_1[,"temp"], method="spearman", use='complete.obs')
    gc()# clean up the workspace
  }
  
  # repeat for M2
  for(i in 1:n){
    
    summary_1 <- try(readRDS(file.path(out_d, paste0("SA_config_M2_", i),  "sgen3sis.rds")))
    NSP <- tail(summary_1$summary$phylo_summary[, "alive"])[1]
    if(NSP > 10000){next}
    if(class(summary_1 )=='try-error'){next}
    richness_1 <- summary_1$summary$`richness-final`
    phy_1 <- read.nexus(file.path(out_d, paste0("SA_config_M2_", i),  "phy.nex"))
    phy_1 <- multi2di(phy_1)
    species_1 <- readRDS(file.path(out_d, paste0("SA_config_M2_", i), "species", "species_t_0.rds"))
    all_cells <- rownames(landscape_1$coordinates)
    all_species_presence <- do.call( cbind, lapply(species_1, FUN = function(x) {ifelse(all_cells %in% names(x$abundance), 1, 0)}))
    colnames(all_species_presence ) <- paste0("species",unlist(lapply(species_1, function(x){x$id})))
    presence_absence_matrix <- cbind(landscape_1$coordinates, all_species_presence)
    
    pd_estimate <- pd.query(phy_1, presence_absence_matrix[, 3:ncol(presence_absence_matrix)], standardize = T)
    params_m2$PD[i] <- mean(pd_estimate, na.rm=T)
    rm(pd_estimate)
    
    mpd_estimate <- mpd.query(phy_1, presence_absence_matrix[, 3:ncol(presence_absence_matrix)], standardize = T)
    params_m2$MPD[i] <- mean(mpd_estimate, na.rm=T)
    rm(mpd_estimate)
    
    mntd_estimate <- mpd.query(phy_1, presence_absence_matrix[, 3:ncol(presence_absence_matrix)], standardize = T)
    params_m2$MNTD[i] <- mean(mntd_estimate, na.rm=T)
    rm(mntd_estimate)
    traits_1 <- do.call(rbind, lapply(species_1, FUN=function(x){if(!is.null(x)){colMeans(x$traits)}else {data.frame( temp=NA, body_size=NA, dispersal=NA) }}))
    
    params_m2$NSP[i] <- NSP
    params_m2$LDG[i] <- cor(richness_1[,3], abs(richness_1[,2]), method="spearman", use="complete.obs")
    params_m2$Occ[i] <- tail(summary_1$summary$occupancy,1)
    params_m2$Gamma[i] <- gammaStat(phy_1)
    params_m2$MBS[i] <- mean(traits_1[,"body_size"], na.rm=T)
    params_m2$SDBS[i] <- sd(traits_1[,"body_size"], na.rm=T)
    params_m2$MT[i] <- mean(traits_1[,"temp"], na.rm=T)
    params_m2$SDT[i] <- sd(traits_1[,"temp"], na.rm=T)
    params_m2$TBS[i] <- cor(traits_1[,"body_size"], traits_1[,"temp"], method="spearman", use='complete.obs')
    gc() # clean up the workspace
  }
  
  write.csv(params_m1,  file=file.path(exp_d, "summary_table_m1.csv"), row.names = FALSE)
  write.csv(params_m2,  file=file.path(exp_d, "summary_table_m2.csv"), row.names = FALSE)
  
} else{ # just read in pre-calculated metrics
  
  params_m1 <- read.csv( file=file.path(exp_d, "summary_table_m1.csv"))
  params_m2 <- read.csv( file=file.path(exp_d, "summary_table_m2.csv"))
}

#lets have a quick look at some of them
ggplot(params_m1)+
  geom_histogram(aes(x=LDG ))+
  theme_classic()


########## PART 4: SENSITIVITY ANALYSIS  ############

#install new packages
install.packages("dismo")
install.packages("reshape")
install.packages("jtools")
install.packages("corrplot")

# libraries
library(dismo)
library(reshape)
library(jtools)
library(corrplot)

# one of the most important questions form modelling biodiversity patterns is finding out how the model parameters shape variation in the emergent patterns
# this is known as sensitivity analysis
# ... how sensitive are model outputs to variation in model parameters?

#### PARAMETER SPACE ------------

# The first thing we might be interested in after summarizing some biodiversity patterns...
# ...is whether we feel like we have sufficiently sampled the parameter space
# This is part of an iterative process in which we need to check whether our simulated patterns match some target...
#... and possibly rerun some simulations either expanding or contracting parameter space
# The target could be an empirical pattern or it could be based on defining some rules a priori 
# For example, one rule could be we are interested in simulations that generate > 20 species but < 1000 species

#lets plot out the relationship between number of species and each model parameter
# Divergence threshold
ggplot(params_m1, aes(x=divergence_threshold, y=log(NSP)))+
  geom_point()+
  theme_classic()+
  stat_smooth(method='lm')+
  geom_hline(yintercept=log(20), linetype="dashed", colour="red")+
  geom_hline(yintercept=log(1000), linetype="dashed", colour="red")

# Niche Evolution
ggplot(params_m1, aes(x=sigma, y=log(NSP)))+
  geom_point()+
  theme_classic()+
  stat_smooth(method='lm')+
  geom_hline(yintercept=log(20), linetype="dashed", colour="red")+
  geom_hline(yintercept=log(1000), linetype="dashed", colour="red")

# Dispersal
ggplot(params_m1, aes(x=dispersal_scale, y=log(NSP)))+
  geom_point()+
  theme_classic()+
  stat_smooth(method='lm')+
  geom_hline(yintercept=log(20), linetype="dashed", colour="red")+
  geom_hline(yintercept=log(1000), linetype="dashed", colour="red")

# Q: are there any recommendations you would make if you were to rerun more simulations based on our 'rule'?

#### CORRELATIONS ------------

# After visually inspected the relationships, we want to wauntify the relationships between parameters and outputs
# perhaps the simplest way to do this is using correlations
# lets use a Spearman correlation
cor_table <- cor(params_m1[, c(1:3, 5:16)], method="spearman", use="complete.obs")

# plot it out
corrplot(cor_table, type="lower", method="square")

#### MULTIPLE REGRESSION ------------

# correlations cannot infrom us of additive or interactive effects of parameters
# instead a model based approach might give us some more infromation on sensitivity.
# A familiar modelling approach is multiple linear regression
# here the response variable is a metric and the predictors are the model parameters

# first we should scale and center the model parameters
params_m1_scaled <- params_m1
params_m1_scaled[, 1:3] <- scale(params_m1[, 1:3])

# in this case we'll also scale the repsonse variables, so visualising the results is a bit easier (not necessary though)
params_m1_scaled[, 5:16] <- scale(params_m1_scaled[, 5:16])

# we can fit a seperate model for each metric 
m1  <- lm(NSP ~ dispersal_scale + sigma  + divergence_threshold, data=params_m1_scaled)
m2  <- lm(LDG ~ dispersal_scale + sigma  + divergence_threshold, data=params_m1_scaled)
m3  <- lm(Occ ~ dispersal_scale + sigma  + divergence_threshold, data=params_m1_scaled)
m4  <- lm(Gamma ~ dispersal_scale + sigma  + divergence_threshold, data=params_m1_scaled)
m5  <- lm(MBS ~ dispersal_scale + sigma  + divergence_threshold, data=params_m1_scaled)
m6  <- lm(SDBS ~ dispersal_scale + sigma  + divergence_threshold, data=params_m1_scaled)
m7  <- lm(MT ~ dispersal_scale + sigma  + divergence_threshold, data=params_m1_scaled)
m8  <- lm(SDT ~ dispersal_scale + sigma  + divergence_threshold, data=params_m1_scaled)
m9 <- lm(TBS ~ dispersal_scale + sigma  + divergence_threshold, data=params_m1_scaled)
m10 <- lm(PD ~ dispersal_scale + sigma  + divergence_threshold, data=params_m1_scaled)
m11 <- lm(MPD ~ dispersal_scale + sigma  + divergence_threshold, data=params_m1_scaled)
m12 <- lm(MNTD ~ dispersal_scale + sigma  + divergence_threshold, data=params_m1_scaled)

# check out the model summaries
summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)
summary(m9)
summary(m10)
summary(m11)
summary(m12)


# can plot out the regression coefficients to see how metrics respond to different parameters
# plot the first 5
#ALEX I can't run this without installing broom. and these other packages... keeping this comment here JIC..!
# library(broom)
# library("ggstance")
# library("broom.mixed")
plot_summs(m1, m2, m3, m4, m5, m6) # I had troubles with this...
# plot the second 5
plot_summs(m7, m8, m9, m10, m11, m12)

# We could also increase complexity and add interaction terms
m1_int  <- lm(NSP ~ dispersal_scale * sigma  * divergence_threshold, data=params_m1_scaled)
m2_int  <- lm(LDG ~ dispersal_scale * sigma  * divergence_threshold, data=params_m1_scaled)
m3_int  <- lm(Occ ~ dispersal_scale * sigma  * divergence_threshold, data=params_m1_scaled)
m4_int  <- lm(Gamma ~ dispersal_scale * sigma  * divergence_threshold, data=params_m1_scaled)
m5_int  <- lm(MBS ~ dispersal_scale * sigma  * divergence_threshold, data=params_m1_scaled)
m6_int  <- lm(SDBS ~ dispersal_scale * sigma  * divergence_threshold, data=params_m1_scaled)
m7_int  <- lm(MT ~ dispersal_scale * sigma  * divergence_threshold, data=params_m1_scaled)
m8_int  <- lm(SDT ~ dispersal_scale * sigma  * divergence_threshold, data=params_m1_scaled)
m9_int <- lm(TBS ~ dispersal_scale * sigma  * divergence_threshold, data=params_m1_scaled)
m10_int <- lm(PD ~ dispersal_scale * sigma  * divergence_threshold, data=params_m1_scaled)
m11_int <- lm(MPD ~ dispersal_scale * sigma  * divergence_threshold, data=params_m1_scaled)
m12_int <- lm(MNTD ~ dispersal_scale * sigma  * divergence_threshold, data=params_m1_scaled)

# check out the model summaries
summary(m1_int)
summary(m2_int)
summary(m3_int)
summary(m4_int)
summary(m5_int)
summary(m6_int)
summary(m7_int)
summary(m8_int)
summary(m9_int)
summary(m10_int)
summary(m11_int)
summary(m12_int)

# questions to think about: what are the R-squared's of these models? 
# What does that tell you about sensitivity of metrics to the parameters?
# Which parameters have the largest effect?
# does it look like there are interactions between parameters?

#### BOOSTED REGRESSION TREES ------------

# linear regression is one to get an overview of how the variation in biodiversity metrics is explained by the parameters
# however, the relationships between parameters and metrics may be complex and non-linear
# machine learning approaches provide another tool to look at the relationships
# here we will follow the procedure of Prowse et al (2016; Ecosphere; https://doi.org/10.1002/ecs2.1238)
# in that study, the authors provide a framework for global sensitivity analysis of stochastic simulation models based on boosted regression trees (BRT)
# model parameters are predictor variables
predictor_vars <- c("divergence_threshold",  "sigma", "dispersal_scale")

# set up an empty list for the models
var_list <- vector("list", 12)

# for each variable we will fit a BRT with a learning rate of 0.01 and a tree complexity of 1
learn_rate <- 0.01

for(i in 1:12) {
  
  # response var is summary statistic i
  response_var <- names(params_m1_scaled)[5:16][i]
  
  # predictor columns (model parameters)
  x_col <- which(names(params_m1_scaled)%in% predictor_vars)
  
  # response column (biodiversity metric)
  y_col <- which(names(params_m1_scaled)==response_var)
  
  # make sure there are no NAs in the data
  brt_dataset <- params_m1_scaled[!is.na(params_m1_scaled[,y_col]),]
  
  # fit the BRT model
  brt_fit <- try(gbm.step( learning.rate = learn_rate, data=brt_dataset, gbm.x=x_col, gbm.y=y_col,family="gaussian",tree.complexity=1,n.folds=5,tolerance.method='auto',max.trees=200000))
  
  # increase learning rate for unsuccessful fits
  while(is.null(brt_fit)){
    learn_rate <- learn_rate-0.001
    brt_fit <- try(gbm.step( learning.rate = learn_rate, data=brt_dataset, gbm.x=x_col, gbm.y=y_col,family="gaussian",tree.complexity=1,n.folds=5,tolerance.method='auto',max.trees=200000))
  }
  
  # add the model to the list
  var_list[[i]] <- brt_fit
}

# lets now summarise those BRT models into some interpretable infromation
# we want the contribution of each variable to the BRT as well as the pseudo-R-sqaured and
sensitivity_df <- data.frame(var= names(params_m1_scaled)[5:16], sigma=NA,  dispersal_scale=NA, divergence_threshold=NA, R2=NA)

# then loop over each summary statitic (i) and calultae the sensitivity infromation
for(i in 1:12){
  
  # move on if model didn;t work
  if(class(var_list[[i]]) == 'try-error'){next}
  
  # variable contributions
  cont <- var_list[[i]]$contributions 
  
  # again, move on if model didn;t work
  if(is.null(cont)){next}
  
  # add variable contributions
  sensitivity_df$sigma[i]  <- cont[which(cont$var == "sigma"), 2]
  sensitivity_df$dispersal_scale[i]  <- cont[which(cont$var == "dispersal_scale"), 2]
  sensitivity_df$divergence_threshold[i]  <- cont[which(cont$var == "divergence_threshold"), 2]
  
  # get the R-squared
  sensitivity_df$R2[i]  <- (cor(var_list[[i]]$fit, var_list[[i]]$data$y))^2
}

# lets plot the results
# first melt the data for ggplot2
cont_melt <- melt(sensitivity_df[, 1:4])

# plot the variable contributions for each statistic
ggplot(cont_melt, aes(x=var, y=value))+
  geom_bar(aes(fill=variable),position="fill", stat="identity")+
  theme(axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme_classic()

# plot the R2 of each summary statistic as  bar plot
ggplot(sensitivity_df, aes(x=var, y=R2))+
  geom_bar(stat="identity")+
  theme( axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme_classic()

# Questions: 
# Which biodiversity patterns are strongly predicted by model parameters?
# Which parameter appears to have the strongest influence over biodiversity patterns?

########## PART 5: MODEL DISCRIMINATION  ############

#install new packages
install.packages("caret")
install.packages("gbm")

# libraries
library(caret)
library(gbm)

# now that we have explored a bit how the parameters under M1 shape emergent biodiversity patterns, lets see if M2 differs from M1
# lets combine data for M1 and M2 and have a visual inspection of some of the metrics
params_m1$generating_model <- "M1"
params_m2$generating_model <- "M2"
params_comb <- rbind(params_m1, params_m2)

# plot some out
ggplot(params_comb, aes(x=log(NSP), fill=generating_model))+
  geom_density(alpha=0.5)+
  theme_classic()

ggplot(params_comb, aes(x=MPD, fill=generating_model))+
  geom_density(alpha=0.5)+
  theme_classic()

ggplot(params_comb, aes(x=SDBS, fill=generating_model))+
  geom_density(alpha=0.5)+
  theme_classic()

#### BOOSTED REGRESSION TREES ------------

# seems like the models do produce some different summary statistics, lets dive in a bit further
# for our different summary statistics we might be interested in whether the model parameters have a stronger influence over the variance or if the generating model does
# for this we can repeat the BRT procedure above on the combined data set, this time including the generating model as a predictor variable

# again lets scale the predictors
params_comb_scale <- params_comb
params_comb_scale[, 5:16] <- scale(params_comb_scale[, 5:16])

# make the generating model a factor
params_comb_scale$generating_model <- factor(params_comb_scale$generating_model)

# define predictor vars names
predictor_vars <- c("divergence_threshold",  "sigma", "dispersal_scale", "generating_model")

# set up a new list for the BRT models
var_list_2 <- list()

# run the loop (same as above)
for(i in 1:12) {
  learn_rate <- 0.01
  response_var <- names(params_comb_scale)[5:16][i]
  brt_fit <- NULL
  x_col <- which(names(params_comb_scale)%in% predictor_vars)
  y_col <- which(names(params_comb_scale)==response_var)
  brt_dataset <- params_comb_scale[!is.na(params_comb_scale[,y_col]),]
  brt_fit <- try(gbm.step(learning.rate = learn_rate, data=brt_dataset, gbm.x=x_col, gbm.y=y_col,family="gaussian",tree.complexity=1,n.folds=5,tolerance.method='auto',max.trees=200000))
  while(is.null(brt_fit)){
    learn_rate <- learn_rate-0.001
    brt_fit <- try(gbm.step( learning.rate = learn_rate, data=brt_dataset, gbm.x=x_col, gbm.y=y_col,family="gaussian",tree.complexity=1,n.folds=5,tolerance.method='auto',max.trees=200000))
  }
  var_list_2[[i]] <- brt_fit
}

# create a new data frame
sensitivity_df_2 <- data.frame(var= names(params_comb_scale)[5:16], sigma=NA,  dispersal_scale=NA, divergence_threshold=NA, generating_model=NA, R2=NA)

# get variable contribution and R-squared (same as above)
for(i in 1:12){
  if(class(var_list_2[[i]]) == 'try-error'){next}
  cont <- var_list_2[[i]]$contributions 
  if(is.null(cont)){next}
  sensitivity_df_2$sigma[i]  <- cont[which(cont$var == "sigma"), 2]
  sensitivity_df_2$dispersal_scale[i]  <- cont[which(cont$var == "dispersal_scale"), 2]
  sensitivity_df_2$divergence_threshold[i]  <- cont[which(cont$var == "divergence_threshold"), 2]
  sensitivity_df_2$generating_model[i]  <- cont[which(cont$var == "generating_model"), 2]
  sensitivity_df_2$R2[i]  <- (cor(var_list_2[[i]]$fit, var_list_2[[i]]$data$y))^2
}

# lets plot the results
cont_melt_2 <- melt(sensitivity_df_2[, 1:5])

# plot the variable contributions for each statistic
ggplot(cont_melt_2, aes(x=var, y=value))+
  geom_bar(aes(fill=variable),position="fill", stat="identity")+
  theme(axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme_classic()

# plot the R2 of each summary statistic as  bar plot
ggplot(sensitivity_df_2, aes(x=var, y=R2))+
  geom_bar(stat="identity")+
  theme( axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme_classic()

#### CLASSIFICATION TOOLS ------------

# OK, looks like the generating model is able to explain the most variance in some metrics but not others
# one way to discern between models is using machine classification models, such a linear discriminant analysis or random forests
# below we'll go through ways to fit these classification models using the caret package

# index train and test data
# here we are dividing the dataset into a training (2/3) and testing (1/3) portion
# the models will be fit on the training data and predicted on the testing dataset
# this allows us to cross-validate the predictive power of the models and avoid overfitting

# set the seed for replication
set.seed(666)

# can't use sims with NA values in the metrics, so remove these
params_comb <- na.omit(params_comb) 
params_comb$generating_model <- factor(params_comb$generating_model)
# partition a training (two thirds) and testing (one third) dataset 
train_index <- createDataPartition(params_comb$generating_model, p = .66, list = FALSE,  times = 1)
train_data <- params_comb[ train_index ,]
test_data  <- params_comb[-train_index ,]

# preprocess values - we will scale and center values
preprocessed_values <- preProcess(train_data[5:17], method = c("center", "scale"))
train_transformed   <- predict(preprocessed_values, train_data)
test_transformed    <- predict(preprocessed_values, test_data )

# configure the cross-validation paramaters
# 10-fold cross-validation repeated 10 times
train_control <- trainControl( method = "repeatedcv", number = 10, repeats = 10, classProbs = TRUE, savePredictions = TRUE)

# fit models
# to start we are not going to use all of the biodiversity metrics to illustrate a point
f1 <- formula(paste("generating_model ~ ", paste(names(params_comb)[c(5:11, 14:16)], collapse=" + ")))

# we'll fit four alternative classification models
## LINEAR DISCRIMINANT ANALYSIS
lda_train        <- train(f1, data=train_transformed, method = "lda", trControl = train_control, verbose = T)

## GRADIENT BOOSTED MODEL
gbm_train        <- train(f1, data=train_transformed,  method = "gbm", trControl = train_control, verbose = T)

## RANDOM FOREST
rf_train         <- train(f1, data=train_transformed,  method = "rf", trControl = train_control, verbose = T)

## SUPPORT-VECTOR MACHINES
svmLinear_train  <- train(f1, data=train_transformed,  method = "svmLinear", trControl = train_control, verbose = T)

# use the fit classification models to predict the generating model on the testing data
lda_test        <- predict(lda_train, test_transformed)
rf_test         <- predict(rf_train, test_transformed)
gbm_test        <- predict(gbm_train, test_transformed)
svmLinear_test  <- predict(svmLinear_train, test_transformed)

# confusion matrix is a summary of the number of models which were successfully predicted
# from this we can estimate different metrics of classification accuracy
lda_cm        <- confusionMatrix(data = lda_test, reference = test_transformed$generating_model, mode = "prec_recall")
rf_cm         <- confusionMatrix(data = rf_test, reference = test_transformed$generating_model, mode = "prec_recall")
gbm_cm        <- confusionMatrix(data = gbm_test, reference = test_transformed$generating_model, mode = "prec_recall")
svmLinear_cm  <- confusionMatrix(data = svmLinear_test , reference = test_transformed$generating_model, mode = "prec_recall")

# look at the confusion matrices
rf_cm

# how well did each model perform?
# collect the data from the cross-validation
cvValues <- resamples(list(LDA = lda_train, 
                           RF  = rf_train,    
                           GBM = gbm_train,
                           SVM = svmLinear_train))
# this gives us the mean and standard error of the classification accuracy metrics (Kappa and Accuracy) - so plot it out
dotplot(cvValues, metric="Kappa") 
dotplot(cvValues, metric="Accuracy") 

# which classification models perform best?
# What is the accuracy? Is this reasonable?
# Accuracy is only just above 0.6! remember flipping a coin would give you an accuracy of 0.5!

# looks pretty terrible - seems our summary statistics are not very good at discriminating between models 
# this time lets include some additional summary statistics that we left out last time
f2 <- formula(paste("generating_model ~ ", paste(names(params_comb)[5:16], collapse=" + ")))

## LINEAR DISCRIMINANT ANALYSIS
lda_train_f2        <- train(f2, data=train_transformed, method = "lda", trControl = train_control, verbose = T)

## GRADIENT BOOSTED MODEL
gbm_train_f2        <- train(f2, data=train_transformed,  method = "gbm", trControl = train_control, verbose = T)

## RANDOM FOREST
rf_train_f2         <- train(f2, data=train_transformed,  method = "rf", trControl = train_control, verbose = T)

## SUPPORT-VECTOR MACHINES
svmLinear_train_f2  <- train(f2, data=train_transformed,  method = "svmLinear", trControl = train_control, verbose = T)

#predict on test data
lda_test_f2        <- predict(lda_train_f2, test_transformed)
rf_test_f2         <- predict(rf_train_f2, test_transformed)
gbm_test_f2        <- predict(gbm_train_f2, test_transformed)
svmLinear_test_f2  <- predict(svmLinear_train_f2, test_transformed)

# confusion matrics
lda_cm_f2        <- confusionMatrix(data = lda_test_f2, reference = test_transformed$generating_model, mode = "prec_recall")
rf_cm_f2         <- confusionMatrix(data = rf_test_f2, reference = test_transformed$generating_model, mode = "prec_recall")
gbm_cm_f2        <- confusionMatrix(data = gbm_test_f2, reference = test_transformed$generating_model, mode = "prec_recall")
svmLinear_cm_f2  <- confusionMatrix(data = svmLinear_test_f2 , reference = test_transformed$generating_model, mode = "prec_recall")


# how well did each model perform
cvValues_f2 <- resamples(list(LDA = lda_train_f2, 
                           RF  = rf_train_f2,    
                           GBM = gbm_train_f2,
                           SVM = svmLinear_train_f2))

# plot it out again
dotplot(cvValues_f2, metric="Kappa") 
dotplot(cvValues_f2, metric="Accuracy") 

# wowee - now we have perfect classification accuracy! Outstanding!
# seems like the inclusion of those missing summary metrics adds a lot of discrimination between the models

# Variable importance
lda_varI <- varImp(lda_train_f2, scale = T)
rf_varI <- varImp(rf_train_f2, scale = T)
gbm_varI <- varImp(gbm_train_f2, scale = T)
svmLinear_varI <- varImp(svmLinear_train_f2, scale = T)

# lets plot it out
ggplot(lda_varI, aes(Importance))+
  theme_classic()

ggplot(rf_varI, aes(Importance))+
  theme_classic()

ggplot(gbm_varI, aes(Importance))+
  theme_classic()

ggplot(svmLinear_varI, aes(Importance))+
  theme_classic()

# Which variables were the most important?
# Why might this be the case? can you relate this back to the configs?


########## PART 6: MODEL SELECTION  ############

# ok we've made it to the last part of the practical, woohoo!
# now we're going to perform model selection on some data for which you don't know the generating model
# this is the case with empirical data, but here we'll use pseudo-empirical data (some data I've simulated)

# for reference this how I took the 'empirical data'
#empirical <- rbind(params_comb[c(3, 8, 57, 346, 284, 259),], params_comb[c(1:2),])
#empirical[7, 5:16] <- colMeans(empirical[, 5:16], na.rm=T)
#empirical[8, 5:16] <- apply(empirical[, 5:16], 2, FUN=median, na.rm=T)
#empirical$clade <- c("clade_1", "clade_2", "clade_3", "clade_4", "clade_5", "clade_6", "clade_7", "clade_8")
#empirical <- empirical[, c(17, 5:16)]
#write.csv(empirical, file.path(exp_d, "pseudo_empirical_data.csv"), row.names = FALSE)

# load the data
empirical <- read.csv(file.path(exp_d, "pseudo_empirical_data.csv"))

# just so it matches the simulated daataset column names...
# ...we'll give the clade names column label as generating_model
colnames(empirical)[which(colnames(empirical) %in% "clade")] <- "generating_model"

# gotta scale the predictors in the same units as the simulated data
empirical_transformed <- predict(preprocessed_values, empirical)

# list of models
model_set <- list(lda_train_f2, rf_train_f2, 
                  gbm_train_f2, svmLinear_train_f2)

# predict on empirical
class_predictions   <- predict(model_set, newdata = empirical_transformed, type = "raw", na.action = na.omit)
class_probabilities <- predict(model_set, newdata = empirical_transformed, type = "prob", na.action = na.omit)

# tally the results
class_prediction_table <- lapply(class_predictions, table)


# put it in a data frame
support <- data.frame(clade=empirical_transformed$generating_model,
                      lda=class_predictions[[1]],
                      rf=class_predictions[[2]],
                      gbm=class_predictions[[3]],
                      svl=class_predictions[[4]])

# melt it down
melted_support <- melt(support, id.vars="clade")

# plot it out # this shows number of classifcation models that predict each class
ggplot(melted_support , aes(x=clade, fill=value))+
  geom_bar()+
  theme_classic() 

# plot probablities from the LDA
lda_probs <- class_probabilities[[1]]
lda_probs$clade <- c("clade_1", "clade_2", "clade_3", "clade_4", "clade_5", "clade_6", "clade_7", "clade_8")
lda_melted <- melt(lda_probs)

# plot LDA
ggplot(lda_melted  , aes(x=clade,y=value, fill=variable))+
  geom_bar(stat = "identity")+
  theme_classic() 

# plot probablities from the RF
rf_probs <- class_probabilities[[2]]
rf_probs$clade <- c("clade_1", "clade_2", "clade_3", "clade_4", "clade_5", "clade_6", "clade_7", "clade_8")
rf_melted <- melt(rf_probs)

# plot RF
ggplot(rf_melted  , aes(x=clade,y=value, fill=variable))+
  geom_bar(stat = "identity")+
  theme_classic() 

# plot probablities from the gbm
gbm_probs <- class_probabilities[[4]]
gbm_probs$clade <- c("clade_1", "clade_2", "clade_3", "clade_4", "clade_5", "clade_6", "clade_7", "clade_8")
gbm_melted <- melt(gbm_probs)

# plot GBM
ggplot(gbm_melted  , aes(x=clade,y=value, fill=variable))+
  geom_bar(stat = "identity")+
  theme_classic() 

# plot probablities from the SVM
svl_probs <- class_probabilities[[4]]
svl_probs$clade <- c("clade_1", "clade_2", "clade_3", "clade_4", "clade_5", "clade_6", "clade_7", "clade_8")
svl_melted <- melt(svl_probs)

# plot LDA
ggplot(svl_melted  , aes(x=clade,y=value, fill=variable))+
  geom_bar(stat = "identity")+
  theme_classic()

# ok so we can see that the classification models differ a bit from each other
# which clade's flip around the most?
# what does this mean for interpreting results?