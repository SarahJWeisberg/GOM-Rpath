#Title: GOM Rpath Time Series Fitting

# Purpose: This script fits the Gulf of Maine Rpath model 
#           to time series data from 1985-2019

# DataFiles: 'commercial_landings_gom_80_19.RData'

# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu

# Tue Sep 27 14:50:13 2022 ------------------------------

#Testing with a simple example
#Modeled on Kerim Aydin's code: https://github.com/NOAA-REEM/alaska_ecopath/blob/main/R/EBS_ACLIM_72_fitting_example.r


#Load packages
library(Rpath); library(data.table);library(dplyr);library(here)

#Pull in code from GitHub
library(devtools)
source_url('https://github.com/NOAA-EDAB/Rpath/blob/fit_alpha/R/ecofitting.R')

#Load balanced model
load(here("fitting/GOM_Rpath.Rdata"))
load(here("fitting/GOM_params_Rpath.Rdata"))
source(here("fitting/catch_time.R"))

#define fit years
fit.years <- 1985:2019

catch.datafile<- paste("landings_fit.csv",sep = "")
#could also accomplish this by running catch_time.R script
biomass.datafile  <- paste("biomass_fit.csv",sep='')

# Setup Base Ecopath and Base Rsim scenario
basescene85 <- rsim.scenario(GOM, GOM.params, years = fit.years)
basescene85$params$NoIntegrate[4:5]<-0# Manually change NoIntegrate flags so I can use AB method
scene0 <- basescene85

# Read in fitting data
# Biomass data (e.g. surveys)
scene0 <- read.fitting.biomass(scene0, biomass.datafile)

# Read time series of catch data and re-arrange catch forcing
scene0 <- read.fitting.catch(scene0, catch.datafile) 
# Apply the fit catch as a forcing catch
scene0 <- fitcatch.to.forcecatch(scene0)
# Turn off fishing effort, freeze discards/offal using forced biomass
scene0 <- adjust.fishing(scene0, "ForcedEffort", rpath.gears(GOM), fit.years, value=0.0)
scene0$forcing$ForcedBio[,"Discards"] <- GOM$Biomass["Discards"]

# For species without catch, reapply Ecopath F (originally through gears) to ForcedFRate
F_equil <- (rowSums(GOM$Landings) + rowSums(GOM$Discards))/(GOM$Biomass) 
Equil_species <- GOM.groups[!RPATH %in% spp.land$Group]
#Equil_species <- rbind(Equil_species,d.d)
for (sp in Equil_species){
  scene0 <- adjust.fishing(scene0, 'ForcedFRate', sp, fit.years, value=F_equil[sp])
}
#doing it this way is causing issues - getting NAs for some groups in the middle
#of the time series
#putting in all 0s --> not sure where this is coming from
for (sp in Equil_species){
  scene0 <- adjust.fishing(scene0, 'ForcedFRate', sp, fit.years, value=0)
}

# Run model
run0 <- rsim.run(scene0, method='AB', years=fit.years)

# Some Diagnostics 
rsim.fit.table(scene0,run0)

# Species to test 
test_sp <- "Haddock"
data_type <- "index"  #"absolute"
# Set data weightings for all data input low (zeros not allowed)
scene0$fitting$Biomass$wt[] <- 1e-36
scene0$fitting$Catch$wt[]   <- 1e-36
# Set data type for test species
scene0$fitting$Biomass$Type[scene0$fitting$Biomass$Group =="Haddock"] <- data_type
# Set data weighting for species to fit to 1
scene0$fitting$Biomass$wt[scene0$fitting$Biomass$Group =="Haddock"]   <- 1

# all combined
fit_values   <- c(rep(0,length(test_sp)),rep(0,length(test_sp)),rep(0,length(test_sp))) 
#fit_values   <- c(rep(0.2,length(test_sp)),rep(0.02,length(test_sp)),rep(0.02,length(test_sp))) 
fit_species  <- c(test_sp,test_sp,test_sp)
fit_vartype  <- c(rep("mzero",length(test_sp)),
                  rep("predvul",length(test_sp)),
                  rep("preyvul",length(test_sp)))

# Core fitting procedure here
data.frame(fit_vartype,fit_species,fit_values)

fit.initial  <- rsim.fit.run(fit_values, fit_species, fit_vartype, scene0, verbose=T,
                             run_method='AB', years=fit.years)
rsim.plot.biomass(scene0, fit.initial, test_sp)

# Run optimization
fit.optim    <- optim(fit_values, rsim.fit.run, #lower=0, #upper=3, 
                      species=fit_species, vartype=fit_vartype, scene=scene0,   
                      run_method='AB', years=fit.years) 
out_values <- fit.optim$par

data.frame(fit_vartype,fit_species,fit_values,out_values)
fit.final  <- rsim.fit.run(out_values, fit_species, fit_vartype, scene0, verbose=T,
                           run_method='AB', years=fit.years) 
rsim.plot.biomass(scene0, fit.final, test_sp)


scene1 <- rsim.fit.update(out_values, fit_species, fit_vartype, scene0)
run1 <- rsim.run(scene1, method='AB', years=fit.years)

