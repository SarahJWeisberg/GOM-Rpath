#Title: GOM Rpath Time Series Fitting

# Purpose: This script fits the Gulf of Maine Rpath model 
#           to time series data from 1985-2019

# DataFiles: 'commercial_landings_gom_80_19.RData'

# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu

# Thu Apr 14 11:29:00 2022 ------------------------------

#Testing with a simple example
#Modeled on Kerim Aydin's code: https://github.com/NOAA-REEM/alaska_ecopath/blob/main/R/EBS_ACLIM_72_fitting_example.r

#Generate balanced model

#Load packages
library(Rpath); library(data.table);library(dplyr);library(here)

#Pull in code from GitHub

#Fill in balanced model
source(here("R/rpath_balance_attempt_3.R"))

#define fit years
fit.years <- 1985:2019

catch.datafile<- read.csv("landings_fit.csv")
#could also accomplish this by running catch_time.R script

# Setup Base Ecopath and Base Rsim scenario
basescene85 <- rsim.scenario(REco, REco.params, years = fit.years) # Ecosim params
scene0 <- basescene85


# Read in fitting data
# Biomass data (e.g. surveys)
#scene0 <- read.fitting.biomass(scene0, biomass.datafile)

# Read time series of catch data and re-arrange catch forcing
scene0 <- read.fitting.catch(scene0, catch.datafile) 
# Apply the fit catch as a forcing catch
scene0 <- fitcatch.to.forcecatch(scene0)
# Turn off fishing effort, freeze discards/offal using forced biomass
scene0 <- adjust.fishing(scene0, "ForcedEffort", rpath.gears(bal), fit.years, value=0.0)
scene0$forcing$ForcedBio[,"Discards.offal"] <- bal$Biomass["Discards.offal"]

