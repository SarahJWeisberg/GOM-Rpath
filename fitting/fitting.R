#Title: GOM Rpath Time Series Fitting

# Purpose: This script fits the Gulf of Maine Rpath model 
#           to time series data from 1985-2019

# DataFiles: 'commercial_landings_gom_80_19.RData'

# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu

# Tue Jul 16 09:14:03 2024 ------------------------------



# #Load packages and initial model ----------------------------------------
remotes::install_github('NOAA-EDAB/Rpath', ref='fit_beta', force = T)
library(Rpath); library(data.table);library(dplyr);library(here)

#Load balanced model
load(here("outputs/GOM_Rpath.Rdata"))
load(here("outputs/GOM_params_Rpath.Rdata"))


# Set up fitting, input time series -----------------------------------------
#define fit years
fit.years <- 1985:2019

catch.datafile<- paste("fitting/landings_fit.csv",sep = "")
#could also accomplish this by running catch_time.R script
biomass.datafile  <- paste("fitting/biomass_fit.csv",sep='')

# Setup Base Ecopath and Base Rsim scenario
basescene85 <- rsim.scenario(GOM, GOM.params, years = fit.years)
basescene85$params$NoIntegrate[4:5]<-0 #Manually change NoIntegrate flags so I can use AB method
#These are LgCopepods and GelZooplankton 
#they don't get caught in the algorithmic check for non-integration and biomass blows up if not changed
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
#Turn off catch forcing for groups with high uncertainty
scene0 <- adjust.fishing(scene0, "ForcedCatch", c("SmFlatfishes","OtherShrimps","Mesopelagics"), fit.years, value=0.0)
scene0$fitting$Catch[which(scene0$fitting$Group %in% c("SmFlatfishes","OtherShrimps","Mesopelagics"))]<-0

# For species without catch, reapply Ecopath F (originally through gears) to ForcedFRate
F_equil <- rowSums(GOM$Landings)  /(GOM$Biomass)  #+ rowSums(GOM$Discards))

Equil_species <- c( "Phytoplankton", "Bacteria", "Microzooplankton", "GelZooplankton", "LgCopepods",
                    "SmCopepods", "Micronekton", "OtherCephalopods", "HMS", "Goosefish", "SeaBirds",
                    "Pinnipeds", "BaleenWhales","Odontocetes")
for (sp in Equil_species){
  scene0 <- adjust.fishing(scene0, 'ForcedFRate', sp, fit.years, value=0)
}


# Add lower trophic level forcing -----------------------------------------
#force phytoplankton biomass from 1998 onwards
source(here("fitting/Phyto_time.R"))
pp_force <- pp_force %>% filter(Year < 2020)
scene0<-adjust.forcing(scene0,"ForcedBio","Phytoplankton",sim.year = 1998:2019,bymonth = F,value=pp_force$force_b)

#force sm and lg copepod biomass
source(here("fitting/copepods_time.R"))
scene0<-adjust.forcing(scene0,"ForcedBio","SmCopepods",sim.year = sm$Year,bymonth = F,value=sm$force_b)
scene0<-adjust.forcing(scene0,"ForcedBio","LgCopepods",sim.year = lg$Year,bymonth = F,value=lg$force_b)

#Try adding forced migration terms for lobster
#force lobster migration
# # scene0<-adjust.forcing(scene0,"ForcedMigrate","AmLobster",sim.year = 1989:2000,bymonth = F,value=-1)
scene0<-adjust.forcing(scene0,"ForcedMigrate","AmLobster",sim.year = 2009:2014,bymonth = F,value=-0.7)
scene0<-adjust.forcing(scene0,"ForcedMigrate","AmLobster",sim.year = 2015:2019,bymonth = F,value=-0.2)


# Initial fitting - base case ---------------------------------------------

# Run model
run0 <- rsim.run(scene0, method='AB', years=fit.years)

# Some Diagnostics 
rsim.fit.table(scene0,run0)

# Species to test 
# These are the ones whose parameters will vary
test_sp <- c("Redfish","AtlHerring", "Cod","AmLobster")
#Species with trends treated as indexes rather than absolute values
#I've found that fitting benthic species as index helps match the observations more closely
index_sp<-c("Goosefish","AmPlaice","AtlHalibut",
            "WitchFlounder","YTFlounder","Fourspot","WinterFlounder")
data_type <- "index"  #"absolute"

# Set data weightings for all data input low (zeros not allowed)
scene0$fitting$Biomass$wt[] <- 1e-36
scene0$fitting$Catch$wt[]   <- 1e-36
# Set data type for test species
scene0$fitting$Biomass$Type[scene0$fitting$Biomass$Group %in% index_sp] <- data_type
scene0$fitting$Biomass$Type[!(scene0$fitting$Biomass$Group %in% index_sp)] <- "absolute"
# Set data weighting for species to fit
# Can also penalize outputs for not matching trends of other species 
scene0$fitting$Biomass$wt[scene0$fitting$Biomass$Group %in% c("Redfish","AtlHerring","Cod","AmLobster")] <- 1

# Choosing parameters to change
fit_values   <- c(rep(0,length(test_sp)),rep(0,length(test_sp)),rep(0,length(test_sp))) 
fit_species  <- c(test_sp,test_sp,test_sp)
fit_vartype  <- c(rep("mzero",length(test_sp)),
                  rep("predvul",length(test_sp)),
                  rep("preyvul",length(test_sp)))

#Initial fit - before changes are made
fit.initial  <- rsim.fit.run(fit_values, fit_species, fit_vartype, scene0, verbose=T,
                             run_method='AB', years=fit.years)
par(mfrow= c(2,2))
for (i in 1:length(test_sp)){
  rsim.plot.biomass(scene0, fit.initial, test_sp[i])
  #rsim.plot.catch(scene0, fit.initial, test_sp[i])
}

# Run optimization
fit.optim    <- optim(fit_values, rsim.fit.run, #lower=0, #upper=3, 
                      species=fit_species, vartype=fit_vartype, scene=scene0,   
                      run_method='AB', years=fit.years) 
out_values <- fit.optim$par

fit0<-data.frame(fit_vartype,fit_species,fit_values,out_values)
fit.final  <- rsim.fit.run(out_values, fit_species, fit_vartype, scene0, verbose=T,
                           run_method='AB', years=fit.years) 
par(mfrow= c(2,2))
for (i in 1:length(test_sp)){
  rsim.plot.biomass(scene0, fit.final, test_sp[i])
  #rsim.plot.catch(scene0, fit.final, test_sp[i])
}

dem_sp<-c("SilverHake","RedHake","Pollock","AmLobster")
for (i in 1:length(dem_sp)){
  rsim.plot.biomass(scene0, fit.final, dem_sp[i])
  rsim.plot.catch(scene0, fit.final, dem_sp[i])
}

#Save as pdf as better way to see all fits
#pdf(file = "fitting/Plots/Fits_july_2024.pdf")
par(mfrow= c(3,2))
for (i in 1:length(GOM.params$model$Group[1:57])){
  rsim.plot.biomass(scene0, fit.final, GOM.params$model$Group[i])
  #rsim.plot.catch(scene0, fit.final, GOM.params$model$Group[i])
}
#dev.off()

#Save fits as new rsim run
scene_fit0 <- rsim.fit.update(out_values, fit_species, fit_vartype, scene0)
run_fit0 <- rsim.run(scene_fit0, method='AB', years=fit.years)


# Experiments -------------------------------------------------------------

# Try adding benthic group forcing ---------------------------------------
#force macro and megabenthos biomass
scene1<-scene0
source(here("fitting/benthic_time.R"))
scene1<-adjust.forcing(scene1,"ForcedBio","Macrobenthos",sim.year = macro_time$Time,bymonth = F,value=macro_time$biomass)
scene1<-adjust.forcing(scene1,"ForcedBio","Megabenthos",sim.year = mega_time$Time,bymonth = F,value=mega_time$biomass)

# Choosing parameters to change
fit_values   <- c(rep(0,length(test_sp)),rep(0,length(test_sp)),rep(0,length(test_sp))) 
fit_species  <- c(test_sp,test_sp,test_sp)
fit_vartype  <- c(rep("mzero",length(test_sp)),
                  rep("predvul",length(test_sp)),
                  rep("preyvul",length(test_sp)))
#Initial fit - before changes are made
fit.initial  <- rsim.fit.run(fit_values, fit_species, fit_vartype, scene1, verbose=T,
                             run_method='AB', years=fit.years)
par(mfrow= c(2,2))
for (i in 1:length(test_sp)){
  rsim.plot.biomass(scene1, fit.initial, test_sp[i])
  #rsim.plot.catch(scene1, fit.initial, test_sp[i])
}

# Run optimization
fit.optim    <- optim(fit_values, rsim.fit.run, #lower=0, #upper=3, 
                      species=fit_species, vartype=fit_vartype, scene=scene1,   
                      run_method='AB', years=fit.years) 
out_values <- fit.optim$par

fit1<-data.frame(fit_vartype,fit_species,fit_values,out_values)
fit.final  <- rsim.fit.run(out_values, fit_species, fit_vartype, scene1, verbose=T,
                           run_method='AB', years=fit.years) 
par(mfrow= c(2,2))
for (i in 1:length(test_sp)){
  rsim.plot.biomass(scene1, fit.final, test_sp[i])
  #rsim.plot.catch(scene1, fit.final, test_sp[i])
}

#Save as pdf as better way to see all fits
#pdf(file = "fitting/Plots/Fits_july_2024.pdf")
par(mfrow= c(3,2))
for (i in 1:length(GOM.params$model$Group[1:57])){
  rsim.plot.biomass(scene1, fit.final, GOM.params$model$Group[i])
  rsim.plot.catch(scene1, fit.final, GOM.params$model$Group[i])
}
#dev.off()

#Save fits as new rsim run
scene_fit1 <- rsim.fit.update(out_values, fit_species, fit_vartype, scene1)
run_fit1 <- rsim.run(scene_fit1, method='AB', years=fit.years)


# Try adding bioenergetic forcing ----------------------------------------
#using monthly bottom temp values from GLORYS
scene2<-scene0
source(here("fitting/act_resp_force.R"))
#first 8 years (prior to 1993 = start of GLORYS data) have no forcing
for (i in 1:length(cons$RPATH)){
  group<-cons_groups$RPATH[i]
  cons_time_group<- cons_time_fixed %>% filter (RPATH == group)
  index<-as.numeric(unique(cons_time_group$index)) + 1 #add 1 because of Outside
  force_act_resp <- c(rep(1,8*12),cons_time_group$reL_act_resp)
  #adjust forcing
  scene2$forcing$ForcedActresp[,index]<-force_act_resp
}

#Initial fit - before changes are made
fit.initial  <- rsim.fit.run(fit_values, fit_species, fit_vartype, scene2, verbose=T,
                             run_method='AB', years=fit.years)
par(mfrow= c(2,2))
for (i in 1:length(test_sp)){
  rsim.plot.biomass(scene2, fit.initial, test_sp[i])
  #rsim.plot.catch(scene2, fit.initial, test_sp[i])
}

# Run optimization
fit.optim    <- optim(fit_values, rsim.fit.run, #lower=0, #upper=3, 
                      species=fit_species, vartype=fit_vartype, scene=scene2,   
                      run_method='AB', years=fit.years) 
out_values <- fit.optim$par

fit2<-data.frame(fit_vartype,fit_species,fit_values,out_values)
fit.final  <- rsim.fit.run(out_values, fit_species, fit_vartype, scene2, verbose=T,
                           run_method='AB', years=fit.years) 
par(mfrow= c(2,2))
for (i in 1:length(test_sp)){
  rsim.plot.biomass(scene2, fit.final, test_sp[i])
  #rsim.plot.catch(scene2, fit.final, test_sp[i])
}

#Save as pdf as better way to see all fits
#pdf(file = "fitting/Plots/Fits_july_2024.pdf")
par(mfrow= c(3,2))
for (i in 1:length(GOM.params$model$Group[1:57])){
  rsim.plot.biomass(scene2, fit.final, GOM.params$model$Group[i])
  rsim.plot.catch(scene2, fit.final, GOM.params$model$Group[i])
}
#dev.off()

#Save fits as new rsim run
scene_fit2 <- rsim.fit.update(out_values, fit_species, fit_vartype, scene0)
run_fit2 <- rsim.run(scene_fit2, method='AB', years=fit.years)


# Try without changing M0 -------------------------------------------------
#need to add more lobster forcing
#scene0<-adjust.forcing(scene0,"ForcedMigrate","AmLobster",sim.year = 1989:2000,bymonth = F,value=-0.5)
scene0<-adjust.forcing(scene0,"ForcedMigrate","AmLobster",sim.year = 2009:2014,bymonth = F,value=-1.2)
scene0<-adjust.forcing(scene0,"ForcedMigrate","AmLobster",sim.year = 2015:2019,bymonth = F,value=-0.7)

# Choosing parameters to change
fit_values   <- c(rep(0,length(test_sp)),rep(0,length(test_sp))) 
fit_species  <- c(test_sp,test_sp)
fit_vartype  <- c(rep("predvul",length(test_sp)),
                  rep("preyvul",length(test_sp)))

# Run optimization
fit.optim    <- optim(fit_values, rsim.fit.run, #lower=0, #upper=3, 
                      species=fit_species, vartype=fit_vartype, scene=scene0,   
                      run_method='AB', years=fit.years) 
out_values <- fit.optim$par

fit3<-data.frame(fit_vartype,fit_species,fit_values,out_values)
fit.final  <- rsim.fit.run(out_values, fit_species, fit_vartype, scene0, verbose=T,
                           run_method='AB', years=fit.years) 
par(mfrow= c(2,2))
for (i in 1:length(test_sp)){
  rsim.plot.biomass(scene0, fit.final, test_sp[i])
  #rsim.plot.catch(scene0, fit.final, test_sp[i])
}

#Save as pdf as better way to see all fits
#pdf(file = "fitting/Plots/Fits_july_2024.pdf")
par(mfrow= c(3,2))
for (i in 1:length(GOM.params$model$Group[1:57])){
  rsim.plot.biomass(scene0, fit.final, GOM.params$model$Group[i])
  rsim.plot.catch(scene0, fit.final, GOM.params$model$Group[i])
}
#dev.off()

#Save fits as new rsim run
scene_fit3 <- rsim.fit.update(out_values, fit_species, fit_vartype, scene0)
run_fit3 <- rsim.run(scene_fit3, method='AB', years=fit.years)

