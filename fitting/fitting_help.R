#Title: GOM Rpath Time Series Fitting - Troubleshooting Catch

# Purpose: This script fits the Gulf of Maine Rpath model 
#           to time series data from 1985-2019. Current script
#           is having issues with forcing the catch time series.

# DataFiles: 'commercial_landings_gom_80_19.RData'

# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu

# Thu Jul 20 14:57:16 2023 ------------------------------

#Load packages
library(Rpath); library(data.table);library(dplyr);library(here);library(gtools)

#Pull in code from GitHub
source(here("fitting/ecofitting.R"))

#Load balanced model
load(here("outputs/GOM_Rpath.Rdata"))
load(here("outputs/GOM_params_Rpath.Rdata"))

#define fit years
fit.years <- 1985:2019

catch.datafile<- paste("fitting/landings_fit.csv",sep = "")
#could also accomplish this by running catch_time.R script
biomass.datafile  <- paste("fitting/biomass_fit.csv",sep='')

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
#Turn off catch forcing for groups with high uncertainty
scene0 <- adjust.fishing(scene0, "ForcedCatch", c("SmFlatfishes","OtherShrimps","Mesopelagics"), fit.years, value=0.0)
scene0$fitting$Catch[which(scene0$fitting$Group %in% c("SmFlatfishes","OtherShrimps","Mesopelagics"))]<-0
#scene0 <- adjust.fishing(scene0, "ForcedEffort", rpath.gears(GOM), fit.years, value=1.0)
#scene0$forcing$ForcedBio[,"Discards"] <- GOM$Biomass["Discards"]

# For species without catch, reapply Ecopath F (originally through gears) to ForcedFRate
F_equil <- rowSums(GOM$Landings)  /(GOM$Biomass)  #+ rowSums(GOM$Discards))

Equil_species <- c( "Phytoplankton", "Bacteria", "Microzooplankton", "GelZooplankton", "LgCopepods",
                    "SmCopepods", "Micronekton", "OtherCephalopods", "HMS", "Goosefish", "SeaBirds",
                    "Pinnipeds", "BaleenWhales","Odontocetes")
for (sp in Equil_species){
  scene0 <- adjust.fishing(scene0, 'ForcedFRate', sp, fit.years, value=0)
}

#force lobster migration
scene0<-adjust.forcing(scene0,"ForcedMigrate","AmLobster",sim.year = 1989:2000,bymonth = F,value=-1)
scene0<-adjust.forcing(scene0,"ForcedMigrate","AmLobster",sim.year = 2009:2014,bymonth = F,value=-1.8)
scene0<-adjust.forcing(scene0,"ForcedMigrate","AmLobster",sim.year = 2015:2019,bymonth = F,value=-1.2)

# Run model
run0 <- rsim.run(scene0, method='AB', years=fit.years)


# Some Diagnostics 
rsim.fit.table(scene0,run0)

# Species to test 
test_sp <- c("Haddock", "AmLobster", "Redfish","AtlHerring", "Cusk", "Cod")
index_sp<-c("Goosefish","AmPlaice","SilverHake","AtlHalibut",
            "WitchFlounder","YTFlounder","Fourspot","WinterFlounder")
data_type <- "index"  #"index"

# Set data weightings for all data input low (zeros not allowed)
scene0$fitting$Biomass$wt[] <- 1e-36
scene0$fitting$Catch$wt[]   <- 1e-36
# Set data type for test species
scene0$fitting$Biomass$Type[scene0$fitting$Biomass$Group %in% index_sp] <- data_type
scene0$fitting$Biomass$Type[!(scene0$fitting$Biomass$Group %in% index_sp)] <- "absolute"
# Set data weighting for species to fit
scene0$fitting$Biomass$wt[scene0$fitting$Biomass$Group %in% c("Haddock","Redfish")] <- 1
scene0$fitting$Biomass$wt[scene0$fitting$Biomass$Group %in% c("AtlHerring","Cod","RedHake","Pollock")] <- 0.1
scene0$fitting$Biomass$wt[scene0$fitting$Biomass$Group %in% c("Cusk")] <- 0.5

# all combined
fit_values   <- c(rep(0,length(test_sp)),rep(0,length(test_sp)),rep(0,length(test_sp))) 
#fit_values   <- c(rep(0.2,length(test_sp)),rep(0.02,length(test_sp)),rep(0.02,length(test_sp))) 
fit_species  <- c(test_sp,test_sp,test_sp)
fit_vartype  <- c(rep("mzero",length(test_sp)),
                  rep("predvul",length(test_sp)),
                  rep("preyvul",length(test_sp)))

#Initial fit
fit.initial  <- rsim.fit.run(fit_values, fit_species, fit_vartype, scene0, verbose=T,
                             run_method='AB', years=fit.years)

rsim.plot.biomass(scene0, fit.initial, "AmLobster")
rsim.plot.catch(scene0, fit.initial,"AmLobster")

# Run optimization
fit.optim    <- optim(fit_values, rsim.fit.run, #lower=0, #upper=3, 
                      species=fit_species, vartype=fit_vartype, scene=scene0,   
                      run_method='AB', years=fit.years) 
out_values <- fit.optim$par

#data.frame(fit_vartype,fit_species,fit_values,out_values)
fit.final  <- rsim.fit.run(out_values, fit_species, fit_vartype, scene0, verbose=T,
                           run_method='AB', years=fit.years) 


scene1 <- rsim.fit.update(out_values, fit_species, fit_vartype, scene0)
run1 <- rsim.run(scene1, method='AB', years=fit.years)
rsim.plot(run1)

rsim.fit.table(scene1,run1)
