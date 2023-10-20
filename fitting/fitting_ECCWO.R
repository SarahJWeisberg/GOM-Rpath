#Title: GOM Rpath Time Series Fitting

# Purpose: This script fits the Gulf of Maine Rpath model 
#           to time series data from 1985-2019

# DataFiles: 'commercial_landings_gom_80_19.RData'

# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu

# Thu Jan 26 14:31:10 2023 ------------------------------


#Testing with a simple example
#Modeled on Kerim Aydin's code: https://github.com/NOAA-REEM/alaska_ecopath/blob/main/R/EBS_ACLIM_72_fitting_example.r


#Load packages
library(Rpath); library(data.table);library(dplyr);library(here);library(gtools)

#Pull in code from GitHub
#library(devtools)
#source_url('https://github.com/NOAA-EDAB/Rpath/blob/fit_alpha/R/ecofitting.R')
source(here("fitting/ecofitting.R"))

#Load balanced model
load(here("outputs/GOM_Rpath.Rdata"))
load(here("outputs/GOM_params_Rpath.Rdata"))
source(here("fitting/catch_time.R"))

#define fit years
fit.years <- 1985:2019

catch.datafile<- paste("fitting/landings_fit.csv",sep = "")
#could also accomplish this by running catch_time.R script
biomass.datafile  <- paste("fitting/biomass_fit.csv",sep='')
#make biomass accumulation adjustments
#source(here("fitting/ba_adjust.R"))

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

#run active respiration forcing
source(here("fitting/act_resp_force.R"))

# For species without catch, reapply Ecopath F (originally through gears) to ForcedFRate
F_equil <- rowSums(GOM$Landings)  /(GOM$Biomass)  #+ rowSums(GOM$Discards))
          
Equil_species <- GOM.groups[!RPATH %in% spp.land$Group]
#Equil_species <- rbind(Equil_species,d.d)
#for (sp in Equil_species){
 # scene0 <- adjust.fishing(scene0, 'ForcedFRate', sp, fit.years, value=F_equil[sp])
#}
#doing it this way is causing issues - getting NAs for some groups in the middle
#of the time series
#putting in all 0s --> not sure where this is coming from
for (sp in Equil_species){
  scene0 <- adjust.fishing(scene0, 'ForcedFRate', sp, fit.years, value=0)
}


#add pp biomass forcing
#data from Phyto_time.R script
source(here("fitting/Phyto_time.R"))
scene0<-adjust.forcing(scene0,'ForcedBio','Phytoplankton',sim.year = 2001:2019,value = pp_force$force_b[1:19])
# 
# #add copepod biomass forcing
#toning down LgCopes bump
#ts$force_b[15:16]<-c(24,19)
# #from copes_time.Rmd script -- needs tidying
scene0<-adjust.forcing(scene0,'ForcedBio','LgCopepods',sim.year = 1987:2019,value = lg$force_b)
scene0<-adjust.forcing(scene0,'ForcedBio','SmCopepods',sim.year = 1987:2019,value = sm$force_b)

 
# #force lobster biomass
 lobster<-scene0$fitting$Biomass %>% filter(Group == "AmLobster")
 scene0<-adjust.forcing(scene0,'ForcedBio','AmLobster',sim.year = 1985:2019,value = lobster$Value)

#force lobster migration
#scene0<-adjust.forcing(scene0,"ForcedMigrate","AmLobster",sim.year = 1989:2000,bymonth = T,value=-2)

#force shrimp biomss
# shrimp<-scene0$fitting$Biomass %>% filter(Group == "NShrimp")
# scene0<-adjust.forcing(scene0,'ForcedBio','NShrimp',sim.year = 1986:2018,value = shrimp$Value)

#force cusk biomass
# cusk<-scene0$fitting$Biomass %>% filter(Group == "Cusk")
# scene0<-adjust.forcing(scene0,'ForcedBio','Cusk',sim.year = 1985:2019,value = cusk$Value)
# 
# #force herring biomass
# #try just for the last few years
# herring<-scene0$fitting$Biomass %>% filter(Group == "AtlHerring")
# # herring$rescaled<-herring$Value/10
# # #read in data from stock assessment
# # herring_sa<-read.csv(here("fitting/Herring_Stock_Assessment.csv"))
# # herring_sa<-na.omit(herring_sa) %>% filter(Year <2020)
# # herring$Year<-as.numeric(herring$Year)
# # 
# # herring_scaled<-c(herring$rescaled,herring_sa$scaled_biomass,test_run)
# # herring_scaled<-as.data.frame(cbind(herring_scaled,rep(1985:2019,3)))
# # colnames(herring_scaled)<-c("Biomass","Year")
# # herring_scaled$source<-c(rep("survey",35),rep("sa",35),rep("survey_runmean",35))
# # 
# # ggplot(data=herring_scaled, aes(x=Year,y=Biomass, color=source))+
# #   geom_line()
# # 
# 
# herring$runmean<-caTools::runmean(x=herring$Value,k=2)
# 
# scene0<-adjust.forcing(scene0,'ForcedBio','AtlHerring',sim.year = 2010:2019,value = herring$runmean[26:35])
# scene0$fitting$Biomass$Value[scene0$fitting$Biomass$Group =="AtlHerring"] <-herring$rescaled

# 
# #force mackerel biomass
# mackerel<-scene0$fitting$Biomass %>% filter(Group == "AtlMackerel")
# mackerel$runmean<-caTools::runmean(x=mackerel$Value,k=5)
# scene0<-adjust.forcing(scene0,'ForcedBio','AtlMackerel',sim.year = 1985:2019,value = mackerel$runmean)
# 
# #force river herring biomass
# river_herring<-scene0$fitting$Biomass %>% filter(Group == "RiverHerring")
# river_run<-caTools::runmean(x=river_herring$Value,k=5)
# river_herring$runmean<-river_run
# scene0<-adjust.forcing(scene0,'ForcedBio','RiverHerring',sim.year = 1985:2019,value = river_herring$runmean)
# 
# #force spiny
# #BA seems too high
# spiny<-scene0$fitting$Biomass %>% filter(Group == "SpinyDogfish")
# scene0<-adjust.forcing(scene0,'ForcedBio','SpinyDogfish',sim.year = 1985:2019,value = spiny$Value)
#force scallop biomass
#scallop<-scene0$fitting$Biomass %>% filter(Group == "AtlScallop")
#scene0<-adjust.forcing(scene0,'ForcedBio','AtlScallop',sim.year = 1985:2019,value = scallop$Value)

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

#force lobster migration
# scene0<-adjust.forcing(scene0,"ForcedMigrate","AmLobster",sim.year = 1989:2000,bymonth = F,value=-1)
# scene0<-adjust.forcing(scene0,"ForcedMigrate","AmLobster",sim.year = 2009:2014,bymonth = F,value=-1.8)
# scene0<-adjust.forcing(scene0,"ForcedMigrate","AmLobster",sim.year = 2015:2019,bymonth = F,value=-1.2)
# # Core fitting procedure here
#data.frame(fit_vartype,fit_species,fit_values)

fit.initial  <- rsim.fit.run(fit_values, fit_species, fit_vartype, scene0, verbose=T,
                             run_method='AB', years=fit.years)

# rsim.plot.biomass(scene0, fit.initial, "AmLobster")
# rsim.plot.catch(scene0, fit.initial,"AmLobster")

# Run optimization
fit.optim    <- optim(fit_values, rsim.fit.run, #lower=0, #upper=3, 
                      species=fit_species, vartype=fit_vartype, scene=scene0,   
                      run_method='AB', years=fit.years) 
out_values <- fit.optim$par

#data.frame(fit_vartype,fit_species,fit_values,out_values)
fit.final  <- rsim.fit.run(out_values, fit_species, fit_vartype, scene0, verbose=T,
                           run_method='AB', years=fit.years) 

for (i in 1:length(test_sp)){
  rsim.plot.biomass(scene0, fit.final, test_sp[i])
}
for (i in 1:length(plots)){
  rsim.plot.biomass(scene0, fit.final, plots[i])
}


#pdf(file = "test.pdf")
#par( mfrow= c(3,2) )
for (i in 1:length(cons_groups$RPATH)){
  rsim.plot.biomass(scene0, fit.final, cons_groups$RPATH[i])
}

for (i in 1:length(GOM.groups$RPATH)){
  rsim.plot.catch(scene0, fit.final, GOM.groups$RPATH[i])
}
#dev.off()
rsim.plot.biomass(scene0, fit.final, "Phytoplankton")

plots<- c("Haddock", "Redfish","AtlHerring", "Cusk", "Cod","SmCopepods","LgCopepods")


#rsim.plot.biomass(scene0, fit.final, test_sp[1])

rsim.fit.obj(scene0,fit.final)$tot


scene1 <- rsim.fit.update(out_values, fit_species, fit_vartype, scene0)
run1 <- rsim.run(scene1, method='AB', years=fit.years)
rsim.plot(run1)

rsim.fit.table(scene1,run1)
