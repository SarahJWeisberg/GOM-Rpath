#Title: GOM Ecosense

# Purpose: This script uses Ecosense simplified Bayesian synthesis to
#           generate plausible versions of the GOM food web (1980-85)
#           These plausible webs incorporate parameter uncertainty, as
#           determined by data pedigree.

# DataFiles:"GOM_params_Rpath.RData"; "GOM_Rpath.RData"

# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu

# Tue Nov  1 15:39:28 2022 ------------------------------


#Required packages--------------------------------------------------------------
library(here); library(data.table); library(Rpath)

#Source code from sense_beta branch of Rpath Repo
library(devtools)
source_url('https://raw.githubusercontent.com/NOAA-EDAB/Rpath/sense_beta/R/ecosense.R')

#Load balanced model
load(here("outputs/GOM_params_Rpath.RData"))
load(here("outputs/GOM_Rpath.RData"))

#Fix PB/QB pedigree values so that GE cannot >1
#GE<-REco$GE
#limits<-(1-REco.params$pedigree[,QB])/(1+REco.params$pedigree[, PB])
#Identify which groups violate this rule
#which(limits<GE[1:58])

#Manually adjust groups as needed
#Adjust bacteria pedigree
GOM.params$pedigree[2,QB :=0.6]
GOM.params$pedigree[2,PB :=0.6]
#Adjust microzooplankton pedigree
GOM.params$pedigree[3,QB :=0.5]
GOM.params$pedigree[3,PB :=0.6]
#Adjust GelZooplankton pedigree
GOM.params$pedigree[4,QB :=0.6]
GOM.params$pedigree[4,PB :=0.6]
#Adjust LgCopepods pedigree
GOM.params$pedigree[5,QB :=0.5]
GOM.params$pedigree[5,PB :=0.5]
#Adjust SmCopepods pedigree
GOM.params$pedigree[6,QB :=0.6]
GOM.params$pedigree[6,PB :=0.6]
#Adjust Micronekton pedigree
GOM.params$pedigree[7,QB :=0.4]
GOM.params$pedigree[7,PB :=0.5]
#Adjust OtherCeph pedigree
GOM.params$pedigree[10,QB :=0.5]
GOM.params$pedigree[10,PB :=0.6]
#Adjust Macrobenthos pedigree
GOM.params$pedigree[11,QB :=0.7]
#Adjust OceanPout pedigree
GOM.params$pedigree[13,PB :=0.5]
#Adjust SmPelagics pedigree
GOM.params$pedigree[14,QB :=0.4]
GOM.params$pedigree[14,PB :=0.6]
#Adjust SmFlatfishes pedigree
GOM.params$pedigree[15,QB :=0.5]
GOM.params$pedigree[15,PB :=0.6]
#Adjust OtherShrimps pedigree
GOM.params$pedigree[17,QB :=0.5]
GOM.params$pedigree[17,PB :=0.6]
#Adjust OtherPelagics pedigree
GOM.params$pedigree[20,QB :=0.5]
GOM.params$pedigree[20,PB :=0.4]
#Adjust AtlHerring pedigree
GOM.params$pedigree[21,QB :=0.2]
GOM.params$pedigree[21,PB :=0.8]
#Adjust Mesopelagics pedigree
GOM.params$pedigree[22,QB :=0.3]
GOM.params$pedigree[22,PB :=0.5]
#Adjust Cod pedigree
GOM.params$pedigree[24,QB :=0.4]
#Adjust AtlMackerel pedigree
GOM.params$pedigree[27,QB :=0.4]
GOM.params$pedigree[27,PB :=0.7]
#Adjust OtherDemersals pedigree
GOM.params$pedigree[31,QB :=0.3]
GOM.params$pedigree[31,PB :=0.4]
#Adjust HMS pedigree
GOM.params$pedigree[32,PB :=0.4]
#Adjust OtherSkates pedigree
GOM.params$pedigree[33,QB :=0.7]
#Adjust Barndoor pedigree
GOM.params$pedigree[35,QB :=0.2]
GOM.params$pedigree[35,PB :=0.4]
#Adjust Fourspot pedigree
GOM.params$pedigree[36,QB :=0.3]
GOM.params$pedigree[36,PB :=0.4]
#Adjust Pollock pedigree
GOM.params$pedigree[38,QB :=0.7]
GOM.params$pedigree[38,PB :=0.4]
#Adjust Goosefish pedigree
GOM.params$pedigree[39,QB :=0.6]
GOM.params$pedigree[39,PB :=0.2]
#Adjust SilverHake pedigree
GOM.params$pedigree[40,QB :=0.5]
GOM.params$pedigree[40,PB :=0.7]
#Adjust WhiteHake pedigree
GOM.params$pedigree[41,QB :=0.7]
#Adjust SpinyDogfish pedigree
GOM.params$pedigree[42,QB :=0.4]
#Adjust LittleSkate pedigree
GOM.params$pedigree[44,QB :=0.4]
#Adjust WinterFlounder pedigree
GOM.params$pedigree[47,QB :=0.7]
GOM.params$pedigree[47,PB :=0.6]
#Adjust WitchFlounder pedigree
GOM.params$pedigree[48,QB :=0.7]
GOM.params$pedigree[48,PB :=0.7]
#Adjust Sharks pedigree
GOM.params$pedigree[51,QB :=0.6]
GOM.params$pedigree[51,PB :=0.6]
#Adjust Megabenthos pedigree
GOM.params$pedigree[56,QB :=0.4]
GOM.params$pedigree[56,PB :=0.7]

#Adjust SmCopepod biomass pedigree
#For OSM / WARMEM goals
#Keep phyto contant
#REco.params$pedigree[6,Biomass :=0.8]
#REco.params$pedigree[1,Biomass := 0.0]
#REco.params$pedigree[1,PB := 0.0]

#Also try keeping EVERYTHING CONSTANT but copepod groups
#REco.params$pedigree[c(1:4,7:58),Biomass :=0.0]
#REco.params$pedigree[c(1:4,7:58),PB :=0.0]
#REco.params$pedigree[c(1:4,7:58),QB :=0.0]
#REco.params$pedigree[c(1:4,7:58),Diet :=0.0]

#Set up sense runs
all_years <- 1:50
scene <- rsim.scenario(GOM, GOM.params, years = all_years)
orig.biomass<-scene$start_state$Biomass

# ----- Set up ecosense generator ----- #######################################
scene$params$BURN_YEARS <- 50
NUM_RUNS <- 7700
parlist <- as.list(rep(NA, NUM_RUNS))
kept <- rep(NA, NUM_RUNS)

set.seed(19)
for (irun in 1:NUM_RUNS){
  GOMsense <- copy(scene)
  # INSERT SENSE ROUTINE BELOW
  parlist[[irun]] <- GOMsense$params 		# Base ecosim params
  parlist[[irun]] <- rsim.sense(GOMsense, GOM.params)	# Replace the base params with Ecosense params  
  #GOMsense$start_state$Biomass <- parlist[[irun]]$B_BaseRef #took out this line on May 2, 2022
  parlist[[irun]]$BURN_YEARS <- 50			# Set Burn Years to 50
  GOMsense$params <- parlist[[irun]]
  GOMtest <- rsim.run(GOMsense, method = "RK4", years = all_years)
  failList <- which((is.na(GOMtest$end_state$Biomass) | GOMtest$end_state$Biomass/orig.biomass > 1000 | GOMtest$end_state$Biomass/orig.biomass < 1/1000))
  {if (length(failList)>0)
  {cat(irun,": fail in year ",GOMtest$crash_year,": ",failList,"\n"); kept[irun] <- F; flush.console()}
    else 
    {cat(irun,": success!\n"); kept[irun]<-T;  flush.console()}}
  parlist[[irun]]$BURN_YEARS <- 1
}

# KEPT tells you which ecosystems were kept
KEPT <- which(kept==T)
nkept <- length(KEPT)
nkept
GOM_sense <- parlist[KEPT]

save(GOM_sense, file = "outputs/GOM_sense.RData")

