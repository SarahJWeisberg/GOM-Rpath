#Gulf of Maine Rpath Ecosense
#SJW

#Required packages--------------------------------------------------------------
library(here); library(data.table); library(Rpath)

#Source code from sense_beta branch of Rpath Repo
library(devtools)
source_url('https://raw.githubusercontent.com/NOAA-EDAB/Rpath/sense_beta/R/ecosense.R')

#Load and balance model
source(here('R/rpath_balance_attempt_3.R'))

#Fix PB/QB pedigree values so that GE cannot >1
#GE<-REco$GE
#limits<-(1-REco.params$pedigree[,QB])/(1+REco.params$pedigree[, PB])
#Identify which groups violate this rule
#which(limits<GE[1:58])

#Manually adjust groups as needed
#Adjust bacteria pedigree
REco.params$pedigree[2,QB :=0.6]
REco.params$pedigree[2,PB :=0.6]
#Adjust microzooplankton pedigree
REco.params$pedigree[3,QB :=0.5]
REco.params$pedigree[3,PB :=0.6]
#Adjust GelZooplankton pedigree
REco.params$pedigree[4,QB :=0.6]
REco.params$pedigree[4,PB :=0.6]
#Adjust LgCopepods pedigree
REco.params$pedigree[5,QB :=0.5]
REco.params$pedigree[5,PB :=0.5]
#Adjust SmCopepods pedigree
REco.params$pedigree[6,QB :=0.6]
REco.params$pedigree[6,PB :=0.6]
#Adjust Micronekton pedigree
REco.params$pedigree[7,QB :=0.4]
REco.params$pedigree[7,PB :=0.5]
#Adjust OtherCeph pedigree
REco.params$pedigree[10,QB :=0.5]
REco.params$pedigree[10,PB :=0.6]
#Adjust Macrobenthos pedigree
REco.params$pedigree[11,QB :=0.7]
#Adjust OceanPout pedigree
REco.params$pedigree[13,PB :=0.5]
#Adjust SmPelagics pedigree
REco.params$pedigree[14,QB :=0.4]
REco.params$pedigree[14,PB :=0.6]
#Adjust SmFlatfishes pedigree
REco.params$pedigree[15,QB :=0.5]
REco.params$pedigree[15,PB :=0.6]
#Adjust OtherShrimps pedigree
REco.params$pedigree[17,QB :=0.5]
REco.params$pedigree[17,PB :=0.6]
#Adjust OtherPelagics pedigree
REco.params$pedigree[20,QB :=0.5]
REco.params$pedigree[20,PB :=0.4]
#Adjust AtlHerring pedigree
REco.params$pedigree[21,QB :=0.2]
REco.params$pedigree[21,PB :=0.8]
#Adjust Mesopelagics pedigree
REco.params$pedigree[22,QB :=0.3]
REco.params$pedigree[22,PB :=0.5]
#Adjust Cod pedigree
REco.params$pedigree[24,QB :=0.4]
#Adjust AtlMackerel pedigree
REco.params$pedigree[27,QB :=0.4]
REco.params$pedigree[27,PB :=0.7]
#Adjust OtherDemersals pedigree
REco.params$pedigree[31,QB :=0.3]
REco.params$pedigree[31,PB :=0.4]
#Adjust HMS pedigree
REco.params$pedigree[32,PB :=0.4]
#Adjust OtherSkates pedigree
REco.params$pedigree[33,QB :=0.7]
#Adjust Barndoor pedigree
REco.params$pedigree[35,QB :=0.2]
REco.params$pedigree[35,PB :=0.4]
#Adjust Fourspot pedigree
REco.params$pedigree[36,QB :=0.3]
REco.params$pedigree[36,PB :=0.4]
#Adjust Pollock pedigree
REco.params$pedigree[38,QB :=0.7]
REco.params$pedigree[38,PB :=0.4]
#Adjust Goosefish pedigree
REco.params$pedigree[39,QB :=0.6]
REco.params$pedigree[39,PB :=0.2]
#Adjust SilverHake pedigree
REco.params$pedigree[40,QB :=0.5]
REco.params$pedigree[40,PB :=0.7]
#Adjust WhiteHake pedigree
REco.params$pedigree[41,QB :=0.7]
#Adjust SpinyDogfish pedigree
REco.params$pedigree[42,QB :=0.4]
#Adjust LittleSkate pedigree
REco.params$pedigree[44,QB :=0.4]
#Adjust WinterFlounder pedigree
REco.params$pedigree[47,QB :=0.7]
REco.params$pedigree[47,PB :=0.6]
#Adjust WitchFlounder pedigree
REco.params$pedigree[48,QB :=0.7]
REco.params$pedigree[48,PB :=0.7]
#Adjust Sharks pedigree
REco.params$pedigree[51,QB :=0.6]
REco.params$pedigree[51,PB :=0.6]
#Adjust Megabenthos pedigree
REco.params$pedigree[56,QB :=0.4]
REco.params$pedigree[56,PB :=0.7]


#Adjust SmCopepod biomass pedigree
#For OSM / WARMEM goals
#Keep phyto contant
REco.params$pedigree[6,Biomass :=0.8]
REco.params$pedigree[1,Biomass := 0.0]
REco.params$pedigree[1,PB := 0.0]

#Also try keeping EVERYTHING CONSTANT but copepod groups
REco.params$pedigree[c(1:4,7:58),Biomass :=0.0]
REco.params$pedigree[c(1:4,7:58),PB :=0.0]
REco.params$pedigree[c(1:4,7:58),QB :=0.0]
REco.params$pedigree[c(1:4,7:58),Diet :=0.0]
#Set up sense runs
all_years <- 1:50
scene <- rsim.scenario(REco, REco.params, years = all_years)
orig.biomass<-scene$start_state$Biomass

# ----- Set up ecosense generator ----- #######################################
scene$params$BURN_YEARS <- 50
NUM_RUNS <- 100
parlist <- as.list(rep(NA, NUM_RUNS))
kept <- rep(NA, NUM_RUNS)

set.seed(19)
for (irun in 1:NUM_RUNS){
  REcosense <- copy(scene)
  # INSERT SENSE ROUTINE BELOW
  parlist[[irun]] <- REcosense$params 		# Base ecosim params
  parlist[[irun]] <- rsim.sense(REcosense, REco.params)	# Replace the base params with Ecosense params  
  REcosense$start_state$Biomass <- parlist[[irun]]$B_BaseRef
  parlist[[irun]]$BURN_YEARS <- 50			# Set Burn Years to 50
  REcosense$params <- parlist[[irun]]
  REcotest <- rsim.run(REcosense, method = "RK4", years = all_years)
  failList <- which((is.na(REcotest$end_state$Biomass) | REcotest$end_state$Biomass/orig.biomass > 1000 | REcotest$end_state$Biomass/orig.biomass < 1/1000))
  {if (length(failList)>0)
  {cat(irun,": fail in year ",REcotest$crash_year,": ",failList,"\n"); kept[irun] <- F; flush.console()}
    else 
    {cat(irun,": success!\n"); kept[irun]<-T;  flush.console()}}
  parlist[[irun]]$BURN_YEARS <- 1
}

# KEPT tells you which ecosystems were kept
KEPT <- which(kept==T)
nkept <- length(KEPT)
nkept
# 396/10000 = 3.67%
#Seems reasonaable
#1763/50,000 = 3.53%
#Also seems reasonable
REco.sense <- parlist[KEPT]

save(REco.sense, file = "REco.sense_All_But_Cope_Constant.RData")

#try running one of these ecosystems forward - 10 years
#no perturbations
#REco.sense1<-REco.sense[[1]]
#REco.sense2<-REco.sense[[2]]

#REco.sim.sense <- rsim.scenario(REco, REco.params, years = 1:50)
#REco.sim.sense$params<-REco.sense2
#REco.run.sense <- rsim.run(REco.sim.sense, method = 'RK4', years = 1:50)
