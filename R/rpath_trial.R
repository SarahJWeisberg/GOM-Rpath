#RPath model run - version 1

# Fri Jul  2 14:07:39 2021 ------------------------------


#Load packages
library(Rpath); library(data.table);library(dplyr);library(here)

source("R/groups_fleets.R")

#Set up model with group names and types
groups<-as.vector(groups_fleets$RPATH)
types<-c(1,rep(0,55),rep(3,10),rep(2,2))
REco.params<-create.rpath.params(group = groups,type=types)

#Fill in biomass estimates
source("R/EMAX_biomass_estimates.R")
#Get an error: in Ops.units(GOM.EMAX[RPATH == "Megabenthos", sum(Biomass)], GOM.groups[RPATH ==  : 
#both operands of the expression should be "units" objects
#Something funny is happening here, need to fix
#For now, just try to get the model to run by running EMAX biomass code separately
#Somehow that works fine

biomass_80s<-na.omit(biomass_80s)

biomass<-left_join(groups_fleets,biomass_80s,by="RPATH")

#Remove barndoor
biomass<-biomass[-35,]

#Turn biomass into vector & fill model
biomass<-as.vector(biomass$Biomass)
REco.params$model[,Biomass:=biomass]

#Fill pb
source("R/biological_parameters.R")
pb<-cbind(GOM.groups,PB)
pb<-left_join(groups_fleets,pb,by="RPATH")
pb<-as.vector(pb$PB)
REco.params$model[,PB:=pb]

#Fill qb
qb<-cbind(GOM.groups,QB)
qb<-left_join(groups_fleets,qb,by="RPATH")
qb<-as.vector(qb$QB)
REco.params$model[,QB:=qb]

#Fill biomass accumulation
source("R/Biomass_Accumulation.R")
ba<-left_join(groups_fleets,biomass.accum,by="RPATH")
ba<-as.vector(ba$ba)
REco.params$model[,BioAcc:=ba]

#Fill unassimilated consumption
REco.params$model[, Unassim := c(0,rep(0.4,5),rep(0.2, 50), rep(NA, 10),rep(0,2))]
#COME BACK TO THIS

#Detrital Fate
REco.params$model[, Detritus := c(rep(1, 56), rep(0, 12))]
REco.params$model[, Discards := c(rep(0, 56), rep(1, 10),rep(0,2))]

#Fisheries
#Landings
source("R/landings_conversion.R")



