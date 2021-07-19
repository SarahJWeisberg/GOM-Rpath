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

#biomass_80s<-na.omit(biomass_80s)

biomass<-left_join(groups_fleets,biomass_80s,by="RPATH")

#Remove barndoor
#biomass<-biomass[-35,]


#Turn biomass into vector
biomass<-as.vector(biomass$Biomass)

#Change barndoor to 0 biomass
biomass[35]<-0

#Fill model
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
source("R/discards.R")

#Fixed Gear
fixed<-left_join(groups_fleets,fixed,by="RPATH")
fixed<-as.vector(fixed$landings)
fixed[67:68]<-0
REco.params$model[, "Fixed Gear" := fixed]

#Large Mesh
lg_mesh<-left_join(groups_fleets,lg_mesh,by="RPATH")
lg_mesh<-as.vector(lg_mesh$landings)
lg_mesh[67:68]<-0
REco.params$model[, "LG Mesh" := lg_mesh]

#Other
other<-left_join(groups_fleets,other,by="RPATH")
other<-as.vector(other$landings)
other[67:68]<-0
REco.params$model[, "Other" := other]

#Small Mesh
sm_mesh<-left_join(groups_fleets,sm_mesh,by="RPATH")
sm_mesh<-as.vector(sm_mesh$landings)
sm_mesh[67:68]<-0
REco.params$model[, "SM Mesh" := sm_mesh]

#Scallop Dredge
scallop<-left_join(groups_fleets,scallop,by="RPATH")
scallop<-as.vector(scallop$landings)
scallop[67:68]<-0
REco.params$model[, "Scallop Dredge" := scallop]

#Trap
trap<-left_join(groups_fleets,trap,by="RPATH")
trap<-as.vector(trap$landings)
trap[67:68]<-0
REco.params$model[, "Trap" := trap]

#HMS.fleet
hms<-left_join(groups_fleets,hms,by="RPATH")
hms<-as.vector(hms$landings)
hms[67:68]<-0
REco.params$model[, "HMS" := hms]

#Pelagic
pelagic<-left_join(groups_fleets,pelagic,by="RPATH")
pelagic<-as.vector(pelagic$landings)
pelagic[67:68]<-0
REco.params$model[, "Pelagic" := pelagic]

#Other Dredge
other_dredge<-left_join(groups_fleets,other_dredge,by="RPATH")
other_dredge<-as.vector(other_dredge$landings)
other_dredge[67:68]<-0
REco.params$model[, "Other Dredge" := other_dredge]

#Clam
clam<-left_join(groups_fleets,clam,by="RPATH")
clam<-as.vector(clam$landings)
clam[67:68]<-0
REco.params$model[, "Clam Dredge" := clam]

#Fill in discards
#Fixed Gear
fixed.d<-left_join(groups_fleets,fixed.d,by="RPATH")
fixed.d<-as.vector(fixed.d$discards)
fixed.d[67:68]<-0
REco.params$model[, "Fixed Gear.disc" := fixed.d]

#Lg Mesh
lg_mesh.d<-left_join(groups_fleets,lg_mesh.d,by="RPATH")
lg_mesh.d<-as.vector(lg_mesh.d$discards)
lg_mesh.d[67:68]<-0
REco.params$model[, "LG Mesh.disc" := lg_mesh.d]

#Other
other.d<-left_join(groups_fleets,other.d,by="RPATH")
other.d<-as.vector(other.d$discards)
other.d[67:68]<-0
REco.params$model[, "Other.disc" := other.d]

#SM Mesh
sm_mesh.d<-left_join(groups_fleets,sm_mesh.d,by="RPATH")
sm_mesh.d<-as.vector(sm_mesh.d$discards)
sm_mesh.d[67:68]<-0
REco.params$model[, "SM Mesh.disc" := sm_mesh.d]

#Scallop Dredge
scallop.d<-left_join(groups_fleets,scallop.d,by="RPATH")
scallop.d<-as.vector(scallop.d$discards)
scallop.d[67:68]<-0
REco.params$model[, "Scallop Dredge" := scallop.d]

#Trap
trap.d<-left_join(groups_fleets,trap.d,by="RPATH")
trap.d<-as.vector(trap.d$discards)
trap.d[67:68]<-0
REco.params$model[, "Trap.disc" := trap.d]

#HMS
hms.d<-left_join(groups_fleets,hms.d,by="RPATH")
hms.d<-as.vector(hms.d$discards)
hms.d[67:68]<-0
REco.params$model[, "HMS.disc" := hms.d]

#Pelagic
pelagic.d<-left_join(groups_fleets,pelagic.d,by="RPATH")
pelagic.d<-as.vector(pelagic.d$discards)
pelagic.d[67:68]<-0
REco.params$model[, "Pelagic.disc" := pelagic.d]

#Other Dredge
other_dredge.d<-left_join(groups_fleets,other_dredge.d,by="RPATH")
other_dredge.d<-as.vector(other_dredge.d$discards)
other_dredge.d[67:68]<-0
REco.params$model[, "Other Dredge.disc" := other_dredge.d]

#Clam Dredge
clam.d<-c(rep(0,56),rep(NA,10),rep(0,2))
REco.params$model[, "Clam Dredge.disc" := clam.d]


#Complete diet table
source("R/diet.R")

#Run diet filling
source("R/diet_filling.R")

#Run model
REco <- rpath(REco.params, eco.name = 'GOM Ecosystem')
REco
