#RPath model run - version 1

# Fri Jul  2 14:07:39 2021 ------------------------------


#Load packages
library(Rpath); library(data.table);library(dplyr);library(here)

source(here("R/groups_fleets.R"))

#Set up model with group names and types
groups<-as.vector(groups_fleets$RPATH)
types<-c(1,rep(0,55),rep(2,2),rep(3,10))
REco.params<-create.rpath.params(group = groups,type=types)

#Fill in biomass estimates
source(here("R/EMAX_biomass_estimates.R"))

#biomass_80s<-na.omit(biomass_80s)

biomass<-left_join(groups_fleets,biomass_80s,by="RPATH")

#Remove barndoor
#biomass<-biomass[-35,]


#Turn biomass into vector
biomass<-as.vector(biomass$Biomass)

#Change barndoor to 0 biomass
biomass[35]<-0

#Multiply OtherCeph biomass by 10
biomass[10]<-biomass[10]*10

#Multiply SmFlatfish biomass by 10
biomass[15]<-biomass[15]*10

#Fill model
REco.params$model[,Biomass:=biomass]


#Fill pb
source(here("R/biological_parameters.R"))
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
source(here("R/Biomass_Accumulation.R"))
ba<-left_join(groups_fleets,biomass.accum,by="RPATH")
ba<-as.vector(ba$ba)
ba[is.na(ba)]<-0
ba[59:68]<-NA
REco.params$model[,BioAcc:=ba]

#Fill unassimilated consumption
REco.params$model[, Unassim := c(0,rep(0.4,5),rep(0.2, 50),rep(0,2), rep(NA, 10))]
#COME BACK TO THIS

#Detrital Fate
REco.params$model[, Detritus := c(rep(1, 56), rep(0, 12))]
REco.params$model[, Discards := c(rep(0, 56), rep(0,2),rep(1, 10))]

#Fisheries
#Landings
source(here("R/discards.R"))

#Fixed Gear
fixed<-left_join(groups_fleets,fixed,by="RPATH")
fixed<-as.vector(fixed$landings)
fixed[57:58]<-0
REco.params$model[, "Fixed Gear" := fixed]

#Large Mesh
lg_mesh<-left_join(groups_fleets,lg_mesh,by="RPATH")
lg_mesh<-as.vector(lg_mesh$landings)
lg_mesh[57:58]<-0
REco.params$model[, "LG Mesh" := lg_mesh]

#Other
other<-left_join(groups_fleets,other,by="RPATH")
other<-as.vector(other$landings)
other[57:58]<-0
REco.params$model[, "Other" := other]

#Small Mesh
sm_mesh<-left_join(groups_fleets,sm_mesh,by="RPATH")
sm_mesh<-as.vector(sm_mesh$landings)
sm_mesh[57:58]<-0
REco.params$model[, "SM Mesh" := sm_mesh]

#Scallop Dredge
scallop<-left_join(groups_fleets,scallop,by="RPATH")
scallop<-as.vector(scallop$landings)
scallop[57:58]<-0
REco.params$model[, "Scallop Dredge" := scallop]

#Trap
trap<-left_join(groups_fleets,trap,by="RPATH")
trap<-as.vector(trap$landings)
trap[57:58]<-0
REco.params$model[, "Trap" := trap]

#HMS.fleet
hms<-left_join(groups_fleets,hms,by="RPATH")
hms<-as.vector(hms$landings)
hms[57:58]<-0
REco.params$model[, "HMS Fleet" := hms]

#Pelagic
pelagic<-left_join(groups_fleets,pelagic,by="RPATH")
pelagic<-as.vector(pelagic$landings)
pelagic[57:58]<-0
REco.params$model[, "Pelagic" := pelagic]

#Other Dredge
other_dredge<-left_join(groups_fleets,other_dredge,by="RPATH")
other_dredge<-as.vector(other_dredge$landings)
other_dredge[57:58]<-0
REco.params$model[, "Other Dredge" := other_dredge]

#Clam
clam<-left_join(groups_fleets,clam,by="RPATH")
clam<-as.vector(clam$landings)
clam[57:58]<-0
REco.params$model[, "Clam Dredge" := clam]

#Fill in discards
#Fixed Gear
fixed.d<-left_join(groups_fleets,fixed.d,by="RPATH")
fixed.d<-as.vector(fixed.d$discards)
fixed.d[57:58]<-0
REco.params$model[, "Fixed Gear.disc" := fixed.d]

#Lg Mesh
lg_mesh.d<-left_join(groups_fleets,lg_mesh.d,by="RPATH")
lg_mesh.d<-as.vector(lg_mesh.d$discards)
lg_mesh.d[57:58]<-0
REco.params$model[, "LG Mesh.disc" := lg_mesh.d]

#Other
other.d<-left_join(groups_fleets,other.d,by="RPATH")
other.d<-as.vector(other.d$discards)
other.d[57:58]<-0
REco.params$model[, "Other.disc" := other.d]

#SM Mesh
sm_mesh.d<-left_join(groups_fleets,sm_mesh.d,by="RPATH")
sm_mesh.d<-as.vector(sm_mesh.d$discards)
sm_mesh.d[57:58]<-0
REco.params$model[, "SM Mesh.disc" := sm_mesh.d]

#Scallop Dredge
scallop.d<-left_join(groups_fleets,scallop.d,by="RPATH")
scallop.d<-as.vector(scallop.d$discards)
scallop.d[57:58]<-0
REco.params$model[, "Scallop Dredge" := scallop.d]

#Trap
trap.d<-left_join(groups_fleets,trap.d,by="RPATH")
trap.d<-as.vector(trap.d$discards)
trap.d[57:58]<-0
REco.params$model[, "Trap.disc" := trap.d]

#HMS
hms.d<-left_join(groups_fleets,hms.d,by="RPATH")
hms.d<-as.vector(hms.d$discards)
hms.d[57:58]<-0
REco.params$model[, "HMS Fleet.disc" := hms.d]

#Pelagic
pelagic.d<-left_join(groups_fleets,pelagic.d,by="RPATH")
pelagic.d<-as.vector(pelagic.d$discards)
pelagic.d[57:58]<-0
REco.params$model[, "Pelagic.disc" := pelagic.d]

#Other Dredge
other_dredge.d<-left_join(groups_fleets,other_dredge.d,by="RPATH")
other_dredge.d<-as.vector(other_dredge.d$discards)
other_dredge.d[57:58]<-0
REco.params$model[, "Other Dredge.disc" := other_dredge.d]

#Clam Dredge
clam.d<-c(rep(0,56),rep(0,2),rep(NA,10))
REco.params$model[, "Clam Dredge.disc" := clam.d]


#Complete diet table
source(here("R/diet.R"))

#Run diet filling
source(here("R/diet_filling.R"))

#Change DC of Cod(24)
#Decrease predation on OtherCeph(10)by 1.1%
#Decrease predation on SmFlatfishes(15) by .1%
#Increase predation on Illex(8) by 1.2%
REco.params$diet[10,25]<-REco.params$diet[10,25]-.011
REco.params$diet[15,25]<-REco.params$diet[15,25]-.001
REco.params$diet[8,25]<-REco.params$diet[8,25]+0.012

#Shift predation of Other Skates (33) from OtherCeph(10) to Illex(8)
#Shift 4%
REco.params$diet[10,34]<-REco.params$diet[10,34]-0.04
REco.params$diet[8,34]<-REco.params$diet[8,34]+0.04

#Shift predation of WhiteHake OtherCeph(10) to Illex(8)
#Shift 1.5%
REco.params$diet[10,42]<-REco.params$diet[10,42]-0.015
REco.params$diet[8,42]<-REco.params$diet[8,42]+0.015

#Shift predation of SpinyDogfish(42) from OtherCeph(10) to Illex(8)
#Shift 1.5%
REco.params$diet[10,43]<-REco.params$diet[10,43]-0.015
REco.params$diet[8,43]<-REco.params$diet[8,43]+0.015

#Shift predation of Cusk(30) from OtherCeph(10) to Illex(8)
#Shift 10%
REco.params$diet[10,31]<-REco.params$diet[10,31]-0.1
REco.params$diet[8,31]<-0.1

#Shift predation of Pollock(38) from OtherCeph(10) to Illex(8)
#Shift 0.5%
REco.params$diet[10,39]<-REco.params$diet[10,39]-0.005
REco.params$diet[8,39]<-REco.params$diet[8,39]+0.005

#Shift predation of Haddock(25) from OtherCeph(10) to Illex(8)
#Shift 0.8%
REco.params$diet[10,26]<-REco.params$diet[10,26]-0.008
REco.params$diet[8,26]<-0.008

#Run model
REco <- rpath(REco.params, eco.name = 'GOM Ecosystem')
REco

check.rpath.params(REco.params)

