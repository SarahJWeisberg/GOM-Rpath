# Title: RPath model balancing
# Purpose: This script generates a mass-balanced model of the 
#         Gulf of Maine (GoM) food web. Initial parameter estimates are pulled
#         from relevant data sources and then modified.

# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu


# Thu Dec  1 10:09:52 2022 ------------------------------


#Load packages
#install.packages(c('devtools','tinytex','lwgeom'))
library(devtools)
#install_github('NOAA-EDAB/Rpath')
#install_github('NOAA-EDAB/survdat')
library(Rpath); library(data.table);library(dplyr);library(here);library(tinytex);
library(survdat); library(lwgeom)

#Load groups and fleets
source(here("R/groups_fleets.R"))

#Set up model with group names and types
#1 = primary producer, 0 = consumer, 2 = detritus, 3 = fleet
groups<-as.vector(groups_fleets$RPATH)
types<-c(1,rep(0,55),rep(2,1),rep(3,9))
GOM.params<-create.rpath.params(group = groups,type=types)
rm(types)

#Fill in biomass estimates
source(here("R/EMAX_biomass_estimates.R"))

#Remove Discards 
biomass_80s<-biomass_80s %>% filter(RPATH !="Discards")
#and set Detritus biomass to NA
biomass_80s[which(RPATH == "Detritus")]$Biomass<-NA

biomass<-left_join(groups_fleets,biomass_80s,by="RPATH")

#Turn biomass into vector
biomass<-as.vector(biomass$Biomass)

#Change barndoor biomass to non-zero
#If set to 0, Ecosense does not work properly
biomass[35]<-2.5*10^-5

#Changes to biomass for balancing
#Multiply OtherCeph biomass by 30
biomass[10]<-biomass[10]*30

#Multiply SmFlatfish biomass by 105
biomass[15]<-biomass[15]*105

#Multiply SpinyDogfish biomass by 0.5
biomass[42]<-biomass[42]*0.5

#Multiply OtherPelagics biomass by 180
biomass[20]<-biomass[20]*180

#Multiply SmPelagics biomass by 25
biomass[14]<-biomass[14]*25

#Multiply Mesopelagics biomass by 20
biomass[22]<-biomass[22]*20

#Multiply SummerFlounder biomass by 4
biomass[29]<-biomass[29]*4

#Multiply Sharks biomass by 3
biomass[51]<-biomass[51]*3

#Multiply Cod biomass by 0.5
#Similar to Yong's estimate
biomass[24]<-biomass[24]*0.5

#Multiply RiverHerring biomass by 20
biomass[18]<-biomass[18]*20

#Multiply AtlHerring biomass by 10
#In accordance with Yong
biomass[21]<-biomass[21]*10

#Multiply AtlMackerel biomass by 5.06
biomass[27]<-biomass[27]*5.06

#Multiply AmLobster biomass by 3
#In accordance with Yong's estimates
biomass[12]<-biomass[12]*3

#Multiply WinterFlounder biomass by 2.7
biomass[47]<-biomass[47]*2.7

#Multiply Windowpane by 2
biomass[46]<-biomass[46]*2

#Multiply WhiteHake biomass by 0.75
biomass[41]<-biomass[41]*0.75

#Multiply SilverHake biomass by 2.15
biomass[40]<-biomass[40]*2.15

#Multiply WitchFlounder biomass by 5
biomass[48]<-biomass[48]*5

#Multiply OtherSkates biomass by 0.2
biomass[33]<-biomass[33]*0.2

#Multiply Redfish biomass by 3
biomass[43]<-biomass[43]*3

#Multiply OtherShrimps biomass by 6
biomass[17]<-biomass[17]*6

#Multiply NShrimp biomass by 5
#Similar estimate to Zheng & Chen
biomass[16]<-biomass[16]*5

#Multiply Pollock biomass by 0.75
biomass[38]<-biomass[38]*0.75

#Multiply Butterfish biomass by 3
biomass[50]<-biomass[50]*3

#Multiply OceanPout biomass by 2
biomass[13]<-biomass[13]*2

#Multiply Macrobenthos biomass by 0.5
biomass[11]<-biomass[11]*0.5

#Multiply AmPlaice biomass by 1.21
biomass[26]<-biomass[26]*1.21

#Multiply YTFlounder biomass by 1.15
biomass[23]<-biomass[23]*1.15

#Multiply Fourspot biomass by 1.06
biomass[36]<-biomass[36]*1.06

#Multiply BlackSeaBass biomass by 1.4
biomass[49]<-biomass[49]*1.4

#Multiply OtherDemersals biomass by 2.1
biomass[31]<-biomass[31]*2.1

#Multiply RedHake biomass by 1.1
biomass[34]<-biomass[34]*1.1

#Multiply Loligo biomass by 1.03
biomass[9]<-biomass[9]*1.03

#Fill model
GOM.params$model[,Biomass:=biomass]


#Fill pb
source(here("R/biological_parameters.R"))
pb<-cbind(GOM.groups,PB)
pb<-left_join(groups_fleets,pb,by="RPATH")
pb<-as.vector(pb$PB)

#Increase pb of SmPelagics to 2
#Value from Sean's GB model
pb[14]<-2

#Increase pb of RedHake to 1.3
#Value from NWACS
pb[34]<-1.3

#Increase pb of AtlMackerel 3x
#Copying Sean
pb[27]<-pb[27]*3

#Increase pb of AtlHerring to 1.64
#Copying Sean
pb[21]<-1.64

#Increase pb of SilverHake to 0.735
#0.735 according to Yong; 0.4 according to Sean
pb[40]<-0.9

#Increase pb of OtherShrimps 2.25x
#Copying Sean
pb[17]<-pb[17]*2.25

#Increase pb of OtherDemersals 1.8x
#Similar to Sean
pb[31]<-pb[31]*1.8

#Increase pb of Mesopelagics 1.5x
#Copying Sean
pb[22]<-pb[22]*1.5

#Increase pb of WinterFlounder 2.2x
#Based on PREBAL results
pb[47]<-pb[47]*2.2

#Increase pb of SpinyDogfish 1.2x
#Similar to Sean
pb[42]<-pb[42]*1.2

#Increase pb of WitchFlounder 2x
#Based on PREBAL results
pb[48]<-pb[48]*2

#Decrease pb of OceanPout to 0.86
#Based on conversation with Mike/Sean
pb[13]<-0.86

#Decrease pb of Barndoor to 0.2
#Based on conversation with Mike/Sean
pb[35]<-0.2

#Decrease pb of Sharks to 0.13
#Based on conversation with Mike/Sean
pb[51]<-0.13

#Increase pb of BlackSeaBass to 0.65
pb[49]<-0.65

GOM.params$model[,PB:=pb]

#Fill qb
qb<-cbind(GOM.groups,QB)
qb<-left_join(groups_fleets,qb,by="RPATH")
qb<-as.vector(qb$QB)

#Decrease qb of Pollock - 0.5x
#Similar to Sean
qb[38]<-qb[38]*0.5

#Decrease qb of Spiny Dogfish - 0.5x
qb[42]<-qb[42]*0.5

#Decrease qb of SilverHake to 3.06
#Copying Sean
qb[40]<-3.06

#Increase qb of SilverHake to 4.26
#Copying Yong
#qb[40]<-4.26

#Decrease qb of Goosefish - 0.5x
qb[39]<-qb[39]*0.5

#Decrease qb of Redfish - 0.5x
qb[43]<-qb[43]*0.5

#Decrease qb of WhiteHake - 0.75x
qb[41]<-qb[41]*0.75

#Decrease qb of Macrobenthos - 0.75x
qb[11]<-qb[11]*0.75

#Decrease qb of OtherPelagics - 0.5x
qb[20]<-qb[20]*0.5

#Increase qb of RedHake to 3.85
#Value from NWACS
qb[34]<-3.85

#Increase qb of OtherShrimps 2.25x
#Copying Sean
qb[17]<-qb[17]*2.25

#Increase qb of WinterFlounder 2.2x
#Based on PREBAL results
qb[47]<-qb[47]*2.2

#Increase qb of WitchFlounder 2x
#Based on PREBAL results
qb[48]<-qb[48]*2

#Increase qb of AtlMackerel 2.2x
#Keep GE reasonable
qb[27]<-qb[27]*2.2

#Increase qb of OtherDemersals 1.2x
#Keep GE reasonable
qb[31]<-qb[31]*1.2

#Decrease qb of OceanPout to 3.85
#Lowering pb, keeping ge the same
qb[13]<-3.85

GOM.params$model[,QB:=qb]

#Fill biomass accumulation
source(here("R/biomass_accumulation.R"))
ba<-left_join(groups_fleets,biomass.accum,by="RPATH")
ba<-as.vector(ba$ba)
ba[is.na(ba)]<-0
ba[58:66]<-NA 

#Change barndoor ba
ba[35]<-ba[35]/1000

#Change OceanPout ba
ba[13]<- -0.004

GOM.params$model[,BioAcc:=ba]

#Fill unassimilated consumption
GOM.params$model[, Unassim := c(0,rep(0.25,5),rep(0.2, 50),rep(0,1), rep(NA, 9))]
#COME BACK TO THIS

#Detrital Fate
GOM.params$model[, Detritus := c(rep(1, 56), rep(0, 10))]
#GOM.params$model[, Discards := c(rep(0, 56), rep(0,2),rep(1, 9))]

#Fisheries
#Landings
source(here("R/discards.R"))

#Fixed Gear
fixed<-left_join(groups_fleets,fixed,by="RPATH")
fixed<-as.vector(fixed$landings)
fixed[57]<-0
GOM.params$model[, "Fixed Gear" := fixed]

#Large Mesh
lg_mesh<-left_join(groups_fleets,lg_mesh,by="RPATH")
lg_mesh<-as.vector(lg_mesh$landings)
lg_mesh[57]<-0
GOM.params$model[, "LG Mesh" := lg_mesh]

#Other
other<-left_join(groups_fleets,other,by="RPATH")
other<-as.vector(other$landings)
other[57]<-0
GOM.params$model[, "Other" := other]

#Small Mesh
sm_mesh<-left_join(groups_fleets,sm_mesh,by="RPATH")
sm_mesh<-as.vector(sm_mesh$landings)
sm_mesh[57]<-0
GOM.params$model[, "SM Mesh" := sm_mesh]

#Scallop Dredge
#scallop<-left_join(groups_fleets,scallop,by="RPATH")
#scallop<-as.vector(scallop$landings)
#scallop[57:58]<-0
#GOM.params$model[, "Scallop Dredge" := scallop]

#Trap
trap<-left_join(groups_fleets,trap,by="RPATH")
trap<-as.vector(trap$landings)
trap[57]<-0
GOM.params$model[, "Trap" := trap]

#HMS.fleet
hms<-left_join(groups_fleets,hms,by="RPATH")
hms<-as.vector(hms$landings)
hms[57]<-0
GOM.params$model[, "HMS Fleet" := hms]

#Pelagic
pelagic<-left_join(groups_fleets,pelagic,by="RPATH")
pelagic<-as.vector(pelagic$landings)
pelagic[57]<-0

#Reduce fishing on OtherPelagics, multiply by 0.89
pelagic[20]<-pelagic[20]*0.89

GOM.params$model[, "Pelagic" := pelagic]

#Other Dredge
other_dredge<-left_join(groups_fleets,other_dredge,by="RPATH")
other_dredge<-as.vector(other_dredge$landings)
other_dredge[57]<-0
GOM.params$model[, "Other Dredge" := other_dredge]

#Clam
clam<-left_join(groups_fleets,clam,by="RPATH")
clam<-as.vector(clam$landings)
clam[57]<-0
GOM.params$model[, "Clam Dredge" := clam]

#Fill in discards
#Fixed Gear
#fixed.d<-left_join(groups_fleets,fixed.d,by="RPATH")
#fixed.d<-as.vector(fixed.d$discards)
#fixed.d[57:58]<-0
#GOM.params$model[, "Fixed Gear.disc" := fixed.d]

#Lg Mesh
#lg_mesh.d<-left_join(groups_fleets,lg_mesh.d,by="RPATH")
#lg_mesh.d<-as.vector(lg_mesh.d$discards)
#lg_mesh.d[57:58]<-0
#GOM.params$model[, "LG Mesh.disc" := lg_mesh.d]

#Other
#other.d<-left_join(groups_fleets,other.d,by="RPATH")
#other.d<-as.vector(other.d$discards)
#other.d[57:58]<-0
#GOM.params$model[, "Other.disc" := other.d]

#SM Mesh
#sm_mesh.d<-left_join(groups_fleets,sm_mesh.d,by="RPATH")
#sm_mesh.d<-as.vector(sm_mesh.d$discards)
#sm_mesh.d[57:58]<-0
#GOM.params$model[, "SM Mesh.disc" := sm_mesh.d]

#Scallop Dredge
#scallop.d<-left_join(groups_fleets,scallop.d,by="RPATH")
#scallop.d<-as.vector(scallop.d$discards)
#scallop.d[57:58]<-0
#GOM.params$model[, "Scallop Dredge" := scallop.d]

#Trap
#trap.d<-left_join(groups_fleets,trap.d,by="RPATH")
#trap.d<-as.vector(trap.d$discards)
#trap.d[57:58]<-0
#GOM.params$model[, "Trap.disc" := trap.d]

#HMS
#hms.d<-left_join(groups_fleets,hms.d,by="RPATH")
#hms.d<-as.vector(hms.d$discards)
#hms.d[57:58]<-0
#GOM.params$model[, "HMS Fleet.disc" := hms.d]

#Pelagic
#pelagic.d<-left_join(groups_fleets,pelagic.d,by="RPATH")
#pelagic.d<-as.vector(pelagic.d$discards)
#pelagic.d[57:58]<-0
#GOM.params$model[, "Pelagic.disc" := pelagic.d]

#Other Dredge
#other_dredge.d<-left_join(groups_fleets,other_dredge.d,by="RPATH")
#other_dredge.d<-as.vector(other_dredge.d$discards)
#other_dredge.d[57:58]<-0
#GOM.params$model[, "Other Dredge.disc" := other_dredge.d]

#Clam Dredge
#clam.d<-c(rep(0,56),rep(0,2),rep(NA,9))
#GOM.params$model[, "Clam Dredge.disc" := clam.d]


#Complete diet table
source(here("R/diet.R"))

#Run diet filling
source(here("R/diet_filling.R"))

#Shifting predation on OtherCeph

#Shift predation of Cod (24) from OtherCeph(10) to Illex(8)
#Shift 1.1%
GOM.params$diet[10,25]<-GOM.params$diet[10,25]-.011
GOM.params$diet[8,25]<-GOM.params$diet[8,25]+0.011

#Shift predation of OtherSkates(33) from OtherCeph(10) to Illex(8)
#Shift 4.15%
GOM.params$diet[10,34]<-GOM.params$diet[10,34]-0.0415
GOM.params$diet[8,34]<-GOM.params$diet[8,34]+0.0415

#Shift predation of WhiteHake(41) from OtherCeph(10) to Illex(8)
#Shift 1.75%
GOM.params$diet[10,42]<-GOM.params$diet[10,42]-0.0175
GOM.params$diet[8,42]<-GOM.params$diet[8,42]+0.0175

#Shift predation of SpinyDogfish(42) from OtherCeph(10) to Illex(8)
#Shift 1.7%
GOM.params$diet[10,43]<-GOM.params$diet[10,43]-0.017
GOM.params$diet[8,43]<-GOM.params$diet[8,43]+0.017

#Shift predation of Cusk(30) from OtherCeph(10) to Illex(8)
#Shift 11%
GOM.params$diet[10,31]<-GOM.params$diet[10,31]-0.11
GOM.params$diet[8,31]<-0.11

#Shift predation of Pollock(38) from OtherCeph(10) to Illex(8)
#Shift 0.58%
GOM.params$diet[10,39]<-GOM.params$diet[10,39]-0.0058
GOM.params$diet[8,39]<-GOM.params$diet[8,39]+0.0058

#Shift predation of Haddock(25) from OtherCeph(10) to Illex(8)
#Shift 0.83%
GOM.params$diet[10,26]<-GOM.params$diet[10,26]-0.0083
GOM.params$diet[8,26]<-0.0083

#Shift predation of OtherPelagics(20) from OtherCeph(10) to Illex(8)
#Shift 0.4%
GOM.params$diet[10,21]<-GOM.params$diet[10,21]-0.004
GOM.params$diet[8,21]<-0.004

#Shift predation of Goosefish(39) from OtherCeph(10) to Illex(8)
#Shift 1.8%
GOM.params$diet[10,40]<-GOM.params$diet[10,40]-0.018
GOM.params$diet[8,40]<-GOM.params$diet[8,40]+0.018

#Shift predation of SilverHake(40) from OtherCeph(10) to Illex(8)
#Shift 0.4%
GOM.params$diet[10,41]<-GOM.params$diet[10,41]-0.004
GOM.params$diet[8,41]<-GOM.params$diet[8,41]+0.004

#Shift predation of RedHake(34) from OtherCeph(10) to Illex(8)
#Shift 1.1%
GOM.params$diet[10,35]<-GOM.params$diet[10,35]-0.011
GOM.params$diet[8,35]<-GOM.params$diet[8,35]+0.011

#Shift predation of Redfish(43) from OtherCeph(10) to Illex(8)
#Shift 0.21%
GOM.params$diet[10,44]<-GOM.params$diet[10,44]-0.0021
GOM.params$diet[8,44]<-0.0021

#Shift predation of AtlHerring(27) from OtherCeph(10) to Illex(8)
#Shift 1.1%
GOM.params$diet[10,28]<-GOM.params$diet[10,28]-0.011
GOM.params$diet[8,28]<-0.011

#Shift predation of AtlMackerel(21) from OtherCeph(10) to Illex(8)
#Shift 0.029%
GOM.params$diet[10,22]<-GOM.params$diet[10,22]-0.00029
GOM.params$diet[8,22]<-GOM.params$diet[8,22]+0.00029

#Shift predation of WinterFlounder(47) from OtherCeph(10) to AmLobster(12)
#Shift 1.6%
GOM.params$diet[10,48]<-GOM.params$diet[10,48]-0.016
GOM.params$diet[12,48]<-GOM.params$diet[12,48]+0.016

#Shift predation on SmFlatfishes

#Shift predation of Goosefish(39) from SmFlat(15) to WitchFlounder(48)
#Shift 0.25%
GOM.params$diet[15,40]<-GOM.params$diet[15,40]-0.0025
GOM.params$diet[48,40]<-GOM.params$diet[48,40]+0.0025

#Shift predation of WhiteHake(41) from SmFlat(15) to Illex(8)
#Shift 0.005%
GOM.params$diet[15,42]<-GOM.params$diet[15,42]-0.00005
GOM.params$diet[8,42]<-GOM.params$diet[8,42]+0.00005

#Shift predation of RedHake(34) from SmFlatfishes(15) to Illex(8)
#Shift 0.025%
GOM.params$diet[15,35]<-GOM.params$diet[15,35]-0.00025
GOM.params$diet[8,35]<-GOM.params$diet[8,35]+0.00025

#Shift predation of Cod (24) from SmFlatfishes(15) to Illex(8)
#Shift 0.11%
GOM.params$diet[15,25]<-GOM.params$diet[15,25]-.0011
GOM.params$diet[8,25]<-GOM.params$diet[8,25]+0.0011

#Shifting predation on OtherDemersals
#Shift predation of WhiteHake(41) from OtherDem(31) to Megabenthos(56)
#Shift 2%
GOM.params$diet[31,42]<-GOM.params$diet[31,42]-0.02
GOM.params$diet[56,42]<-GOM.params$diet[56,42]+0.02

#Shift predation of WhiteHake(41) from OtherDem(31) to WhiteHake(41)
#Shift 3%
GOM.params$diet[31,42]<-GOM.params$diet[31,42]-0.03
GOM.params$diet[41,42]<-GOM.params$diet[41,42]+0.03

#Shift predation of WhiteHake(41) from OtherDem(31) to Micronekton(7)
#Shift 0.5%
GOM.params$diet[31,42]<-GOM.params$diet[31,42]-0.005
GOM.params$diet[7,42]<-GOM.params$diet[7,42]+0.005

#Shift predation of OtherSkates(33) from OtherDem(31) to Megabenthos(56)
#Shift 13%
GOM.params$diet[31,34]<-GOM.params$diet[31,34]-0.13
GOM.params$diet[56,34]<-GOM.params$diet[56,34]+0.13

#Shift predation of OtherSkates(33) from OtherDem(31) to RedHake(34)
#Shift 1%
GOM.params$diet[31,34]<-GOM.params$diet[31,34]-0.01
GOM.params$diet[34,34]<-GOM.params$diet[34,34]+0.01

#Shift predation of OtherPelagics(20) from OtherDem(31) to OtherPelagics(20)
#Shift 9%
GOM.params$diet[31,21]<-GOM.params$diet[31,21]-0.09
GOM.params$diet[20,21]<-GOM.params$diet[20,21]+0.09

#Shift predation of SpinyDogfish(42) from OtherDem(31) to WhiteHake(41)
#Shift 0.5%
GOM.params$diet[31,43]<-GOM.params$diet[31,43]-0.005
GOM.params$diet[41,43]<-GOM.params$diet[41,43]+0.005

#Shift predation of Goosefish(39) from OtherDem(31) to WhiteHake(41)
#Shift 5%
GOM.params$diet[31,40]<-GOM.params$diet[31,40]-0.05
GOM.params$diet[41,40]<-GOM.params$diet[41,40]+0.05

#Shift predation of Cod(24) from OtherDem(31) to RedHake(34)
#Shift 3%
GOM.params$diet[31,25]<-GOM.params$diet[31,25]-0.03
GOM.params$diet[34,25]<-GOM.params$diet[34,25]+0.03

#Shift predation of Cusk(30) from OtherDem(31) to AmLobster(12)
#Can cite Boudreau & Worm (2010)
#Shift 7%
GOM.params$diet[31,31]<-GOM.params$diet[31,31]-0.07
GOM.params$diet[12,31]<-0.07

#Shift predation of Cusk(30) from OtherDem(31) to Micronekton(7)
#Shift 9%
GOM.params$diet[31,31]<-GOM.params$diet[31,31]-0.09
GOM.params$diet[7,31]<-GOM.params$diet[7,31]+0.09

#Shift predation of Cusk(30) from OtherDem(31) to OtherShrimps(17)
#Shift 3%
GOM.params$diet[31,31]<-GOM.params$diet[31,31]-0.03
GOM.params$diet[17,31]<-GOM.params$diet[17,31]+0.03

#Shift predation of Cusk(30) from OtherDem(31) to Megabenthos(56)
#Shift 12%
GOM.params$diet[31,31]<-GOM.params$diet[31,31]-0.12
GOM.params$diet[56,31]<-GOM.params$diet[56,31]+0.12


#Shift predation of Goosefish(39) from OtherDem(31) to RedHake(34)
#Shift 4%
GOM.params$diet[31,40]<-GOM.params$diet[31,40]-0.04
GOM.params$diet[34,40]<-GOM.params$diet[34,40]+0.04

#Shift predation of Goosefish(39) from OtherDem(31) to Goosefish(39)
#Shift 5%
GOM.params$diet[31,40]<-GOM.params$diet[31,40]-0.05
GOM.params$diet[39,40]<-GOM.params$diet[39,40]+0.05

#Shift predation of Haddock(25) from OtherDem(31) to Megabenthos(56)
#Shift 0.9%
GOM.params$diet[31,26]<-GOM.params$diet[31,26]-0.009
GOM.params$diet[8,26]<-GOM.params$diet[8,26]+0.009

#Shifting some predation of Butterfish 

#Shift predation of OtherPelagics(20) from Butterfish(50) to Illex(8)
#Shift 7.6%
GOM.params$diet[50,21]<-GOM.params$diet[50,21]-0.076
GOM.params$diet[8,21]<-GOM.params$diet[8,21]+0.076

#Shift predation of SilverHake(40) from Butterfish(50) to Illex(8)
#Shift 0.8%
GOM.params$diet[50,41]<-GOM.params$diet[50,41]-0.008
GOM.params$diet[8,41]<-GOM.params$diet[8,41]+0.008

#Shift predation of BaleenWhales(54) from Butterfish(50) to Illex(8)
#Shift 0.7%
GOM.params$diet[50,55]<-GOM.params$diet[50,55]-0.007
GOM.params$diet[8,55]<-GOM.params$diet[8,55]+0.007

#Shift predation of SummerFlounder(29) from Butterfish(50) to OtherPelagics(20) 
#Shift 8%
GOM.params$diet[50,30]<-GOM.params$diet[50,30]-0.08
GOM.params$diet[20,30]<-GOM.params$diet[20,30]+0.08

#Shift predation of SummerFlounder(29) from Butterfish(50) to Megabenthos(56) 
#Shift 8%
GOM.params$diet[50,30]<-GOM.params$diet[50,30]-0.08
GOM.params$diet[56,30]<-GOM.params$diet[56,30]+0.08

#Shift predation of SpinyDogfish(42) from Butterfish(50) to Illex(8)
#Shift 0.9%
GOM.params$diet[50,43]<-GOM.params$diet[50,43]-0.009
GOM.params$diet[8,43]<-GOM.params$diet[8,43]+0.009


#Shifting some predation of WinterFlounder

#Shift predation of OtherPelagics(20) from WinterFlounder(47) to Illex(8)
#Shift 2%
GOM.params$diet[47,21]<-GOM.params$diet[47,21]-0.02
GOM.params$diet[8,21]<-GOM.params$diet[8,21]+.02

#Shift predation of Macrobenthos(11) from WinterFlounder(47) to OtherSkates(33)
#Shift 0.002%
GOM.params$diet[47,12]<-GOM.params$diet[47,12]-0.00002
GOM.params$diet[33,12]<-GOM.params$diet[33,12]+0.00002

#Shifting some predation of BSB

#Shift predation of SpinyDogfish(42) from BlackSeaBass(49) to Megabenthos(56)
#Shift 0.04%
GOM.params$diet[49,43]<-GOM.params$diet[49,43]-0.0004
GOM.params$diet[56,43]<-GOM.params$diet[56,43]+0.0004

#Shift predation of SilverHake(40) from BlackSeaBass(49) to Megabenthos(56)
#Shift 0.009%
GOM.params$diet[49,41]<-GOM.params$diet[49,41]-0.00009
GOM.params$diet[56,41]<-GOM.params$diet[56,41]+0.00009

#Shift predation of Cod(24) from BlackSeaBass(49) to Illex(8)
#Shift 0.01%
GOM.params$diet[49,25]<-GOM.params$diet[49,25]-0.0001
GOM.params$diet[8,25]<-GOM.params$diet[8,25]+0.0001

#Shift some predation of Macrobenthos(11) from BlackSeaBass(49) to AtlScallop(19)
#Shift 0.00001%
GOM.params$diet[49,12]<-GOM.params$diet[49,12]-0.0000001
GOM.params$diet[19,12]<-GOM.params$diet[19,12]+0.0000001

#Shift some predation of Goosefish(39) from BlackSeaBass(49) to Illex(8)
#Shift 0.015%
GOM.params$diet[49,40]<-GOM.params$diet[49,40]-0.00015
GOM.params$diet[8,40]<-GOM.params$diet[8,40]+0.00015

#Shifting some predation of Windowpane

#Shift predation of OtherSkates(33) from Windowpane(46) to Megabenthos(56)
#Shift 0.15%
GOM.params$diet[46,34]<-GOM.params$diet[46,34]-0.0015
GOM.params$diet[56,34]<-GOM.params$diet[56,34]+0.0015

#Shifting some predation of Mesopelagics
#Shift predation of Pollock(38) from Mesopelagics(22) to Illex(8)
#Shift 0.7%
GOM.params$diet[22,39]<-GOM.params$diet[22,39]-0.007
GOM.params$diet[8,39]<-GOM.params$diet[8,39]+0.007

#Shifting some predation of Sharks
#Shift predation of Cod(24) from Sharks(51) to Pollock(38)
#Shift 0.17%
GOM.params$diet[51,25]<-GOM.params$diet[51,25]-0.0017
GOM.params$diet[38,25]<-GOM.params$diet[38,25]+0.0017

#Shifting some predation of Redfish
#Shift predation of Macrobenthos(11) from Redfish(43) to Megabenthos(56)
#Shift 0.011%
GOM.params$diet[43,12]<-GOM.params$diet[43,12]-0.00011
GOM.params$diet[56,12]<-GOM.params$diet[56,12]+0.00011

#Shifting some predation of SilverHake
#Shift predation of SilverHake(40) from SilverHake(40) to Megabenthos(56)
#Shift 2.5%
GOM.params$diet[40,41]<-GOM.params$diet[40,41]-0.025
GOM.params$diet[56,41]<-GOM.params$diet[56,41]+0.025

#Shift predation of SilverHake(40) from SilverHake(40) to OtherShrimps(17)
#Shift 2.5%
GOM.params$diet[40,41]<-GOM.params$diet[40,41]-0.025
GOM.params$diet[17,41]<-GOM.params$diet[17,41]+0.025

#Shift predation of SilverHake(40) from SilverHake(40) to SmPelagics(14)
#Shift 9%
GOM.params$diet[40,41]<-GOM.params$diet[40,41]-0.09
GOM.params$diet[14,41]<-GOM.params$diet[14,41]+0.09

#Shift predation of Pollock(38) from SilverHake(40) to Micronekton(7)
#Shift 3%
GOM.params$diet[40,39]<-GOM.params$diet[40,39]-0.03
GOM.params$diet[7,39]<-GOM.params$diet[7,39]+0.03

#Shift predation of Pollock(38) from SilverHake(40) to SmPelagics(14)
#Shift 10%
GOM.params$diet[40,39]<-GOM.params$diet[40,39]-0.1
GOM.params$diet[14,39]<-GOM.params$diet[14,39]+0.1

#Shift predation of Pollock(38) from SilverHake(40) to Pollock(38)
#Shift 3%
GOM.params$diet[40,39]<-GOM.params$diet[40,39]-0.03
GOM.params$diet[38,39]<-GOM.params$diet[38,39]+0.03

#Shift predation of WhiteHake(41) from SilverHake(40) to Megabenthos(56)
#Shift 2%
GOM.params$diet[40,42]<-GOM.params$diet[40,42]-0.02
GOM.params$diet[56,42]<-GOM.params$diet[56,42]+0.02

#Shift predation of WhiteHake(41) from SilverHake(40) to Illex(8)
#Shift 2%
GOM.params$diet[40,42]<-GOM.params$diet[40,42]-0.02
GOM.params$diet[8,42]<-GOM.params$diet[8,42]+0.02

#Shift predation of WhiteHake(41) from SilverHake(40) to RedHake(34)
#Shift 2%
GOM.params$diet[40,42]<-GOM.params$diet[40,42]-0.02
GOM.params$diet[34,42]<-GOM.params$diet[34,42]+0.02

#Shift predation of WhiteHake(41) from SilverHake(40) to WhiteHake(41)
#Shift 2%
GOM.params$diet[40,42]<-GOM.params$diet[40,42]-0.02
GOM.params$diet[41,42]<-GOM.params$diet[41,42]+0.02

#Shift predation of WhiteHake(41) from SilverHake(40) to Micronekton(7)
#Shift 2%
GOM.params$diet[40,42]<-GOM.params$diet[40,42]-0.02
GOM.params$diet[7,42]<-GOM.params$diet[7,42]+0.02

#Shift predation of WhiteHake(41) from SilverHake(40) to SmPelagics(14)
#Shift 8%
GOM.params$diet[40,42]<-GOM.params$diet[40,42]-0.08
GOM.params$diet[14,42]<-GOM.params$diet[14,42]+0.08

#Shift predation of WhiteHake(41) from SilverHake(40) to AtlHerring(21)
#Shift 5%
GOM.params$diet[40,42]<-GOM.params$diet[40,42]-0.05
GOM.params$diet[21,42]<-GOM.params$diet[21,42]+0.05

#Shift predation of Cod(24) from SilverHake(40) to Megabenthos(56)
#Shift 5%
GOM.params$diet[40,25]<-GOM.params$diet[40,25]-0.05
GOM.params$diet[56,25]<-GOM.params$diet[56,25]+0.05

#Shift predation of Cod(24) from SilverHake(40) to AmPlaice(26)
#Shift 3%
GOM.params$diet[40,25]<-GOM.params$diet[40,25]-0.03
GOM.params$diet[26,25]<-GOM.params$diet[26,25]+0.03

#Shift predation of Cod(24) from SilverHake(40) to SmPelagics(14)
#Shift 3%
GOM.params$diet[40,25]<-GOM.params$diet[40,25]-0.03
GOM.params$diet[14,25]<-GOM.params$diet[14,25]+0.03

#Shift predation of RedHake(34) from SilverHake(40) to SmPelagics(14)
#Shift 3%
GOM.params$diet[40,35]<-GOM.params$diet[40,35]-0.03
GOM.params$diet[14,35]<-GOM.params$diet[14,35]+0.03

#Shift predation of OtherPelagics(20) from SilverHake(40) to SmPelagics(14)
#Shift 10%
GOM.params$diet[40,21]<-GOM.params$diet[40,21]-0.1
GOM.params$diet[14,21]<-GOM.params$diet[14,21]+0.1

#Shift predation of AtlHerring(21) from SilverHake(40) to SmPelagics(14)
#Shift 1.5%
GOM.params$diet[40,22]<-GOM.params$diet[40,22]-0.015
GOM.params$diet[14,22]<-GOM.params$diet[14,22]+0.015

#Shifting some predation of OceanPout
#Shift predation of OtherPelagics(20) from OceanPout(13) to Illex(8)
#Shift 0.29%
GOM.params$diet[13,21]<-GOM.params$diet[13,21]-0.029
GOM.params$diet[8,21]<-GOM.params$diet[8,21]+0.029

#Shift predation of Cod(42) from OceanPout(13) to Goosefish(39)
#Shift 0.05%
GOM.params$diet[13,25]<-GOM.params$diet[13,25]-0.005
GOM.params$diet[39,25]<-GOM.params$diet[39,25]+0.005

#Shift predation of SpinyDogfish(42) from OceanPout(13) to Illex(8)
#Shift 0.05%
GOM.params$diet[13,43]<-GOM.params$diet[13,43]-0.005
GOM.params$diet[39,43]<-GOM.params$diet[39,43]+0.005

#Shifting some predation to AtlScallop (EE too low)
#Shift predation of Macrobenthos(11) from AtlHalibut(28) to AtlScallop(19)
#Shift 0.0004%
GOM.params$diet[28,12]<-GOM.params$diet[28,12]-0.000004
GOM.params$diet[19,12]<-GOM.params$diet[19,12]+0.000004

#Shift predation of Macrobenthos(11) from Fourspot(36) to AtlScallop(19)
#Shift 0.0001%
GOM.params$diet[36,12]<-GOM.params$diet[36,12]-0.000001
GOM.params$diet[19,12]<-GOM.params$diet[19,12]+0.000001

#Shift predation of Macrobenthos(11) from Windowpane(46) to AtlScallop(19)
#Shift #Shift 0.0002%
GOM.params$diet[46,12]<-GOM.params$diet[46,12]-0.000002
GOM.params$diet[19,12]<-GOM.params$diet[19,12]+0.000002

#Shift predation of Macrobenthos(11) from WinterFlounder(47) to AtlScallop(19)
#Shift 0.0004%
GOM.params$diet[47,12]<-GOM.params$diet[47,12]-0.000004
GOM.params$diet[19,12]<-GOM.params$diet[19,12]+0.000004

#Shift predation of Macrobenthos(11) from WinterSkate(52) to AtlScallop(19)
#Shift 0.002%
GOM.params$diet[52,12]<-GOM.params$diet[52,12]-0.00002
GOM.params$diet[19,12]<-GOM.params$diet[19,12]+0.00002

#Shift predation of OtherPelagics
#Shift predation of OtherPelagics(20) from OtherPelagics(20) to Megabenthos(56)
#Shift 4%
GOM.params$diet[20,21]<-GOM.params$diet[20,21]-0.04
GOM.params$diet[56,21]<-GOM.params$diet[56,21]+0.04

#Shift predation of SilverHake (40) from OtherPelagics(20) to Illex(8)
#Shift 1.5%
GOM.params$diet[20,41]<-GOM.params$diet[20,41]-0.015
GOM.params$diet[8,41]<-GOM.params$diet[8,41]+0.015

#Shifting Sharks diet
#Move predation of Sharks(51) from Detritus(57) to Odontocetes(55)
#Shift 2.5%
GOM.params$diet[57,52]<-GOM.params$diet[57,52]-0.025
GOM.params$diet[55,52]<-GOM.params$diet[55,52]+0.025

#Move predation of Sharks(51) from Detritus(57) to Pinnipeds(53)
#Shift 2.5%
GOM.params$diet[57,52]<-GOM.params$diet[57,52]-0.025
GOM.params$diet[53,52]<-GOM.params$diet[53,52]+0.025

#Move predation of Sharks(51) from LgCopepods(5) to Goosefish(39)
#Shift 2.5%
GOM.params$diet[5,52]<-GOM.params$diet[5,52]-0.025
GOM.params$diet[39,52]<-0.025



#Assign data pedigree
source(here("R/data_pedigree.R"))

#Run model
GOM <- rpath(GOM.params, eco.name = 'GOM Ecosystem')

check.rpath.params(GOM.params)

#Examine EEs
EE<-GOM$EE
EE[order(EE)]
#Print EEs
#write.csv(EE,"outputs/EE_8.csv")

#Print final modeal
GOM

#Save files
save(GOM, file = "outputs/GOM_Rpath.RData")
save(GOM.params,file = "outputs/GOM_params_Rpath.RData")

#Initiate webplot
#webplot(GOM, labels = T)

#Examine TLs
#TL<-REco$TL
#TL[order(TL)]

#Rsim basic
#GOM.sim <- rsim.scenario(GOM, GOM.params, years = 1:50)
#For AB method, need to set NoIntegrate flag for 
#GOM.sim$params$NoIntegrate[4:5]<-0
#Run simulation
#GOM.run1 <- rsim.run(GOM.sim, method = 'AB', years = 1:50)


