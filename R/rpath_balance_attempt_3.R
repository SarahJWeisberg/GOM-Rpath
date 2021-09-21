#RPath model run - balancing attempt 3
#Tweaking the balanced model from attempt 2

# Thu Sep  2 14:21:54 2021 ------------------------------



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

#Change barndoor to 10^-5 biomass
biomass[35]<-10^-5

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

#Multiply AtlMackerel biomass by 5.05
biomass[27]<-biomass[27]*5.05

#Multiply AmLobster biomass by 3
#In accordance with Yong's estimates
biomass[12]<-biomass[12]*3

#Multiply WinterFlounder biomass by 2.7x
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
#Getting closer to Yong's shrimp group
biomass[16]<-biomass[16]*5

#Multiply Pollock biomass by 0.75
biomass[38]<-biomass[38]*0.75

#Multiply Butterfish biomass by 4
biomass[50]<-biomass[50]*4

#Multiply OceanPout biomass by 1.5
biomass[13]<-biomass[13]*1.5

#Multiply Macrobenthos biomass by 0.5
#This makes much more sense with EMAX, Sean & Yong
#How did initial estimate get to be so high?
biomass[11]<-biomass[11]*0.5

#Multiply AmPlaice biomass by 1.2
biomass[26]<-biomass[26]*1.2

#Multiply YTFlounder biomass by 1.15
biomass[23]<-biomass[23]*1.15

#Multiply Fourspot biomass by 1.05
biomass[36]<-biomass[36]*1.05

#Multiply BlackSeaBass biomass by 1.4
biomass[49]<-biomass[49]*1.4

#Multiply OtherDemersals biomass by 2
biomass[31]<-biomass[31]*2.1

#Multiply Phytoplankton biomass by 2
#biomass[1]<-biomass[1]/2

#Multiply RedHake biomass by 1.1
biomass[34]<-biomass[34]*1.1

#Multiply Loligo biomass by 1.03
biomass[9]<-biomass[9]*1.03

#Fill model
REco.params$model[,Biomass:=biomass]


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

#Increase pb of AtlHerring 1.5x
#Copying Sean
pb[21]<-pb[21]*1.5

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

#Increase barndoor production
#pb[35]<-1.45

REco.params$model[,PB:=pb]

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

REco.params$model[,QB:=qb]

#Fill biomass accumulation
source(here("R/Biomass_Accumulation.R"))
ba<-left_join(groups_fleets,biomass.accum,by="RPATH")
ba<-as.vector(ba$ba)
ba[is.na(ba)]<-0
ba[59:68]<-NA

#Change barndoor ba
ba[35]<-ba[35]/1000
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

#Reduce fishing on OtherPelagics, multiply by 0.9
pelagic[20]<-pelagic[20]*0.9

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

#Shifting predation on OtherCeph

#Shift predation of Cod (24) from OtherCeph(10) to Illex(8)
#Shift 1.1%
REco.params$diet[10,25]<-REco.params$diet[10,25]-.011
REco.params$diet[8,25]<-REco.params$diet[8,25]+0.011

#Shift predation of OtherSkates(33) from OtherCeph(10) to Illex(8)
#Shift 4.15%
REco.params$diet[10,34]<-REco.params$diet[10,34]-0.0415
REco.params$diet[8,34]<-REco.params$diet[8,34]+0.0415

#Shift predation of WhiteHake(41) from OtherCeph(10) to Illex(8)
#Shift 1.75%
REco.params$diet[10,42]<-REco.params$diet[10,42]-0.0175
REco.params$diet[8,42]<-REco.params$diet[8,42]+0.0175

#Shift predation of SpinyDogfish(42) from OtherCeph(10) to Illex(8)
#Shift 1.7%
REco.params$diet[10,43]<-REco.params$diet[10,43]-0.017
REco.params$diet[8,43]<-REco.params$diet[8,43]+0.017

#Shift predation of Cusk(30) from OtherCeph(10) to Illex(8)
#Shift 11%
REco.params$diet[10,31]<-REco.params$diet[10,31]-0.11
REco.params$diet[8,31]<-0.11

#Shift predation of Pollock(38) from OtherCeph(10) to Illex(8)
#Shift 0.58%
REco.params$diet[10,39]<-REco.params$diet[10,39]-0.0058
REco.params$diet[8,39]<-REco.params$diet[8,39]+0.0058

#Shift predation of Haddock(25) from OtherCeph(10) to Illex(8)
#Shift 0.83%
REco.params$diet[10,26]<-REco.params$diet[10,26]-0.0083
REco.params$diet[8,26]<-0.0083

#Shift predation of OtherPelagics(20) from OtherCeph(10) to Illex(8)
#Shift 0.4%
REco.params$diet[10,21]<-REco.params$diet[10,21]-0.004
REco.params$diet[8,21]<-0.004

#Shift predation of Goosefish(39) from OtherCeph(10) to Illex(8)
#Shift 1.8%
REco.params$diet[10,40]<-REco.params$diet[10,40]-0.018
REco.params$diet[8,40]<-REco.params$diet[8,40]+0.018

#Shift predation of SilverHake(40) from OtherCeph(10) to Illex(8)
#Shift 0.4%
REco.params$diet[10,41]<-REco.params$diet[10,41]-0.004
REco.params$diet[8,41]<-REco.params$diet[8,41]+0.004

#Shift predation of RedHake(34) from OtherCeph(10) to Illex(8)
#Shift 1.1%
REco.params$diet[10,35]<-REco.params$diet[10,35]-0.011
REco.params$diet[8,35]<-REco.params$diet[8,35]+0.011

#Shift predation of Redfish(43) from OtherCeph(10) to Illex(8)
#Shift 0.21%
REco.params$diet[10,44]<-REco.params$diet[10,44]-0.0021
REco.params$diet[8,44]<-0.0021

#Shift predation of AtlHerring(27) from OtherCeph(10) to Illex(8)
#Shift 1.1%
REco.params$diet[10,28]<-REco.params$diet[10,28]-0.011
REco.params$diet[8,28]<-0.011

#Shift predation of AtlMackerel(21) from OtherCeph(10) to Illex(8)
#Shift 0.029%
REco.params$diet[10,22]<-REco.params$diet[10,22]-0.00029
REco.params$diet[8,22]<-REco.params$diet[8,22]+0.00029

#Shift predation of WinterFlounder(47) from OtherCeph(10) to AmLobster(12)
#Shift 1.6%
REco.params$diet[10,48]<-REco.params$diet[10,48]-0.016
REco.params$diet[12,48]<-REco.params$diet[12,48]+0.016

#Shift predation on SmFlatfishes

#Shift predation of Goosefish(39) from SmFlat(15) to WitchFlounder(48)
#Shift 0.25%
REco.params$diet[15,40]<-REco.params$diet[15,40]-0.0025
REco.params$diet[48,40]<-REco.params$diet[48,40]+0.0025

#Shift predation of WhiteHake(41) from SmFlat(15) to Illex(8)
#Shift 0.005%
REco.params$diet[15,42]<-REco.params$diet[15,42]-0.00005
REco.params$diet[8,42]<-REco.params$diet[8,42]+0.00005

#Shift predation of RedHake(34) from SmFlatfishes(15) to Illex(8)
#Shift 0.025%
REco.params$diet[15,35]<-REco.params$diet[15,35]-0.00025
REco.params$diet[8,35]<-REco.params$diet[8,35]+0.00025

#Shift predation of Cod (24) from SmFlatfishes(15) to Illex(8)
#Shift 0.11%
REco.params$diet[15,25]<-REco.params$diet[15,25]-.0011
REco.params$diet[8,25]<-REco.params$diet[8,25]+0.0011

#Shifting predation on OtherDemersals
#Shift predation of WhiteHake(41) from OtherDem(31) to Megabenthos(56)
#Shift 2%
REco.params$diet[31,42]<-REco.params$diet[31,42]-0.02
REco.params$diet[56,42]<-REco.params$diet[56,42]+0.02

#Shift predation of WhiteHake(41) from OtherDem(31) to WhiteHake(41)
#Shift 3%
REco.params$diet[31,42]<-REco.params$diet[31,42]-0.03
REco.params$diet[41,42]<-REco.params$diet[41,42]+0.03

#Shift predation of WhiteHake(41) from OtherDem(31) to Micronekton(7)
#Shift 0.5%
REco.params$diet[31,42]<-REco.params$diet[31,42]-0.005
REco.params$diet[7,42]<-REco.params$diet[7,42]+0.005

#Shift predation of OtherSkates(33) from OtherDem(31) to Megabenthos(56)
#Shift 13%
REco.params$diet[31,34]<-REco.params$diet[31,34]-0.13
REco.params$diet[56,34]<-REco.params$diet[56,34]+0.13

#Shift predation of OtherSkates(33) from OtherDem(31) to RedHake(34)
#Shift 1%
REco.params$diet[31,34]<-REco.params$diet[31,34]-0.01
REco.params$diet[34,34]<-REco.params$diet[34,34]+0.01

#Shift predation of OtherPelagics(20) from OtherDem(31) to OtherPelagics(20)
#Shift 9%
REco.params$diet[31,21]<-REco.params$diet[31,21]-0.09
REco.params$diet[20,21]<-REco.params$diet[20,21]+0.09

#Shift predation of SpinyDogfish(42) from OtherDem(31) to WhiteHake(41)
#Shift 0.5%
REco.params$diet[31,43]<-REco.params$diet[31,43]-0.005
REco.params$diet[41,43]<-REco.params$diet[41,43]+0.005

#Shift predation of Goosefish(39) from OtherDem(31) to WhiteHake(41)
#Shift 5%
REco.params$diet[31,40]<-REco.params$diet[31,40]-0.05
REco.params$diet[41,40]<-REco.params$diet[41,40]+0.05

#Shift predation of Cod(24) from OtherDem(31) to RedHake(34)
#Shift 3%
REco.params$diet[31,25]<-REco.params$diet[31,25]-0.03
REco.params$diet[34,25]<-REco.params$diet[34,25]+0.03

#Shift predation of Cusk(30) from OtherDem(31) to AmLobster(12)
#Can cite Boudreau & Worm (2010)
#Shift 7%
REco.params$diet[31,31]<-REco.params$diet[31,31]-0.07
REco.params$diet[12,31]<-0.07

#Shift predation of Cusk(30) from OtherDem(31) to Micronekton(7)
#Shift 9%
REco.params$diet[31,31]<-REco.params$diet[31,31]-0.09
REco.params$diet[7,31]<-REco.params$diet[7,31]+0.09

#Shift predation of Cusk(30) from OtherDem(31) to OtherShrimps(17)
#Shift 3%
REco.params$diet[31,31]<-REco.params$diet[31,31]-0.03
REco.params$diet[17,31]<-REco.params$diet[17,31]+0.03

#Shift predation of Cusk(30) from OtherDem(31) to Megabenthos(56)
#Shift 12%
REco.params$diet[31,31]<-REco.params$diet[31,31]-0.12
REco.params$diet[56,31]<-REco.params$diet[56,31]+0.12


#Shift predation of Goosefish(39) from OtherDem(31) to RedHake(34)
#Shift 4%
REco.params$diet[31,40]<-REco.params$diet[31,40]-0.04
REco.params$diet[34,40]<-REco.params$diet[34,40]+0.04

#Shift predation of Goosefish(39) from OtherDem(31) to Goosefish(39)
#Shift 5%
REco.params$diet[31,40]<-REco.params$diet[31,40]-0.05
REco.params$diet[39,40]<-REco.params$diet[39,40]+0.05

#Shift predation of Haddock(25) from OtherDem(31) to Megabenthos(56)
#Shift 0.9%
REco.params$diet[31,26]<-REco.params$diet[31,26]-0.009
REco.params$diet[8,26]<-REco.params$diet[8,26]+0.009

#Shifting some predation of Butterfish 

#Shift predation of OtherPelagics(20) from Butterfish(50) to Illex(8)
#Shift 7.6%
REco.params$diet[50,21]<-REco.params$diet[50,21]-0.076
REco.params$diet[8,21]<-REco.params$diet[8,21]+0.076

#Shift predation of SilverHake(40) from Butterfish(50) to Illex(8)
#Shift 0.8%
REco.params$diet[50,41]<-REco.params$diet[50,41]-0.008
REco.params$diet[8,41]<-REco.params$diet[8,41]+0.008

#Shift predation of BaleenWhales(54) from Butterfish(50) to Illex(8)
#Shift 0.7%
REco.params$diet[50,55]<-REco.params$diet[50,55]-0.007
REco.params$diet[8,55]<-REco.params$diet[8,55]+0.007

#Shift predation of SummerFlounder(29) from Butterfish(50) to OtherPelagics(20) 
#Shift 8%
REco.params$diet[50,30]<-REco.params$diet[50,30]-0.08
REco.params$diet[20,30]<-REco.params$diet[20,30]+0.08

#Shift predation of SummerFlounder(29) from Butterfish(50) to Megabenthos(56) 
#Shift 8%
REco.params$diet[50,30]<-REco.params$diet[50,30]-0.08
REco.params$diet[56,30]<-REco.params$diet[56,30]+0.08

#Shift predation of SpinyDogfish(42) from Butterfish(50) to Illex(8)
#Shift 0.9%
REco.params$diet[50,43]<-REco.params$diet[50,43]-0.009
REco.params$diet[8,43]<-REco.params$diet[8,43]+0.009


#Shifting some predation of WinterFlounder

#Shift predation of OtherPelagics(20) from WinterFlounder(47) to Illex(8)
#Shift 2%
REco.params$diet[47,21]<-REco.params$diet[47,21]-0.02
REco.params$diet[8,21]<-REco.params$diet[8,21]+.02

#Shift predation of Macrobenthos(11) from WinterFlounder(47) to OtherSkates(33)
#Shift 0.002%
REco.params$diet[47,12]<-REco.params$diet[47,12]-0.00002
REco.params$diet[33,12]<-REco.params$diet[33,12]+0.00002

#Shifting some predation of BSB

#Shift predation of SpinyDogfish(42) from BlackSeaBass(49) to Megabenthos(56)
#Shift 0.04%
REco.params$diet[49,43]<-REco.params$diet[49,43]-0.0004
REco.params$diet[56,43]<-REco.params$diet[56,43]+0.0004

#Shift predation of SilverHake(40) from BlackSeaBass(49) to Megabenthos(56)
#Shift 0.009%
REco.params$diet[49,41]<-REco.params$diet[49,41]-0.00009
REco.params$diet[56,41]<-REco.params$diet[56,41]+0.00009

#Shift predation of Cod(24) from BlackSeaBass(49) to Illex(8)
#Shift 0.01%
REco.params$diet[49,25]<-REco.params$diet[49,25]-0.0001
REco.params$diet[8,25]<-REco.params$diet[8,25]+0.0001

#Shift some predation of Macrobenthos(11) from BlackSeaBass(49) to AtlScallop(19)
#Shift 0.00001%
REco.params$diet[49,12]<-REco.params$diet[49,12]-0.0000001
REco.params$diet[19,12]<-REco.params$diet[19,12]+0.0000001

#Shift some predation of Goosefish(39) from BlackSeaBass(49) to Illex(8)
#Shift 0.015%
REco.params$diet[49,40]<-REco.params$diet[49,40]-0.00015
REco.params$diet[8,40]<-REco.params$diet[8,40]+0.00015

#Shifting some predation of Windowpane

#Shift predation of OtherSkates(33) from Windowpane(46) to Megabenthos(56)
#Shift 0.15%
REco.params$diet[46,34]<-REco.params$diet[46,34]-0.0015
REco.params$diet[56,34]<-REco.params$diet[56,34]+0.0015

#Shifting some predation of Mesopelagics
#Shift predation of Pollock(38) from Mesopelagics(22) to Illex(8)
#Shift 0.7%
REco.params$diet[22,39]<-REco.params$diet[22,39]-0.007
REco.params$diet[8,39]<-REco.params$diet[8,39]+0.007

#Shifting some predation of Sharks
#Shift predation of Cod(24) from Sharks(51) to Pollock(38)
#Shift 0.17%
REco.params$diet[51,25]<-REco.params$diet[51,25]-0.0017
REco.params$diet[38,25]<-REco.params$diet[38,25]+0.0017

#Shifting some predation of Redfish
#Shift predation of Macrobenthos(11) from Redfish(43) to Megabenthos(56)
#Shift 0.011%
REco.params$diet[43,12]<-REco.params$diet[43,12]-0.00011
REco.params$diet[56,12]<-REco.params$diet[56,12]+0.00011

#Shifting some predation of SilverHake
#Shift predation of SilverHake(40) from SilverHake(40) to Megabenthos(56)
#Shift 2.5%
REco.params$diet[40,41]<-REco.params$diet[40,41]-0.025
REco.params$diet[56,41]<-REco.params$diet[56,41]+0.025

#Shift predation of SilverHake(40) from SilverHake(40) to OtherShrimps(17)
#Shift 2.5%
REco.params$diet[40,41]<-REco.params$diet[40,41]-0.025
REco.params$diet[17,41]<-REco.params$diet[17,41]+0.025

#Shift predation of SilverHake(40) from SilverHake(40) to SmPelagics(14)
#Shift 9%
REco.params$diet[40,41]<-REco.params$diet[40,41]-0.09
REco.params$diet[14,41]<-REco.params$diet[14,41]+0.09

#Shift predation of Pollock(38) from SilverHake(40) to Micronekton(7)
#Shift 3%
REco.params$diet[40,39]<-REco.params$diet[40,39]-0.03
REco.params$diet[7,39]<-REco.params$diet[7,39]+0.03

#Shift predation of Pollock(38) from SilverHake(40) to SmPelagics(14)
#Shift 10%
REco.params$diet[40,39]<-REco.params$diet[40,39]-0.1
REco.params$diet[14,39]<-REco.params$diet[14,39]+0.1

#Shift predation of Pollock(38) from SilverHake(40) to Pollock(38)
#Shift 3%
REco.params$diet[40,39]<-REco.params$diet[40,39]-0.03
REco.params$diet[38,39]<-REco.params$diet[38,39]+0.03

#Shift predation of WhiteHake(41) from SilverHake(40) to Megabenthos(56)
#Shift 2%
REco.params$diet[40,42]<-REco.params$diet[40,42]-0.02
REco.params$diet[56,42]<-REco.params$diet[56,42]+0.02

#Shift predation of WhiteHake(41) from SilverHake(40) to Illex(8)
#Shift 2%
REco.params$diet[40,42]<-REco.params$diet[40,42]-0.02
REco.params$diet[8,42]<-REco.params$diet[8,42]+0.02

#Shift predation of WhiteHake(41) from SilverHake(40) to RedHake(34)
#Shift 2%
REco.params$diet[40,42]<-REco.params$diet[40,42]-0.02
REco.params$diet[34,42]<-REco.params$diet[34,42]+0.02

#Shift predation of WhiteHake(41) from SilverHake(40) to WhiteHake(41)
#Shift 2%
REco.params$diet[40,42]<-REco.params$diet[40,42]-0.02
REco.params$diet[41,42]<-REco.params$diet[41,42]+0.02

#Shift predation of WhiteHake(41) from SilverHake(40) to Micronekton(7)
#Shift 2%
REco.params$diet[40,42]<-REco.params$diet[40,42]-0.02
REco.params$diet[7,42]<-REco.params$diet[7,42]+0.02

#Shift predation of WhiteHake(41) from SilverHake(40) to SmPelagics(14)
#Shift 8%
REco.params$diet[40,42]<-REco.params$diet[40,42]-0.08
REco.params$diet[14,42]<-REco.params$diet[14,42]+0.08

#Shift predation of WhiteHake(41) from SilverHake(40) to AtlHerring(21)
#Shift 5%
REco.params$diet[40,42]<-REco.params$diet[40,42]-0.05
REco.params$diet[21,42]<-REco.params$diet[21,42]+0.05

#Shift predation of Cod(24) from SilverHake(40) to Megabenthos(56)
#Shift 5%
REco.params$diet[40,25]<-REco.params$diet[40,25]-0.05
REco.params$diet[56,25]<-REco.params$diet[56,25]+0.05

#Shift predation of Cod(24) from SilverHake(40) to AmPlaice(26)
#Shift 3%
REco.params$diet[40,25]<-REco.params$diet[40,25]-0.03
REco.params$diet[26,25]<-REco.params$diet[26,25]+0.03

#Shift predation of Cod(24) from SilverHake(40) to SmPelagics(14)
#Shift 3%
REco.params$diet[40,25]<-REco.params$diet[40,25]-0.03
REco.params$diet[14,25]<-REco.params$diet[14,25]+0.03

#Shift predation of RedHake(34) from SilverHake(40) to SmPelagics(14)
#Shift 3%
REco.params$diet[40,35]<-REco.params$diet[40,35]-0.03
REco.params$diet[14,35]<-REco.params$diet[14,35]+0.03

#Shift predation of OtherPelagics(20) from SilverHake(40) to SmPelagics(14)
#Shift 10%
REco.params$diet[40,21]<-REco.params$diet[40,21]-0.1
REco.params$diet[14,21]<-REco.params$diet[14,21]+0.1

#Shift predation of AtlHerring(21) from SilverHake(40) to SmPelagics(14)
#Shift 1.5%
REco.params$diet[40,22]<-REco.params$diet[40,22]-0.015
REco.params$diet[14,22]<-REco.params$diet[14,22]+0.015

#Shifting some predation of OceanPout
#Shift predation of OtherPelagics(20) from OceanPout(13) to Illex(8)
#Shift 0.29%
REco.params$diet[13,21]<-REco.params$diet[13,21]-0.029
REco.params$diet[8,21]<-REco.params$diet[8,21]+0.029

#Shifting some predation to AtlScallop (EE too low)
#Shift predation of Macrobenthos(11) from AtlHalibut(28) to AtlScallop(19)
#Shift 0.0004%
REco.params$diet[28,12]<-REco.params$diet[28,12]-0.000004
REco.params$diet[19,12]<-REco.params$diet[19,12]+0.000004

#Shift predation of Macrobenthos(11) from Fourspot(36) to AtlScallop(19)
#Shift 0.0001%
REco.params$diet[36,12]<-REco.params$diet[36,12]-0.000001
REco.params$diet[19,12]<-REco.params$diet[19,12]+0.000001

#Shift predation of Macrobenthos(11) from Windowpane(46) to AtlScallop(19)
#Shift #Shift 0.0002%
REco.params$diet[46,12]<-REco.params$diet[46,12]-0.000002
REco.params$diet[19,12]<-REco.params$diet[19,12]+0.000002

#Shift predation of Macrobenthos(11) from WinterFlounder(47) to AtlScallop(19)
#Shift 0.0004%
REco.params$diet[47,12]<-REco.params$diet[47,12]-0.000004
REco.params$diet[19,12]<-REco.params$diet[19,12]+0.000004

#Shift predation of Macrobenthos(11) from WinterSkate(52) to AtlScallop(19)
#Shift 0.002%
REco.params$diet[52,12]<-REco.params$diet[52,12]-0.00002
REco.params$diet[19,12]<-REco.params$diet[19,12]+0.00002

#Shift predation of OtherPelagics

#Shift predation of OtherPelagics(20) from OtherPelagics(20) to Megabenthos(56)
#Shift 4%
REco.params$diet[20,21]<-REco.params$diet[20,21]-0.04
REco.params$diet[56,21]<-REco.params$diet[56,21]+0.04

#Shift predation of SilverHake (40) from OtherPelagics(20) to Illex(8)
#Shift 1.5%
REco.params$diet[20,41]<-REco.params$diet[20,41]-0.015
REco.params$diet[8,41]<-REco.params$diet[8,41]+0.015

#Shift predation onto Barndoor
#Shift predation of OtherDemersals(31) from OtherSkates(33) to Barndoor(35)
#Shift 0.00001%
REco.params$diet[33,32]<-REco.params$diet[33,32]-0.0000001
REco.params$diet[35,32]<-0.0000001

#Shift predation of SpinyDogfish(42) from OtherSkates(33) to Barndoor(35)
#Shift 0.000001%
REco.params$diet[33,43]<-REco.params$diet[33,43]-0.00000001
REco.params$diet[35,43]<-0.00000001

#Shift predation of Macrobenthos(11) from OtherSkates(33) to Barndoor(35)
#Shift 0.00000001%
REco.params$diet[33,12]<-REco.params$diet[33,12]-0.000000001
REco.params$diet[35,12]<-0.000000001

#Run model
REco <- rpath(REco.params, eco.name = 'GOM Ecosystem')

check.rpath.params(REco.params)

#Examine EEs
EE<-REco$EE
EE[order(EE)]
#Print EEs
#write.csv(EE,"outputs/EE_8.csv")

#write.Rpath(REco,morts=T,"outputs/GOM_Rpath_14.csv")

#Print final modeal
REco

#Run EcoSim
#Run model forward 50 years
REco.sim <- rsim.scenario(REco, REco.params, years = 1:50)
REco.run1 <- rsim.run(REco.sim, method = 'RK4', years = 1:50)
#rsim.plot(REco.run1, groups[1:7])
#rsim.plot(REco.run1, groups[8:14])
#rsim.plot(REco.run1, groups[15:21])
#rsim.plot(REco.run1, groups[22:28])
#rsim.plot(REco.run1, groups[29:35])
#rsim.plot(REco.run1, groups[36:42])
#rsim.plot(REco.run1, groups[43:49])
#rsim.plot(REco.run1, groups[50:56])

