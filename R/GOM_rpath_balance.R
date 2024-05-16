# Title: RPath model balancing
# Purpose: This script generates a mass-balanced model of the 
#         Gulf of Maine (GoM) food web. Initial parameter estimates are pulled
#         from relevant data sources and then modified.

# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu


# Wed May  8 11:16:39 2024 ------------------------------

# Load packages ------------------------------------------------------------

library(devtools)
remotes::install_github('NOAA-EDAB/Rpath')
remotes::install_github('NOAA-EDAB/survdat')
library(Rpath); library(data.table);library(dplyr);library(here);library(tinytex);
library(survdat); library(lwgeom)


# Initial set up and biomass estimates ------------------------------------
#Load groups and fleets
source(here("R/groups_fleets.R"))

#Set up model with group names and types
#1 = primary producer, 0 = consumer, 2 = detritus, 3 = fleet
groups<-as.vector(groups_fleets$RPATH)
types<-c(1,rep(0,56),rep(2,1),rep(3,9))
GOM.params<-create.rpath.params(group = groups,type=types)
rm(types)

#Fill in biomass estimates
source(here("R/EMAX_biomass_estimates.R"))
biomass_80s<-biomass_80s %>% filter(RPATH !="Discards")
#and set Detritus biomass to NA
biomass_80s[which(RPATH == "Detritus")]$Biomass<-NA

biomass<-left_join(groups_fleets,biomass_80s,by="RPATH")

#Turn biomass into vector
biomass<-as.vector(biomass$Biomass)

#Change barndoor biomass to non-zero
#If set to 0, Ecosense does not work properly
biomass[35]<-2.5*10^-5


# Changes to biomass for balancing ---------------------------------------

#Multiply OtherCeph biomass by 65
biomass[10]<-biomass[10]*65

#Multiply SmFlatfish biomass by 150
biomass[15]<-biomass[15]*150

#Multiply SpinyDogfish biomass by 0.7
biomass[42]<-biomass[42]*0.7

#Multiply OtherPelagics biomass by 115
biomass[20]<-biomass[20]*115

#Multiply SmPelagics biomass by 25
biomass[14]<-biomass[14]*25

#Multiply Mesopelagics biomass by 20
biomass[22]<-biomass[22]*20

#Multiply SummerFlounder biomass by 5
biomass[29]<-biomass[29]*5

#Multiply Sharks biomass by 3
biomass[51]<-biomass[51]*3

#Multiply RiverHerring biomass by 20
biomass[18]<-biomass[18]*20

#Multiply AtlHerring biomass by 10
#In accordance with Yong
biomass[21]<-biomass[21]*10

#Multiply AtlMackerel biomass by 15
biomass[27]<-biomass[27]*15

#Multiply AmLobster biomass by 3
#In accordance with Yong's estimates
biomass[12]<-biomass[12]*3

#Multiply WinterFlounder biomass by 2.7
biomass[47]<-biomass[47]*2.7

#Change Windowpane to match assessment
biomass[46]<-0.033519469
#OR multiply by 2
#biomass[46]<-biomass[46]*2

#Multiply WhiteHake biomass by 0.75
biomass[41]<-biomass[41]*0.75

#Multiply SilverHake biomass by 2.75
biomass[40]<-biomass[40]*2.75

#Multiply WitchFlounder biomass by 5
biomass[48]<-biomass[48]*5

#Multiply OtherSkates biomass by 0.4
biomass[33]<-biomass[33]*0.4

#Multiply Redfish biomass by 3.25
biomass[43]<-biomass[43]*3.25

#Multiply OtherShrimps biomass by 15
biomass[17]<-biomass[17]*15

#Multiply NShrimp biomass by 6
#Similar estimate to Zheng & Chen
biomass[16]<-biomass[16]*6

#Multiply Pollock biomass by 0.75
biomass[38]<-biomass[38]*0.75

#Multiply Butterfish biomass by 3.15
biomass[50]<-biomass[50]*3.15

#Multiply OceanPout biomass by 4
biomass[13]<-biomass[13]*4

#Multiply Macrobenthos biomass by 0.4
biomass[11]<-biomass[11]*0.4

#Multiply Megabenthos biomass by 0.4
biomass[56]<-biomass[56]*0.4

#Multiply AmPlaice biomass by 1.35
biomass[26]<-biomass[26]*1.35

#Multiply YTFlounder biomass by 1.2
biomass[23]<-biomass[23]*1.2

#Multiply Fourspot biomass by 1.5
biomass[36]<-biomass[36]*1.5

#Multiply BlackSeaBass biomass by 1.4
biomass[49]<-biomass[49]*1.4

#Multiply OtherDemersals biomass by 5
biomass[31]<-biomass[31]*5

#Multiply RedHake biomass by 1.8
biomass[34]<-biomass[34]*1.8

#Multiply Loligo biomass by 1.65
biomass[9]<-biomass[9]*1.65

#Multiply LittleSkate biomass by 1.25
biomass[44]<-biomass[44]*1.25

#Multiply WinterSkate biomass by 1.75
biomass[52]<-biomass[52]*1.75

#Multiply AtlScallop biomass by 1.35
biomass[19]<-biomass[19]*1.35

#Fill model
GOM.params$model[,Biomass:=biomass]

# Fill PB, QB -------------------------------------------------------------
#Fill pb
source(here("R/biological_parameters.R"))
pb<-cbind(GOM.groups,PB)
pb<-left_join(groups_fleets,pb,by="RPATH")
pb<-as.vector(pb$PB)

#PB changes below mostly based on longevity estimates
#Tend to be lower than NWACS estimates

#Increase pb of SmPelagics to 2
#Value from Sean's GB model
pb[14]<-2

#Increase pb of Mesopelagics 1.5x
#Copying Sean
pb[22]<-pb[22]*1.5

#Decrease pb of Sharks to 0.13
#Based on conversation with Mike/Sean
pb[51]<-0.13

#Increase AtlHerring to 1.4
pb[21]<-1.4

#Decrease AmLobster pb to 1.5
#EMAX recommends ~1.5, NWACS estimate = 2.3
#Zhang & Chen (2007) have adults at 1.2, juveniles at 2.4
pb[12]<-1.5

#Decrease AmPlaice pb to 0.45
#Previous estimate = 0.6 from Heymans 2001
#max lifespan = 20 years
pb[26]<-0.45

#Decrease Barndoor pb to 0.2
#max lifespan = 30 years
pb[35]<-0.2

#Increase BlackSeaBass pb to 0.45- 49
#max lifespan = 20 years
pb[49]<-0.45

#Decrease Cod pb to 0.4  - 24
#0.87 value from NWACS, seems too high
#max lifespan = 25 years
pb[24]<-0.4

#Decreased Goosefish to 0.35
#0.4 value from Sean's dissertation
#max lifespan = 30 years
pb[39]<-0.35

#Haddock - 25
#0.7 from NWACS
#max lifespan = 20 years
pb[25]<-0.4

#Decrease Loligo / Illex / OtherCeph pb to 3
#5.72 estimate from NWACS is too high
pb[9]<-pb[8]<-pb[10]<-3

#Decrease OceanPout pb to 0.57
#max lifespan ~ 13 years
pb[13]<-0.57

#Increase OtherDemersals pb to 0.58
# 0.52 in NWACS, 0.9 in EMAX
pb[31]<-0.58

#Decrease OtherPelagics pb to 0.75
#1.17 in NWACS
pb[20]<-0.75

#Decrease OtherSkates pb to 0.4
#0.47 in Heymans 2001
pb[33]<-0.4

#Decrease Redfish pb to 0.25
#0.3 from Heymans 2001
#max lifespan = 40 years
pb[43]<-0.25

#Increase SilverHake pb to 0.6
#0.59 in Sissenwine 
#max lifespan = 12 years
pb[40]<-0.6

#Decrease SpinyDogfish pb to 0.27
#max lifespan = 31 years
#0.32 in NWACS
pb[42]<-0.27

#Decrease SummerFlounder pb to 0.7
#1.12 in NWACS, 0.46 in Heymans
#max lifespan = 9 years
pb[29]<-0.7

#Increase WhiteHake pb to 0.43
#max lifespan = 23 years
pb[41] <- 0.43

#Decrease Windowpane pb to 0.15
#0.25 from NWACS
pb[46]<-0.15

#Increase WinterFlounder pb to 0.57
#max lifespan = 14 years
pb[47]<-0.57

#Increase WitchFlounder pb to 0.4
#max lifespan = 25 years
pb[48]<-0.4

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

#Decrease qb of Goosefish - 0.5x
qb[39]<-qb[39]*0.5

#Decrease qb of Redfish - 0.5x
qb[43]<-qb[43]*0.5

#Decrease qb of Macrobenthos - 0.75x
qb[11]<-qb[11]*0.75

#Increase qb of WinterFlounder 2.2x
#Based on PREBAL results
qb[47]<-qb[47]*2.2

#Increase qb of WitchFlounder 2x
#Based on PREBAL results
qb[48]<-qb[48]*2

#Increase qb of SeaBirds
#Really low estimate in EMAX, higher in NWACS
qb[45]<-70

#QB changes below made to keep PB/QB ratios within 0.1-0.3 range

# Increase BlackSeaBass qb to 1.66
qb[49]<-1.66

#Decrease Cod qb to 1.2
qb[24]<-1.2

#Decrease Fourspot qb to 1.84
qb[36]<-1.84

#Increase LittleSkate qb to 1.245
qb[44]<-1.245

#Decrease OceanPout qb to 2
qb[13]<-2

#Decrease OtherPelagics qb to 2
qb[20]<-2

#Decrease OtherSkates qb to 1.1
qb[33]<-1.1

#Decrease SilverHake qb to 2
qb[40]<-2

#Decrease WhiteHake qb to 2
qb[41]<-2

#Increase qb of BaleenWhales to 4
#Closer to initial EMAX estimate
qb[54]<-3.75

#Increase qb of Odontocetes to 8
#In line with other models
qb[55]<-8

GOM.params$model[,QB:=qb]

# Fill biomass accoumation, unassimilated fraction, detrital fate ---------
#Fill biomass accumulation
source(here("R/biomass_accumulation.R"))
ba<-left_join(groups_fleets,biomass.accum,by="RPATH")
ba<-as.vector(ba$ba)
ba[is.na(ba)]<-0
ba[59:67]<-NA 

#Change barndoor ba
ba[35]<-ba[35]/1000

#Change OceanPout ba
ba[13]<- -0.004

GOM.params$model[,BioAcc:=ba]

#Fill unassimilated consumption
GOM.params$model[, Unassim := c(0,rep(0.4,5),rep(0.2, 51),rep(0,1), rep(NA, 9))]

#Increase unassim to 0.3 for other detritovores
GOM.params$model[Group %in% c('AmLobster', 'Macrobenthos', 'Megabenthos', 
                                 'AtlScallop', 'OtherShrimps'), 
                    Unassim := 0.3]

#Detrital Fate
GOM.params$model[, Detritus := c(rep(1, 57), rep(0, 10))]
#GOM.params$model[, Discards := c(rep(0, 56), rep(0,2),rep(1, 9))]


# Fisheries ---------------------------------------------------------------

#Landings
source(here("R/discards.R"))

#Fixed Gear
fixed<-left_join(groups_fleets,fixed,by="RPATH")
fixed<-as.vector(fixed$landings)
fixed[58]<-0
GOM.params$model[, "Fixed Gear" := fixed]

#Large Mesh
lg_mesh<-left_join(groups_fleets,lg_mesh,by="RPATH")
lg_mesh<-as.vector(lg_mesh$landings)
lg_mesh[58]<-0
GOM.params$model[, "LG Mesh" := lg_mesh]

#Other
other<-left_join(groups_fleets,other,by="RPATH")
other<-as.vector(other$landings)
other[58]<-0
GOM.params$model[, "Other" := other]

#Small Mesh
sm_mesh<-left_join(groups_fleets,sm_mesh,by="RPATH")
sm_mesh<-as.vector(sm_mesh$landings)
sm_mesh[58]<-0
GOM.params$model[, "SM Mesh" := sm_mesh]

#Scallop Dredge
#scallop<-left_join(groups_fleets,scallop,by="RPATH")
#scallop<-as.vector(scallop$landings)
#scallop[58:59]<-0
#GOM.params$model[, "Scallop Dredge" := scallop]

#Trap
trap<-left_join(groups_fleets,trap,by="RPATH")
trap<-as.vector(trap$landings)
trap[58]<-0
GOM.params$model[, "Trap" := trap]

#HMS.fleet
hms<-left_join(groups_fleets,hms,by="RPATH")
hms<-as.vector(hms$landings)
hms[58]<-0
GOM.params$model[, "HMS Fleet" := hms]

#Pelagic
pelagic<-left_join(groups_fleets,pelagic,by="RPATH")
pelagic<-as.vector(pelagic$landings)
pelagic[58]<-0

#Reduce fishing on OtherPelagics, multiply by 0.3
pelagic[20]<-pelagic[20]*0.3

GOM.params$model[, "Pelagic" := pelagic]

#Other Dredge
other_dredge<-left_join(groups_fleets,other_dredge,by="RPATH")
other_dredge<-as.vector(other_dredge$landings)
other_dredge[58]<-0
GOM.params$model[, "Other Dredge" := other_dredge]

#Clam
clam<-left_join(groups_fleets,clam,by="RPATH")
clam<-as.vector(clam$landings)
clam[58]<-0
GOM.params$model[, "Clam Dredge" := clam]

#Fill in discards
#Fixed Gear
#fixed.d<-left_join(groups_fleets,fixed.d,by="RPATH")
#fixed.d<-as.vector(fixed.d$discards)
#fixed.d[58:59]<-0
#GOM.params$model[, "Fixed Gear.disc" := fixed.d]

#Lg Mesh
#lg_mesh.d<-left_join(groups_fleets,lg_mesh.d,by="RPATH")
#lg_mesh.d<-as.vector(lg_mesh.d$discards)
#lg_mesh.d[58:59]<-0
#GOM.params$model[, "LG Mesh.disc" := lg_mesh.d]

#Other
#other.d<-left_join(groups_fleets,other.d,by="RPATH")
#other.d<-as.vector(other.d$discards)
#other.d[58:59]<-0
#GOM.params$model[, "Other.disc" := other.d]

#SM Mesh
#sm_mesh.d<-left_join(groups_fleets,sm_mesh.d,by="RPATH")
#sm_mesh.d<-as.vector(sm_mesh.d$discards)
#sm_mesh.d[58:59]<-0
#GOM.params$model[, "SM Mesh.disc" := sm_mesh.d]

#Scallop Dredge
#scallop.d<-left_join(groups_fleets,scallop.d,by="RPATH")
#scallop.d<-as.vector(scallop.d$discards)
#scallop.d[58:59]<-0
#GOM.params$model[, "Scallop Dredge" := scallop.d]

#Trap
#trap.d<-left_join(groups_fleets,trap.d,by="RPATH")
#trap.d<-as.vector(trap.d$discards)
#trap.d[58:59]<-0
#GOM.params$model[, "Trap.disc" := trap.d]

#HMS
#hms.d<-left_join(groups_fleets,hms.d,by="RPATH")
#hms.d<-as.vector(hms.d$discards)
#hms.d[58:59]<-0
#GOM.params$model[, "HMS Fleet.disc" := hms.d]

#Pelagic
#pelagic.d<-left_join(groups_fleets,pelagic.d,by="RPATH")
#pelagic.d<-as.vector(pelagic.d$discards)
#pelagic.d[58:59]<-0
#GOM.params$model[, "Pelagic.disc" := pelagic.d]

#Other Dredge
#other_dredge.d<-left_join(groups_fleets,other_dredge.d,by="RPATH")
#other_dredge.d<-as.vector(other_dredge.d$discards)
#other_dredge.d[58:59]<-0
#GOM.params$model[, "Other Dredge.disc" := other_dredge.d]

#Clam Dredge
#clam.d<-c(rep(0,56),rep(0,2),rep(NA,9))
#GOM.params$model[, "Clam Dredge.disc" := clam.d]


# Diet --------------------------------------------------------------------


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

#Shift predation of AtlMackerel(27) from OtherCeph(10) to Illex(8)
#Shift 1.1%
GOM.params$diet[10,28]<-GOM.params$diet[10,28]-0.011
GOM.params$diet[8,28]<-0.011

#Shift predation of AtlHerring(21) from OtherCeph(10) to Illex(8)
#Shift 0.029%
GOM.params$diet[10,22]<-GOM.params$diet[10,22]-0.00029
GOM.params$diet[8,22]<-GOM.params$diet[8,22]+0.00029

#Shift predation of Windowpane(46) from OtherCeph(10) to AmLobster(12)
#Shift 0.05%
GOM.params$diet[10,47]<-GOM.params$diet[10,47]-0.0005
GOM.params$diet[12,47]<-0.0005

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
#Shift 1%
GOM.params$diet[31,42]<-GOM.params$diet[31,42]-0.01
GOM.params$diet[56,42]<-GOM.params$diet[56,42]+0.01

#Shift predation of WhiteHake(41) from OtherDem(31) to WhiteHake(41)
#Shift 3%
GOM.params$diet[31,42]<-GOM.params$diet[31,42]-0.03
GOM.params$diet[41,42]<-GOM.params$diet[41,42]+0.03

#Shift predation of RedHake(34) from OtherDem(31) to Megabenthos(56)
#Shift 2%
GOM.params$diet[31,35]<-GOM.params$diet[31,35]-0.02
GOM.params$diet[56,35]<-GOM.params$diet[56,35]+0.02

#Shift predation of WhiteHake(41) from OtherDem(31) to Micronekton(7)
#Shift 0.5%
GOM.params$diet[31,42]<-GOM.params$diet[31,42]-0.005
GOM.params$diet[7,42]<-GOM.params$diet[7,42]+0.005

#Shift predation of OtherSkates(33) from OtherDem(31) to Megabenthos(56)
#Shift 13%
GOM.params$diet[31,34]<-GOM.params$diet[31,34]-0.13
GOM.params$diet[56,34]<-GOM.params$diet[56,34]+0.13

#Shift predation of SpinyDogfish(42) from OtherDem(31) to WhiteHake(41)
#Shift 0.5%
GOM.params$diet[31,43]<-GOM.params$diet[31,43]-0.005
GOM.params$diet[41,43]<-GOM.params$diet[41,43]+0.005

#Shift predation of Goosefish(39) from OtherDem(31) to WhiteHake(41)
#Shift 5%
GOM.params$diet[31,40]<-GOM.params$diet[31,40]-0.05
GOM.params$diet[41,40]<-GOM.params$diet[41,40]+0.05

#Shift predation of Cusk(30) from OtherDem(31) to AmLobster(12)
#Can cite Boudreau & Worm (2010)
#Shift 7%
GOM.params$diet[31,31]<-GOM.params$diet[31,31]-0.07
GOM.params$diet[12,31]<-0.07

#Shift predation of Cusk(30) from OtherDem(31) to Micronekton(7)
#Shift 9%
GOM.params$diet[31,31]<-GOM.params$diet[31,31]-0.09
GOM.params$diet[7,31]<-GOM.params$diet[7,31]+0.09

#Shift predation of Cusk(30) from OtherDem(31) to Megabenthos(56)
#Shift 12%
GOM.params$diet[31,31]<-GOM.params$diet[31,31]-0.12
GOM.params$diet[56,31]<-GOM.params$diet[56,31]+0.12

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

#Shift predation of OtherDemersals(31) from Redfish(43) to Megabenthos(56)
#Shift 0.2%
GOM.params$diet[43,32]<-GOM.params$diet[43,32]-0.002
GOM.params$diet[56,32]<-GOM.params$diet[56,32]+0.002

#Shift predation of WhiteHake(41) from Redfish(43) to Megabenthos(56)
#Shift 1%
GOM.params$diet[43,42]<-GOM.params$diet[43,42]-0.01
GOM.params$diet[56,42]<-GOM.params$diet[56,42]+0.01

#Shift predation of Cusk(30) from Redfish(43) to Megabenthos(56)
#Shift 5%
GOM.params$diet[43,31]<-GOM.params$diet[43,31]-0.05
GOM.params$diet[56,31]<-GOM.params$diet[56,31]+0.05

#Shifting some predation of SilverHake
#Shift predation of SilverHake(40) from SilverHake(40) to Megabenthos(56)
#Shift 2.5%
GOM.params$diet[40,41]<-GOM.params$diet[40,41]-0.025
GOM.params$diet[56,41]<-GOM.params$diet[56,41]+0.025

#Shift predation of SilverHake(40) from SilverHake(40) to SmPelagics(14)
#Shift 15%
GOM.params$diet[40,41]<-GOM.params$diet[40,41]-0.15
GOM.params$diet[14,41]<-GOM.params$diet[14,41]+0.15

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

#Shift predation of WhiteHake(41) from SilverHake(40) to WhiteHake(41)
#Shift 5%
GOM.params$diet[40,42]<-GOM.params$diet[40,42]-0.05
GOM.params$diet[41,42]<-GOM.params$diet[41,42]+0.05

#Shift predation of WhiteHake(41) from SilverHake(40) to Micronekton(7)
#Shift 4%
GOM.params$diet[40,42]<-GOM.params$diet[40,42]-0.04
GOM.params$diet[7,42]<-GOM.params$diet[7,42]+0.04

#Shift predation of WhiteHake(41) from SilverHake(40) to SmPelagics(14)
#Shift 10%
GOM.params$diet[40,42]<-GOM.params$diet[40,42]-0.1
GOM.params$diet[14,42]<-GOM.params$diet[14,42]+0.1

#Shift predation of Cod(24) from SilverHake(40) to Megabenthos(56)
#Shift 5%
GOM.params$diet[40,25]<-GOM.params$diet[40,25]-0.05
GOM.params$diet[56,25]<-GOM.params$diet[56,25]+0.05

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

# #Shift predation of Macrobenthos(11) from AtlHalibut(28) to AmLobster(12)
# #Shift 0.0004%
GOM.params$diet[28,12]<-GOM.params$diet[28,12]-0.000004
GOM.params$diet[12,12]<-GOM.params$diet[12,12]+0.000004
# 
# #Shift predation of Macrobenthos(11) from Fourspot(36) to AmLobster(12)
# #Shift 0.0001%
GOM.params$diet[36,12]<-GOM.params$diet[36,12]-0.000001
GOM.params$diet[12,12]<-GOM.params$diet[12,12]+0.000001

#Shift predation of OtherPelagics
#Shift predation of OtherPelagics(20) from OtherPelagics(20) to Megabenthos(56)
#Shift 4%
GOM.params$diet[20,21]<-GOM.params$diet[20,21]-0.04
GOM.params$diet[56,21]<-GOM.params$diet[56,21]+0.04

#Shift predation of OtherPelagics
#Shift predation of OtherPelagics(20) from OtherPelagics(20) to Megabenthos(56)
#Shift 1%
GOM.params$diet[20,41]<-GOM.params$diet[20,41]-0.01
GOM.params$diet[14,41]<-GOM.params$diet[14,41]+0.01

#Shifting Sharks diet
#Move predation of Sharks(51) from Detritus(58) to Odontocetes(55)
#Shift 2.5%
GOM.params$diet[58,52]<-GOM.params$diet[58,52]-0.025
GOM.params$diet[55,52]<-GOM.params$diet[55,52]+0.025

#Move predation of Sharks(51) from Detritus(58) to Pinnipeds(53)
#Shift 2.5%
GOM.params$diet[58,52]<-GOM.params$diet[58,52]-0.025
GOM.params$diet[53,52]<-GOM.params$diet[53,52]+0.025

#Move predation of Sharks(51) from LgCopepods(5) to Goosefish(39)
#Shift 2.5%
GOM.params$diet[5,52]<-GOM.params$diet[5,52]-0.025
GOM.params$diet[39,52]<-0.025

#Macrobenthos
#Move predation of Macrobenthos(11) from Macrobenthos(11) to Detritus (58)
#Shift 4%
GOM.params$diet[11,12]<-GOM.params$diet[11,12]-0.03
GOM.params$diet[58,12]<-GOM.params$diet[58,12]+0.03

#Reassign copepod groups
source(here("R/redo_copes.R"))

#Shift predation of BaleenWhales(54) from Butterfish(50) to SmCopepods(6)
#Shift 0.7%
GOM.params$diet[50,55]<-GOM.params$diet[50,55]-0.007
GOM.params$diet[6,55]<-GOM.params$diet[6,55]+0.007
#Assign data pedigree
source(here("R/data_pedigree.R"))

# Check for balance -------------------------------------------------------
#Load Sean's prebal functions
source(url("https://github.com/NOAA-EDAB/GBRpath/blob/master/R/PreBal.R?raw=true"))

#Run model
GOM <- rpath(GOM.params, eco.name = 'GOM Ecosystem')

check.rpath.params(GOM.params)

#Examine EEs
EE<-GOM$EE
EE[order(EE)]
#Print EEs
#write.csv(EE,"outputs/EE_8.csv")

#Print final model
#GOM

# #Save files
save(GOM, file = "outputs/GOM_Rpath.RData")
save(GOM.params,file = "outputs/GOM_params_Rpath.RData")
