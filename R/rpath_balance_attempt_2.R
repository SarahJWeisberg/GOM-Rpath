#RPath model run - balancing attempt 2
#Differs from attempt 1 in the decrease in fishing pressure on OtherPel

# Mon Aug  2 18:38:05 2021 ------------------------------


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

#Multiply SmFlatfish biomass by 20
biomass[15]<-biomass[15]*20

#Multiply SpinyDogfish biomass by 0.5
biomass[42]<-biomass[42]*0.5

#Multiply OtherPelagics biomass by 100
biomass[20]<-biomass[20]*100

#Multiply SmPelagics biomass by 10
biomass[14]<-biomass[14]*10

#Multiply Mesopelagics biomass by 100
biomass[22]<-biomass[22]*10

#Multiply SummerFlounder biomass by 2
biomass[29]<-biomass[29]*2

#Multiply Sharks biomass by 3
biomass[51]<-biomass[51]*3

#Multiply Cod biomass by 0.4
#In accordance with Yong
biomass[24]<-biomass[24]*0.4

#Multiply RiverHerring biomass by 20
biomass[18]<-biomass[18]*20

#Multiply AtlHerring biomass by 5
#In accordance with Yong
biomass[21]<-biomass[21]*5

#Multiply AmLobster biomass by 3
#In accordance with Yong
biomass[12]<-biomass[12]*3

#Multiply WinterFlounder biomass by 2x
biomass[47]<-biomass[47]*2

#Multiply Phytoplankton biomass by 0.5
#biomass[1]<-biomass[1]*0.5

#Multiply Macrobenthos biomass by 0.5
#biomass[11]<-biomass[11]*0.5

#Multiply WhiteHake biomass by 0.4
#biomass[41]<-biomass[41]*0.4

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
pb[27]<-pb[27]*1.5

#Increase pb of SilverHake to 0.735
#Value from Yong
pb[40]<-0.735

#Increase pb of OtherShrimps 2.25x
#Copying Sean
pb[17]<-pb[17]*2.25

#Increase pb of OtherDemersals 1.36x
#Copying Sean
pb[31]<-pb[31]*1.36

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

#Reduce fishing on OtherPel by 50%
pelagic[20]<-pelagic[20]*0.5

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

#Focus on shifting predation from OtherCeph to Illex to be more realistic
#Starting OtherCeph EE is ~1000, starting Illex EE is ~0.3

#Shift predation of Other Skates (33) from OtherCeph(10) to Illex(8)
#Shift 4%
REco.params$diet[10,34]<-REco.params$diet[10,34]-0.04
REco.params$diet[8,34]<-REco.params$diet[8,34]+0.04

#Shift predation of WhiteHake OtherCeph(10) to Illex(8)
#Shift 2%
REco.params$diet[10,42]<-REco.params$diet[10,42]-0.02
REco.params$diet[8,42]<-REco.params$diet[8,42]+0.02

#Shift predation of SpinyDogfish(42) from OtherCeph(10) to Illex(8)
#Shift 2%
REco.params$diet[10,43]<-REco.params$diet[10,43]-0.02
REco.params$diet[8,43]<-REco.params$diet[8,43]+0.02

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

#Shift predation of OtherPelagics(20) from OtherCeph(10) to Illex(8)
#Shift 0.4%
REco.params$diet[10,21]<-REco.params$diet[10,21]-0.004
REco.params$diet[8,21]<-0.004

#Shift predation of Goosefish(39) from OtherCeph(10) to Illex(8)
#Shift 1%
REco.params$diet[10,40]<-REco.params$diet[10,40]-0.01
REco.params$diet[8,40]<-REco.params$diet[8,40]+0.01

#Shifting some predation on SmFlatfishes to SummerFlounder
#Starting EE of SmFlat is ~900, starting EE of SummerFlounder is ~1.5

#Shift predation of Goosefish(39) from SmFlat(15) to SummerFlounder (29)
#Shift 0.2%
REco.params$diet[15,40]<-REco.params$diet[15,40]-0.002
REco.params$diet[29,40]<-0.002


#Shifting some predation on OtherDemersals to Pollock
#Starting EE of OtherDemersals is ~16, starting EE of Pollock is ~0.25

#Shift predation of WhiteHake(41) from OtherDem(31) to Pollock(38)
#Shift 2%
REco.params$diet[31,42]<-REco.params$diet[31,42]-0.02
REco.params$diet[38,42]<-REco.params$diet[38,42]+0.02

#Shift predation of OtherSkates(33) from OtherDem(31) to Pollock(38)
#Shift 5%
REco.params$diet[31,34]<-REco.params$diet[31,34]-0.05
REco.params$diet[38,34]<-0.05

#Shifting some predation on OtherDemersals to Cusk
#Starting EE of OtherDemersals is ~16, starting EE of Pollock is ~0.6

#Shift predation of WhiteHake(41) from OtherDem(31) to Cusk(30)
#Shift 0.5%
REco.params$diet[31,42]<-REco.params$diet[31,42]-0.005
REco.params$diet[30,42]<-0.005

#Shifting some predation of Butterfish 

#Shift predation of OtherPelagics(20) from Butterfish(50) to Illex(8)
#Shift 5%
REco.params$diet[50,21]<-REco.params$diet[50,21]-0.05
REco.params$diet[8,21]<-REco.params$diet[8,21]+0.05

#Shifting some predation of WinterFlounder

#Shift predation of OtherPelagics(20) from WinterFlounder(47) to Illex(8)
#Shift 2%
REco.params$diet[47,21]<-REco.params$diet[47,21]-0.02
REco.params$diet[8,21]<-REco.params$diet[8,21]+.02

#Shift predation of Macrobenthos(11) from WinterFlounder(47) to OtherSkates(33)
#Shift 0.002%
REco.params$diet[47,12]<-REco.params$diet[47,12]-0.00002
REco.params$diet[33,12]<-REco.params$diet[33,12]+0.00002

#Run model
REco <- rpath(REco.params, eco.name = 'GOM Ecosystem')

check.rpath.params(REco.params)

#Examine EEs
EE<-REco$EE
EE[order(EE)]

#Print EEs
#write.csv(EE,"outputs/EE_5.csv")

#write.Rpath(REco,morts=T,"outputs/GOM_Rpath_4.csv")
