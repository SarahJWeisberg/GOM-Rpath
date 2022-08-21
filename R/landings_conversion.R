#Title: Landings conversion

# Purpose: To convert commercial landings data to proper units (t/km^2) for use in
#   GOM Rpath model. Also to align landings data with model functional groups

# DataFiles:'mean_landings_gom_80_85.RData'; 'speciescodesandstrata/Species_codes.Rdata'

# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu

# Last modified: # Fri Jul  2 10:07:21 2021 ------------------------------

#Load packages
library(here);library(tidyr);library(dplyr);library(survdat)

#Load data
load("data/mean_landings_gom_80_85.RData")

#Change "HMS" to "HMS Fleet" to avoid confusion
mean.land[FLEET =="HMS",FLEET:="HMS Fleet"]

#Calculate total GOM area
#area<-sf::st_read(dsn=system.file("extdata","strata.shp",package="survdat"))
#area<-get_area(areaPolygon = area, areaDescription="STRATA")
#GOM.area<-subset(area, area$STRATUM %in% c(1220, 1240, 1260:1290, 1360:1400, 3560:3830))
#GOM.area<-sum(GOM.area$Area)

#Load species codes for matching & filter for just NESPP3 and RPath codes
#load("data/speciescodesandstrata/Species_codes.RData")
spp<-select(spp,one_of(c("NESPP3","RPATH")))
spp<-unique(na.exclude(spp)) 

#5 NESPP3 codes are matched with 2 RPATH codes
#remove duplicate NESPP3 codes
spp<-distinct(spp,NESPP3,.keep_all=TRUE)

#Load GOM groups
#source('R/Groups.R')

#Merge landings data with species codes & remove NESPP3 column
mean.land<-left_join(mean.land,spp,by="NESPP3")
mean.land<-na.exclude(mean.land[,-1])

#Sum landings for multispecies groups
mean.land<-mean.land %>% group_by(FLEET,RPATH) %>% summarise(SPPLIVMT=sum(SPPLIVMT))

#Convert to t/km^2
#Remove SPPLIVMT for clarity
mean.land$landings<-mean.land$SPPLIVMT/GOM.area
mean.land<-mean.land[,-3]

#Separate each fleet into its own data frame
fixed<-filter(mean.land,FLEET == "Fixed Gear")
lg_mesh<-filter(mean.land,FLEET == "LG Mesh")
other<-filter(mean.land,FLEET == "Other")
sm_mesh<-filter(mean.land,FLEET == "SM Mesh")
scallop<-filter(mean.land,FLEET == "Scallop Dredge")
trap<-filter(mean.land,FLEET == "Trap")
hms<-filter(mean.land,FLEET == "HMS Fleet")
pelagic<-filter(mean.land,FLEET == "Pelagic")
other_dredge<-filter(mean.land,FLEET == "Other Dredge")
clam<-filter(mean.land,FLEET == "Clam Dredge")

#Combine with all groups
#Fill in zeros for groups not commercially exploited
#Repeat for each gear type
#Start with fixed gear
fixed<-left_join(GOM.groups,fixed,by="RPATH")
fixed$FLEET<-"Fixed Gear"
fixed[is.na(fixed)]<-0

#Large mesh
lg_mesh<-left_join(GOM.groups,lg_mesh,by="RPATH")
lg_mesh$FLEET<-"LG Mesh"
lg_mesh[is.na(lg_mesh)]<-0

#Other
other<-left_join(GOM.groups,other,by="RPATH")
other$FLEET<-"Other"
other[is.na(other)]<-0

#Small mesh
sm_mesh<-left_join(GOM.groups,sm_mesh,by="RPATH")
sm_mesh$FLEET<-"SM Mesh"
sm_mesh[is.na(sm_mesh)]<-0

#Scallop
scallop<-left_join(GOM.groups,scallop,by="RPATH")
scallop$FLEET<-"Scallop"
scallop[is.na(scallop)]<-0

#Trap
trap<-left_join(GOM.groups,trap,by="RPATH")
trap$FLEET<-"Trap"
trap[is.na(trap)]<-0

#HMS
hms<-left_join(GOM.groups,hms,by="RPATH")
hms$FLEET<-"HMS Fleet"
hms[is.na(hms)]<-0

#Pelagic
pelagic<-left_join(GOM.groups,pelagic,by="RPATH")
pelagic$FLEET<-"Pelagic"
pelagic[is.na(pelagic)]<-0

#Other dredge
other_dredge<-left_join(GOM.groups,other_dredge,by="RPATH")
other_dredge$FLEET<-"Other Dredge"
other_dredge[is.na(other_dredge)]<-0

#Clam
clam<-left_join(GOM.groups,clam,by="RPATH")
clam$FLEET<-"Clam Dredge"
clam[is.na(clam)]<-0




