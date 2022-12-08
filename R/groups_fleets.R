#Title: GOM Rpath Functional Groups

# Purpose: This script simply creates a data table with all of the groups, including
#     functional groups and fleets, used in the GOM Rpath model.

# DataFile: data/mean_landings_gom_80_85.RData

# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu

# Thu Dec  1 10:02:21 2022 ------------------------------


#Load needed packages
library(here);library(data.table)

#Load landings data
load(here("data/mean_landings_gom_80_85.RData"))

#Change "HMS" to "HMS Fleet" to avoid confusion
mean.land[FLEET =="HMS",FLEET:="HMS Fleet"]

#Load functional groups
source(here("R/Groups.R"))

#Add detritus and discards
detritus<-as.data.table("Detritus")
colnames(detritus)<-"RPATH"
groups_fleets<-rbind(GOM.groups,detritus)

#Pull out unique fleets
fleets<-as.data.table(unique(mean.land$FLEET))
colnames(fleets)<-"RPATH"

#Remove Scallop Dredge (no landings)
fleets<-subset(fleets, RPATH != "Scallop Dredge")

#Bind fleets and functional groups
groups_fleets<-rbind(groups_fleets,fleets)

#Remove unneeded items from environment
rm(detritus,fleets)


