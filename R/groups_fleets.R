#Title: GOM Rpath Functional Groups

# Purpose: This script simply creates a data table with all of the groups, including
#     functional groups and fleets, used in the GOM Rpath model.

# DataFile: data/mean_landings_gom_80_85.RData

# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu

# Sun Nov 13 15:00:15 2022 ------------------------------


#Load needed packages
library(here);library(data.table)

#Load landings data
load(here("data/mean_landings_gom_80_85.RData"))

#Change "HMS" to "HMS Fleet" to avoid confusion
mean.land[FLEET =="HMS",FLEET:="HMS Fleet"]

#Load functional groups
source(here("R/Groups.R"))

#Add detritus and discards
d.d<-as.data.table(rbind("Detritus","Discards"))
colnames(d.d)<-"RPATH"
groups_fleets<-rbind(GOM.groups,d.d)

#Pull out unique fleets
fleets<-as.data.table(unique(mean.land$FLEET))
colnames(fleets)<-"RPATH"

#Bind fleets and functional groups
groups_fleets<-rbind(groups_fleets,fleets)

#remove Scallop Dredge
groups_fleets<-groups_fleets %>% filter(RPATH != "Scallop Dredge")

#Remove unneeded items from environment
rm(d.d,fleets)



