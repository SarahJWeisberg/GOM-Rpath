#Title: GOM Rpath Functional Groups

# Purpose: This script simply creates a data table with all of the groups, including
#     functional groups and fleets, used in the GOM Rpath model.

# DataFile: data/mean_landings_gom_80_85.RData

# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu

# Last modified: # Fri Jul  2 14:10:11 2021 ------------------------------


#Load needed packages
library(here);library(data.table)

#Load landings data
load("~/Desktop/GOM-Rpath/data/mean_landings_gom_80_85.RData")

#Load functional groups
source("R/Groups.R")

#Pull out unique fleets
fleets<-as.data.table(unique(mean.land$FLEET))
colnames(fleets)<-"RPATH"
fleets[fleets$RPATH == "HMS"]<-"HMS.fleet"

#Bind fleets and functional groups
groups_fleets<-rbind(GOM.groups,fleets)

#Add detritus and discards
d.d<-as.data.table(rbind("Detritus","Discards"))
colnames(d.d)<-"RPATH"
groups_fleets<-rbind(groups_fleets,d.d)


