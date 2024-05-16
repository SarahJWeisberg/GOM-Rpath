#Data pedigree assignments
#Author: Sarah J. Weisberg

# Thu Dec  1 10:53:39 2022 ------------------------------


#0.2 if estimate is directly from data without modification
#0.4 if estimate is from data but data are variable or sparse
#0.5 if estimate is from stock assessment/regional analysis
#0.6 if estimate is from FishBase or other source
#0.8 if estimate was dramatically adjusted in balancing 

#Load required packages
library(data.table)

#Load pedigree table
pedigree<-as.data.table(read.csv(here('data/GOM_data_pedigree.csv')))
#Remove unneeded columns
pedigree<-pedigree[,-c(2:7)]
#pedigree<-left_join(groups_fleets,pedigree,by='RPATH')
#Remove pedigree for discards
pedigree<-pedigree[1:58]

#Remove pedigree for fleets & discards
GOM.params$pedigree<-GOM.params$pedigree[1:58]

#Biomass
GOM.params$pedigree[, Biomass := pedigree$Biomass]

#PB
GOM.params$pedigree[, PB := pedigree$PB]

#QB
GOM.params$pedigree[, QB := pedigree$QB]

#Diet
GOM.params$pedigree[, Diet := pedigree$Diet]

#Fleets
#GOM.params$pedigree[, c('Fixed Gear','HMS Fleet','LG Mesh','Trap','Other Dredge','Other','Pelagic','SM Mesh','Clam Dredge') := pedigree$Fleets]


