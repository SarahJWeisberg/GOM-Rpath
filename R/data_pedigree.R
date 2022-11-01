#Data pedigree assignments
#Author: Sarah J. Weisberg

# Mon Sep 13 13:29:59 2021 ------------------------------

#0.2 if estimate is directly from data without modification
#0.4 if estimate is from data but data are variable or sparse
#0.5 if estimate is from stock assessment/regional analysis
#0.6 if estimate is from FishBase or other source
#0.8 if estimate was dramatically adjusted in balancing 

#Load pedigree table
pedigree<-as.data.table(read.csv(here('data/GOM_data_pedigree.csv')))
#Remove unneeded columns
pedigree<-pedigree[,-c(2:7)]
#pedigree<-left_join(groups_fleets,pedigree,by='RPATH')

#Remove pedigree for fleets
REco.params$pedigree<-REco.params$pedigree[1:58]

#Biomass
REco.params$pedigree[, Biomass := pedigree$Biomass]

#PB
REco.params$pedigree[, PB := pedigree$PB]

#QB
REco.params$pedigree[, QB := pedigree$QB]

#Diet
REco.params$pedigree[, Diet := pedigree$Diet]

#Fleets
REco.params$pedigree[, c('Fixed Gear','HMS Fleet','LG Mesh','Scallop Dredge','Trap','Other Dredge','Other','Pelagic','SM Mesh','Clam Dredge') := pedigree$Fleets]


