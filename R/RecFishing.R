##Estimate RecFishing to determine importance / model inclusion
#Based on Kurt Heim's code

library(here); library(data.table); library(ggplot2)

#Load data
catch<-read.csv("mrip_estim_catch_year_1981_2020.csv")
load("speciescodesandstrata/Species_codes.RData")

#Use name of states instead of codes
statedf<-data.frame(ST = c(23,33,25),ST_name =c( "ME","NH", "MA"))
catch<- as.data.table(merge(catch, statedf))
catch<-subset(catch[,c('YEAR','ST_name','LANDING','SP_CODE')])

#Merge with MRIP species codes
MRIP_codes <-read.csv('species_list.csv')
setnames(MRIP_codes,'sp_code','SP_CODE')
MRIP_codes<-unique(subset(MRIP_codes[,c('SP_CODE', 'SCIENTIFIC_NAME')]))
setnames(MRIP_codes, 'SCIENTIFIC_NAME','SCINAME')

#Merge with RPATH species codes
spp<-unique(subset(spp[,c('RPATH','SCINAME')]))
spp<-(merge(spp, MRIP_codes, by = 'SCINAME', all=T))

#Pull out species that are caught in MA,ME,NH
catch.names <-as.data.table(unique(catch$SCINAME))
setnames(catch.names, 'V1','SCINAME')
spp<-merge(spp,catch.names, all=T)
write.csv(spp,'MRIP_RPATH_Species_Names.csv')

#Remove redundancies & clean by hand
names.cleaned<-read.csv('MRIP_RPATH_Species_Names.csv')
names.cleaned<-names.cleaned[,2:3]

#Merge MRIP codes with cleaned species names
MRIP_codes <-merge(MRIP_codes,names.cleaned, by = 'SCINAME')

#Merge catch data with RPATH groups
catch<-merge(catch,MRIP_codes, by = 'SP_CODE', all=T)

#Convert lb to mt
catch <- catch[, catch.mt := LANDING / 2204.6]

#Remove all catch values less than 0.01mt
catch<- subset(catch, catch.mt >0.01)

write.csv(catch, 'RecFishingCatch.csv')

#ME Species
catch.ME<-subset(catch, catch$ST_name == 'ME')
sp.ME<-as.data.table(unique(catch.ME$RPATH))
#where did these dupes come from?

#Remove all species not in ME from original list (eg. BSB, scup)
catch.clean <- unique(catch[RPATH %in% sp.ME$V1])

#Sum species landing by year & state
catch.clean<-catch.clean[,sum(catch.mt), by =c('YEAR','RPATH')]
setnames(catch.clean,'V1','catch.mt')

#Divide by total area
catch.clean <-catch.clean[,catch.mt := catch.mt/GOM.com.area]
