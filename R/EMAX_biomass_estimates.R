#Title: GOM Rpath Biomass estimates (EMAX)

# Purpose: This script generates biomass estimates for 
#       the multi-species functional groups used in the GOM-Rpath model.
#       Data are sourced from the EMAX modeling exercise. Model is built
#       to be analogous to Georges Bank Rpath model
#       https://github.com/NOAA-EDAB/GBRpath

# DataFile:'NEFSC_BTS_2021_all_seasons.RData'

# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu

xfun::session_info()
#Last modified: # Tue Jun 15 17:31:55 2021 ------------------------------

#Load needed packages
library(here); library(data.table)

#load EMAX model
GOM.EMAX<-as.data.table(read.csv('data/GOM_EMAX_params.csv'))

#load GOM groups
source('R/Groups.R')

#load biomass estimates from survey (single species)
source('R/survey_biomass_estimates.R')

#Merge group list with survey biomass estimates
GOM.groups<-merge(biomass_80s, GOM.groups, by = 'RPATH',all.y = TRUE)
setnames(GOM.groups,'V1','Biomass', skip_absent = TRUE)

#Convert biomass to numeric
GOM.groups$Biomass<-as.numeric(GOM.groups$Biomass)

#Filter EMAX model for group and biomass only
setnames(GOM.EMAX,'model.Group','RPATH')
setnames(GOM.EMAX,'model.Biomass','Biomass')
GOM.EMAX<-GOM.EMAX[, c('RPATH','Biomass')]

#Change EMAX names to match RPATH names
GOM.EMAX<-GOM.EMAX[RPATH == 'Small copepods', RPATH := 'SmCopepods',]
GOM.EMAX<-GOM.EMAX[RPATH == 'Large Copepods', RPATH := 'LgCopepods',]
GOM.EMAX<-GOM.EMAX[RPATH == 'Sea Birds', RPATH := 'SeaBirds',]
GOM.EMAX<-GOM.EMAX[RPATH %like% 'Macro', RPATH:='Macrobenthos',]
GOM.EMAX<-GOM.EMAX[RPATH %like% 'Baleen', RPATH := 'BaleenWhales',]
GOM.EMAX<-GOM.EMAX[RPATH %like% 'Gel',RPATH := 'GelZooplankton',]
GOM.EMAX<-GOM.EMAX[RPATH %like% 'Phyto', RPATH :='Phytoplankton',]
GOM.EMAX<-GOM.EMAX[RPATH %like% 'Megabenthos',RPATH := 'Megabenthos',]
GOM.EMAX<-GOM.EMAX[RPATH %like% 'Pelagics',RPATH := 'Pelagics',]
GOM.EMAX<-GOM.EMAX[RPATH %like% 'Demersals',RPATH := 'Demersals',]
GOM.EMAX<-GOM.EMAX[RPATH %like% 'Sharks', RPATH :='Sharks']

#Aggregate EMAX macrobenthos groups into one 
MacrobenthosBiomass<-GOM.EMAX[RPATH == 'Macrobenthos', sum(Biomass),]
GOM.EMAX<-GOM.EMAX[RPATH == 'Macrobenthos', Biomass:=MacrobenthosBiomass,]

#Aggregate EMAX megabenthos groups into one, subtracting lobster and scallops
MegabenthosBiomass<-GOM.EMAX[RPATH == 'Megabenthos', sum(Biomass)] - GOM.groups[RPATH == 'AmLobster', Biomass] - GOM.groups[RPATH == 'AtlScallop', Biomass]
GOM.EMAX<-GOM.EMAX[RPATH == 'Megabenthos', Biomass:=MegabenthosBiomass,]

#Remove groups not included in Rpath model
GOM.EMAX<- GOM.EMAX[!RPATH %in% c('Larval-juv fish- all','Shrimp et al.','Pelagics','Demersals','Discard','Detritus-POC','Fishery'),]
GOM.EMAX<-unique(GOM.EMAX)

#Merge survey and EMAX biomass estimates
biomass_80s<-merge(GOM.groups,GOM.EMAX,by='RPATH', all=TRUE)
biomass_80s<-biomass_80s[is.na(Biomass.y) , Biomass.y := Biomass.x]
biomass_80s<-biomass_80s[,Biomass.x :=NULL]
setnames(biomass_80s,"Biomass.y","Biomass")
