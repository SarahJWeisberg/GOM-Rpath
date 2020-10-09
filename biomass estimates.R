#Calculate biomass estimates for RPath groups
#Using Sean's stratified mean biomass & swept area biomass functions for surveyed groups
#Using EMAX values for others

library(data.table); library(rgdal); library(Survdat); library(here)

#User define

'%notin%' <-Negate('%in%')

#Grab survdat.r
#load(file.path(data.dir, 'Survdat.RData'))
#Load species list
load("Survdat.RData")

#Load species list
load("Species_codes.RData")

#Grab strata
#strata <- readOGR(gis.dir, 'strata')
strata<- readOGR('speciescodesandstrata','strata')

#Generate area table
strat.area<- getarea(strata, 'STRATA')
setnames(strat.area, 'STRATA', 'STRATUM')

#Subset by season/ strata set - fall for GOM (inc. inshore)
fall.GOM <- survdat[SEASON == 'FALL' & STRATUM %in% c(1220, 1240, 1260:1290, 1360:1400, 3560:3830), ]

#Run stratification prep
setnames(fall.GOM, 'EST_YEAR', 'YEAR')
GOM.prep <- stratprep(fall.GOM, strat.area, strat.col = 'STRATUM', area.col = 'Area')

#Merge with RPATH names
spp <- spp[!duplicated(spp$SVSPP),]
#GOM.prep<-merge(GOM.prep, spp[,list(SVSPP,COMNAME,RPATH,SCINAME)], by = 'SVSPP')

#Calculate stratified means 
mean.biomass <- stratmean(GOM.prep, group.col = 'SVSPP', 
                      strat.col = 'STRATUM')
#merge with RPATH names
mean.biomass<-merge(mean.biomass,spp[,list(SVSPP,RPATH,SCINAME,Fall.q)], by = 'SVSPP')

#Calculate total biomass/abundance estimates
total.biomass <- sweptarea(GOM.prep, mean.biomass, strat.col = 'STRATUM', 
                           area.col = 'Area')

#merge with RPATH names
total.biomass<-merge(total.biomass,spp[,list(SVSPP,RPATH,SCINAME,Fall.q)], by = 'SVSPP')

#total area
GOM.strat.area <- strat.area[STRATUM %in% c(1220, 1240, 1260:1290, 1360:1400, 3560:3830), sum(Area)]

#biomass / area in mt
total.biomass <- total.biomass[, biomass.t_area   :=       (tot.biomass*.001)/(Fall.q*GOM.strat.area)]

#average over 1980s
GOM.biomass.80s <- total.biomass[YEAR %in% 1980:1985, mean(biomass.t_area), by = RPATH]
setnames(GOM.biomass.80s,'V1','Biomass')

#Output results to csv
write.csv(GOM.biomass.80s, 'Mean Biomass_GoM_1980-85.csv')

##add EMAX groups
GOM.groups<- read.csv('Groups.csv')
setnames(GOM.groups, 'GOM.Group','RPATH')
GOM.groups<-GOM.groups[1:53,]

GOM.groups<-merge(GOM.biomass.80s, GOM.groups, by = 'RPATH', all.y=TRUE)
setnames(GOM.groups,'V1','Biomass')

##Add GOM.EMAX biomass from Ecobase pulls (separate script)
setnames(GOM.EMAX,'Group','RPATH')
GOM.EMAX<-GOM.EMAX[, 1:5]
GOM.EMAX<-GOM.EMAX[RPATH %like% 'Macro', RPATH:='Macrobenthos',]
GOM.EMAX<-GOM.EMAX[RPATH == 'Small copepods', RPATH := 'SmCopepods',]
GOM.EMAX<-GOM.EMAX[RPATH == 'Large Copepods',RPATH := 'LgCopepods',]
GOM.EMAX<-GOM.EMAX[RPATH %like% 'Baleen',RPATH := 'BaleenWhales',]
GOM.EMAX<-GOM.EMAX[RPATH %like% 'Gel',RPATH := 'GelZooplankton',]
GOM.EMAX<-GOM.EMAX[RPATH == 'Small Copepods',RPATH := 'SmCopepods',]
GOM.EMAX<-GOM.EMAX[RPATH %like% 'Phyto', RPATH :='Phytoplankton',]
GOM.EMAX<-GOM.EMAX[RPATH == 'SeaBirds',RPATH := 'Sea Bird',]
GOM.EMAX<-GOM.EMAX[RPATH %like% 'Megabenthos',RPATH := 'Mega',]
GOM.EMAX<-GOM.EMAX[RPATH %like% 'Pelagics',RPATH := 'Pelagics',]
GOM.EMAX<-GOM.EMAX[RPATH %like% 'Demersals',RPATH := 'Demersals',]
GOM.EMAX<- GOM.EMAX[RPATH %notin% c('Larval-juv fish- all','Sharks- pelagics','Shrimp et al.','Mega','Pelagics','Demersals','Discard','Detritus-POC','Macrobenthos'),]


BenthosBiomass<-GOM.EMAX[RPATH == 'Macrobenthos', sum(Biomass),]
BenthosPB<-GOM.EMAX[RPATH == 'Macrobenthos',weighted.mean(PB)]
BenthosTL<-GOM.EMAX[RPATH == 'Macrobenthos',weighted.mean(TL)]
BenthosQB<-GOM.EMAX[RPATH == 'Macrobenthos',weighted.mean(QB)]

GOM.EMAX<-GOM.EMAX[RPATH == 'Macrobenthos', PB:=BenthosPB,]
GOM.EMAX<-GOM.EMAX[RPATH == 'Macrobenthos', QB:=BenthosQB,]
GOM.EMAX<-GOM.EMAX[RPATH == 'Macrobenthos', Biomass:=BenthosBiomass,]
GOM.EMAX<-GOM.EMAX[!duplicated(GOM.EMAX),]

GOM.final<-merge(GOM.groups,GOM.EMAX,by='RPATH',all=TRUE)
GOM.final<-as.data.table(GOM.final)
GOM.final<-GOM.final[,"TL" :=NULL]
GOM.final<-GOM.final[is.na(Biomass.x) , Biomass.x := Biomass.y]

GOM.final<-GOM.final[,"Biomass.y" :=NULL]
setnames(GOM.final,"Biomass.x","Biomass")

#save as .csv
write.csv(GOM.final, 'GOM_Parameters_V1.csv')
