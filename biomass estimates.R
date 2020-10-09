#Calculate biomass estimates for RPath groups
#Using Sean's stratified mean biomass & swept area biomass functions for surveyed groups
#Using EMAX values for others

library(data.table); library(rgdal); library(Survdat); library(here)

here()
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

#Input species list
GOM.groups <-as.data.table(c('Phytoplankton', 'Bacteria', 'Microzooplankton', 'GelZooplankton','LgCopepods', 'SmCopepods', 'Micronekton', 'Illex',' Loligo', 'OtherCephalopods','Macrobenthos','AmLobster','OceanPout','SmPelagics','SmFlatfishes','NShrimp','RiverHerring','AtlScallop','OtherPelagics','SummerFlounder','AtlHerring','Mesopelagics','SouthernDemersals','YTFlounder','Cod','Haddock','AmPlaice','AtlMackerel','Cusk','OtherDemersals','HMS','OtherSkates','RedHake','Barndoor Skate','Fourspot','SmoothDogfish','Pollock','Goosefish','SilverHake','WhiteHake','SpinyDogfish','Redfish','LittleSkate','SeaBirds','Windowpane','WinterFlounder','WitchFlounder','BlackSeaBass','Sharks','WinterSkate','Pinnipeds','BaleenWhales','Odontocetes'))
setnames(GOM.groups,'V1','RPATH')

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
total.biomass <- sweptarea(GOM.prep, mean.biomass, strat.col = 'STRATUM', area.col = 'Area')

#merge with RPATH names
total.biomass<-merge(total.biomass,spp[,list(SVSPP,RPATH,SCINAME,Fall.q)], by = 'SVSPP')

#total area
GOM.strat.area <- strat.area[STRATUM %in% c(1220, 1240, 1260:1290, 1360:1400, 3560:3830), sum(Area)]

#biomass / area in mt
total.biomass <- total.biomass[, biomass.t_area :=(tot.biomass*.001)/(Fall.q*GOM.strat.area)]

#average over 1980s
GOM.biomass.80s <- total.biomass[YEAR %in% 1980:1985, mean(biomass.t_area), by = RPATH]
setnames(GOM.biomass.80s,'V1','Biomass')

#Output results to csv
write.csv(GOM.biomass.80s, 'Mean Biomass_GoM_1980-85.csv')

##add EMAX groups
GOM.EMAX<-as.data.table(read.csv('GOM_EMAX_params.csv'))

##Merge group list with survey biomass estimates
GOM.groups<-merge(GOM.biomass.80s, GOM.groups, by = 'RPATH', all.y=TRUE)
setnames(GOM.groups,'V1','Biomass', skip_absent = TRUE)

#Filter EMAX model for group, biomass
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
GOM.EMAX<-GOM.EMAX[RPATH %like% 'Megabenthos',RPATH := 'Mega',]
GOM.EMAX<-GOM.EMAX[RPATH %like% 'Pelagics',RPATH := 'Pelagics',]
GOM.EMAX<-GOM.EMAX[RPATH %like% 'Demersals',RPATH := 'Demersals',]
GOM.EMAX<-GOM.EMAX[RPATH %like% 'Sharks', RPATH :='Sharks']

#Aggregate EMAX macrobenthos groups into one 
BenthosBiomass<-GOM.EMAX[RPATH == 'Macrobenthos', sum(Biomass),]
GOM.EMAX<-GOM.EMAX[RPATH == 'Macrobenthos', Biomass:=BenthosBiomass,]

#Remove groups not included in Rpath model
GOM.EMAX<- GOM.EMAX[RPATH %notin% c('Larval-juv fish- all','Shrimp et al.','Mega','Pelagics','Demersals','Discard','Detritus-POC','Fishery'),]
GOM.EMAX<-unique(GOM.EMAX)

#Merge survey and EMAX biomass estimates
GOM.biomass.80s<-merge(GOM.groups,GOM.EMAX,by='RPATH', all=TRUE)
GOM.biomass.80s<-GOM.biomass.80s[is.na(Biomass.y) , Biomass.y := Biomass.x]
GOM.biomass.80s<-GOM.biomass.80s[,Biomass.x :=NULL]
setnames(GOM.biomass.80s,"Biomass.y","Biomass")

#Save as .csv
write.csv(GOM.biomass, 'GOM_Parameters_V1.csv')
