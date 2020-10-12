#Generate simple linear regressions for all functional groups
#Determine if biomass accumulation (BA) terms are necessary

#Load data-------------------------------------------------------------------

#Load survey data
load('Survdat.RData')

#Load strata
strata <- readOGR('speciescodesandstrata', 'strata')

#Load species code list
load("speciescodesandstrata/Species_codes.RData")

#Generate area table
strat.area <- getarea(strata, 'STRATA')
setnames(strat.area, 'STRATA', 'STRATUM')

#---------------------------------------------------------------------------

#Subset by season & strata set - choose fall for GOM strata
fall.GOM <- survdat[SEASON == 'FALL' & STRATUM %in% c(1220, 1240, 1260:1290, 1360:1400, 3560:3830), ]

#Calculate total survey area
GOM.strat.area <- strat.area[STRATUM %in% c(1220, 1240, 1260:1290, 1360:1400, 3560:3830), sum(Area)]

#Run stratification prep
setnames(fall.GOM, 'EST_YEAR', 'YEAR')
GOM.prep <- stratprep(fall.GOM, strat.area, strat.col = 'STRATUM', area.col = 'Area')

#Merge with RPATH names
spp <- spp[!duplicated(spp$SVSPP),]
GOM.prep<-merge(GOM.prep, spp[,list(SVSPP,COMNAME,RPATH,SCINAME)], by = 'SVSPP')

#Calculate stratified means by RPATH group
mean.biomass <- stratmean(GOM.prep, group.col = 'RPATH', strat.col = 'STRATUM')
setnames(mean.biomass, 'strat.biomass','biomass')

#RPATH GOM groups
RPATH.GOM <- unique(mean.biomass$RPATH, na.rm=true)
RPATH.GOM[2]

#Write a loop to run lm for all RPATH groups, extract slope
ba <- c()
for (i in 2:59) {
  lm<-lm(biomass~YEAR, data = subset(mean.biomass, RPATH == RPATH.GOM[i]))
  ba[i]<-as.numeric(lm[[1]][2])
}

ba<-cbind(RPATH.GOM,ba)





  
  
  