#Generate simple linear regressions for all functional groups
#Determine if biomass accumulation (BA) terms are necessary

library(Survdat); library(ggplot2); library(data.table); library(rgdal)


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
mean.biomass <- stratmean(GOM.prep, group.col = 'SVSPP', strat.col = 'STRATUM')

#Calculate total biomass estimates
total.biomass <- sweptarea(GOM.prep, mean.biomass, strat.col = 'STRATUM', area.col = 'Area')

#Merge total biomass with RPATH names
total.biomass<-merge(total.biomass,spp[,list(SVSPP,RPATH,SCINAME,Fall.q)], by = 'SVSPP')

#Calcuate biomass / area in mt
total.biomass <- total.biomass[, biomass.t_area   :=    (tot.biomass*.001)/(Fall.q*GOM.strat.area)]

#RPATH GOM groups
RPATH.GOM <- unique(total.biomass$RPATH, na.rm=true)

#Write a loop to run lm for all RPATH groups, extract slope

ba<-c()
p<-c()
for (i in 2:59){
    lm<-lm(biomass.t_area~YEAR, data = subset(total.biomass, RPATH == RPATH.GOM[i]))
    spF <- as.numeric(summary(lm)$fstatistic)
    p[i] <- pf(spF[1], spF[2], spF[3], lower = F)
    ba[i] <-as.numeric(lm[[1]][2])
}

ba<-cbind(RPATH.GOM,ba,p)

#write to csv
write.csv(ba, 'ba.csv')

#sample single species lm plot
plot(biomass.t_area~YEAR, data = subset(total.biomass, RPATH == 'AtlHerring'))
abline(lm(biomass.t_area~YEAR, data = subset(total.biomass, RPATH == 'AtlHerring')))
summary(lm(biomass.t_area~YEAR, data = subset(total.biomass, RPATH == 'AtlHerring')))

#Sean's code
groups <- unique(GB.raw[, RPATH])
BA.all <- c()
for(igroup in groups){
  species <- GB.raw[RPATH == igroup, ]
  #Default is BA = 0 unless significant
  BA.sp <- data.table(RPATH = igroup, BA = 0, m = 0, b = 0)
  #Test significance using lm
  if(length(species[, swept.bio.mt]) > 2){
    spLM <- lm(swept.bio.mt ~ YEAR, data = species)
    spF <- summary(spLM)$fstatistic
    spP <- pf(spF[1], spF[2], spF[3], lower = F)
    #If significant replace 0 with slope divided by GB area (scaled)
    if(spP <= 0.05) BA.sp[, BA := spLM$coefficients[2] / A]
    #Add slope and intercept term for plots
    BA.sp[, m := spLM$coefficients[2]]
    BA.sp[, b := spLM$coefficients[1]]
  }
  BA.all <- rbindlist(list(BA.all, BA.sp))
}
BA.sig <- BA.all[BA != 0, RPATH]

summary(lm)$fstatistic



  
  
  