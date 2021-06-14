# Calculate mean stratified biomass to identify most relevant species
# Using code from https://github.com/slucey/RSurvey/blob/master/survey_template.R

#-------------------------------------------------------------------------------
#Required packages
#May need to download Survdat package from GitHub
devtools::install_github('slucey/RSurvey/Survdat')
library(data.table); library(rgdal); library(Survdat)

#-------------------------------------------------------------------------------
#User created functions

#-------------------------------------------------------------------------------
setwd("~/Desktop/GOM-RPath")

#Grab survdat.r
load("~/Desktop/GOM-Rpath/Survdat.RData")

#Grab strata
strata<- readOGR('/Users/sjw/Desktop/GOM-Rpath/speciescodesandstrata','strata')

#Generate area table
strat.area <- getarea(strata, 'STRATA')
setnames(strat.area, 'STRATA', 'STRATUM')

#Load species list
load("~/Desktop/GOM-Rpath/speciescodesandstrata/Species_codes.RData")

#Subset by season/ strata set
#Start with fall data
fall.GOM <- survdat[SEASON == 'FALL' & STRATUM %in%  c(1220, 1240, 1260:1290, 1360:1400, 3560:3830)]

#Run stratification prep
setnames(fall.GOM, 'EST_YEAR', 'YEAR')
GOM.prep <- stratprep(fall.GOM, strat.area, strat.col = 'STRATUM', area.col = 'Area')
as.data.table(GOM.prep)
as.data.table(spp)
spp <- spp[!duplicated(spp$SVSPP),]

#Merge survdat with Rpath species & group names
GOM.prep<-merge(GOM.prep, spp[,list(SVSPP,COMNAME,RPATH,SCINAME)], by = 'SVSPP')


#Calculate stratified means - can do multiple species at once 
#Note: The function will merge aggregated groups for you
GOM.strat.mean <- stratmean(GOM.prep, group.col = 'RPATH', strat.col = 'STRATUM')

#Plot data - all species, facet by species, individual species
ggplot(data = GOM.strat.mean) +
  geom_point(mapping = aes(x = YEAR, y = strat.biomass, color = RPATH))

ggplot(subset(GOM.strat.mean, RPATH == 'BlackSeaBass')) +
  geom_point( mapping = aes(x = YEAR, y = strat.biomass)) 

ggplot(subset(GOM.strat.mean, RPATH == 'AtlHerring')) +
  geom_point( mapping = aes(x = YEAR, y = strat.biomass)) 

ggplot(subset(GOM.strat.mean, RPATH == 'AtlHalibut')) +
  geom_point( mapping = aes(x = YEAR, y = strat.biomass, color = RPATH)) 

ggplot(subset(GOM.strat.mean, RPATH == 'AtlCroaker')) +
  geom_point( mapping = aes(x = YEAR, y = strat.biomass)) 

ggplot(subset(GOM.strat.mean, RPATH == 'Bluefish')) +
  geom_point( mapping = aes(x = YEAR, y = strat.biomass))

ggplot(subset(GOM.strat.mean, RPATH == 'Bluefish')) +
  geom_point( mapping = aes(x = YEAR, y = strat.abund))

ggplot(subset(GOM.strat.mean, RPATH == 'StripedBass')) +
  geom_point( mapping = aes(x = YEAR, y = strat.biomass))

ggplot(subset(GOM.strat.mean, RPATH == 'StripedBass')) +
  geom_point( mapping = aes(x = YEAR, y = strat.abund))

ggplot(data = GOM.strat.mean) +
  geom_point(mapping = aes(x = YEAR, y = strat.biomass)) +
  facet_wrap(~ RPATH, nrow = 4)

ggplot(data = GOM.strat.mean) +
  geom_point(mapping = aes(x = YEAR, y = strat.abund)) +
  facet_wrap(~ RPATH, nrow = 4)

ggplot(subset(GOM.strat.mean, RPATH == 'RedCrab')) +
  geom_point(mapping = aes(x = YEAR, y = strat.abund)) 

ggplot(subset(GOM.strat.mean, RPATH == 'Fourspot')) +
  geom_point(mapping = aes(x = YEAR, y = strat.abund)) 

ggplot(subset(GOM.strat.mean, RPATH == 'Windowpane')) +
  geom_point(mapping = aes(x = YEAR, y = strat.abund))

ggplot(subset(GOM.strat.mean, RPATH == 'Windowpane')) +
  geom_point(mapping = aes(x = YEAR, y = strat.biomass))

#Try removing large biomass groups
ggplot(subset(GOM.strat.mean, RPATH != 'Haddock')) +
  geom_point(mapping = aes(x = YEAR, y = strat.abund)) +
  facet_wrap(~ RPATH, nrow = 4)

ggplot(subset(GOM.strat.mean, RPATH != 'SpinyDogfish')) +
  geom_point(mapping = aes(x = YEAR, y = strat.abund)) +
  facet_wrap(~ RPATH, nrow = 4)

#Plot high biomass species separately
GOM.fall.strat.mean.high<-GOM.strat.mean[GOM.strat.mean$RPATH %in% c('SpinyDogfish','Haddock','Redfish','Pollock'),]

GOM.fall.strat.mean.low<-GOM.strat.mean[GOM.strat.mean$RPATH %notin% c('SpinyDogfish','Haddock','Redfish','Pollock'),]

ggplot(data = GOM.fall.strat.mean.low) +
  geom_point(mapping = aes(x = YEAR, y = strat.biomass)) +
  facet_wrap(~ RPATH, nrow = 4)

ggplot(data = GOM.fall.strat.mean.high) +
  geom_point(mapping = aes(x = YEAR, y = strat.biomass)) +
  facet_wrap(~ RPATH)

#Calculate total biomass/abundance estimates
#total.biomass <- sweptarea(GOM.prep, lob.mean, strat.col = 'STRATUM', area.col = 'Area')

#Output results either to a flat .csv file
write.csv(GOM.strat.mean, file = 'StratMean_FallGoM.csv')

##Do same for spring
spring.GOM <- survdat[SEASON == 'SPRING' & STRATUM %in%  c(1220, 1240, 1260:1290, 1360:1400, 3560:3830)]

#Run stratification prep
setnames(spring.GOM, 'EST_YEAR', 'YEAR')
GOM.spring.prep <- stratprep(spring.GOM, strat.area, strat.col = 'STRATUM', 
                      area.col = 'Area')
as.data.table(GOM.spring.prep)

#Merge survdat with Rpath species & group names
GOM.spring.prep2<-merge(GOM.spring.prep, spp2[,list(SVSPP,COMNAME,RPATH,SCINAME)], by = 'SVSPP')

#Calculate stratified means - can do multiple species at once (i.e. groups = 
#c(72, 73, 75))
#You can also aggregate species here
#GOM.prep[SVSPP %in% 72:76, Group := 'Gadid']
#then use groups = 'Gadid' and group.col = 'Group' in stratmean
#Note: The function will merge aggregated groups for you
GOM.spring.strat.mean <- stratmean(GOM.spring.prep2, group.col = 'RPATH', strat.col = 'STRATUM')

#Plot biomass and abundance data
ggplot(data = GOM.spring.strat.mean) +
  geom_point(mapping = aes(x = YEAR, y = strat.biomass, color = RPATH))

ggplot(data = GOM.spring.strat.mean) +
  geom_point(mapping = aes(x = YEAR, y = strat.biomass)) +
  facet_wrap(~ RPATH, nrow = 4)

'%notin%' <-Negate('%in%')

GOM.spring.strat.mean.high<-GOM.spring.strat.mean[GOM.spring.strat.mean$RPATH %in% c('SpinyDogfish','Haddock','Redfish','Pollock'),]

GOM.spring.strat.mean.low<-GOM.spring.strat.mean[GOM.spring.strat.mean$RPATH %notin% c('SpinyDogfish','Haddock','Redfish','Pollock'),]

ggplot(data = GOM.spring.strat.mean.low) +
  geom_point(mapping = aes(x = YEAR, y = strat.biomass)) +
  facet_wrap(~ RPATH, nrow = 4)

ggplot(data = GOM.spring.strat.mean.high) +
  geom_point(mapping = aes(x = YEAR, y = strat.biomass)) +
  facet_wrap(~ RPATH)

#Output results either to a flat .csv file
write.csv(GOM.spring.strat.mean, file = 'StratMean_SpringGoM.csv')

#Average by species over 1980-1985 time period
GOM.fall.80s <- GOM.strat.mean[GOM.strat.mean$YEAR %in% 1980:1985,]
GOM.fall.80s[is.na(strat.biomass), strat.biomass := 0]
GOM.fall.80s <- GOM.fall.80s[,mean(strat.biomass), by = RPATH]
setnames(GOM.fall.80s, c('Average Biomass','V1'), c('RPATH','AVG BIOMASS'))
setkey(GOM.fall.80s, 'AVG BIOMASS')

write.csv(GOM.fall.80s, file = 'Fall Biomass Avg 1980-1985.csv')

#Average by species over 2013-2018 time period
GOM.fall.2010s <- GOM.strat.mean[GOM.strat.mean$YEAR %in% 2013:2018,]
GOM.fall.2010s <- GOM.fall.2010s[,sum(strat.biomass), by = RPATH]
setnames(GOM.fall.2010s, c('RPATH','V1'), c('RPATH','AVG BIOMASS'))
setkey(GOM.fall.2010s, 'AVG BIOMASS')

write.csv(GOM.fall.2010s, file = 'Fall Biomass Avg 2013-2018.csv')

which(GOM.fall.2010s$RPATH %notin% GOM.fall.80s$RPATH)

which(GOM.fall.80s$RPATH %notin% GOM.fall.2010s$RPATH)

length(unique(GOM.strat.mean$RPATH))

#segment 'OtherDemersals' by species
OtherDemersals <-spp2[spp2$RPATH %in% 'OtherDemersals']
GOM.OtherDemersals.Fall <- GOM.prep2[GOM.prep2$RPATH %in% 'OtherDemersals']
GOM.OtherDemersals.splist<-as.vector(unique(GOM.OtherDemersals.Fall$SCINAME))
GOM.OtherDemersals.splist

write.csv(GOM.OtherDemersals.splist,'OtherDem - Species List.csv')

GOM.OtherDemersals.Fall.stratmean <-stratmean(GOM.OtherDemersals.Fall, group.col = 'SCINAME', strat.col = 'STRATUM')
#GOM.OtherDemersals.Fall.stratmean <-stratmean(GOM.OtherDemersals.Fall, group.col = 'COMNAME.x', strat.col = 'STRATUM')
ggplot(data = GOM.OtherDemersals.Fall.stratmean) +
  geom_point(mapping = aes(x = YEAR, y = strat.biomass)) +
  facet_wrap(~ COMNAME.x, nrow = 4) +
  theme(strip.text = element_text(face="bold", size=5))

GOM.OtherDemersals.Fall.80s <- GOM.OtherDemersals.Fall.stratmean[GOM.OtherDemersals.Fall.stratmean$YEAR %in% 1980:1985,]
GOM.OtherDemersals.Fall.80s<-GOM.OtherDemersals.Fall.80s[,sum(strat.biomass), by = COMNAME.x]
setnames(GOM.OtherDemersals.Fall.80s, c('COMNAME.x','V1'), c('RPATH','AVG BIOMASS'))
setkey(GOM.OtherDemersals.Fall.80s, 'AVG BIOMASS')

write.csv(GOM.OtherDemersals.Fall.80s, file = 'Other Demersals Fall 1980-1985.csv')

#Calculate total biomass/abundance estimates
total.biomass <- sweptarea(GOM.prep2, GOM.strat.mean, strat.col = 'STRATUM', 
                           area.col = 'Area')



