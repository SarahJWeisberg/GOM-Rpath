#Handling aggregate groups for biomass estimates & diet data

GOM.groups <-as.data.table(c('Phytoplankton', 'Bacteria', 'Microzooplankton', 'GelZooplankton','LgCopepods', 'SmCopepods', 'Micronekton', 'Illex','Loligo', 'OtherCephalopods','Macrobenthos','AmLobster','OceanPout','SmPelagics','SmFlatfishes','NShrimp','OtherShrimps','RiverHerring','AtlScallop','OtherPelagics','AtlHerring','Mesopelagics','YTFlounder','Cod','Haddock','AmPlaice','AtlMackerel','AtlHalibut','SummerFlounder', 'Cusk','OtherDemersals','HMS','OtherSkates','RedHake','Barndoor Skate','Fourspot','SmoothDogfish','Pollock','Goosefish','SilverHake','WhiteHake','SpinyDogfish','Redfish','LittleSkate','SeaBirds','Windowpane','WinterFlounder','WitchFlounder','BlackSeaBass','Butterfish','Sharks','WinterSkate','Pinnipeds','BaleenWhales','Odontocetes', 'Megabenthos'))
setnames(GOM.groups,'V1','RPATH')

```


## Compiling Biomass Data

### Generating Biomass Estimates of Surveyed Species
Load in NEFSC bottom trawl data ('survdat') from all time, shape file of survey strata ('strata') and species list for matching survey species codes with RPATH codes ('spp'). Note species of lower abundance / biomass will be aggregated into larger functional groups. Calculate mean stratified biomass for each RPATH group per year, using fall data only. Multiply by total area surveyed per year and divide by area swept per tow to generate swept area biomass. Divide by total survey area to arrive at biomass density estimate (t/km^2). Average over 1980-1985.

```{r survey biomass}
#Load data------------------------------------------------------------------

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
fall <- survdat[SEASON == 'FALL' & STRATUM %in% c(1220, 1240, 1260:1290, 1360:1400, 3560:3830), ]

#Calculate total survey area
area <- strat.area[STRATUM %in% c(1220, 1240, 1260:1290, 1360:1400, 3560:3830), sum(Area)]

#Run stratification prep
setnames(fall, 'EST_YEAR', 'YEAR')
prep <- stratprep(fall, strat.area, strat.col = 'STRATUM', area.col ='Area')

#Merge with RPATH names
#Aggregate groups below 0.05 t/km^2 threshold
spp <- spp[!duplicated(spp$SVSPP),]
spp <- spp[RPATH == 'RedCrab', RPATH := 'Macrobenthos',]
spp <- spp[RPATH == 'Scup', RPATH := 'OtherDemersals',]
spp <- spp[RPATH == 'OffHake', RPATH := 'OtherDemersals',]
spp <- spp[RPATH == 'Bluefish', RPATH := 'OtherPelagics',]
spp <- spp[RPATH == 'AmShad', RPATH := 'SmPelagics',]
spp <- spp[SCINAME == 'BROSME BROSME', RPATH := 'Cusk']
spp <- spp[RPATH == 'SmallPelagics', RPATH := 'SmPelagics']
prep<-merge(prep, spp[,list(SVSPP,COMNAME,RPATH,SCINAME)], by = 'SVSPP')

#Calculate stratified means 
stratmean <- stratmean(prep, group.col = 'SVSPP', strat.col = 'STRATUM')

#Merge stratified means with RPATH names
stratmean<-merge(stratmean,spp[,list(SVSPP,RPATH,SCINAME,Fall.q)], by = 'SVSPP')

#Calculate swept area biomass
swept <- sweptarea(prep, stratmean, strat.col = 'STRATUM', area.col = 'Area')

#Merge swept area biomass with RPATH names
swept<-merge(swept,spp[,list(SVSPP,RPATH,SCINAME,Fall.q)], by = 'SVSPP')

#Calcuate biomass / area in mt
swept <- swept[, biomass.area   := (tot.biomass*.001)/(Fall.q*area)]

#Sum multispecies groups
#OtherDemersals
OtherDemersals.Biomass<-swept[RPATH == 'OtherDemersals', sum(biomass.area), by = YEAR]
setnames(OtherDemersals.Biomass,'V1','Biomass')
OtherDemersals.80s <- OtherDemersals.Biomass[YEAR %in% 1980:1985, mean(Biomass)]

#SmFlatfishes
SmFlatfishes.Biomass<-swept[RPATH == 'SmFlatfishes', sum(biomass.area), by = YEAR]
setnames(SmFlatfishes.Biomass,'V1','Biomass')
SmFlatFishes.80s <- SmFlatfishes.Biomass[YEAR %in% 1980:1985, mean(Biomass)]

#OtherPelagics
OtherPelagics.Biomass<-swept[RPATH == 'OtherPelagics', sum(biomass.area), by = YEAR]
setnames(OtherPelagics.Biomass,'V1','Biomass')
OtherPelagics.80s <- OtherPelagics.Biomass[YEAR %in% 1980:1985, mean(Biomass)]

#OtherCephalopods
OtherCeph.Biomass<-swept[RPATH == 'OtherCephalopods', sum(biomass.area), by = YEAR]
setnames(OtherCeph.Biomass,'V1','Biomass')
OtherCeph.80s <- OtherCeph.Biomass[YEAR %in% 1980:1985, mean(Biomass)]

#Mesopelagics
Mesopelagics.Biomass<-swept[RPATH == 'Mesopelagics', sum(biomass.area), by = YEAR]
setnames(Mesopelagics.Biomass,'V1','Biomass')
Mesopelagics.80s <- Mesopelagics.Biomass[YEAR %in% 1980:1985, mean(Biomass)]

#SmPelagics
SmPelagics.Biomass<-swept[RPATH == 'SmPelagics', sum(biomass.area), by = YEAR]
setnames(SmPelagics.Biomass,'V1','Biomass')
SmPelagics.80s <- SmPelagics.Biomass[YEAR %in% 1980:1985, mean(Biomass)]

#OtherShrimps
#OtherShrimps.Biomass<-swept[RPATH == 'OtherShrimps', sum(biomass.area), by = YEAR]
#setnames(OtherShrimps.Biomass,'V1','Biomass')
#OtherShrimps.80s <- OtherShrimps.Biomass[YEAR %in% 1980:1985, mean(Biomass)]
setkey(swept,RPATH,YEAR)
swept2 <- swept[, sum(biomass.area), by = key(swept)]
setnames(swept2, 'V1','Biomass')
avgs.check<-swept2[YEAR %in% 1980:1985, mean(Biomass), by=RPATH]


#OtherSkates
OtherSkates.Biomass<-swept[RPATH == 'OtherSkates', sum(biomass.area), by = YEAR]
setnames(OtherSkates.Biomass,'V1','Biomass')
OtherSkates.80s <- OtherSkates.Biomass[YEAR %in% 1980:1985, mean(Biomass)]

#Average over 1980-1985
biomass.80s <- swept[YEAR %in% 1980:1985, mean(biomass.area), by = RPATH]
setnames(biomass.80s,'V1','Biomass')
biomass.80s<-biomass.80s[RPATH == 'OtherDemersals', Biomass := OtherDemersals.80s,]
biomass.80s<-biomass.80s[RPATH == 'Mesopelagics', Biomass := Mesopelagics.80s,]
biomass.80s<-biomass.80s[RPATH == 'OtherCephalopods', Biomass := OtherCeph.80s,]
biomass.80s<-biomass.80s[RPATH == 'OtherPelagics', Biomass := OtherPelagics.80s,]
biomass.80s<-biomass.80s[RPATH == 'OtherShrimps', Biomass := OtherShrimps.80s,]
biomass.80s<-biomass.80s[RPATH == 'OtherSkates', Biomass := OtherSkates.80s,]
biomass.80s<-biomass.80s[RPATH == 'SmFlatfishes', Biomass := SmFlatFishes.80s,]
biomass.80s<-biomass.80s[RPATH == 'SmPelagics', Biomass := SmPelagics.80s,]


kable(biomass.80s, booktabs = F, align = 'c')

#Output results to csv
write.csv(biomass.80s, 'Survey_Biomass_GoM_1980-85.csv')


##curious which groups are still unaccounted for
leftovers<-survey.groups[!RPATH %in% biomass.80s$RPATH,]
