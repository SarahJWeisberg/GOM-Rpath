#Title: GOM Rpath Biomass Estimates (Survey)

# Purpose: This script generates biomass estimates for 
#       the single-species functional groups used in the GOM-Rpath model.
#       Data are sourced from the NEFSC bottom trawl. Model is parameterized
#       for 1980-85 using fall survey data only. Model is built
#       to be analogous to Georges Bank Rpath model
#       https://github.com/NOAA-EDAB/GBRpath

# DataFile:'NEFSC_BTS_2021_all_seasons.RData'

# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu

xfun::session_info()
# Last modified: Mon Jun 14 14:55:48 2021 ------------------------------

#Load needed packages
#Load required packages
library(here);library(data.table);library(survdat)

#Load survey data
load('data/NEFSC_BTS_2021_all_seasons.RData')

#Load species codes
#Use these codes to translate survey species codes ('SVSPP') to RPATH species codes ('spp')
load('data/speciescodesandstrata/Species_codes.Rdata')

#Load strata
#strata<- readOGR('speciescodesandstrata','strata')

#Calculate total GOM area
area<-sf::st_read(dsn=system.file("extdata","strata.shp",package="survdat"))
area<-get_area(areaPolygon = area, areaDescription="STRATA")
GOM.area<-subset(area, area$STRATUM %in% c(1220, 1240, 1260:1290, 1360:1400, 3560:3830))
GOM.area<-sum(GOM.area$Area)

#Calculate mean stratified biomass
#GOM strata
#Fall season only
stratmean<-calc_stratified_mean(surveyData=survey$survdat, areaPolygon = 'NEFSC strata', areaDescription = 'STRATA', filterByArea = c(1220, 1240, 1260:1290, 1360:1400, 3560:3830), filterBySeason= "FALL", groupDescription = "SVSPP", filterByGroup = "all", mergesexFlag = T, tidy = F, returnPrepData = F)

#Merge stratified means with RPATH names
stratmean<-merge(stratmean,spp[,list(SVSPP,RPATH,SCINAME,Fall.q)], by = 'SVSPP')

#Calculate swept area biomass
swept<-calc_swept_area(surveyData=survey$survdat, areaPolygon = 'NEFSC strata', areaDescription = 'STRATA', filterByArea = c(1220, 1240, 1260:1290, 1360:1400, 3560:3830), filterBySeason= "FALL", groupDescription = "SVSPP", filterByGroup = "all", mergesexFlag = T,tidy = F, q = NULL, a = 0.0384)

#Merge with RPATH names
#RPATH names are the same as in Georges Bank model
#Aggregate groups below 0.05 t/km^2 threshold
spp <- spp[!duplicated(spp$SVSPP),] #SVSPPs are NEFSC species-specific codes
spp <- spp[RPATH == 'RedCrab', RPATH := 'Macrobenthos']
#spp <- spp[RPATH == 'AtlScallop', RPATH := 'Megabenthos'] #Need to come back to this!
spp <- spp[RPATH == 'Clams', RPATH := 'Megabenthos']
spp <- spp[RPATH == 'Scup', RPATH := 'OtherDemersals']
spp <- spp[RPATH == 'OffHake', RPATH := 'OtherDemersals']
spp <- spp[RPATH == 'Bluefish', RPATH := 'OtherPelagics']
spp <- spp[RPATH == 'AmShad', RPATH := 'SmPelagics']
spp <- spp[RPATH == 'SmallPelagics', RPATH := 'SmPelagics']
spp <- spp[RPATH == 'AtlCroaker', RPATH := 'SouthernDemersals']
spp <- spp[RPATH == 'LargePelagics', RPATH := 'OtherPelagics']
spp <- spp[RPATH == 'OtherFlatfish', RPATH := 'OtherDemersals']
spp <- spp[RPATH == 'StripedBass', RPATH := 'OtherPelagics']
spp <- spp[RPATH == 'Sturgeon', RPATH := 'OtherDemersals']
spp <- spp[RPATH == 'Weakfish', RPATH := 'OtherDemersals']

#Cusk not included in Georges Bank model -- need to create RPATH name
spp <- spp[SCINAME == 'BROSME BROSME', RPATH := 'Cusk']

#Merge swept area biomass with RPATH names
swept<-merge(swept,spp[,list(SVSPP,RPATH,SCINAME,Fall.q)], by = 'SVSPP')

#Calcuate biomass / area in mt
swept <- swept[, biomass.area   := (tot.biomass*.001)/(Fall.q*GOM.area)]

#average over 1980-85
setkey(swept,RPATH,YEAR)
swept <- swept[, sum(biomass.area), by = key(swept)]
setnames(swept, 'V1','Biomass')
biomass_80s<-swept[YEAR %in% 1980:1985, mean(Biomass), by=RPATH]
setnames(biomass_80s, 'V1','Biomass')
