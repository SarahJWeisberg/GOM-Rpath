## Processing landings data & creating fleet structture
library(Survdat); library(ggplot2); library(data.table); library(rgdal)

#load commercial landings data (comland)
load("/Users/sjw/Desktop/RPath-GOM-workshop/comland_meatwt_deflated_stat_areas.RData")

#load species codes (spp)
load("speciescodesandstrata/Species_codes.RData")

#Load shape files and calculate total area
stat.areas <- readOGR('gis','Statistical_Areas_2010')
stat.areas.area <- getarea(stat.areas, 'Id')
GOM.com.area <- stat.areas.area[Id %in% c(500, 510, 512:515, 521, 522), sum(Area)]

#delete NAs
spp <- unique(spp[!is.na(NESPP3), list(NESPP3, RPATH)])
comland <- comland[!is.na(NESPP3)]

#check for duplicate NESPP3 entries, reassign, remove duplicates
spp.dupes<-spp[duplicated(NESPP3)]
spp<-spp[NESPP3 %in% spp.dupes$NESPP3, RPATH := 'OtherDemersals']
spp <- unique(spp)

#merge landings with species codes
landings <- as.data.table(merge(comland, spp, by = 'NESPP3'))

#subset for GOM 
GOM.stat.areas <- c(500, 510, 512:515, 521, 522)
GOM.landings <- landings[AREA %in% GOM.stat.areas, ]


#Sum by Rpath group, all gear types
GOM.sums <- GOM.landings[, sum(SPPLIVMT), by = c('YEAR','RPATH')]
setnames(GOM.sums, 'V1', 'SPPLIVMT')

#Exploratory plots to look at landings trends
ggplot(subset(GOM.sums, RPATH != 'AtlHerring')) +
  geom_point(mapping = aes(x = YEAR, y = SPPLIVMT)) +
  theme(text = element_text(size=10)) +
  scale_x_continuous(breaks = seq(1960,2020,20)) +
  facet_wrap(~RPATH, nrow = 4)

ggplot(subset(GOM.sums, RPATH == 'AtlHerring')) +
  geom_point(mapping = aes(x = YEAR, y = SPPLIVMT)) 


