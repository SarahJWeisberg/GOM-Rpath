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

#Decouple small and large otter trawl fisheries
GOM.landings[SIZE == 'small' & GEAR == 'otter', GEAR := 'otter.sm']
GOM.landings[SIZE == 'large' & GEAR == 'otter', GEAR := 'otter.lg']


#Sum by Rpath group
GOM.sums <- GOM.landings[, sum(SPPLIVMT), by = c('YEAR', 'RPATH', 'GEAR')]
setnames(GOM.sums, 'V1', 'SPPLIVMT')

#Subset & sum for 1980-1985
GOM.landings <- GOM.landings[YEAR %in% 1980:1985]
GOM.sums <- GOM.landings[, sum(SPPLIVMT), by = c('YEAR', 'RPATH', 'GEAR')]
setnames(GOM.sums, 'V1', 'SPPLIVMT')

#Get five year means
GOM.landings <- GOM.sums[YEAR %in% 1980:1985, mean(SPPLIVMT), by = c('RPATH', 'GEAR')]
setnames(GOM.landings, 'V1', 'SPPLIVMT')


#Calculate landings in mt / km^2
GOM.landings <- GOM.landings[, SPPLIVMT := SPPLIVMT / GOM.com.area]

#play with plotting results
dredge <- GOM.landings[GEAR %in% c('dredge.o','dredge.sc'),]

ggplot(data = dredge, aes (x= GEAR, y = SPPLIVMT, fill = RPATH)) +
  geom_bar(stat = "identity") 

ggplot(data = GOM.landings, aes (x= GEAR, y = SPPLIVMT, fill = RPATH)) +
  geom_bar(stat = "identity") 

ggplot(subset(GOM.landings, GEAR %in% c('dredge.o','dredge.sc','longline','midwater')), aes (x= GEAR, y = SPPLIVMT)) +
  geom_col(aes(fill = RPATH))



