# Purpose: This script summarizes catch in t/km^2
#           for all fished RPath groups per year from 1980-2019.
#           Used to generate data for time series fitting routine

# DataFiles: 'commercial_landings_gom_80_19.RData'

# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu

# Thu Apr 14 11:29:00 2022 ------------------------------

#load required packages
library(ggplot2); library(here); library(units); library(data.table)

load(here('data/commercial_landings_gom_80_19.Rdata'))

#load list of groups included in model
source('R/Groups.R')

#code below modified from landings_conversion.R
com.land[FLEET =="HMS",FLEET:="HMS Fleet"]

#Load species codes
#Use these codes to translate landings species codes ('SVSPP') to RPATH species codes ('spp')
load('data/speciescodesandstrata/Species_codes.Rdata')

#Calculate total GOM area
area<-sf::st_read(dsn=system.file("extdata","strata.shp",package="survdat"))
area<-get_area(areaPolygon = area, areaDescription="STRATA")
GOM.area<-subset(area, area$STRATUM %in% c(1220, 1240, 1260:1290, 1360:1400, 3560:3830))
GOM.area<-sum(GOM.area$Area)

#Load species codes for matching & filter for just NESPP3 and RPath codes
load("data/speciescodesandstrata/Species_codes.RData")
spp<-select(spp,one_of(c("NESPP3","RPATH")))
spp<-unique(na.exclude(spp)) 

#5 NESPP3 codes are matched with 2 RPATH codes
#remove duplicate NESPP3 codes
spp<-distinct(spp,NESPP3,.keep_all=TRUE)

#Load GOM groups
source('R/Groups.R')

#Merge landings data with species codes & remove NESPP3 column
com.land<-left_join(com.land,spp,by="NESPP3")
com.land<-na.exclude(com.land[,-2])

#Sum landings for multispecies groups
com.land<-com.land %>% 
  group_by(FLEET,RPATH,YEAR) %>% 
  summarise(SPPLIVMT=sum(SPPLIVMT))

#Convert to t/km^2
#Remove SPPLIVMT for clarity
com.land$landings<-com.land$SPPLIVMT/GOM.area
com.land<-com.land[,-4]

#sum all landings per group
spp.land<-com.land %>% 
  group_by(RPATH,YEAR) %>% 
  summarise(landings=sum(landings))

#remove groups not in final model
spp.land<-as.data.table(spp.land)
spp.land<-spp.land[RPATH %in% GOM.groups$RPATH]

#drop units for plotting purposes
spp.land$landings <- drop_units(spp.land$landings)

#rename columns to comply with fitting code
colnames(spp.land)<-c("Group", "Year", "Value")

#add sd, scale columns
spp.land[,Stdev := Value * 0.1]
spp.land[,Scale := rep(1,length(spp.land$Value))]

#visualize landings trends over time
ggplot(spp.land,aes(x=Year, y = Value)) +
  geom_point() +
  facet_wrap(vars(Group),ncol = 4)

#save file
write.csv(spp.land,"landings_fit.csv")
