#Date modified: # Thu Jun 17 09:30:56 2021 ------------------------------

#Load packages
library(here);library(data.table);library(ggplot2)

#Load data
#Average commercial landings by species & gear type, 1980-85
#Data pull by Sean, run through 'commercial_landings.R' script
load("~/Desktop/GOM-Rpath/data/mean_landings_gom_80_85.RData")
load("~/Desktop/GOM-Rpath/data/speciescodesandstrata/Species_codes.RData")

#Merge mean landings with RPATH codes
landings<-unique(merge(mean.land,spp[,list(NESPP3,RPATH)],by='NESPP3'))

#Summarize
gear.total<-landings[,.(sum(SPPLIVMT)),by=.(FLEET)]

#Species by gear
landings<-merge(landings,gear.total,by='FLEET')
setnames(landings, 'V1','Fleet.Sum')
landings<-landings[, Fleet.Prop := SPPLIVMT/Fleet.Sum]

#Look at species caught in small or large meshes only
mesh.only<-landings[FLEET %in% c('LG Mesh','SM Mesh'),]
mesh.only<-mesh.only[Fleet.Prop>0.01,]

#try plotting
ggplot(data=mesh.only,aes(fill=RPATH,y=SPPLIVMT,x=FLEET)) +
  geom_bar(position = "stack",stat = "identity") #too many species in one plot!

ggplot(data=mesh.only, aes(y=SPPLIVMT,x=FLEET)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~RPATH) +
  xlab("")
