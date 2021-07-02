#Title: GOM Rpath Discards - exploratory analysis

# Purpose: To explore observer data from the GOM and determine how to best incorporate 
#       discards into GOM 
#       https://github.com/NOAA-EDAB/GBRpath

# DataFile:'observer_data.RData'

# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu

# Last modified: # Tue Jun 29 15:21:05 2021 ------------------------------

# NOTE: Clam dredge is in the landings data but not in observer data

library(tidyr); library(dplyr); library(ggplot2)

#Load observer data
load("~/Desktop/GOM-Rpath/data/observer_data.RData")

#Load species codes for matching & filter for just NESPP3 and RPath codes
load("~/Desktop/GOM-Rpath/data/speciescodesandstrata/Species_codes.RData")
spp<-select(spp,one_of(c("NESPP3","RPATH")))
spp<-unique(na.exclude(spp)) #remove NAs and duplicates

##CAUTION: 5 NESPP3 codes are matched with 2 RPATH codes
#remove duplicate NESPP3 codes
spp<-distinct(spp,NESPP3,.keep_all=TRUE)

#Filter observer data for GOM, keep NAs
#ob.gom.na<-filter(ob.all, is.na(EPU) | EPU == "GOM")

#Filter observer data for just GOM
ob_gom<-filter(ob.all, EPU == "GOM")

#Merge filtered observer data with species codes
ob_gom$NESPP3<-as.numeric(ob_gom$NESPP3)
ob_gom<-left_join(ob_gom,spp,by="NESPP3")

#Remove NAs
ob_gom<-na.exclude(ob_gom)

#Make new colume with fleet+group
ob_gom<-mutate(ob_gom,FLEET_GROUP = paste(FLEET, RPATH))

#Largest discard:kept ratio is for turtles

#Because of the many:1 relationship of NESPP3:RPATH, will have some redundancy
#Try to average over this
#Could come back and try to weight the multispecies groups by biomass but that is 
# probably overkill
ob_gom <- ob_gom %>% group_by(FLEET_GROUP)
DK_means<-ob_gom %>% group_by(FLEET_GROUP,YEAR,FLEET,RPATH) %>% summarise(DK = mean(DK))

#Explore trends over time
#Look at mean & sd (DK), n of observations, range, first year, and coeff. of variation
sums<-DK_means %>% group_by(FLEET_GROUP) %>% summarise(avg = mean (DK),sd=sd(DK),min=min(DK),max=max(DK),first=min(YEAR),
                                                       n=length(YEAR),cv=(mean(DK)/sd(DK)))
               

sums_filter<-filter(sums,n>10)

#Separate each fleet into its own data frame
fixed<-filter(DK_means,FLEET == "Fixed Gear")
lg_mesh<-filter(DK_means,FLEET == "LG Mesh")
other<-filter(DK_means,FLEET == "Other")
sm_mesh<-filter(DK_means,FLEET == "SM Mesh")
scallop<-filter(DK_means,FLEET == "Scallop Dredge")
trap<-filter(DK_means,FLEET == "Trap")
hms<-filter(DK_means,FLEET == "HMS")
pelagic<-filter(DK_means,FLEET == "Pelagic")
other_dredge<-filter(DK_means,FLEET == "Other Dredge")


spiny_fixed<-filter(fixed, RPATH == "SpinyDogfish" & YEAR < 1995,.keep_all=TRUE)
spiny_fixed_sums<-cbind(mean(spiny_fixed$DK),sd(spiny_fixed$DK))

#Try visualizing
#Start with fixed gear which goes back to 1989
#Exclude turtles because they throw the scale off
ggplot(mapping = aes(x=YEAR,y=DK),data = filter(fixed,RPATH != "Turtles")) +
  geom_point() +
  facet_wrap(~RPATH) +
  labs(title = "Fixed Gear")
#Looks like the main one to be worried about is spiny dogfish -- bounces around
#But, it is relatively constant at the beginning of the time series

#Large Mesh, with Turtles removed
#Goes back to 1989 for most species
ggplot(mapping = aes(x=YEAR,y=DK),data = filter(lg_mesh,RPATH != "Turtles")) +
  geom_point() +
  facet_wrap(~RPATH) +
  labs(title="Large Mesh")

#Small Mesh, with Turtles removed
#Goes back to 1989
ggplot(mapping = aes(x=YEAR,y=DK),data = filter(sm_mesh, RPATH != "Turtles")) +
  geom_point() +
  facet_wrap(~RPATH) +
  labs(title="Small Mesh")


#Scallop Dredge, with Turtles removed
#Goes back to 1991
ggplot(mapping = aes(x=YEAR,y=DK),data = filter(scallop,RPATH != "Turtles")) +
  geom_point() +
  facet_wrap(~RPATH) +
  labs(title="Scallop Dredge")
#Skates & Scallops bounce around a bit, especially at the beginning of the time series

#Pelagic
#Data are spotty, start in 1995 but most groups start in 2003
ggplot(mapping = aes(x=YEAR,y=DK),data = filter(pelagic,RPATH != "Turtles")) +
  geom_point() +
  facet_wrap(~RPATH) +
  labs(title="Pelagic")
#Spiny dogfish jumps around a little, as does herring

#Trap
#Data are spotty, start in 1991 but more data starting in 1993
ggplot(mapping = aes(x=YEAR,y=DK),data = filter(trap,RPATH != "Turtles")) +
  geom_point() +
  facet_wrap(~RPATH) +
  labs(title="Trap")
#One year of very high value for lobster, otherwise all seem consistent (and low)


#Other Dredge 
#Only since 2015
ggplot(mapping = aes(x=YEAR,y=DK),data = filter(other_dredge,RPATH != "Turtles")) +
  geom_point() +
  facet_wrap(~RPATH) +
  labs(title="Other Dredge")
#All look consistent except skats (winter, little, other)

#HMS
#Since 1992, but most data since 2005
ggplot(mapping = aes(x=YEAR,y=DK),data = filter(hms,RPATH != "Turtles")) +
  geom_point() +
  facet_wrap(~RPATH) +
  labs(title="HMS")
#Spiny jumps around



