# Title: RPath model balancing
# Purpose: Copepod groups are rearranged
#         from EMAX categorization.

# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu

# Thu Dec 14 11:11:01 2023 ------------------------------
library(sf) #r spatial package
library(ggplot2) #plotting
library(rnaturalearth) #simple map 
library(lubridate) #dates
library(spatialEco) #more advanced GIS
library(dplyr) 
library(tidyverse) 
library(here)
world <- ne_countries(scale = "medium", returnclass = "sf")
#EMAX reported values
lg_start<-34.85
sm_start<-8.25

#load in data from Harvey Walsh
#data in individuals per 10m^2
ZooData_GOM <- read_csv("data/ZooData_GOM.csv")
#filter for 1980-1985
copes<-ZooData_GOM %>% filter(YEAR <= 1985 & YEAR >= 1980)

#plot
zoo_sp <- st_as_sf(x = copes, 
                      coords = c("LON", "LAT"),
                      crs = 4326)
ggplot() + geom_sf(data = world) + 
  geom_sf(data = zoo_sp, size = .2) + 
  coord_sf(xlim=c(-72, -65), ylim=c(40,46), expand = TRUE)
#run copes_time
# cal_ratio<-initial_biomass_emax$avg_80_85[1]/sum(initial_biomass_emax$avg_80_85)
# lg_new<-lg_start * cal_ratio
# sm_new<-sm_start+lg_start-lg_new

#create season column based on 2 month period
copes$season <- ifelse(copes$MONTH == 1 | copes$MONTH == 2, "JanFeb", ifelse(copes$MONTH == 3 | copes$MONTH == 4, "MarApril",ifelse(copes$MONTH == 5 | copes$MONTH == 6, "MayJun",ifelse(copes$MONTH == 7 | copes$MONTH == 8, "JulAug",ifelse(copes$MONTH == 9 | copes$MONTH == 10, "SepOct",ifelse(copes$MONTH == 11 | copes$MONTH == 12, "NovDec", NA))))))

#tidy the dataset
#first split out x (timing) and y (species)
copes_x <- as.data.frame(cbind(copes$YEAR, copes$season))
colnames(copes_x)<-c("Year","Season")
copes_y <- copes %>% select("CALANUS_FINMARCHICUS":"CLAUSOCALANUS_ARCUICORNIS")
copes_y[is.na(copes_y)] <- 0 #NAs should be true 0s
copes <- cbind(copes_x, copes_y)
copes_ynames <- colnames(copes_y)
copes_long <- copes %>% pivot_longer(all_of(copes_ynames), names_to = "spp")

#look at temporal coverage
cruises<-copes_long %>% select(Year,Season) %>% distinct()

#average abundance per cruise
copes_cruise<-copes_long %>% group_by(Year,Season,spp) %>% dplyr::summarise(abd = mean(value))
#annual average (across cruises)
copes_annual<-copes_cruise %>% group_by(Year,spp) %>% dplyr::summarise(abd=mean(abd))
#average across 1980-85
copes_avg<-copes_annual %>% group_by(spp) %>% dplyr::summarise(abd=mean(abd))

#convert to weight
#first bring in average lengths (from Harvey Walsh)
sizes <- read_csv("data/EcoMon_Copepod size.csv")
sizes<-sizes[-1,c(1,3)]
colnames(sizes)<-c("spp","length")
sizes$length<-as.numeric(sizes$length)
#use l-w conversion from EMAX (wet weight)
#unit is mg
sizes$weight<-0.0881*sizes$length^2.8514

#join back with abd estimates
copes_avg<-left_join(copes_avg,sizes,by="spp")
#multiply ind * mg/ind
copes_avg$biomass<-copes_avg$abd*copes_avg$weight
#sum biomass by class
copes_avg<-copes_avg %>% filter(spp %in% c("CALANUS_FINMARCHICUS","CENTROPAGES_TYPICUS","METRIDIA_LUCENS"))
copes_avg$class<-ifelse(copes_avg$spp == "CALANUS_FINMARCHICUS", "lg","sm")
copes_lg <- copes_avg %>% filter(class == "lg") %>% select(biomass)
copes_sm <- copes_avg %>% filter(class == "sm") %>% dplyr::summarise(biomass=sum(biomass))

#generate ratio
copes_ratio <- copes_lg$biomass/(copes_lg$biomass+copes_sm$biomass)

#Reallocate Copepod Biomass
#SmCopepods
biomass[6]<-biomass[6]+biomass[5]*(1-copes_ratio)
#Lg Copepods
biomass[5]<- biomass[5]*copes_ratio
#Fill model
GOM.params$model[,Biomass:=biomass]

#Diet adjustments ----------------
#Special groups (based on lit review: AmLobster, AtlHerring, AtlMackerel, RiverHerring, SmCopepods)
#For most groups reallocate based on est. proportion of Calanus in original LgCope group5
#For AtlHerring, use ratio of Calanus to all copepods
cal_all<-biomass[5]/(biomass[5]+biomass[6])
#need to get rid of NAs first
GOM.params$diet[6,2:58]<-replace(GOM.params$diet[6,2:58],is.na(GOM.params$diet[6,2:58]),0)
GOM.params$diet[5,2:58]<-replace(GOM.params$diet[5,2:58],is.na(GOM.params$diet[5,2:58]),0)

#Shift AmLobster[12] predation
#Shift 0.05% from Macrobenthos[11] to LgCopepods[5]
GOM.params$diet[11,13]<-GOM.params$diet[11,13]-0.005
GOM.params$diet[5,13]<-GOM.params$diet[5,13]+0.005
#Shift 0.05% from Macrobenthos[11] to SmCopepods[6]
GOM.params$diet[11,13]<-GOM.params$diet[11,13]-0.005
GOM.params$diet[6,13]<-GOM.params$diet[6,13]+0.005

#Shift AtlHerring[21] predation
GOM.params$diet[6,22]<-GOM.params$diet[6,22]+GOM.params$diet[5,22]*(1-cal_all)
GOM.params$diet[5,22]<-GOM.params$diet[5,22]*cal_all

#Shift AtlMackerel[27] predation
#Remove 6% from Macrobenthos[11]
#Remove 6% from Micronekton[7]
#Remove 18% from LgCopepods[5]
#Move 30% to SmCopepods[6]
GOM.params$diet[11,28]<-GOM.params$diet[11,28]-0.06
GOM.params$diet[7,28]<-GOM.params$diet[7,28]-0.06
GOM.params$diet[5,28]<-GOM.params$diet[5,28]-0.18
GOM.params$diet[6,28]<-GOM.params$diet[6,28]+0.3

#Shift RiverHerring[18] predation
GOM.params$diet[6,19]<-GOM.params$diet[5,19]*0.75
GOM.params$diet[5,19]<-GOM.params$diet[5,19]*0.25

#Shift SmCopepod[6] predation
GOM.params$diet[5,7]<-GOM.params$diet[6,7]*0.05
GOM.params$diet[6,7]<-GOM.params$diet[6,7]*0.95

#Shift 3% predation from Microzooplankton[3] to Bacteria[2]
GOM.params$diet[2,7]<-GOM.params$diet[2,7]+0.03
GOM.params$diet[3,7]<-GOM.params$diet[3,7]-0.03

#Shift LgCopepod[5] predation
#Shift 1% predation from Microzooplankton[3] to Bacteria[2]
GOM.params$diet[2,6]<-GOM.params$diet[2,6]+0.01
GOM.params$diet[3,6]<-GOM.params$diet[3,6]-0.01

#For rest of groups, add portion to SmCope consumption
GOM.params$diet[6,c(2:5,7:11,13:17,19:20,22:26,28:58)]<-
  GOM.params$diet[6,c(2:5,7:11,13:17,19:20,22:26,28:58)]+
  GOM.params$diet[5,c(2:5,7:11,13:17,19:20,22:26,28:58)]*(1-copes_ratio)
GOM.params$diet[5,c(2:5,7:11,13:17,19:20,22:26,28:58)]<-
  GOM.params$diet[5,c(2:5,7:11,13:17,19:20,22:26,28:58)]*copes_ratio


check.rpath.params(GOM.params)
 