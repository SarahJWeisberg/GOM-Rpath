# Title: Copepod Biomass Time Series
# Purpose: This script generates a time series of large and small copepod biomass to be
#           used for fitting the 1980-85 Gulf of Maine Rpath model to data.
#         
# Data files: (1) data/ZooData_GOM.csv, (2) data/EcoMon_Copepod size.csv from Harvey Walsh
#            (1) Ecomon data filtered for region, taxa of interest
#            (2) Parameters for converting abundance to biomass for taxa of interest
#
# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu

# Tue Sep 26 12:46:19 2023 ------------------------------

#load packages
library(sf) #r spatial package
library(ggplot2) #plotting
library(rnaturalearth) #simple map 
library(lubridate) #dates
library(spatialEco) #more advanced GIS
library(dplyr) 
library(tidyverse)
library(zoo)
library(broom)
library(here)

world <- ne_countries(scale = "medium", returnclass = "sf")

#load data
ecomon <- read.csv(here("data/ZooData_GOM.csv"))

#combine with the strata dataset 
#load strata
strata <- read_sf(here("speciescodesandstrata/EcomonStrata_v4b.shp"))
#set coordinate system
st_crs(strata) = 4326 #this is WGS 1984

#make ecomon spatial with the same defined coordinate system
ecomon_sp <- st_as_sf(x = ecomon, 
                      coords = c("LON", "LAT"),
                      crs = 4326)

#checkit with ggplot
ggplot() + geom_sf(data = world) + 
  geom_sf(data = strata, fill = NA, colour = "red") + 
  geom_sf(data = subset(ecomon_sp, ecomon_sp$year ==1997), size = .2) + 
  coord_sf(xlim=c(-80, -65), ylim=c(34,46), expand = TRUE)+ 
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        legend.title=element_blank())+ labs(y = element_blank(), x = element_blank(),
                                            plot.title = element_text(hjust = 0.5))+ theme(legend.position="bottom", 
                                                                                           legend.title = element_blank())+ guides(colour = guide_legend(override.aes = list(size=6))) + 
  ggtitle("Ecomon Data", subtitle ="1997")

#extract stratum values to points
#get a weird error - need to follow these steps: https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data
sf::sf_use_s2(FALSE)
ecomon_extract <- st_intersection(ecomon_sp, strata)
#checkit with a plot 

ecomon_extract <- as.data.frame(ecomon_extract)#note you need to convert it to a data frame to do any sort of aesthetics in ggplot because ggplot doesn't like spatial objects. Could use base R if you want
ecomon <- ecomon_extract

#create season column based on 2 month period
ecomon$season <- ifelse(ecomon$MONTH == 1 | ecomon$MONTH == 2, "JanFeb", ifelse(ecomon$MONTH == 3 | ecomon$MONTH == 4, "MarApril",ifelse(ecomon$MONTH == 5 | ecomon$MONTH == 6, "MayJun",ifelse(ecomon$MONTH == 7 | ecomon$MONTH == 8, "JulAug",ifelse(ecomon$MONTH == 9 | ecomon$MONTH == 10, "SepOct",ifelse(ecomon$MONTH == 11 | ecomon$MONTH == 12, "NovDec", NA))))))

#tidy
#first split out x (timing, stratum, area) and y (species)
ecomon_x <- as.data.frame(cbind(ecomon$YEAR, ecomon$season, ecomon$Name, ecomon$Area))
colnames(ecomon_x)<-c("Year","Season","Stratum","Area")
ecomon_y <- ecomon_extract %>% select("CALANUS_FINMARCHICUS":"CLAUSOCALANUS_ARCUICORNIS")
ecomon_y[is.na(ecomon_y)] <- 0 #NAs should be true 0s
ecomon <- cbind(ecomon_x, ecomon_y)

#pull out species names
ecomon_ynames <- colnames(ecomon_y)

#make long dataframe
ecomon_long <- ecomon %>% pivot_longer(all_of(ecomon_ynames), names_to = "spp")

#make Area numeric
ecomon_long$Area<-as.numeric(ecomon_long$Area)

#do this on a grouped dataset
ecomon_grouped_long <- ecomon_long %>% group_by(Year,Season, Stratum, spp, Area) %>% 
  dplyr::summarise(value = mean(value)) %>% 
  mutate(value = value*Area) #get abundance per stratum

#classify season as spring / fall
ecomon_grouped_long$SF<-ifelse(ecomon_grouped_long$Season %in% c("JanFeb","MarApr","MayJun"), "Spring","Fall")

#sum by spring/fall, find annual density metric (ind/km^2)
#do this to account for holes in sampling
ecomon_SF_sums<-ecomon_grouped_long %>% group_by(Year,spp,SF) %>% dplyr::summarise(abd = sum(value),area = sum(Area)) %>% mutate(ratio = abd/area)
ecomon_year_sums<-ecomon_SF_sums %>% group_by(Year,spp) %>% dplyr::summarise(abd = mean(ratio))

#convert to biomass
sizes<-read.csv(here("data/EcoMon_Copepod size.csv"))
sizes<-sizes[-1,]
sizes<-sizes %>% select(c("X","Literature"))
colnames(sizes)<-c("spp","length")
sizes$length<-as.numeric(sizes$length)
#use l-w conversion from EMAX (wet weight)
#unit is mg
sizes$weight<-0.0881*sizes$length^2.8514

#join back with abd density estimates
annual<-left_join(ecomon_year_sums,sizes,by="spp")
#multiply ind * mg/ind
annual$biomass<-annual$abd*annual$weight
#this is not quite right but that's ok for now - what I want is the temporal trend

#classify as sm/lg
annual$class<-ifelse(annual$spp == "CALANUS_FINMARCHICUS", "lg","sm")
ts<-annual %>% group_by(class,Year) %>% dplyr::summarise(biomass = sum(biomass))

# #scale to starting values (from initial balanced model)
sm_start<-13.162
lg_start<-9.196
cope_start<-as.data.frame(cbind(c("lg","sm"),c(lg_start,sm_start)))
colnames(cope_start)<-c("class","biomass_start")
cope_start$biomass_start<-as.numeric(cope_start$biomass_start)

initial_biomass<-ts %>% filter(Year<=1986) %>% group_by(class) %>% dplyr::summarise(avg_80_85 = mean(biomass))

ts<-left_join(ts,initial_biomass,by="class")
ts<-left_join(ts,cope_start,by="class")
ts<- ts %>% filter(Year>1986 & Year<2020) %>% mutate(force_b = biomass/avg_80_85*biomass_start)

#calculate anomalies
ts<-ts %>% group_by(class) %>% mutate(mean = mean(force_b), anomaly = (force_b - mean)/mean)

#plot
ts$Year<-as.numeric(ts$Year)
P<-ts %>%
  ggplot(aes(x = Year, y = anomaly)) +
  geom_line(aes(color = class)) 
P

lg<-ts %>% filter(class == "lg") %>% ungroup()
sm<-ts %>% filter(class == "sm") %>% ungroup()

