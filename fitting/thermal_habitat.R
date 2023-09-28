#looking at thermal occupancy / niches based on trawl survey data


#load packages
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(here)
library(data.table)
library(ggplot2)
library(Rpath)
library(remotes)
library(sf) #r spatial package
library(rnaturalearth) #simple map 
library(lubridate) #dates
library(spatialEco) #more advanced GIS
library(broom)
library(RColorBrewer)
library(units)

world <- ne_countries(scale = "medium", returnclass = "sf")

#remotes::install_github('NOAA-EDAB/survdat')
#remotes::install_github('NOAA-EDAB/Rpath')
#Load survey data
load(here('data/NEFSC_BTS_2021_all_seasons.RData'))
survey<-survey$survdat
#subset GOM data
GOM_survey<-subset(survey, survey$STRATUM %in% c(1220, 1240, 1260:1290, 1360:1400, 3560:3830))
#load species codes
load(here('data/speciescodesandstrata/Species_codes.RData'))
#load model groups
source(here('R/Groups.R'))

#load strata shapefile
strata <- read_sf(here("data/speciescodesandstrata/strata.shp"))

#plot different ways to get a sense of what it looks like
# plot(strata)
# plot(st_geometry(strata), col = sf.colors(12, categorical = TRUE), border = 'grey', 
#      axes = TRUE)

st_crs(strata) = 4326 #this is WGS 1984

#subset GOM strata
GOM_strata<-subset(strata, strata$STRATA %in% c(1220, 1240, 1260:1290, 1360:1400, 3560:3830))
# plot(st_geometry(GOM_strata), col = sf.colors(12, categorical = TRUE), border = 'grey', 
#      axes = TRUE)

#calculate mean fall bottom temp per stratum, 1980-85
GOM_survey_80s<-GOM_survey %>% filter(YEAR < 1986 & YEAR > 1979 & SEASON == "FALL") %>% 
  group_by(STRATUM) %>% mutate(mean_bt = mean(BOTTEMP, na.rm = T)) %>% rename(STRATA = STRATUM)

#append temp to strata
GOM_strata <- GOM_strata %>% left_join(select(GOM_survey_80s, STRATA,mean_bt),by="STRATA")

#plot bottom temperature
# ggplot() + geom_sf(data = world) + 
#   geom_sf(data = GOM_strata, aes(fill=mean_bt), colour = "black") +
#   scale_fill_gradient(low = "blue",high = "red") +
#   coord_sf(xlim=c(-71.5, -65), ylim=c(41,45), expand = TRUE) 
#   # theme(panel.background = element_rect(fill = "white", colour = "black"),
#   #       legend.title=element_blank())+ labs(y = element_blank(), x = element_blank(),
#   #                                           plot.title = element_text(hjust = 0.5))+ theme(legend.position="bottom", 
  #                                                                              legend.title = element_blank())+ guides(colour = guide_legend(override.aes = list(size=

#Get strata area
area<-sf::st_read(dsn=system.file("extdata","strata.shp",package="survdat"))
area<-get_area(areaPolygon = area, areaDescription="STRATA")
GOM_area<-subset(area, area$STRATUM %in% c(1220, 1240, 1260:1290, 1360:1400, 3560:3830))
GOM_area$Area<-drop_units(GOM_area$Area)

#calculate mean bottom temp per stratum, 1980-85
GOM_survey_80s<-GOM_survey %>% filter(YEAR < 1986 & YEAR > 1979) %>% 
  group_by(STRATUM,YEAR) %>% mutate(mean_bt = mean(BOTTEMP, na.rm = T)) %>% select(YEAR,STRATUM,mean_bt) %>%
  distinct()

#append temp to area, calculate area-weighted temperature
GOM_area<-GOM_area %>% left_join(GOM_survey_80s,by="STRATUM") %>% mutate(area_temp = Area*mean_bt)
GOM_temp<-GOM_area %>% ungroup %>% group_by(YEAR) %>% select(YEAR,Area,area_temp) %>%
  mutate (area_sum = sum(Area, na.rm = T)) %>% mutate(weighted_bt = sum(area_temp, na.rm = T)/area_sum) %>%
  select(YEAR,weighted_bt) %>% distinct() %>% na.omit()

ggplot(data = GOM_temp, aes(x=YEAR,y=weighted_bt))+
  geom_line()

#plot trawl data (my calcs) next to ecodata results
#act_resp_force.R
GOM_survey<-GOM_survey %>% filter(YEAR > 1976) %>% #ecodata starts in 1977
  group_by(STRATUM,YEAR) %>% mutate(mean_bt = mean(BOTTEMP, na.rm = T)) %>% select(YEAR,STRATUM,mean_bt) %>%
  distinct()

#append temp to area, calculate area-weighted temperature
GOM_area<-GOM_area %>% left_join(GOM_survey,by="STRATUM") %>% mutate(area_temp = Area*mean_bt)
GOM_temp<-GOM_area %>% ungroup %>% group_by(YEAR) %>% select(YEAR,Area,area_temp) %>%
  mutate (area_sum = sum(Area, na.rm = T)) %>% mutate(weighted_bt = sum(area_temp, na.rm = T)/area_sum) %>%
  select(YEAR,weighted_bt) %>% distinct() %>% na.omit()

ggplot(data = GOM_temp, aes(x=YEAR,y=weighted_bt))+
  geom_line()

#merge into one data frame
GOM_temp$source<-"survey"
bottom_temp$source<-"ecodata"
colnames(GOM_temp)<-c("Time","Value","source")
temp_comp <- rbind(GOM_temp,bottom_temp)
#plot
ggplot(data = temp_comp, aes(x=Time,y=Value,color=source))+
  geom_line()
