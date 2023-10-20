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
GOM_strat_temp<-GOM_survey %>% filter(YEAR > 1976) %>% #ecodata starts in 1977
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

#Pull Tmax_survey from survey data
#Load survey data
load(here('data/NEFSC_BTS_2021_all_seasons.RData'))
#load species codes
load(here('data/speciescodesandstrata/Species_codes.RData'))
#load model groups
source(here('R/Groups.R'))

#pull out fish groups chosen for variable bioenergetics
cons_groups<-GOM.groups %>% dplyr::filter(RPATH %in% c('OceanPout',
                                                       'RiverHerring',
                                                       'AtlHerring','YTFlounder','Cod',
                                                       'Haddock','AmPlaice','AtlMackerel','AtlHalibut',
                                                       'SummerFlounder', 
                                                       #'Cusk',
                                                       'RedHake','Fourspot',
                                                       'SmoothDogfish','Pollock','Goosefish','SilverHake',
                                                       'WhiteHake','SpinyDogfish','Redfish',
                                                       'Windowpane','WinterFlounder',
                                                       'WitchFlounder','BlackSeaBass','Butterfish'))


#Cusk not included in Georges Bank model -- need to create RPATH name
spp <- spp[SCINAME == 'BROSME BROSME', RPATH := 'Cusk']

survdat<-survey$survdat
survdat<-left_join(survdat,spp, by = "SVSPP")
GOM_survey<-left_join(GOM_survey,spp, by = "SVSPP")

#need:
#biomass per stratum per cruise
swept<-calc_swept_area(surveyData=survdat, areaPolygon = 'NEFSC strata', areaDescription = 'STRATA', 
                       filterByArea = c(1220), filterBySeason = "all",
                       groupDescription = "RPATH", filterByGroup = "AtlHalibut", mergesexFlag = T,tidy = F, q = NULL, a = 0.0384)
#avg temp per stratum per cruise

#look at halibut
halibut<-GOM_survey %>% dplyr::filter(RPATH == "AtlHalibut")
#where do we find halibut (which strata)?
halibut_strata<- halibut$STRATUM %>% unique()
#calculate biomass per stratum per year, append stratum
halibut_biomass<-c()
for (i in 1:length(halibut_strata)) {
  stratum<-halibut_strata[i]
  swept<-calc_swept_area(surveyData=survdat, areaPolygon = 'NEFSC strata', areaDescription = 'STRATA', 
                         filterByArea = stratum, filterBySeason = "all",
                         groupDescription = "RPATH", filterByGroup = "AtlHalibut", mergesexFlag = T,tidy = F, q = NULL, a = 0.0384)
  biomass<-swept %>% select(YEAR,RPATH,tot.biomass)
  biomass$stratum<-stratum
  halibut_biomass<-rbind(halibut_biomass,biomass)
}

#merge with temp
halibut_biomass<-halibut_biomass %>% rename(STRATUM=stratum)
halibut_biomass<-halibut_biomass %>% left_join(GOM_strat_temp)
halibut_annual_temp<-halibut_biomass %>% na.omit() %>% group_by(YEAR) %>% 
  mutate(biomass_temp = mean_bt*tot.biomass) %>% 
  mutate(weighted_temp = sum(biomass_temp)/sum(tot.biomass)) %>% 
  select(YEAR,weighted_temp) %>% distinct()

ggplot(data=halibut_annual_temp,aes(x=YEAR,y=weighted_temp))+
  geom_line()

#look at black sea bass
bsb<-GOM_survey %>% dplyr::filter(RPATH == "BlackSeaBass")
#where do we find bsb (which strata)?
bsb_strata<- bsb$STRATUM %>% unique()
#calculate biomass per stratum per year, append stratum
bsb_biomass<-c()
for (i in 1:length(bsb_strata)) {
  stratum<-bsb_strata[i]
  swept<-calc_swept_area(surveyData=survdat, areaPolygon = 'NEFSC strata', areaDescription = 'STRATA', 
                         filterByArea = stratum, filterBySeason = "all",
                         groupDescription = "RPATH", filterByGroup = "BlackSeaBass", mergesexFlag = T,tidy = F, q = NULL, a = 0.0384)
  biomass<-swept %>% select(YEAR,RPATH,tot.biomass)
  biomass$stratum<-stratum
  bsb_biomass<-rbind(bsb_biomass,biomass)
}

#merge with temp
bsb_biomass<-bsb_biomass %>% rename(STRATUM=stratum)
bsb_biomass<-bsb_biomass %>% left_join(GOM_strat_temp)
bsb_annual_temp<-bsb_biomass %>% na.omit() %>% group_by(YEAR) %>% 
  mutate(biomass_temp = mean_bt*tot.biomass) %>% 
  mutate(weighted_temp = sum(biomass_temp)/sum(tot.biomass)) %>% 
  select(YEAR,weighted_temp) %>% distinct()

ggplot(data=bsb_annual_temp,aes(x=YEAR,y=weighted_temp))+
  geom_line()

ggplot(data=test,aes(x=YEAR,y=weighted_temp,color=sp))+
  geom_line()

#look at all
biomass_temp<-c()
for (j in 1:length(cons_groups$RPATH)) {
  group<-cons_groups$RPATH[j]
  group_survey<-GOM_survey %>% dplyr::filter(RPATH == group)
  #where do we find bsb (which strata)?
  group_strata<- group_survey$STRATUM %>% unique()
  #calculate biomass per stratum per year, append stratum and group name
  for (i in 1:length(group_strata)) {
    stratum<-group_strata[i]
    swept<-calc_swept_area(surveyData=survdat, areaPolygon = 'NEFSC strata', areaDescription = 'STRATA', 
                           filterByArea = stratum, filterBySeason = "all",
                           groupDescription = "RPATH", filterByGroup = group, mergesexFlag = T,tidy = F, q = NULL, a = 0.0384)
    biomass<-swept %>% select(YEAR,RPATH,tot.biomass)
    biomass$stratum<-stratum
    biomass_temp<-rbind(biomass_temp,biomass)
  }
}
  
  #merge with temp
biomass_temp<-biomass_temp %>% rename(STRATUM=stratum) %>% left_join(GOM_strat_temp)
biomass_annual_temp<-biomass_temp %>% na.omit() %>% group_by(YEAR,RPATH) %>% 
  mutate(biomass_temp = mean_bt*tot.biomass) %>% 
  mutate(weighted_temp = sum(biomass_temp)/sum(tot.biomass)) %>% 
  select(YEAR,weighted_temp,RPATH) %>% distinct()


biomass_annual_temp %>% filter(RPATH %in% c("Haddock")) %>%
  ggplot(aes(x=YEAR,y=weighted_temp,color=RPATH))+
  geom_line()

linear_trends<-c()
for (i in 1:length(cons_groups$RPATH)){
  group<-cons_groups$RPATH[i]
  test<-biomass_annual_temp %>% filter(RPATH == group)
  lm<-lm(data=test,weighted_temp~YEAR)
  p<-summary(lm)[["coefficients"]][8]
  p_group<-cbind(p,group)
  linear_trends<-rbind(linear_trends,p_group)
}

#write.csv(biomass_annual_temp,"outputs/biomass_weighted_annual_temp.csv")                                                                                                         
