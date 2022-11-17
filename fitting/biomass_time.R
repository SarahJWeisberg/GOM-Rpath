# Purpose: This script summarizes biomass in t/km^2
#           for all RPath groups per year from 1986-2019.
#           Used to generate data for time series fitting routine

# DataFiles: 'NEFSC_BTS_2021_all_seasons.RData'

# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu

# Thu Apr 14 17:09:48 2022 ------------------------------

#Code modified from survey_biomass_estimates.R

#Load needed packages
#Load required packages
library(here);library(data.table);library(survdat); library(units); library(ggplot2)

#Load survey data
load(here('data/NEFSC_BTS_2021_all_seasons.RData'))

#Load species codes
#Use these codes to translate survey species codes ('SVSPP') to RPATH species codes ('spp')
load(here('data/speciescodesandstrata/Species_codes.Rdata'))

#Calculate total GOM area
area<-sf::st_read(dsn=system.file("extdata","strata.shp",package="survdat"))
area<-get_area(areaPolygon = area, areaDescription="STRATA")
GOM.area<-subset(area, area$STRATUM %in% c(1220, 1240, 1260:1290, 1360:1400, 3560:3830))
GOM.area<-sum(GOM.area$Area)

#Calculate swept area biomass
swept<-calc_swept_area(surveyData=survey$survdat, areaPolygon = 'NEFSC strata', areaDescription = 'STRATA', filterByArea = c(1220, 1240, 1260:1290, 1360:1400, 3560:3830), filterBySeason= "FALL", groupDescription = "SVSPP", filterByGroup = "all", mergesexFlag = T,tidy = F, q = NULL, a = 0.0384)

#Merge with RPATH names
#RPATH names are the same as in Georges Bank model
#Aggregate groups below 0.05 t/km^2 threshold
spp <- spp[!duplicated(spp$SVSPP),] #SVSPPs are NEFSC species-specific codes
spp <- spp[RPATH == 'RedCrab', RPATH := 'Macrobenthos']
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

#Calcuate biomass, sd / area in mt
swept <- swept[, biomass.area   := (tot.biomass*.001)/(Fall.q*GOM.area)]
swept <- swept[, sd.area   := (sqrt(tot.bio.var)*.001)/(Fall.q*GOM.area)]

#add biomasses (relevant for aggregate groups) 
setkey(swept,RPATH,YEAR)
biomass_fit <- swept[, sum(biomass.area), by = key(swept)]
setnames(biomass_fit, 'V1','Biomass')

#average stderror (relevant for aggregate groups)
#not correcting for area because that shouldn't matter - right??
setkey(swept,RPATH,YEAR)
sd_fit<-swept[,mean(sd.area), by = key(swept)]
setnames(sd_fit,'V1','Stdev')

#merge biomass with SD
biomass_fit<-cbind(biomass_fit,sd_fit$Stdev)
setnames(biomass_fit, 'V2','Stdev')

#subset for 1985-2019
biomass_fit<-biomass_fit[YEAR %in% 1985:2019]

#drop units for plotting purposes
biomass_fit$Biomass <- drop_units(biomass_fit$Biomass)
biomass_fit$Stdev <- drop_units(biomass_fit$Stdev)

biomass_fit<-biomass_fit[!RPATH %in% c(NA, 'Fauna','Freshwater','SouthernDemersals')]

#adjust biomass values for changes made during balancing
source(here("R/groups_fleets.R"))
#pull initial estimates
source(here("R/EMAX_biomass_estimates.R"))
#pull estimates from balanced model
load(here( "outputs/GOM_params_Rpath.RData"))
biomass_80s_balanced<-as.data.frame(cbind(GOM.params$model$Group[1:58], GOM.params$model$Biomass[1:58]))
colnames(biomass_80s_balanced)<-c("RPATH","Biomass_balanced")
#merge
ratio<-left_join(biomass_80s,biomass_80s_balanced,by="RPATH")
ratio$Biomass_balanced<-as.numeric(ratio$Biomass_balanced) #why does this happen??
ratio<-ratio %>% mutate(ratio = Biomass_balanced/Biomass) %>% select(RPATH,ratio)

#Need to deal with Macrobenthos and Megabenthos groups - these come from EMAX not trawl
#But want to keep temporal trend captured by trawl
#So rescale based on starting value
#remove current 
ratio<-ratio %>% filter(RPATH != "Macrobenthos" & RPATH != "Megabenthos")
benthos<-left_join(biomass_fit %>% filter(YEAR == 1985, RPATH %in% c("Macrobenthos","Megabenthos")), biomass_80s_balanced %>% filter(RPATH %in% c("Macrobenthos","Megabenthos")),by="RPATH")
benthos$Biomass_balanced<-as.numeric(benthos$Biomass_balanced)
benthos<-benthos %>% mutate(ratio = Biomass_balanced/Biomass) %>% select(RPATH,ratio)

#merge
ratio<-bind_rows(ratio,benthos)

#add adjusted biomass column 
biomass_adjust<- left_join(biomass_fit,ratio,by="RPATH") %>% replace_na(list(ratio=1))
biomass_adjust <- biomass_adjust %>% mutate(Value = Biomass*ratio)

#rename columns to comply with fitting code
biomass_adjust <- biomass_adjust %>% rename(Group = RPATH, Year = YEAR) %>% select(-Biomass, -ratio)
#set biomass as index or absolute as appropriate
biomass_adjust$Type <- rep("absolute",length(biomass_adjust$Group))
biomass_adjust$Scale <- rep(1,length(biomass_adjust$Group))

#save file
write.csv(biomass_adjust,"fitting/biomass_fit.csv")


#visualize biomass trends over time
ggplot(biomass_adjust,aes(x=Year, y = Value)) +
  geom_point() +
  facet_wrap(vars(Group),ncol = 4, scale = "free")

#ggplot(fitting_groups,aes(x=Year, y = Index)) +
# geom_smooth(method = "lm", se=FALSE, color="grey", formula = y ~ x)+
#geom_point() +
#facet_wrap(vars(Group),ncol = 2, scale = "free") +
#labs(title = "Biomass Trends over Time", y="t/sq.km")
