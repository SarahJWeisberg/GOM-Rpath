#Incorporating temp-dependent P and Q

#Data files: "data/bot_temp_GOM.csv" - Bottom temperature of GOM from ecodata
#             "data/Fitting_Inputs_Bioen.csv" - Bioenergetic parameters from literature reviews

# Data from https://github.com/NOAA-EDAB/ecodata/blob/master/data-raw/bot_temp_GOM.csv
# Data processing code from https://github.com/NOAA-EDAB/ecodata/blob/master/data-raw/get_bottom_temp.R

# Tue Sep 26 18:34:01 2023 ------------------------------


#first need a temperature time series
#code from ecodata
# remotes::install_github('NOAA-EDAB/ecodata',force = T)
# library(ecodata)
# Process ocean temperature anomaly data

#load packages
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(here)
library(data.table)
library(survdat)
library(ggplot2)
library(Rpath)

#in situ bottom temp measurements from ecodata
bottom_temp_GOM_csv<-read.csv(here("data/bot_temp_GOM.csv")) 
temp<- bottom_temp_GOM_csv %>% dplyr::mutate(Time = as.Date(format(lubridate::date_decimal(Time))),
                                                    Var, Var = plyr::mapvalues(Var, from = c("Tsfc_anom",#Rename variables
                                                                                             "Tsfc_ref",
                                                                                             "Tbot_anom",
                                                                                             "Tbot_ref"),
                                                                               to = c("sst_anomaly",
                                                                                      "sst",
                                                                                      "bt_anomaly",
                                                                                      "bt")))
#isolate mean bottom temp
mean_bt<-as.numeric(temp %>% filter(Var == "bt") %>% distinct(Value))

#use mean + amonaly to get time series
bottom_temp <- temp %>% filter(Var == "bt_anomaly") %>% dplyr::group_by(Time = lubridate::year(Time),Var) %>%
  dplyr::summarise(Value = mean(Value)) %>% dplyr::summarise(Value = Value+mean_bt)

#anom <- temp %>% filter(Var %in% c("bt_anomaly","sst_anomaly")) %>% dplyr::group_by(Time = lubridate::year(Time),Var) %>%
  #dplyr::summarise(Value = mean(Value))

ggplot(bottom_temp,aes(x=Time,y=Value))+
  geom_line()

#get SST in absolute values
#sst<-anom %>% filter(Var == "sst_anomaly") %>% mutate(sst = Value+10.3727)

#now try with GLORYS bottom temp data from Laura
bottom_temp_GLORYS<-read.csv(here("data/gom_bt_for_sarah.csv")) 
#remove first column
bottom_temp_GLORYS<-bottom_temp_GLORYS %>% 
  select(-X) %>%
  mutate(year = lubridate::year(time), month = lubridate::month(time),time=as.Date(time)) %>%
  filter(year < 2020)

#let's start easy - respiration curves
resp<-function(temp_c){
  temp_k<-temp_c+273
  tau<-exp(25.55-0.63/(8.62*10^-5*temp_k))
  return(tau)
}

#make this relative to start period of model
#using bottom temperature
start_temp<-bottom_temp %>% filter(Time<=1985 & Time >=1980)
start_temp<-mean(start_temp$Value)
rel_resp<-function(temp_c){
  rel_tau<-resp(temp_c = temp_c)/resp(start_temp)
  return(rel_tau)
}

#consumption function
rc<-function(Tmax,Topt,Q10,Temp){
  x<-((log(Q10)*(Tmax-Topt))^2/400)*(1+(1+(40/(log(Q10)*(Tmax-Topt+2)))^0.5)^2)
  rc<-((Tmax-Temp)/(Tmax-Topt))^x*exp(x*(1-((Tmax-Temp)/(Tmax-Topt))))
  return(rc)
}


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

#Load balanced model
load(here("outputs/GOM_Rpath.Rdata"))
load(here("outputs/GOM_params_Rpath.Rdata"))
#Run Rsim to get initial resp_fraction
GOM.sim<- rsim.scenario(GOM, GOM.params, years = 1985:2019)

#write a loop to pull max observed temp
bt_max_survey<-c()
for (i in 1: length(cons_groups$RPATH)){
  group<-cons_groups$RPATH[i]
  test<-survdat %>% dplyr::filter(RPATH == group)
  bt_max_survey[i]<-max(na.omit(test$BOTTEMP))
}
bt_max_survey <- as.data.frame(cbind(cons_groups,bt_max_survey))

#load data from literature review
bioen<-read.csv(here("data/Fitting_Inputs_Bioen.csv"))
#make empty data frame
cons<-data.frame(matrix(ncol=4,nrow = length((cons_groups$RPATH))))
colnames(cons)<-c("RPATH","Tmax","Topt","Q10")

for (i in 1: length(cons_groups$RPATH)){
  group<-cons_groups$RPATH[i]
  test<-bioen %>% dplyr::filter(Group == group)
  Tmax<- test %>% select(Tmax_lit,Tmax_BTS,Tmax_Aquamaps)
  Tmax<-max(Tmax,na.rm = T) #pick out max of Tmax (from literature, aquamaps, survey)
  Topt<-na.omit(test$Topt)
  Topt_growth <- na.omit(test$Topt_growth)
  if(length(Topt) == 1){
    Topt<- Topt
  } else if (length(Topt) > 1){
    Topt<-mean(Topt)
  } else if (length(Topt_growth) >0){
    Topt <- (mean(Topt_growth)+Tmax)/2
  } else {
    Topt<-0.9*Tmax
  }
  Q10<-na.omit(test$Q10)
  Q10<-ifelse(length(Q10)>0,mean(Q10),2.3)
  cons[i,]<-c(group,Tmax,Topt,Q10)
}
cons$Tmax<-as.numeric(cons$Tmax)
cons$Topt<-as.numeric(cons$Topt)
cons$Q10<-as.numeric(cons$Q10)

#need index from starting model to get QB, B
#calculate starting rc
cons <- cons %>% mutate(index = which(GOM.groups$RPATH %in% cons_groups$RPATH),
                        start_rc = rc(Tmax = Tmax,Topt = Topt,Q10=Q10,Temp=start_temp))

cons <- cons %>% mutate(qb = GOM$QB[cons$index],
                        biomass = GOM$Biomass[cons$index],
                        act_start = GOM.sim$params$ActiveRespFrac[cons$index+1], #because of outside
                        cons_start = qb*biomass,
                        resp_start = cons_start*act_start)

#params over time
#first few years are missing from temp data
#bottom_temp<-bottom_temp %>% filter(Time >= 1991 & Time <=2019)
cons_time_fixed<-c()
for (i in 1:length(cons$RPATH)) {
  group<-cons[i,]
  cons_time_group<-bottom_temp_GLORYS %>%  mutate(rc = rc(Tmax = group$Tmax,Topt = group$Topt,Q10=group$Q10,Temp=bottom_temp_GLORYS$bt), 
                                     rel_rc = rc/group$start_rc,
                                     cons = rel_rc*group$qb*group$biomass,
                                     rel_resp=rel_resp(temp_c = bt),
                                     resp=rel_resp*group$resp_start,
                                     reL_act_resp=resp/(group$act_start*cons),
                                     RPATH = group$RPATH,
                                     index = group$index,
                                     temp_type = "fixed")
  cons_time_fixed<-rbind(cons_time_fixed,cons_time_group)
}

# #plot
# ggplot(data=cons_time, aes(x=temp,y=cons))+
#   geom_line()+
#   facet_wrap(~RPATH)
# 
ggplot(data=cons_time_fixed, aes(x=time,y=reL_act_resp))+
  geom_point(size=0.5)+
  scale_x_date(date_labels = "%Y")+
  facet_wrap(~RPATH)

#look at cons curves for wide range of temp values
temp_range<-as.data.frame(seq(0,15,by=1))
colnames(temp_range)<-"temp"
cons_time<-c()
for (i in 1:length(cons$RPATH)) {
  group<-cons[i,]
  cons_time_group<- temp_range %>%  mutate(rc = rc(Tmax = group$Tmax,Topt = group$Topt,Q10=group$Q10,Temp=temp), 
                                           rel_rc = rc/group$start_rc,
                                           cons = rel_rc*group$qb*group$biomass,
                                           rel_resp=rel_resp(temp_c = temp),
                                           resp=rel_resp*group$resp_start,
                                           reL_act_resp=resp/(group$act_start*cons),
                                           RPATH = group$RPATH,
                                           index = group$index,
                                           temp = temp_range$temp)
  cons_time<-rbind(cons_time,cons_time_group)
}

# # #try with species-specific temp
# #source(here("fitting/thermal_habitat.R"))
# biomass_annual_temp<-read.csv(here("outputs/biomass_weighted_annual_temp.csv"))
# biomass_annual_temp <- biomass_annual_temp %>% filter(YEAR > 1986)
# cons_time<-c()
# for (i in 1:length(cons$RPATH)) {
#   group<-cons[i,]
#   temp_time<-biomass_annual_temp %>% filter(RPATH == group$RPATH)
#   for (j in 1:length(temp_time$YEAR)){
#     temp_year<- temp_time[j,]
#     cons_time_group<- temp_year %>%  mutate(rc = rc(Tmax = group$Tmax,Topt = group$Topt,Q10=group$Q10,Temp=temp_year$weighted_temp),
#                                            rel_rc = rc/group$start_rc,
#                                            cons = rel_rc*group$qb*group$biomass,
#                                            rel_resp=rel_resp(temp_c = weighted_temp),
#                                            resp=rel_resp*group$resp_start,
#                                            reL_act_resp=resp/(group$act_start*cons),
#                                            RPATH = group$RPATH,
#                                            index = group$index,
#                                            temp_type = "variable")
#   cons_time<-rbind(cons_time,cons_time_group)
#   }
# }
# 
# # #merge
# cons_time_vary <- cons_time %>% select(RPATH,YEAR,reL_act_resp,temp_type)
# cons_time_fixed <- cons_time_fixed %>% rename(YEAR = year) %>% select(RPATH,YEAR,reL_act_resp,temp_type)
# comp <- bind_rows(cons_time_vary,cons_time_fixed)
# 
# ggplot(data = comp, aes(x=YEAR,y=reL_act_resp,color=temp_type))+
#   geom_line()+
#   facet_wrap(~RPATH)






