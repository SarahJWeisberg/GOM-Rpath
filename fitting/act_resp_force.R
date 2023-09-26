#Incorporating temp-dependent P and Q

#Data files: "data/bot_temp_GOM.csv" - Bottom temperature of GOM from ecodata
#             "data/Fitting_Inputs_Bioen.csv" - Bioenergetic parameters from literature review

# Tue Sep 26 18:34:01 2023 ------------------------------


#first need a temperature time series
#code from ecodata
library(ecodata)
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
  geom_line()+
  scale_color_brewer(palette = "Accent")

#get SST in absolute values
#sst<-anom %>% filter(Var == "sst_anomaly") %>% mutate(sst = Value+10.3727)

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
#load(here("outputs/GOM_Rpath.Rdata"))
#load(here("outputs/GOM_params_Rpath.Rdata"))
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
colnames(cons)<-c("Group","Tmax","Topt","Q10")

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

#default Topt and Q10 values
#calculate initial RC
#need index from starting model to get QB, B
# cons <- cons %>% mutate(bt_opt = bt_max*0.9, q10 = 2.3,
#                         start_rc = rc(Tmax = bt_max,Topt = bt_opt,Q10=q10,Temp=start_temp),
#                         index = which(GOM.groups$RPATH %in% cons_groups$RPATH))

#change lobster-specific values
#cons <- cons %>% mutate(bt_opt = replace(bt_opt, RPATH == "AmLobster", 20)) %>%
 # mutate(q10 = replace(q10, RPATH == "AmLobster", 1.8))#lobster specific values

cons <- cons %>% mutate(qb = GOM$QB[cons$index],
                        biomass = GOM$Biomass[cons$index],
                        act_start = GOM.sim$params$ActiveRespFrac[cons$index+1], #because of outside
                        cons_start = qb*biomass,
                        resp_start = cons_start*act_start)

#params over time
#first few years are missing from temp data
bottom_temp<-bottom_temp %>% filter(Time >= 1991 & Time <=2019)
cons_time<-c()
for (i in 1:length(cons$RPATH)) {
  group<-cons[i]
  cons_time_group<-bottom_temp %>%  mutate(rc = rc(Tmax = group$bt_max,Topt = group$bt_opt,Q10=group$q10,Temp=bottom_temp$Value), 
                                     rel_rc = rc/group$start_rc,
                                     cons = rel_rc*group$qb*group$biomass,
                                     rel_resp=rel_resp(temp_c = Value),
                                     resp=rel_resp*group$resp_start,
                                     reL_act_resp=resp/(group$act_start*cons),
                                     RPATH = group$RPATH,
                                     index = group$index)
  cons_time<-rbind(cons_time,cons_time_group)
}

#plug into forcing
for (i in 1:length(cons$RPATH)){
  group<-cons_groups$RPATH[i]
  cons_time_group<- cons_time %>% filter (RPATH == group)
  index<-as.numeric(unique(cons_time_group$index)) + 1 #add 1 because of Outside
  force_act_resp <- c(rep(1,6*12),rep(cons_time_group$reL_act_resp,each=12))
  #adjust forcing
  scene0$forcing$ForcedActresp[,index]<-force_act_resp
}

# spiny <- sst %>% 
#   mutate(rc = rc(Tmax = test_max,Topt = test_opt,Q10=test_q10,Temp=sst), 
#          rel_rc = rc/start_rc,
#          cons = rel_rc*qb_spiny*b_spiny,
#          rel_resp=rel_resp(temp_c = sst),
#          resp=rel_resp*resp_start,
#          reL_act_resp=resp/(act_start*cons))

#force_act_resp <- c(rep(1,6*12),rep(force$reL_act_resp,each=12))  #6 years before we start with the forcing

#adjust forcing
#GOM.sim$forcing$ForcedActresp[,43]<-force_act_resp

GOM.sim$params$NoIntegrate[4:5]<-0

#run
GOM.run <- rsim.run(GOM.sim, method = 'AB', years = fit.years)

# #plot
# rsim.plot(GOM.run, spname = GOM.groups$RPATH[1:10])
# rsim.plot(GOM.run, spname = GOM.groups$RPATH[11:20])
# rsim.plot(GOM.run, spname = GOM.groups$RPATH[21:30])
# rsim.plot(GOM.run, spname = GOM.groups$RPATH[31:40])
# rsim.plot(GOM.run, spname = GOM.groups$RPATH[41:56])
# 
# rsim.plot(run0, spname = GOM.groups$RPATH[1:10])
# rsim.plot(run0, spname = GOM.groups$RPATH[11:20])
# rsim.plot(run0, spname = GOM.groups$RPATH[21:30])
# rsim.plot(run0, spname = GOM.groups$RPATH[31:40])
# rsim.plot(run0, spname = GOM.groups$RPATH[41:56])
# 
# rsim.plot(run1, spname = GOM.groups$RPATH[1:10])
# rsim.plot(run1, spname = GOM.groups$RPATH[11:20])
# rsim.plot(run1, spname = GOM.groups$RPATH[21:30])
# rsim.plot(run1, spname = GOM.groups$RPATH[31:40])
# rsim.plot(run1, spname = GOM.groups$RPATH[41:56])



