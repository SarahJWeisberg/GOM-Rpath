#Phyto Time Series

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(zoo)

#raw.dir <- here::here("data-raw")

# input files ----
ppd_csv <- read.csv("19980101_20221231-OCCCI_GLOBCOLOUR-PPD-STATS_ANOMS-NES_EPU_NOESTUARIES-SOE_V2023-SOE_FORMAT.csv")
chl_csv <- read.csv("19980101_20221231-OCCCI_GLOBCOLOUR-CHLOR_A-STATS_ANOMS-NES_EPU_NOESTUARIES-SOE_V2023-SOE_FORMAT.csv")

# transformation ----
#ppd <- read.csv(file.path(raw.dir, ppd_csv)) %>%
  #dplyr::mutate(ALGORITHM = word(str_replace(ALGORITHM, "_", " "))) %>%
  #tidyr::unite(.,VARIABLE, c("VARIABLE","SENSOR","ALGORITHM"), sep = " ") %>%
  # dplyr::mutate(VARIABLE = ifelse(stringr::str_detect(FILENAME, "1998_2020"),
  #                           paste(VARIABLE,"1998_20120")))
  #                          # ifelse(stringr::str_detect(FILENAME, "1998_2020"),
  #                          #        paste(VARIABLE, "1998_2018"),
  #                          #        ifelse(stringr::str_detect(FILENAME, "1997_2019"),
  #                          #               paste(VARIABLE, "1997_2019"),
  #                          #               ifelse(stringr::str_detect(FILENAME, "1997_2018"),
  #                          #                      paste(VARIABLE, "1997_2018"),
  #                          #                      VARIABLE))))) %>%
ppd<-ppd_csv %>% dplyr::select(PERIOD, UNITS, VARIABLE, VALUE, SUBAREA) %>%
  dplyr::rename(Time = PERIOD, Units = UNITS, Var = VARIABLE,
                EPU =SUBAREA, Value = VALUE)

#Filter for just GoM
ppd_gom<-ppd %>% filter(EPU == "GOM")

#Make numeric column for year, get rid of weekly estimates
ppd_gom$Year<-as.numeric(str_remove(ppd_gom$Time,"A_"))
ppd_gom<-na.omit(ppd_gom)

#Make sure values are numeric
ppd_gom$Value<-as.numeric(ppd_gom$Value)

#Turn Var colum into useful categories - anomaly vs. median
ppd_gom$Var<-str_remove(ppd_gom$Var,"ANNUAL_PPD_")

#Plot both median and anomaly
ggplot(data = ppd_gom,aes(x=Year, y=Value))+
  geom_line()+
  stat_smooth(method = "lm")+
  facet_wrap(~Var, scales = "free")

#Plot just anomalies
ppd_gom %>% filter(Var == "RATIO_ANOMALY") %>%
  ggplot(aes(x=Year, y=Value))+
  geom_point()

lm<-lm(Value~Year, data = ppd_gom, subset = (Var == "MEDIAN"))
summary(lm)
#chl <- read.csv(file.path(raw.dir, chl_csv)) %>%
  #dplyr::mutate(ALGORITHM = word(stringr::str_replace(ALGORITHM, "_", " "))) %>%
  #tidyr::unite(.,VARIABLE, c("VARIABLE","SENSOR","ALGORITHM"), sep = " ") %>%
  # dplyr::mutate(VARIABLE = ifelse(stringr::str_detect(FILENAME, "1998_2019"),
  #                          paste(VARIABLE,"1998_2019"),
  #                          ifelse(stringr::str_detect(FILENAME, "1998_2017"),
  #                                 paste(VARIABLE, "1998_2017"),
  #                                 ifelse(stringr::str_detect(FILENAME, "1997_2019"),
  #                                        paste(VARIABLE, "1997_2019"),
  #                                        ifelse(stringr::str_detect(FILENAME, "1997_2017"),
  #                                               paste(VARIABLE, "1997_2017"),
  #                                               VARIABLE))))) %>%
chl<-chl_csv %>% dplyr::select(PERIOD, UNITS, VARIABLE, VALUE, SUBAREA) %>%
 dplyr::rename(Time = PERIOD, Units = UNITS, Var = VARIABLE,
                EPU = SUBAREA, Value = VALUE)

#chl_pp <- rbind(ppd,chl)%>%
 # tibble::as_tibble() %>%
#  dplyr::select(Time, Var, Value, EPU, Units)

#Filter for just GoM
chl_gom<-chl %>% filter(EPU == "GOM")

#Make numeric column for year, get rid of weekly estimates
chl_gom$Year<-as.numeric(str_remove(chl_gom$Time,"A_"))
chl_gom<-na.omit(chl_gom)

#Make sure values are numeric
chl_gom$Value<-as.numeric(chl_gom$Value)

#Turn Var colum into useful categories - anomaly vs. median
chl_gom$Var<-str_remove(chl_gom$Var,"ANNUAL_CHLOR_A_")

#Plot both median and anomaly
ggplot(data = chl_gom,aes(x=Year, y=Value))+
  geom_line()+
  facet_wrap(~Var, scales = "free")

#Plot just anomalies
chl_gom %>% filter(Var == "RATIO_ANOMALY") %>%
  ggplot(aes(x=Year, y=Value))+
  geom_point()

lm<-lm(Value~Year, data = chl_gom, subset = (Var == "RATIO_ANOMALY"))
summary(lm)

#set baselines
#prior to 2001, which is years included in EMAX estimates
#start with ppd
ppd_EMAX<-ppd_gom %>% group_by(Var) %>%
  filter(Year <2001) %>% summarise(mean=mean(Value))

ppd_gom<-left_join(ppd_gom,ppd_EMAX, by="Var")
ppd_gom$anom<-(ppd_gom$Value-ppd_gom$mean)/ppd_gom$mean

#plot just after 2000
ppd_gom %>% filter(Year >2000)%>%
ggplot(aes(x=Year, y=anom))+
  geom_point()+
  geom_line(aes(y=zoo::rollmean(anom,3,na.pad = T)))+
  facet_wrap(~Var)

#repeat with chl
chl_EMAX<-chl_gom %>% group_by(Var) %>%
  filter(Year <2001) %>% summarise(mean=mean(Value))

chl_gom<-left_join(chl_gom,chl_EMAX, by="Var")
chl_gom$anom<-(chl_gom$Value-chl_gom$mean)/chl_gom$mean

#plot just after 2003
chl_gom %>% filter(Year >2000)%>%
  ggplot(aes(x=Year, y=anom))+
  geom_point()+
  geom_line(aes(y=zoo::rollmean(anom,3,na.pad = T)))+
  facet_wrap(~Var, scales = "free")
