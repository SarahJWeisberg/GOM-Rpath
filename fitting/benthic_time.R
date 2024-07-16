#pulling in mega and macrobenthos time series for forcing

# Wed Jul  3 12:20:35 2024 ------------------------------

#load packages
library(dplyr)
library(tidyr)

#Load balanced model
load(here("outputs/GOM_Rpath.Rdata"))
load(here("outputs/GOM_params_Rpath.Rdata"))

#macrobenthos 
url<-"https://github.com/NOAA-EDAB/Rpathdata/blob/main/data-raw/fallmacrobenthosindex.rds?raw=true"
download.file(url,destfile = "data/fallmacrobenthosindex.rds")
macro <- readRDS("data/fallmacrobenthosindex.rds")

#megabenthos
url<-"https://github.com/NOAA-EDAB/Rpathdata/blob/main/data-raw/fallmegabenthosindex.rds?raw=true"
download.file(url,destfile = "data/fallmegabenthosindex.rds")
mega <- readRDS("data/fallmegabenthosindex.rds")

#filter for GOM only, remove unneeded column, relabel biomass, SE for simplicity
macro_gom <- macro %>% 
  filter(EPU == "GOM") %>% 
  select(-Units) %>%
  mutate(Var = ifelse(Var == "Fall Macrobenthos Biomass Index Estimate","biomass","SE"))
mega_gom <- mega %>% 
  filter(EPU == "GOM") %>% 
  select(-Units) %>%
  mutate(Var = ifelse(Var == "Fall Megabenthos Biomass Index Estimate","biomass","SE"))

#calculate 1980-85 mean
macro_start <- macro_gom %>% 
  filter(Time < 1986) %>% 
  group_by(Var) %>%
  summarise(mean_start = mean(Value))
mega_start <- mega_gom %>% 
  filter(Time < 1986) %>% 
  group_by(Var) %>%
  summarise(mean_start = mean(Value))

#merge with time series, calculate anomalies
macro_gom <- left_join(macro_gom,macro_start,by="Var")
macro_gom <- macro_gom %>% mutate(anom = (Value-mean_start)/mean_start)
mega_gom <- left_join(mega_gom,mega_start,by="Var")
mega_gom <- mega_gom %>% mutate(anom = (Value-mean_start)/mean_start)

#generate forcing time series
macro<-GOM.params$model[Group == "Macrobenthos",Biomass]
mega<-GOM.params$model[Group == "Megabenthos",Biomass]

macro_time<-macro_gom %>% 
  filter(Var == "biomass" & Time > 1984) %>% 
  select(Time,anom) %>%
  mutate(biomass = macro*(anom+1))

mega_time<-mega_gom %>% 
  filter(Var == "biomass" & Time > 1984) %>% 
  select(Time,anom) %>%
  mutate(biomass = macro*(anom+1))

#visualize
# macro_gom %>% filter(Var == "biomass" & Time > 1984) %>% 
#   ggplot(aes(x=Time,y=anom))+
#   geom_line()+
#   geom_point()
