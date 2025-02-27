#Title: GOM Rpath Discards Estimates

# Purpose: To estimate discards (t/km^2) for functional groups in GOM RPATH model.
#       Based on data from NOAA observer program.

# DataFile:'discards_fleet_year.csv'

# Author: S. Weisberg
# Contact details: sarah.weisberg@noaa.gov

# Thu Jan  2 13:45:12 2025 ------------------------------

#load packages
library(ggplot2)
library(here)
library(dplyr)

#load file
#see NEUS repo
discards <- read.csv(here("data/discards_fleet_year.csv"))

discards_GOM <- discards %>%
  dplyr::filter(EPU == "GOM")

discards_GOM <- discards_GOM %>%
  dplyr::mutate(disc_t_km2 = as.numeric(sumDISMT/GOM.area))

#plot checks
discards_GOM %>% filter(Fleet == "SM Mesh" & YEAR > 1979) %>%
  ggplot() +
  geom_point(aes(x = YEAR, y= disc_t_km2)) +
  facet_wrap(~RPATH)

#trying to specifically think about the lobster issues
#this looks at totals
lobster_land <- com.land %>% filter(RPATH == "AmLobster") %>% 
  drop_units() %>%
  select(RPATH, FLEET, YEAR, landings) %>%
  mutate(data = "Landings")
lobster_disc <- discards_GOM %>% filter(RPATH == "AmLobster") %>%
  group_by(YEAR) %>% mutate(sum_disc = sum(disc_t_km2))

#let's look by fleet
lobster_disc_fleet <- lobster_disc %>%
  dplyr::select(RPATH, Fleet, YEAR, disc_t_km2) %>%
  mutate(data = "Discards") %>%
  rename(FLEET = Fleet, landings = disc_t_km2)

#compare discards with catch
lobster_comp <- rbind(lobster_land,lobster_disc_fleet) %>% ungroup()
lobster_comp <- as.data.frame(lobster_comp)

#look at trap fishery alone
lobster_comp %>% filter(FLEET == "Trap", YEAR > 1979) %>%
ggplot()+
  geom_line(aes(x = YEAR, y = landings, color = data))+
  ylab("catch (t/km^2)")+
  ggtitle("AmLobster Catch in Trap Fleet (GOM)")

#look at other gear types -- do these results align with assessment?
ggplot(lobster_disc, aes(x = YEAR, y = sumDISMT))+
  geom_point()+
  facet_wrap(~Fleet, scales = "free")

#let's combine all catch data into one df
discards_GOM <- discards_GOM %>% 
  select(Fleet, RPATH, YEAR, disc_t_km2) %>%
  rename(FLEET = Fleet, catch = disc_t_km2) %>%
  mutate(data = "Discards") 

com.land <- com.land %>% 
  mutate(data = "Landings") %>%
  rename(catch = landings) %>%
  drop_units()

catch <- rbind(com.land,discards_GOM)

#smooth dogfish
catch %>% filter(RPATH == "Sharks" & YEAR > 1979) %>%
ggplot() +
  geom_point(aes(x = YEAR, y = catch, colour = data)) +
  facet_wrap(~FLEET, scales = "free")

#compare to starting values
load(here("outputs/GOM_Rpath.RData"))
load(here("outputs/GOM_params_Rpath.RData"))

start_biomass <- as.data.frame(cbind(GOM.params$model$Group,GOM.params$model$Biomass))
colnames(start_biomass) <- c("RPATH","Biomass")
start_biomass <- start_biomass %>%
  dplyr::mutate(Biomass = as.numeric(Biomass))

discards_GOM_agg <- discards_GOM %>%
  dplyr::group_by(YEAR,RPATH) %>%
  dplyr::summarise(tot_disc = sum(disc_t_km2))

discards_GOM_agg <- left_join(discards_GOM_agg, start_biomass,by="RPATH")

discards_GOM_80s <- discards_GOM_agg %>%
  dplyr::filter(YEAR <= 1985 & YEAR >=1980) %>%
  dplyr::filter(!is.na(Biomass)) %>%
  dplyr::group_by(RPATH) %>%
  dplyr::mutate(mean_disc = mean(tot_disc), 
                biomass_ratio = mean_disc/Biomass) %>%
  dplyr::select(RPATH,mean_disc,Biomass,biomass_ratio) %>%
  dplyr::distinct()

#compare with landings
landings <- as.data.frame(cbind(GOM$Group,rowSums(GOM$Landings)))
colnames(landings) <- c("RPATH","landings")
landings <- landings %>%
  mutate(landings = as.numeric(landings))

discards_GOM_80s <- left_join(discards_GOM_80s,landings,by="RPATH")

discards_GOM_80s <- discards_GOM_80s %>%
  mutate(landings_ratio = mean_disc/landings)

long <- discards_GOM_80s %>%
  tidyr::gather(key="metric",value="value",2:6) %>%
  dplyr::filter(metric %in% c("mean_disc","Biomass","landings"))

ggplot(data=long, aes(x=metric,y=value,fill=metric)) +
  geom_bar(stat="identity") +
  facet_wrap(~RPATH,scales="free")


##more generic workflow -- how to get from data to Rpath inputs?
discards_GOM_80s <- discards_GOM %>% 
  dplyr::filter(YEAR < 1986 & YEAR > 1980) %>%
  dplyr::group_by(RPATH, FLEET) %>%
  dplyr::summarise(mean_disc = mean(catch))

#isolate all fleets
fleets <- as.vector(groups_fleets[(nliving+ndead+1):(nliving+ndead+nfleets)]$RPATH)

#fill rpath parameter files with appropriate discards values
for (i in 1:length(fleets)) {
  d <- discards_GOM_80s %>% 
    dplyr::filter(FLEET == fleets[i])
  d<-left_join(groups_fleets,d,by="RPATH")
  d<-as.vector(d$mean_disc)
  d[is.na(d)]<-0
  d[[(nliving+1):(nliving+ndead)]]<-0
  d[(nliving+ndead+1):(nliving+ndead+nfleets)]<-NA
  GOM.params$model[, paste(fleets[i],".disc",sep = "") := d]
}


