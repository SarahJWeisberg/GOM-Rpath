#Title: GOM Rpath Diet Matrix

# Purpose: This script generates a diet matrix for all 
#       functional groups used in the GOM-Rpath model.
#       Data are sourced from the NESFC food habits database. 
#       Data are reported as proportion of prey group in predator diet.
#       Model is built to be analogous to Georges Bank Rpath model
#       https://github.com/NOAA-EDAB/GBRpath

# DataFile:"GOM_foodhabits.RData"

# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu

xfun::session_info()

#Last modified 
# Thu Dec  1 10:52:18 2022 ------------------------------


#Load packages
library(readr);library (data.table);library(here)

#load stomach data
load(here("data/GOM_foodhabits.RData"))

# Import prey naming table
prey <- as.data.table(read_csv("data/SASPREY12B.csv"))

#load EMAX model
EMAX.params<-as.data.table(read.csv('data/GOM_EMAX_params.csv'))

#load biomass estimates from EMAX
#source('R/EMAX_biomass_estimates.R')

#Match prey codes with RPath group names
#Start with 1:1
prey[PYCOMNAM == 'ATLANTIC HERRING',        RPATH := 'AtlHerring']
prey[PYCOMNAM == 'ATLANTIC MACKEREL',       RPATH := 'AtlMackerel']
prey[PYCOMNAM == 'BUTTERFISH',              RPATH := 'Butterfish']
prey[PYCOMNAM == 'BUTTERFISH OTOLITHS',     RPATH := 'Butterfish']
prey[PYCOMNAM == 'ATLANTIC COD',            RPATH := 'Cod']
prey[PYCOMNAM == 'HADDOCK',                 RPATH := 'Haddock']
prey[PYCOMNAM == 'GOOSEFISH',               RPATH := 'Goosefish']
prey[PYCOMNAM == 'OFFSHORE HAKE',           RPATH := 'OtherDemersals']
prey[PYCOMNAM == 'SILVER HAKE',             RPATH := 'SilverHake']
prey[PYCOMNAM == 'SILVER HAKE OTOLITHS',    RPATH := 'SilverHake']
prey[PYCOMNAM == 'RED HAKE',                RPATH := 'RedHake']
prey[PYCOMNAM == 'WHITE HAKE',              RPATH := 'WhiteHake']
prey[PYCOMNAM == 'ACADIAN REDFISH',         RPATH := 'Redfish']
prey[PYCOMNAM == 'POLLOCK',                 RPATH := 'Pollock']
prey[PYCOMNAM == 'OCEAN POUT',              RPATH := 'OceanPout']
prey[PYCOMNAM == 'BLACK SEA BASS',          RPATH := 'BlackSeaBass']
prey[PYCOMNAM == 'BLUEFISH',                RPATH := 'OtherPelagics']
prey[PYCOMNAM == 'SCUP',                    RPATH := 'OtherDemersals']
prey[PYCOMNAM == 'FOURSPOT FLOUNDER',       RPATH := 'Fourspot']
prey[PYCOMNAM == 'SUMMER FLOUNDER',         RPATH := 'SummerFlounder']
prey[PYCOMNAM == 'AMERICAN PLAICE',         RPATH := 'AmPlaice']
prey[PYCOMNAM == 'WINDOWPANE',              RPATH := 'Windowpane']
prey[PYCOMNAM == 'WINTER FLOUNDER',         RPATH := 'WinterFlounder']
prey[PYCOMNAM == 'WITCH FLOUNDER',          RPATH := 'WitchFlounder']
prey[PYCOMNAM == 'YELLOWTAIL FLOUNDER',     RPATH := 'YTFlounder']
prey[PYCOMNAM == 'SPINY DOGFISH',           RPATH := 'SpinyDogfish']
prey[PYCOMNAM == 'SMOOTH DOGFISH',          RPATH := 'SmoothDogfish']
prey[PYCOMNAM == 'LITTLE SKATE',            RPATH := 'LittleSkate']
prey[PYCOMNAM == 'NORTHERN SHORTFIN SQUID', RPATH := 'Illex']
prey[PYNAM    == 'ILLEX SP',                RPATH := 'Illex']
prey[PYCOMNAM == 'AMERICAN LOBSTER',        RPATH := 'AmLobster']
prey[PYCOMNAM == 'KRILL',                   RPATH := 'Micronekton'] 
prey[PYCOMNAM == 'EMPTY STOMACH',           RPATH := 'Empty']
prey[PYCOMNAM == 'BLOWN STOMACH',           RPATH := 'Blown']
prey[PYCOMNAM == 'NORTHERN SHRIMP',         RPATH := 'NShrimp']
prey[PYCOMNAM == 'HORSESHOE CRAB',          RPATH := 'Megabenthos']
prey[PYCOMNAM == 'ALEWIFE',          RPATH := 'RiverHerring']
prey[PYNAM == 'SELENE SETAPINNIS', RPATH := 'SmPelagics']
prey[PYNAM == 'EPIGONUS PANDIONIS', RPATH := 'OtherDemersals']
#Need to revisit River Herring classification

#Easy many to ones
prey[PYCOMNAM %in% c('SEA SCALLOP', 'SEA SCALLOP VISCERA'), RPATH := 'Megabenthos']
prey[PYCOMNAM %in% c('ATLANTIC SURFCLAM', 'SURFCLAM VISCERA', 'OCEAN QUAHOG', 
                     'OCEAN QUAHOG VISCERA', 'OCEAN QUAHOG SHELL'), RPATH := 'Megabenthos']
prey[PYCOMNAM %in% c('LONGFIN SQUID', 'LOLIGO SP PEN'), RPATH := 'Loligo']
prey[PYNAM    %in% c('LOLIGO SP', 'LOLIGO SP BEAKS'),   RPATH := 'Loligo']

#Use higher level categories
#A majority of unassigned prey are in the MODCAT BENINV - whittle out those that 
#don't go into macrobenthos then assign the rest

#Megabenthos - Asteroids, mantis shrimp, crabs other than hermits, lobsters
prey[is.na(RPATH) & AnalCom == 'STARFISH', RPATH := 'Megabenthos']
prey[is.na(RPATH) & AnalCom == 'MANTIS SHRIMPS', RPATH := 'Megabenthos']
prey[is.na(RPATH) & Collcom %in% c('CANCER CRABS', 'DECAPODA CRAB', 'DECAPODA', 
                                   'DECAPODA LARVAE', 'LOBSTER', 'BLUE CRAB', 
                                   'SLIPPER LOBSTERS'), RPATH := 'Megabenthos']

#GelZooplankton/Cephalopods/Shrimp
prey[is.na(RPATH) & AnalCom == 'SQUIDS, OCTOPUS', RPATH := 'OtherCephalopods']
prey[is.na(RPATH) & Collcom %in% c('JELLYFISH', 'CNIDARIA', 'HYDROZOA'), RPATH := 'GelZooplankton']       
prey[is.na(RPATH) & Collcom %in% c('PANDALIDAE', 'PENAEIDAE', 'DECAPODA SHRIMP', 'CRAGONID SHRIMP'), RPATH := 'OtherShrimps']

#Macrobenthos
prey[is.na(RPATH) & MODCAT == 'BENINV', RPATH := 'Macrobenthos']

#MODCAT PELINV
prey[is.na(RPATH) & Collcom == 'COMB JELLIES', RPATH := 'GelZooplankton']
prey[PYCOMNAM == 'ROTIFERS', RPATH := 'Microzooplankton']
prey[is.na(RPATH) & Collcom == 'KRILL', RPATH := 'Micronekton'] 
prey[is.na(RPATH) & Collcom == 'COPEPODA', RPATH := 'LgCopepods']
prey[is.na(RPATH) & MODCAT == 'PELINV', RPATH := 'Micronekton']

#MODCAT LDEM
prey[is.na(RPATH) & ANALCAT %in% c('BOTFAM', 'SOLFAM'), RPATH := 'SmFlatfishes']
prey[is.na(RPATH) & ANALCAT == 'RAJORD', RPATH := 'OtherSkates']
prey[is.na(RPATH) & ANALCAT %in% c('LUTFAM', 'SCAFAM', 'SCIFAM', 'SPAFAM', 'SERFA3'), RPATH := 'OtherDemersals']
prey[is.na(RPATH) & ANALCAT == 'SHARK', RPATH := 'Sharks']
prey[is.na(RPATH) & ANALCAT == 'MACFAM', RPATH := 'Mesopelagics']
prey[is.na(RPATH) & ANALCAT %in% c('PLEFAM', 'PLEORD'), RPATH := 'OtherDemersals']
prey[is.na(RPATH) & MODCAT == 'LDEM', RPATH := 'OtherDemersals']
#Need to revisit ANALCAT GADFAM - Gadidae, urophycis sp

#MODCAT LPEL
prey[PYCOMNAM %in% c('BOA DRAGONFISH', 'VIPERFISH'), RPATH := 'Mesopelagics']
prey[is.na(RPATH) & MODCAT == 'LPEL' & ANALCAT %in% c('CARFAM', 'POMFAM', 'SALSAL','SCOFAM', 'OTHFIS'), RPATH := 'OtherPelagics']
prey[is.na(RPATH) & MODCAT == 'LPEL', RPATH := 'SmPelagics']

#MODCAT SDEM
prey[PYCOMNAM == 'DRAGONET FISH', RPATH := 'Mesopelagics']
prey[is.na(RPATH) & AnalCom == 'GREENEYES', RPATH := 'Mesopelagics']
prey[is.na(RPATH) & MODCAT == 'SDEM', RPATH := 'OtherDemersals']

#MODCAT SPEL
prey[is.na(RPATH) & MODCAT == 'SPEL' & AnalCom == 'LANTERNFISHES', RPATH := 'Mesopelagics']
prey[PYABBR == 'MAUWEI', RPATH := 'Mesopelagics']
prey[is.na(RPATH) & MODCAT == 'SPEL' & AnalCom == 'HERRINGS', RPATH := 'RiverHerring']
prey[is.na(RPATH) & MODCAT == 'SPEL', RPATH := 'SmPelagics']

#Fish Larvae
prey[is.na(RPATH) & MODCAT == 'FISLAR', RPATH := 'Micronekton']

#Ignoring eggs for now
prey[is.na(RPATH) & MODCAT == 'FISEGG', RPATH := 'NotUsed']

#Miscellaneous - Mostly trash (plastic, twine, rubber)
prey[PYABBR %in% c('POLLAR', 'AMPTUB'), RPATH := 'Macrobenthos']
prey[is.na(RPATH) & MODCAT == 'MISC', RPATH := 'NotUsed']

#Other
prey[PYABBR %in% c('INVERT', 'ARTHRO', 'CRUSTA', 'CRUEGG', 'INSECT', 'UROCHO'),
     RPATH := 'Macrobenthos']
prey[PYABBR %in% c('MARMAM', 'MARMA2', 'DELDEL', 'GLOBSP'), RPATH := 'Odontocetes']
prey[PYABBR %in% c('AVES', 'AVEFEA'), RPATH := 'SeaBirds']
prey[PYABBR %in% c('PLANKT', 'DIATOM'), RPATH := 'Phytoplankton']
prey[is.na(RPATH) & MODCAT == 'OTHER', RPATH := 'NotUsed'] #Plants and Parasites

#Leftovers
prey[AnalCom == 'SAND LANCES', RPATH := 'SmPelagics']
prey[PYABBR %in% c('PERORD', 'MYOOCT'), RPATH := 'OtherDemersals']
prey[PYABBR %in% c('CLUSCA', 'CLUHA2'), RPATH := 'AtlHerring']


#Unidentified Stuff
prey[MODCAT == 'AR', RPATH := 'AR']
prey[is.na(RPATH) & AnalCom == 'OTHER FISH', RPATH := 'UNKFish']
prey[PYABBR == 'FISSCA', RPATH := 'UNKFish']
prey[PYABBR == 'CHONDR', RPATH := 'UNKSkate']
prey[PYABBR == 'PRESER', RPATH := 'NotUsed']

#Assign Rpath codes to predators
GOM.fh <- merge(GOM.fh, spp, by = 'SVSPP', all.x = T)
setnames(GOM.fh, 'RPATH', 'Rpred')

#Assign Rpath codes to prey
GOM.fh <- merge(GOM.fh, prey[, list(PYNAM, RPATH)], by = 'PYNAM', all.x = T)
setnames(GOM.fh, 'RPATH', 'Rprey')

#Remove NotUsed, AR, UNKFish and UNKSkate as prey
GOM.fh <- GOM.fh[!Rprey %in% c('NotUsed', 'AR', 'UNKFish', 'UNKSkate'), ]

#Remove Freshwater as predator
GOM.fh <- GOM.fh[!Rpred %in% 'Freshwater', ]

#Assign missing RPATH names
GOM.fh<-GOM.fh[PYNAM=='NEPTUNEA DECEMCOSTATA',Rprey:='Macrobenthos']
GOM.fh<-GOM.fh[PYNAM=='STERNOPTYCHIDAE',Rprey:='Mesopelagics']

#Merge prey items
setkey(GOM.fh, YEAR, SEASON, CRUISE6, STRATUM, STATION, TOW, Rpred, PDID, Rprey)
GOM.fh2 <- GOM.fh[, sum(PYAMTW), by = key(GOM.fh)]
setnames(GOM.fh2, 'V1', 'PYAMTW')

#Calculate Percent weight using a cluster sampling design (Nelson 2014)
#Clusters are station/Rpred combos
cluster <- c('CRUISE6', 'STRATUM', 'STATION', 'TOW', 'Rpred')

#Calculate numbers of fish per cluster
GOM.pred <- unique(GOM.fh2, by = c(cluster, 'PDID'))
GOM.pred[, Mi := length(PDID), by = cluster]
GOM.pred <- unique(GOM.pred[, list(CRUISE6, STRATUM, STATION, TOW, Rpred, Mi)])
GOM.pred[, sumMi := sum(Mi), by = Rpred]
GOM.fh2 <- merge(GOM.fh2, GOM.pred, by = cluster)

#Sum prey weight per stomach
GOM.fh2[, yij := sum(PYAMTW), by = c(cluster, 'Rprey')]
GOM.fh2[, mu := yij / Mi]
GOM.cluster <- unique(GOM.fh2, by = c(cluster, 'Rprey'))
GOM.cluster[, c('PYAMTW', 'yij') := NULL]

#Calculate weighted contribution
GOM.cluster[, Miu := Mi * mu]
GOM.cluster[, rhat := Miu / sumMi]
GOM.cluster[, sum.rhat := sum(rhat), by = .(Rpred, Rprey)]

#Grab unique rows
GOM.diet.survey <- unique(GOM.cluster[, list(Rpred, Rprey, sum.rhat)], by = c('Rpred', 'Rprey'))

#Convert to percentages
GOM.diet.survey[, tot.preyw := sum(sum.rhat), by = Rpred]
GOM.diet.survey[, preyper := sum.rhat / tot.preyw]
GOM.diet.survey[, c('sum.rhat', 'tot.preyw') := NULL]
setkey(GOM.diet.survey, Rpred, preyper)

#Remove OtherDemersals, SmPelagics, Illex and Loligo
#Will use EMAX diet estimates for these 4 groups
GOM.diet.survey <- GOM.diet.survey[!Rpred %in% c('OtherDemersals','SmPelagics','Loligo','Illex')]

#Load in params table with biomass as previously calculated
all.groups <- read_csv("data/GOM_Starting_Parameters.csv")
all.groups <- as.data.table(all.groups[,c(1,2,3)])

#Remove EMAX:RPATH many:1s
all.groups <- all.groups[!RPATH %in% c('Megabenthos','Macrobenthos'),]

#Change Discard to Detritus
all.groups[which(EMAX == "Discard")]$RPATH <- "Detritus"

#Calculate proportionality for EMAX:RPATH many:1s

#Megabenthos groups
#Calculate biomass remaining in Megabenthos- filterers after removing scallops
Megabenthos.filterers <- EMAX.params[model.Group == 'Megabenthos- filterers',model.Biomass] - all.groups[RPATH == 'AtlScallop',Biomass]
Megabenthos.filterers <- as.data.table(cbind('Megabenthos',Megabenthos.filterers,'Megabenthos- filterers'))
setnames(Megabenthos.filterers, c("RPATH","Biomass","EMAX"))
#Calculate biomass remaining in Megabenthos- other after removing lobster
Megabenthos.other <-EMAX.params[model.Group == 'Megabenthos- other',model.Biomass] - all.groups[RPATH == 'AmLobster',Biomass]
Megabenthos.other <- as.data.table(cbind('Megabenthos',Megabenthos.other,'Megabenthos- other'))
setnames(Megabenthos.other, c("RPATH","Biomass","EMAX"))

#Macrobenthos groups
Macrobenthos <- as.data.table(EMAX.params[model.Group %like% 'Macrobenthos',c(2,4)])
Macrobenthos <- cbind(Macrobenthos,"Macrobenthos")
setnames(Macrobenthos,c('EMAX','Biomass','RPATH'))

#Micronekton groups
Micronekton <- as.data.table(EMAX.params[model.Group %in% c('Micronekton','Larval-juv fish- all'),c(2,4)])
Micronekton <- cbind(Micronekton,"Micronekton")
setnames(Micronekton,c('EMAX','Biomass','RPATH'))

#SmPelagics groups
SmPelagics <- as.data.table(EMAX.params[model.Group =='Small Pelagics- anadromous', c(2,4)])
SmPelagics <- cbind(SmPelagics,"SmPelagics")
setnames(SmPelagics,c('EMAX','Biomass','RPATH'))

#Merge all groups
all.groups <- rbind(all.groups,Megabenthos.filterers,Megabenthos.other,Macrobenthos,Micronekton,SmPelagics)
all.groups <- all.groups[,Biomass := as.numeric(Biomass)]
all.groups[, EMAX.tot := sum(Biomass), by = EMAX]
all.groups[, Rpath.prop := Biomass / EMAX.tot]


#Whale diet from Laurel - used in Sarah Gaichas's GOM
balwhale <- data.table(EMAX = c('Large Copepods', 'Micronekton', 'Small Pelagics- commercial',
                                'Small Pelagics- other', 'Small Pelagics- squid',
                                'Demersals- benthivores', 'Demersals- omnivores',
                                'Demersals- piscivores'),
                       DC = c(0.15392800, 0.50248550, 0.15509720, 0.08101361, 
                              0.03142953, 0.01532696, 0.01102390, 0.04969525))
balwhale <- merge(balwhale, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
balwhale[, preyper := DC * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
balwhale <- balwhale[, sum(preyper), by = RPATH]
balwhale[, Rpred := 'BaleenWhales']
setnames(balwhale, c('RPATH', 'V1'), c('Rprey', 'preyper'))

#Odontocetes
tooth <- EMAX.params[, list(diet.Odontocetes,diet.Group)]
setnames(tooth,'diet.Group','EMAX')
tooth <- merge(tooth, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
tooth[, preyper := diet.Odontocetes * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
tooth <- tooth[, sum(preyper), by = RPATH]
tooth[, Rpred := 'Odontocetes']
setnames(tooth, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GOM.diet.EMAX<-rbindlist(list(balwhale,tooth))

rm(balwhale,tooth)

#SeaBirds
birds <- EMAX.params[, list(diet.Sea.Birds,diet.Group)]
setnames(birds,'diet.Group','EMAX')
birds <- merge(birds, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
birds[, preyper := diet.Sea.Birds * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
birds <- birds[, sum(preyper), by = RPATH]
birds[, Rpred := 'SeaBirds']
setnames(birds, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GOM.diet.EMAX<-rbindlist(list(GOM.diet.EMAX,birds))

rm(birds)

#Pinnipeds
seals <- EMAX.params[, list(diet.Pinnipeds,diet.Group)]
setnames(seals,'diet.Group','EMAX')
seals <- merge(seals, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
seals[, preyper := diet.Pinnipeds * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
seals <- seals[, sum(preyper), by = RPATH]
seals[, Rpred := 'Pinnipeds']
setnames(seals, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GOM.diet.EMAX<-rbindlist(list(GOM.diet.EMAX,seals))

rm(seals)

#HMS
hms <- EMAX.params[, list(diet.HMS,diet.Group)]
setnames(hms,'diet.Group','EMAX')
hms <- merge(hms, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
hms[, preyper := diet.HMS * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
hms <- hms[, sum(preyper), by = RPATH]
hms[, Rpred := 'HMS']
setnames(hms, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GOM.diet.EMAX<-rbindlist(list(GOM.diet.EMAX,hms))

rm(hms)

#Sharks
sharks <- EMAX.params[, list(diet.Sharks..pelagics,diet.Group)]
setnames(sharks,'diet.Group','EMAX')
sharks <- merge(sharks, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
sharks[, preyper := diet.Sharks..pelagics * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
sharks <- sharks[, sum(preyper), by = RPATH]
sharks[, Rpred := 'Sharks']
setnames(sharks, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GOM.diet.EMAX<-rbindlist(list(GOM.diet.EMAX,sharks))

rm(sharks)

#OtherShrimp
shrimp <- EMAX.params[, list(diet.Shrimp.et.al.,diet.Group)]
setnames(shrimp,'diet.Group','EMAX')
shrimp <- merge(shrimp, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
shrimp[, preyper := diet.Shrimp.et.al. * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
shrimp <- shrimp[, sum(preyper), by = RPATH]
others <- copy(shrimp[, Rpred := 'OtherShrimps'])
nshrimp<-copy(shrimp[,Rpred := 'NShrimp'])
setnames(others, c('RPATH', 'V1'), c('Rprey', 'preyper'))
setnames(nshrimp, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GOM.diet.EMAX<-rbindlist(list(GOM.diet.EMAX,others))
GOM.diet.EMAX<-rbindlist(list(GOM.diet.EMAX,nshrimp))

rm(others,nshrimp,shrimp)

#GelZooplankton
jelly <- EMAX.params[, list(diet.Gelatinous.Zooplankton,diet.Group)]
setnames(jelly,'diet.Group','EMAX')
jelly <- merge(jelly, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
jelly[, preyper := diet.Gelatinous.Zooplankton * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
jelly <- jelly[, sum(preyper), by = RPATH]
jelly[, Rpred := 'GelZooplankton']
setnames(jelly, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GOM.diet.EMAX<-rbindlist(list(GOM.diet.EMAX,jelly))

rm(jelly)

#Microzooplankton
micro <- EMAX.params[, list(diet.Microzooplankton,diet.Group)]
setnames(micro,'diet.Group','EMAX')
micro <- merge(micro, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
micro[, preyper := diet.Microzooplankton * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
micro <- micro[, sum(preyper), by = RPATH]
micro[, Rpred := 'Microzooplankton']
setnames(micro, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GOM.diet.EMAX<-rbindlist(list(GOM.diet.EMAX,micro))

rm(micro)

#Micronekton
micronekton <- EMAX.params[, list(diet.Micronekton,diet.Group)]
setnames(micronekton,'diet.Group','EMAX')
micronekton <- merge(micronekton, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
micronekton[, preyper := diet.Micronekton * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
micronekton <- micronekton[, sum(preyper), by = RPATH]
micronekton[, Rpred := 'Micronekton']
setnames(micronekton, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GOM.diet.EMAX<-rbindlist(list(GOM.diet.EMAX,micronekton))

rm(micronekton)

#OtherPelagics-- going to try to use diet from food habits database for initial balancing, rather than EMAX

#OtherDemersals
otherdem <- EMAX.params[, list(diet.Demersals..benthivores,diet.Group)]
setnames(otherdem,'diet.Group','EMAX')
otherdem <- merge(otherdem, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
otherdem[, preyper := diet.Demersals..benthivores * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
otherdem <- otherdem[, sum(preyper), by = RPATH]
otherdem[, Rpred := 'OtherDemersals']
setnames(otherdem, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GOM.diet.EMAX<-rbindlist(list(GOM.diet.EMAX,otherdem))

rm(otherdem)

#Lobster - use Megabenthos- other diet
lobster <- EMAX.params[, list(diet.Megabenthos..other,diet.Group)]
setnames(lobster,'diet.Group','EMAX')
lobster <- merge(lobster, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
lobster[, preyper := diet.Megabenthos..other * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
lobster <- lobster[, sum(preyper), by = RPATH]
lobster[, Rpred := 'AmLobster']
setnames(lobster, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GOM.diet.EMAX<-rbindlist(list(GOM.diet.EMAX,lobster))

rm(lobster)

#AtlScallop - use Megabenthos- filterers diet
scallop <- EMAX.params[, list(diet.Megabenthos..filterers,diet.Group)]
setnames(scallop,'diet.Group','EMAX')
scallop <- merge(scallop, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
scallop[, preyper := diet.Megabenthos..filterers * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
scallop <- scallop[, sum(preyper), by = RPATH]
scallop[, Rpred := 'AtlScallop']
setnames(scallop, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GOM.diet.EMAX<-rbindlist(list(GOM.diet.EMAX,scallop))

rm(scallop)

#OtherCephalopods - use EMAX squid diet
ceph <- EMAX.params[, list(diet.Small.Pelagics..squid,diet.Group)]
setnames(ceph,'diet.Group','EMAX')
ceph <- merge(ceph, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
ceph[, preyper := diet.Small.Pelagics..squid * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
ceph <- ceph[, sum(preyper), by = RPATH]
ceph[,Rpred := 'OtherCephalopods']
setnames(ceph, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GOM.diet.EMAX<-rbindlist(list(GOM.diet.EMAX,ceph))

rm(ceph)

#Illex - use EMAX squid diet
illex <- EMAX.params[, list(diet.Small.Pelagics..squid,diet.Group)]
setnames(illex,'diet.Group','EMAX')
illex <- merge(illex, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
illex[, preyper := diet.Small.Pelagics..squid * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
illex <- illex[, sum(preyper), by = RPATH]
illex[,Rpred := 'Illex']
setnames(illex, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GOM.diet.EMAX<-rbindlist(list(GOM.diet.EMAX,illex))

rm(illex)

#Loligo - use EMAX squid diet
loligo <- EMAX.params[, list(diet.Small.Pelagics..squid,diet.Group)]
setnames(loligo,'diet.Group','EMAX')
loligo <- merge(loligo, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
loligo[, preyper := diet.Small.Pelagics..squid * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
loligo <- loligo[, sum(preyper), by = RPATH]
loligo[,Rpred := 'Loligo']
setnames(loligo, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GOM.diet.EMAX<-rbindlist(list(GOM.diet.EMAX,loligo))

rm(loligo)

#SmCopepods
smcope <- EMAX.params[, list(diet.Small.copepods,diet.Group)]
setnames(smcope,'diet.Group','EMAX')
smcope <- merge(smcope, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
smcope[, preyper := diet.Small.copepods * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
smcope <- smcope[, sum(preyper), by = RPATH]
smcope[,Rpred := 'SmCopepods']
setnames(smcope, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GOM.diet.EMAX<-rbindlist(list(GOM.diet.EMAX,smcope))

rm(smcope)

#LgCopepods
lgcope <- EMAX.params[, list(diet.Large.Copepods,diet.Group)]
setnames(lgcope,'diet.Group','EMAX')
lgcope <- merge(lgcope, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
lgcope[, preyper := diet.Large.Copepods * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
lgcope <- lgcope[, sum(preyper), by = RPATH]
lgcope[,Rpred := 'LgCopepods']
setnames(lgcope, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GOM.diet.EMAX<-rbindlist(list(GOM.diet.EMAX,lgcope))

rm(lgcope)

#SmPelagics - use diet from Small Pelagics- other
smpel <- EMAX.params[, list(diet.Small.Pelagics..other,diet.Group)]
setnames(smpel,'diet.Group','EMAX')
smpel <- merge(smpel, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
smpel[, preyper := diet.Small.Pelagics..other * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
smpel <- smpel[, sum(preyper), by = RPATH]
smpel[,Rpred := 'SmPelagics']
setnames(smpel, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GOM.diet.EMAX<-rbindlist(list(GOM.diet.EMAX,smpel))

rm(smpel)

#Bacteria
bacteria <- EMAX.params[, list(diet.Bacteria,diet.Group)]
setnames(bacteria,'diet.Group','EMAX')
bacteria <- merge(bacteria, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
bacteria[, preyper := diet.Bacteria * Rpath.prop]
bacteria <- bacteria[, sum(preyper), by = RPATH]
bacteria[,Rpred := 'Bacteria']
setnames(bacteria, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GOM.diet.EMAX<-rbindlist(list(GOM.diet.EMAX,bacteria))

rm(bacteria)

#Macrobenthos - need to merge multiple EMAX groups
Macrobenthos<-Macrobenthos[,macro.prop := Biomass/sum(Biomass)]
poly<-EMAX.params[, list(diet.Macrobenthos..polychaetes,diet.Group)]
setnames(poly, 'diet.Macrobenthos..polychaetes','DC')
poly<-poly[,macro.prop := Macrobenthos[EMAX == 'Macrobenthos- polychaetes',macro.prop]]
crus<-EMAX.params[, list(diet.Macrobenthos..crustaceans,diet.Group)]
setnames(crus, 'diet.Macrobenthos..crustaceans','DC')
crus<-crus[,macro.prop := Macrobenthos[EMAX == 'Macrobenthos- crustaceans',macro.prop]]
moll<-EMAX.params[, list(diet.Macrobenthos..molluscs,diet.Group)]
setnames(moll, 'diet.Macrobenthos..molluscs','DC')
moll<-moll[,macro.prop := Macrobenthos[EMAX == 'Macrobenthos- molluscs',macro.prop]]
oth<-EMAX.params[, list(diet.Macrobenthos..other,diet.Group)]
setnames(oth, 'diet.Macrobenthos..other','DC')
oth<-oth[,macro.prop := Macrobenthos[EMAX == 'Macrobenthos- other',macro.prop]]

#Combine all EMAX groups into one
combo <- rbindlist(list(poly, crus, moll, oth))
combo[, newDC := DC * macro.prop]
combo<-(combo[,sumnewDC := sum(newDC), by = 'diet.Group'])
combo<-unique(combo[,c('diet.Group','sumnewDC')])
setnames(combo, 'diet.Group','EMAX')
combo <- merge(combo, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
combo[, preyper := sumnewDC * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
combo <- combo[, sum(preyper), by = RPATH]
setnames(combo, c('RPATH','V1'),c('Rprey','preyper'))
combo[, Rpred := 'Macrobenthos']

GOM.diet.EMAX<-rbindlist(list(GOM.diet.EMAX,combo))

rm(poly,crus,moll,combo,oth)

#Megabenthos - need to merge multiple EMAX groups
Megabenthos<-all.groups[RPATH == 'Megabenthos',]
Megabenthos<-Megabenthos[,mega.prop := Biomass/sum(Biomass)]
filter<- EMAX.params[, list(diet.Megabenthos..filterers,diet.Group)]
setnames(filter, 'diet.Megabenthos..filterers','DC')
filter<-filter[,mega.prop := Megabenthos[EMAX == 'Megabenthos- filterers',mega.prop]]
othermega<-EMAX.params[, list(diet.Megabenthos..other,diet.Group)]
setnames(othermega, 'diet.Megabenthos..other','DC')
othermega<-othermega[,mega.prop := Megabenthos[EMAX == 'Megabenthos- other',mega.prop]]

#Combine all EMAX groups into one
combomega <- rbindlist(list(filter, othermega))
combomega[, newDC := DC * mega.prop]
combomega<-(combomega[,sumnewDC := sum(newDC), by = 'diet.Group'])
combomega<-unique(combomega[,c('diet.Group','sumnewDC')])
setnames(combomega, 'diet.Group','EMAX')
combomega <- merge(combomega, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
combomega[, preyper := sumnewDC * Rpath.prop]

#Need to sum many:1 EMAX:Rpath
combomega <- combomega[, sum(preyper), by = RPATH]
setnames(combomega, c('RPATH','V1'),c('Rprey','preyper'))
combomega[, Rpred := 'Megabenthos']

GOM.diet.EMAX<-rbindlist(list(GOM.diet.EMAX,combomega))

rm(filter,othermega,combomega)

rm(all.groups,EMAX.params,GOM.cluster,GOM.fh,GOM.fh2,GOM.groups,GOM.pred,Macrobenthos,Megabenthos,Megabenthos.filterers,
   Megabenthos.other,Micronekton,prey,SmPelagics)

#Merge diet.survey with diet.EMAX
GOM.diet <- rbindlist(list(GOM.diet.survey, GOM.diet.EMAX), use.names = T)

rm(GOM.diet.EMAX,GOM.diet.survey)
