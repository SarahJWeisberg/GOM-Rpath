## Import data
library(readr); library(data.table); library(rgdal)
prey <- as.data.table(read_csv("SASPREY12B.csv"))

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
prey[is.na(RPATH) & Collcom == 'COPEPODA', RPATH := 'Mesozooplankton']
prey[is.na(RPATH) & MODCAT == 'PELINV', RPATH := 'Micronekton']

#MODCAT LDEM
prey[is.na(RPATH) & ANALCAT %in% c('BOTFAM', 'SOLFAM'), RPATH := 'SmFlatfishes']
prey[is.na(RPATH) & ANALCAT == 'RAJORD', RPATH := 'OtherSkates']
prey[is.na(RPATH) & ANALCAT %in% c('LUTFAM', 'SCAFAM', 'SCIFAM', 'SPAFAM', 'SERFA3'), RPATH := 'SouthernDemersals']
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
#Note - RiverHerring biomass was not present enough on Georges Bank during 2012-2016
#They are included as other pelagics here
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
prey[PYABBR %in% c('AVES', 'AVEFEA'), RPATH := 'Seabirds']
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

#load stomach data
load("~/Desktop/GOM-Rpath/GOM_foodhabits.RData")

#Load strata
#strata <- readOGR('speciescodesandstrata', 'strata')

#Load species code list
load("speciescodesandstrata/Species_codes.RData")

#Reassign groups
spp <- spp[!duplicated(spp$SVSPP),]
spp <- spp[RPATH == 'RedCrab', RPATH := 'Macrobenthos']
spp <- spp[RPATH == 'AtlScallop', RPATH := 'Megabenthos']
spp <- spp[RPATH == 'Clams', RPATH := 'Megabenthos']
spp <- spp[RPATH == 'Scup', RPATH := 'OtherDemersals']
spp <- spp[RPATH == 'OffHake', RPATH := 'OtherDemersals']
spp <- spp[RPATH == 'Bluefish', RPATH := 'OtherPelagics']
spp <- spp[RPATH == 'AmShad', RPATH := 'SmPelagics']
spp <- spp[SCINAME == 'BROSME BROSME', RPATH := 'Cusk']
spp <- spp[RPATH == 'SmallPelagics', RPATH := 'SmPelagics']
spp <- spp[RPATH == 'AtlCroaker', RPATH := 'SouthernDemersals']
spp <- spp[RPATH == 'LargePelagics', RPATH := 'OtherPelagics']
spp <- spp[RPATH == 'OtherFlatfish', RPATH := 'OtherDemersals']
spp <- spp[RPATH == 'StripedBass', RPATH := 'OtherPelagics']
spp <- spp[RPATH == 'Sturgeon', RPATH := 'OtherDemersals']
spp <- spp[RPATH == 'Weakfish', RPATH := 'OtherDemersals']

#Assign Rpath codes to predators
GOM.fh <- merge(GOM.fh, spp, by = 'SVSPP', all.x = T)
setnames(GOM.fh, 'RPATH', 'Rpred')

#Assign Rpath codes to prey
GOM.fh <- merge(GOM.fh, prey[, list(PYNAM, RPATH)], by = 'PYNAM', all.x = T)
setnames(GOM.fh, 'RPATH', 'Rprey')

#Remove NotUsed, AR, UNKFish and UNKSkate - Talk to Sarah about how to deal with these
GOM.fh <- GOM.fh[!Rprey %in% c('NotUsed', 'AR', 'UNKFish', 'UNKSkate'), ]

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

#Extract groups not in survey
EMAX.groups<-GOM.groups[!RPATH %in% GOM.diet.survey$Rpred,]

#Calculate proportionality for EMAX:RPATH many:1s
EMAX.params <- as.data.table(read_csv("GOM_EMAX_params.csv"))
Megabenthos.filterers <- EMAX.params[model.Group == 'Megabenthos- filterers',model.Biomass] - all.groups[RPATH == 'AtlScallop',Biomass]
Megabenthos.filterers <- as.data.table(cbind('Megabenthos',Megabenthos.filterers,'Megabenthos- filterers'))
setnames(Megabenthos.filterers, c("RPATH","Biomass","EMAX"))
Megabenthos.other <-EMAX.params[model.Group == 'Megabenthos- other',model.Biomass] - all.groups[RPATH == 'AmLobster',Biomass]
Megabenthos.other <- as.data.table(cbind('Megabenthos',Megabenthos.other,'Megabenthos- other'))
setnames(Megabenthos.other, c("RPATH","Biomass","EMAX"))
Macrobenthos <- as.data.table(EMAX.params[model.Group %like% 'Macrobenthos',c(2,4)])
Macrobenthos <- cbind(Macrobenthos,"Macrobenthos")
setnames(Macrobenthos,c('EMAX','Biomass','RPATH'))

#Load in params table with biomass as previously calculated
all.groups <- read_csv("GOM_Parameters_V4.csv")
all.groups <- as.data.table(all.groups[,c(1,2,3)])
all.groups <- all.groups[!RPATH %in% c('Megabenthos','Macrobenthos'),]
all.groups <- rbind(all.groups,Megabenthos.filterers,Megabenthos.other,Macrobenthos)
all.groups <- all.groups[,Biomass := as.numeric(Biomass)]

all.groups[, EMAX.tot := sum(Biomass), by = EMAX]
all.groups[, Rpath.prop := Biomass / EMAX.tot]

#Whale diet from Laurel/new bird data - used in Sarah's GOM
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

#OtherPelagics
otherpel <- EMAX.params[, list(diet.Medium.Pelagics...piscivores...other.,diet.Group)]
setnames(otherpel,'diet.Group','EMAX')
otherpel <- merge(otherpel, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
otherpel[, preyper := diet.Medium.Pelagics...piscivores...other. * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
otherpel <- otherpel[, sum(preyper), by = RPATH]
otherpel[, Rpred := 'OtherPelagics']
setnames(otherpel, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GOM.diet.EMAX<-rbindlist(list(GOM.diet.EMAX,otherpel))

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

#OtherCephalopods
ceph <- EMAX.params[, list(diet.Small.Pelagics..squid,diet.Group)]
setnames(ceph,'diet.Group','EMAX')
ceph <- merge(ceph, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
ceph[, preyper := diet.Small.Pelagics..squid * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
ceph <- ceph[, sum(preyper), by = RPATH]
ceph[,Rpred := 'OtherCephalopods']
setnames(ceph, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GOM.diet.EMAX<-rbindlist(list(GOM.diet.EMAX,ceph))

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


