#Pull commercial data
#This require you to be behind NEFSC firewalls
#See Sean Lucey for access
library(here); library(data.table); library(comlandr)

#Connect to NEFSC server
channel <- dbutils::connect_to_database(server = "sole", uid = "slucey")

path.yrs <- 1980:1985

comland <- comlandr::get_comland_data(channel, filterByYear = path.yrs, useForeign = F)

#Assigned area to GOM
#Will be using NAFO Stat Areas - 500, 510, 512, 513, 514, 515, 521, 522, 561
#Several stat areas are completely within the GOM EPU
full.areas <- c(500, 510, 512, 513, 514, 515)

landings <- comland$comland[AREA %in% full.areas, ]

#Areas 521, 522, and 561 straddle Georges Bank.  The proprotion of catch inside
#and outside was calulated from the MS Keyrun project for Georges Bank.  We will use
#the proportion outside of Georges Bank for those Stat Areas.  The rest will be 
#100% GOM
proportions <- readRDS(here('data/All_Species_Proportions.rds')) 
proportions <- proportions[AREA %in% c(521, 522, 561) & InOut == 'out', ]  

gom.split.land <- assign_area(comland, proportions, areaDescription = 'InOut', 
                              propDescription = 'MeanProp')
gom.split.land$comland[, InOut := NULL]

#Merge split landings with other landings
landings <- rbindlist(list(landings, gom.split.land$comlands))

#Assign gears to fleets
#Generate NEGEAR2 codes from NEGEAR
landings[NEGEAR < 100, NEGEAR3 := paste0(0, NEGEAR)]
landings[NEGEAR >= 100, NEGEAR3 := NEGEAR]
landings[, NEGEAR2 := as.numeric(substr(NEGEAR3, 1, 2))]

landings[NEGEAR2 %in% c(5, 16, 32, 35, 36), FLEET := 'LG Mesh']
landings[NEGEAR2 == 5 & MESHCAT == 'SM', FLEET := 'SM Mesh']
landings[NEGEAR2 %in% c(1, 2, 8, 10, 50, 52, 14, 26), FLEET := 'Fixed Gear']
landings[NEGEAR2 %in% c(12, 17, 37), FLEET := 'Pelagic']
landings[NEGEAR2 %in% c(18, 15, 19, 20, 21, 23, 30, 33, 53), FLEET := 'Trap']
landings[NEGEAR2 == 13, FLEET := 'Scallop Dredge']
landings[NEGEAR2 == 40, FLEET := 'Clam Dredge']
landings[NEGEAR2 %in% c(22, 25, 38, 41), FLEET := 'Other Dredge']
landings[NEGEAR2 %in% c(3, 4, 6, 11), FLEET := 'HMS']
landings[is.na(FLEET), FLEET := 'Other']
landings[, FLEET := as.factor(FLEET)]

#Calculate mean for Rpath
sum.land <- landings[, sum(SPPLIVMT), by = c('YEAR', 'NESPP3', 'FLEET')]
mean.land <- sum.land[, mean(V1), by = c('NESPP3', 'FLEET')]
setnames(mean.land, 'V1', 'SPPLIVMT')

save(mean.land, file = here('data/mean_landings_gom_80_85.RData'))
