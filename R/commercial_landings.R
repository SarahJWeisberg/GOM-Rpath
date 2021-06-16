#Pull commercial data
#This require you to be behind NEFSC firewalls
#See Sean Lucey for access
library(here); library(data.table); library(comlandr)

#Connect to NEFSC server
channel <- dbutils::connect_to_database(server = "sole", uid = "slucey")

path.yrs <- 1981:1985

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

#Merge split landings with other landings
landings <- rbindlist(list(landings, gom.split.land$comlands))

#Assign gears to fleets


