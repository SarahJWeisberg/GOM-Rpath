#Pull commercial data
#This require you to be behind NEFSC firewalls
#See Sean Lucey for access
library(here); library(data.table); library(comlandr)

#Connect to NEFSC server
channel <- dbutils::connect_to_database(server = "sole", uid = "slucey")

path.yrs <- 1981:1985

comland <- comlandr::get_comland_data(channel, filterByYear = path.yrs, useForeign = F)

#Assigned area to GOM
#Will be using NAFO Stat Areas - 
#Areas 521, 522, and 561 straddle Georges Bank.  The proprotion of catch inside
#and outside was calulated from the MS Keyrun project for Georges Bank.  We will use
#the proportion outside of Georges Bank for those Stat Areas.  The rest will be 
#100% GOM

proportions <- readRDS(here('data/All_Species_Proportions.rds')) 
proportions <- proportions[AREA %in% c(521, 522, 561) & InOut == 'out', ]  


comland <- assign_area(comland, userAreas, areaDescription = 'EPU', 
                       propDescription = 'Prop')
