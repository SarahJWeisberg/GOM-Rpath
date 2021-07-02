#Title: GOM Rpath Discards estimates

# Purpose: To estimate discards (t/km^2) for functional groups in GOM RPATH model.
#       Based on 
#       https://github.com/NOAA-EDAB/GBRpath

# DataFiles:'observer_data.RData';'mean_landings_gom_80_85.RData'

# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu

# Last modified: # Fri Jul  2 09:58:12 2021 ------------------------------

library(here);library(tidyr);library(data.table)

#Load landings data
load("~/Desktop/GOM-Rpath/data/mean_landings_gom_80_85.RData")

#Load 


#Load GOM groups
source('R/Groups.R')
