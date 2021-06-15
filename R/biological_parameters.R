#Title: GOM Rpath Biological Parameters

# Purpose: This script pulls biological parameters from a database of
#       starting values. These values are based on previously published models
#       including EMAX, Georges Bank Rpath, NWACS, Heymans (2001), Dias et al. (2019)
#       Bundy (?) and Fishbase

# DataFile:'GOM_Starting_Parameters.csv'

# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu

xfun::session_info()
# Tue Jun 15 14:54:44 2021 ------------------------------

#Load required packages
library(here)

#Load all found parameter values
params_start<-read.csv('data/GOM_Starting_Parameters.csv')

#Trim to select needed columns only
params<-params_start[,c("RPATH","PB","QB")]

#Make vector of all PB values
PB<-params[,"PB"]

#Make vector of all QB values
QB<-params[,"QB"]
