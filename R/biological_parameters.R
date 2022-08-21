#Title: GOM Rpath Biological Parameters

# Purpose: This script pulls biological parameters from a database of
#       starting values. These values are based on previously published models
#       including EMAX (Link et al., 2006), Georges Bank Rpath, NWACS, Heymans (2001), 
#       Dias et al. (2019), Bundy (?) and Fishbase

# DataFile:'GOM_Starting_Parameters.csv'

# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu

xfun::session_info()
# Sun Aug 21 15:05:25 2022 ------------------------------


#Load required packages
library(here);library(data.table);library(dplyr);library(tidyverse)

#Load all found parameter values
params_start<-read.csv('data/GOM_Starting_Parameters.csv')

#Trim to select needed columns only
params<-params_start[,c("RPATH","PB","QB")]

#Remove mismatched rows
#Merge
params<-filter(params,RPATH %in% GOM.groups$RPATH)
params<-left_join(GOM.groups,params,by="RPATH")

#Make vector of all PB values
PB<-params[,"PB"]

#Make vector of all QB values
QB<-params[,"QB"]

rm(params, params_start)
