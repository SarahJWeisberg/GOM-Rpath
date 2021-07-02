#Title: GOM Rpath Biomass Accumulation

# Purpose: This script generates estimates of biomass accumulation for 
#       the single-species functional groups used in the GOM-Rpath model.
#       Data are sourced from the NEFSC bottom trawl. Model is built
#       to be analogous to Georges Bank Rpath model
#       https://github.com/NOAA-EDAB/GBRpath

# DataFile:'NEFSC_BTS_2021_all_seasons.RData'

# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu

xfun::session_info()

# Last modified Tue Jun 15 17:26:44 2021 -----------------------------

#Load needed packages
library(here);library(data.table)

#Run survey biomass estimate code
source('R/survey_biomass_estimates.R')

# Pull out Rpath groups from survey data
survey.groups <- as.data.table(unique(swept$RPATH))
colnames(survey.groups)<-"RPATH"
survey.groups<-na.omit(survey.groups)

#Remove units
swept$Biomass<-as.numeric(swept$Biomass)

#Fit linear model to biomass trends all surveyed RPATH groups, extract slope
ba<-c()
p<-c()
for (i in 1:length(survey.groups$RPATH)){
  lm<-lm(Biomass~YEAR, data = subset(swept, RPATH == survey.groups$RPATH[i]))
  spF <- as.numeric(summary(lm)$fstatistic)
  p[i] <- pf(spF[1], spF[2], spF[3], lower = F)
  ba[i] <-as.numeric(lm[[1]][2])
}

biomass.accum<-cbind(survey.groups,ba,p)
biomass.accum<-subset(biomass.accum, p<=0.05 & abs(ba) >= 0.005)
biomass.accum<-biomass.accum[,-3]


