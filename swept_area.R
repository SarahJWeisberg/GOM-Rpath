# Calculate mean stratified biomass to identify most relevant species
# Using code from https://github.com/slucey/RSurvey/blob/master/survey_template.R

#-------------------------------------------------------------------------------
#Required packages
#May need to download Survdat package from GitHub
devtools::install_github('slucey/RSurvey/Survdat')
library(data.table); library(rgdal); library(Survdat)

#-------------------------------------------------------------------------------
#User created functions

#-------------------------------------------------------------------------------
setwd("~/Desktop/RPath-GOM")

#Grab survdat.r
#load(file.path(data.dir, 'Survdat.RData'))

#Grab strata
strat.area<- readOGR('/Users/sjw/Desktop/RPath-GOM/speciescodesandstrata','strata')

#Sweaptarea.r v.1.1
#Generate swept area biomass estimate from survey data
#3/14
#v 1.1 - forced renaming of both group and q columns in q variable
#SML

#sweptarea <- function (prestrat.x, stratmean.x, q = NULL, a = 0.0384, strat.col, area.col, group.col = 'SVSPP') {
  #prestrat.x  <- data table output from prestrat
  #stratmean.x <- data table output from stratmean
  #q           <- data table of catchability coefficients (groups, q)
  #a           <- swept area (0.0384 km^2 is a standard Albatross IV tow)
  #strat.col   <- column of prestrat.x with the stratum names
  #area.col    <- column of prestrat.x with the stratum areas
  #group.col   <- column of stratmean.x which the mean is based on (i.e. svspp)
  
  #-------------------------------------------------------------------------------
  #Required packages
  library(data.table)
  
  #-------------------------------------------------------------------------------
  #User created functions
  
  #-------------------------------------------------------------------------------
  #Define a
  a <- 0.0384
  
  #This is necessary to break the link with the original data table
prestrat.x  <- copy(GOM.prep2)
stratmean.x <- copy(GOM.strat.mean)
  
  #Calculate A (Total area)
 #setnames(GOM.prep2, c(strat.col, area.col),c('STRAT', 'S.AREA'))
  
  setkey(prestrat.x, YEAR, STRATUM)
  stratum <- unique(prestrat.x)
  stratum <- stratum[, sum(Area, na.rm = T), by = 'YEAR']
  setnames(stratum, "V1", "A")
  
  #Merge A
  swept.area <- merge(stratmean.x, stratum, by = 'YEAR')
  
  #need to make new df with RPATH, Fall.q
  q<-subset(spp2, select = c('RPATH','Fall.q'))
  q<-q[!duplicated(q$RPATH),]
  
  #Merge q
  swept.area<-merge(swept.area,q,by = 'RPATH')
  
  #Merge q
  #if(is.null(q)) q <- data.table(groups = unique(swept.area[, get(group.col)]), q = 1)
 # setnames(q, names(q), c(group.col, 'q'))
 # swept.area <- merge(swept.area, q, by = group.col)
  
  #total area
  GOM.strat.area <- strat.area[STRATUM %in% c(1220, 1240, 1260:1290, 1360:1400, 3560:3830), sum(Area)]
  
  #Calculate swept area biomass
  swept.area[, Tot.biomass   :=       (strat.biomass * A/a)/Fall.q]
  swept.area[, biomass.k_area   :=       (strat.biomass * A/a)/(Fall.q*GOM.strat.area)]
  swept.area[, biomass.t_area   :=       ((strat.biomass * A/a)*.001)/(Fall.q*GOM.strat.area)]

  
  
  GOM.biomass.80s <- swept.area[YEAR %in% 1980:1985, mean(biomass.t_area), by = RPATH]
  setnames( GOM.biomass.80s, "V1", "Avg Biomass")
  
  
  

