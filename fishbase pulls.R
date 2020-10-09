##Pull biological parameters from Fishbase
#Required packages
devtools::install_github("ropensci/rfishbase")
library(rfishbase); library(data.table)

load("Species_codes.Rdata")

#User functions-----------------------------------------------------------------
rightcase <- function(x){
  out <- c()
  for(i in 1:length(x)){
    first <- toupper(substring(x[i], 1, 1))
    rest  <- tolower(substring(x[i], 2, nchar(x[i])))
    new   <- paste0(first, rest)
    out   <- c(out, new)
  }
  return(out)
}

fixspace <- function(x){
  for(i in 1:length(x)){
    last <- substring(x[i], nchar(x[i]), nchar(x[i]))
    if(last == ' ') x[i] <- substring(x[i], 1, nchar(x[i]) - 1)
  }
  return(x)
}


#Fill-in groups not in Buchheister or EMAX
GOM.fish <- unique(spp[RPATH %in% c('Goosefish', 'OffHake', 'SilverHake', 'RedHake', 
                                       'WhiteHake', 'Redfish', 'Pollock', 'OceanPout', 
                                       'BlackSeaBass', 'Scup', 'Fourspot','AmPlaice', 
                                       'Windowpane', 'WinterFlounder', 'WitchFlounder', 
                                       'SmoothDogfish', 'Barndoor', 'WinterSkate',
                                       'LittleSkate','Cusk','NShrimp','RiverHerring'), list(RPATH, SCINAME)], by = 'SCINAME')
CUSK <-spp[COMNAME=='CUSK', list(COMNAME,SCINAME), by = 'SCINAME']
#Fix scinames with extra space
GOM.fish[, SCINAME := fixspace(as.character(SCINAME))]
CUSK[, SCINAME := fixspace(as.character(SCINAME))]
#Fix names for r package
GOM.fish[, Sciname := rightcase(as.character(SCINAME))]
CUSK[, Sciname := rightcase(as.character(SCINAME))]
#Prime rfishbase
fishbase <- load_taxa(server = "fishbase")

#Validate names
fish <- validate_names(GOM.fish[, Sciname])
cusk<-validate_names(CUSK[, Sciname])
#Query the data base
pb.fishbase <- as.data.table(species(fish, fields = c('Species', 'LongevityWild')))
pb.fishbase <- pb.fishbase[, LongevityWild := as.numeric(LongevityWild)]
pb.fishbase[, PB := 1/LongevityWild]
qb.fishbase <- as.data.table(popqb(fish, fields = c('Species', 'PopQB')))
qb.fishbase <- qb.fishbase[, mean(PopQB), by = 'Species']
qb.cusk <-as.data.table(popqb(cusk, fields = c('Species', 'PopQB')))
setnames(qb.fishbase, 'V1', 'QB')

fish.params <- merge(pb.fishbase, qb.fishbase, by = 'Species', all = T)
fish.params[, 'LongevityWild' := NULL]

#Add RPATH code back on
fish.params[, SCINAME := toupper(Species)]
fish.params <- merge(fish.params, unique(spp[, list(RPATH, SCINAME)]), by = 'SCINAME',
                     all.x = T)
#There are a couple species with extra spaces at the end...need to fix this
fish.params[SCINAME %like% 'PSEUDOPLEURO',  RPATH := 'WinterFlounder']
fish.params[SCINAME %like% 'HIPPOGLOSSINA', RPATH := 'Fourspot']
fish.params[, c('SCINAME', 'Species') := NULL]

#Use EMAX values for aggregate group if still missing
fish.params <- merge(fish.params, unique(species[, list(RPATH, EMAX)]), by = 'RPATH')
EMAX <- data.table(agg = c('Demersals- benthivores', 'Demersals- piscivores',
                           'Demersals- omnivores'),
                   PB.agg = 0.45, QB.agg = c(0.92, 2.44, 0.83))
fish.params[is.na(QB) & EMAX %like% 'Benth', QB := 0.92]
fish.params[is.na(QB) & EMAX %like% 'Pisc',  QB := 2.44]
fish.params[is.na(QB) & EMAX %like% 'Omni',  QB := 0.83]
fish.params[is.na(PB), PB := 0.45]
fish.params[, EMAX := NULL]