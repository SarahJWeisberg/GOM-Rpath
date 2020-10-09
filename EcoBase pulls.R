#Code from Ecobase.ecopath.org to access Ecobase
#To get the list of available Ewe models
library(RCurl); library(XML); library(plyr); library(data.table)

#To obtain the list of available model
h=basicTextGatherer()
curlPerform(url = 'http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client_3.php',writefunction=h$update)

data<-xmlTreeParse(h$value(),useInternalNodes=TRUE)
liste_mod <- as.data.table(ldply(xmlToList(data),data.frame))

#models <- liste_mod[, list(model.model_number, model.model_name, model.country, model.ecosystem_type)]
#shelf.mods <- models[model.ecosystem_type %in% c('continental shelf', 'Continental shelf'), ]
Maine.mod<-liste_mod[model.model_name %like% 'Maine']

all.mod <- c()
for(imod in 1:nrow(Maine.mod)){
  mod.num <- as.numeric(as.character(Maine.mod[imod, model.model_number]))
  h <- basicTextGatherer()
  curlPerform(url = paste0('http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client_output.php?no_model=',
                           mod.num), writefunction = h$update, verbose = T)
  data <- xmlTreeParse(h$value(), useInternalNodes = T)
  model1 <- xpathSApply(data, '//group', xmlToList)
  if(length(model1 > 0)){
    ind.mod <- c()
    for(igroup in 1:ncol(model1)){
      sp.mod <- data.table(Group   = model1[['group_name', igroup]],
                           TL      = as.numeric(model1[['tl', igroup]]),
                           Biomass = as.numeric(model1[['biomass', igroup]]),
                           PB      = as.numeric(model1[['pb', igroup]]),
                           QB      = as.numeric(model1[['qb', igroup]]))
      ind.mod <- rbindlist(list(ind.mod, sp.mod))
    }
    ind.mod[, ModNum := mod.num]
    all.mod <- rbindlist(list(all.mod, ind.mod))
  }
}

#Double check PBs
all.mod[TL == 1 & PB > 0, mean(PB)]
all.mod[Group == 'Bacteria', mean(PB)]
all.mod[Group == 'Bacteria', mean(QB)]
all.mod[Group %like% 'Gel', mean(PB)]
all.mod[Group %like% 'Gel', mean(QB)]
all.mod[!Group %like% 'Gel' & Group %like% 'zoo', mean(PB)]
all.mod[!Group %like% 'Gel' & Group %like% 'zoo', mean(QB)]

all.mod[Group %like% 'Hake',]

all.mod[TL > 1 & TL <=2, mean(PB)]
all.mod[TL > 2 & TL <=3, mean(PB)]
all.mod[TL > 3 & TL <=4, mean(PB)]
all.mod[TL > 4, mean(PB)]

all.mod[TL > 1 & TL <=2, mean(QB)]
all.mod[TL > 2 & TL <=3, mean(QB)]
all.mod[TL > 3 & TL <=4, mean(QB)]
all.mod[TL > 4, mean(QB)]


##Pull EMAX only
#models <- liste_mod[, list(model.model_number, model.model_name, model.country, model.ecosystem_type)]
#shelf.mods <- models[model.ecosystem_type %in% c('continental shelf', 'Continental shelf'), ]
GOM.EMAX<-liste_mod[model.model_number == 704]

for(imod in 1:nrow(GOM.EMAX)){
  mod.num <- as.numeric(as.character(GOM.EMAX[imod, model.model_number]))
  h <- basicTextGatherer()
  curlPerform(url = paste0('http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client_output.php?no_model=',
                           mod.num), writefunction = h$update, verbose = T)
  data <- xmlTreeParse(h$value(), useInternalNodes = T)
  model1 <- xpathSApply(data, '//group', xmlToList)
  if(length(model1 > 0)){
    ind.mod <- c()
    for(igroup in 1:ncol(model1)){
      sp.mod <- data.table(Group   = model1[['group_name', igroup]],
                           TL      = as.numeric(model1[['tl', igroup]]),
                           Biomass = as.numeric(model1[['biomass', igroup]]),
                           PB      = as.numeric(model1[['pb', igroup]]),
                           QB      = as.numeric(model1[['qb', igroup]]))
      ind.mod <- rbindlist(list(ind.mod, sp.mod))
    }
    ind.mod[, ModNum := mod.num]
    GOM.EMAX <- ind.mod
  }
}

##Pull Heymans (2001) model only

GOM.Heymans<-liste_mod[model.model_number == 444]

for(imod in 1:nrow(GOM.Heymans)){
  mod.num <- as.numeric(as.character(GOM.Heymans[imod, model.model_number]))
  h <- basicTextGatherer()
  curlPerform(url = paste0('http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client_output.php?no_model=',
                           mod.num), writefunction = h$update, verbose = T)
  data <- xmlTreeParse(h$value(), useInternalNodes = T)
  model1 <- xpathSApply(data, '//group', xmlToList)
  if(length(model1 > 0)){
    ind.mod <- c()
    for(igroup in 1:ncol(model1)){
      sp.mod <- data.table(Group   = model1[['group_name', igroup]],
                           TL      = as.numeric(model1[['tl', igroup]]),
                           Biomass = as.numeric(model1[['biomass', igroup]]),
                           PB      = as.numeric(model1[['pb', igroup]]),
                           QB      = as.numeric(model1[['qb', igroup]]))
      ind.mod <- rbindlist(list(ind.mod, sp.mod))
    }
    ind.mod[, ModNum := mod.num]
    GOM.Heymans <- ind.mod
  }
}

write.csv(GOM.Heymans,'Heymans_GOM.csv')





