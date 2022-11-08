# Title: Rsim to Rpath conversion

# Purpose: This script converts Rsim parameters to Rpath ones. This is useful
#           for running Ecological Network Analysis (ENA) analyses on Ecosense
#           results.

# DataFiles:"GOM_sense_50k.RData"

# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu

# Tue Nov  1 16:17:13 2022 ------------------------------

#Run GOM_sense
load(here('outputs/GOM_sense_50k.RData'))

#Copy initial Rpath parameters
#Alternative scenarios will be the same except for Biomass, PB, QB, Diet
alt<-copy(GOM.params)
alt.diet<-copy(GOM.params$diet)

#Count number of each group type
ngroups <- nrow(alt$model)
nliving <- nrow(alt$model[Type <  2, ])
ndead   <- nrow(alt$model[Type == 2, ])
ngear   <- nrow(alt$model[Type == 3, ])

#Pull out individual Ecosense scenario
#Loop over all successes
alt.models<-as.list(rep(NA,length(GOM_sense)))

for (i in 1:length(GOM_sense)) {
  #Copy initial Rpath parameters
  Rpath.alt<-copy(GOM.params)
  #Copy scenario
  GOM.alt<-GOM_sense[[i]]
  #Assign biomass
  alt.biomass<-GOM.alt$B_BaseRef[-1]
  Rpath.alt$model[,Biomass:=alt.biomass]
  #Assign PB
  Rpath.alt$model[,PB:=GOM.alt$PBopt[-1]]
  #Assign QB
  alt.QB<-GOM.alt$FtimeQBOpt[-1]
  alt.QB[1]<-0 #How to make this generalizable to models with other PP groups?
  Rpath.alt$model[,QB:=alt.QB]
  #Assign diet
  #Remove first two columns which represent 'outside' and 'PP't
  PreyFrom<-GOM.alt$PreyFrom[-c(1,2)]
  PreyTo<-GOM.alt$PreyTo[-c(1,2)]
  predpreyQ<-GOM.alt$QQ[-c(1,2)]
  #Fill consumption matrix
  QQ<-matrix(nrow = (nliving + ndead + 1),ncol=nliving)
  for (j in 1:length(PreyFrom)){
    prey<-PreyFrom[j]
    pred<-PreyTo[j]
    QQ[prey,pred]<-predpreyQ[j]
  }
  #Convert consumption matrix to diet matrix
  #Divide consumption by (QB*B)
  diet<-matrix(nrow = (nliving + ndead + 1),ncol=nliving)
  for (k in 1:nliving){
    diet[,k]<-QQ[,k]/(alt.QB[k]*alt.biomass[k])
  }
  #Convert to data table and fill in Rpath format
  alt.diet[,2:(nliving+1)]<-as.data.table(diet)
  Rpath.alt$diet<-alt.diet
  #Fill landings
  #Save model
  alt.model<-rpath(Rpath.alt)
  alt.models[[i]]<-alt.model
}

save(alt.models, file = "outputs/GOM_sense_Rpath_50k.RData")
