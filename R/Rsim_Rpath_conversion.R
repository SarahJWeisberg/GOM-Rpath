#Script to convert Ecosense output (i.e. Rsim scenario) to Rpath object

#Author: Sarah J. Weisberg

# Tue Sep 21 09:14:35 2021 ------------------------------

#Run GOM_sense
source(here('R/GOM_sense.R'))

#Copy initial Rpath parameters
#Alternative scenarios will be the same except for Biomass, PB, QB, Diet, Landings
alt<-copy(REco.params)
alt.diet<-copy(REco.params$diet)

#Count number of each group type
ngroups <- nrow(alt$model)
nliving <- nrow(alt$model[Type <  2, ])
ndead   <- nrow(alt$model[Type == 2, ])
ngear   <- nrow(alt$model[Type == 3, ])

#Pull out individual Ecosense scenario
#Loop over all successes

alt.params<-as.list(rep(NA,length(REco.sense)))

for (i in 1:length(REco.sense)) {
  #Copy scenario
  REco.alt<-REco.sense[[i]]
  #Assign biomass
  alt.biomass<-REco.alt$B_BaseRef[-1]
  alt$model[,Biomass:=alt.biomass]
  #Assign PB
  alt$model[,PB:=REco.alt$PBopt[-1]]
  #Assign QB
  alt.QB<-REco.alt$FtimeQBOpt[-1]
  alt.QB[1]<-0 #How to make this generalizable to models with other PP groups?
  alt$model[,QB:=alt.QB]
  #Assign diet
  #Remove first two columns which represent 'outside' and 'PP't
  PreyFrom<-REco.alt$PreyFrom[-c(1,2)]
  PreyTo<-REco.alt$PreyTo[-c(1,2)]
  predpreyQ<-REco.alt$QQ[-c(1,2)]
  #Fill consumption matrix
  QQ<-matrix(nrow = (nliving + ndead + 1),ncol=nliving)
  for (i in 1:length(PreyFrom)){
    prey<-PreyFrom[i]
    pred<-PreyTo[i]
    QQ[prey,pred]<-predpreyQ[i]
  }
  #Convert consumption matrix to diet matrix
  #Divide consumption by (QB*B)
  diet<-matrix(nrow = (nliving + ndead + 1),ncol=nliving)
  for (i in 1:nliving){
    diet[,i]<-QQ[,i]/(alt.QB[i]*alt.biomass[i])
  }
  #Convert to data table and fill in Rpath format
  alt.diet[,2:(nliving+1)]<-as.data.table(diet)
  alt$diet<-alt.diet
  #Fill landings
  #Save model
  alt.params[[i]]<-alt
}

AltEco1 <- rpath(alt)
AltEco1

check.rpath.params(alt)
