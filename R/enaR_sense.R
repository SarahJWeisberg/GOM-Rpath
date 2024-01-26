#Title: enaR Ecosense combination

#Purpose: This script runs ecological network analysis (ENA) on Rpath models
#         that result from Ecosense runs.

# Author: S. Weisberg

# Contact details: sarah.j.weisberg@stonybrook.edu

# DataFile:"GOM_sense_Rpath_50k.RData"

# Thu Nov 17 16:58:51 2022 ------------------------------


#Install package
#Note that I had to pull from github; CRAN version is depreciated
install.packages("sna")
library(devtools)
install_github('SEELab/enaR',ref='borretts', force = T)
library(enaR); library(sna); library(here); library(dplyr);library(Rpath)

#Load initial model
load(here("outputs/GOM_params_Rpath.RData"))
load(here("outputs/GOM_Rpath.RData"))

#First Ecosense, convert Rsim outputs to Rpath models
load(here("outputs/GOM_sense_Rpath_50k_newcopes.RData"))

#Need to calculate respiration, exports, detritus input
#To do so, need to pull unassim, M0, and F (fishing mortality)

#Count number of each group type
#ngroups <- nrow(GOM.params)
nliving <- nrow(GOM.params$model[Type <  2, ])
ndead   <- nrow(GOM.params$model[Type == 2, ])

alt.networks<-as.list(rep(NA,length(alt.models)))

#Load groups and fleets
#source(here("R/groups_fleets.R"))

#Set up model with group names and types
groups<-as.vector(GOM$Group)

for (i in 1:length(alt.models)) {
  #Copy params
  model<-alt.models[[i]]
  #Pull diet matrix
  diet<-model$DC
  #Get consumption values by DC*QB*B
  QQ<-matrix(nrow = (nliving + ndead + 1),ncol=nliving)
  for (j in 1:nliving){
  QQ[,j]<-diet[,j]*model$QB[j]*model$Biomass[j]
  }
  #Ignore Imports
  QQ<-QQ[1:57,]
  colnames(QQ)<-groups[1:56]
  rownames(QQ)<-groups[1:57]
  #Sum discards
  Discards<-rowSums(model$Discards)
  Discards<-Discards[1:57]
  #Calculate flow to detritus
  #If EE >1, assume 0 M0
  M0<-ifelse(model$EE<1,model$PB*(1-model$EE),0)
  #Or don't make that assumption
  #M0<-model$PB*(1-model$EE)
  Detritus<-(M0*model$Biomass+model$QB*model$Biomass*model$Unassim)*model$DetFate[,1]
  #Detritus<-GOM$QB*GOM$Biomass*GOM$Unassim
  Detritus<-Detritus[1:57]
  #Deal with flow to detritus from discards
  #Should be equal to all flow to discards minus consumption by SeaBirds(45)
  #DetInDisc<-sum(Discards)
  #Detritus[58]<-DetInDisc-QQ[58,45]
  #Flow to detritus from detritus = 0
  Detritus[57]<-0
  #Bind diet matrix (QQ) with flow to detritus, discards
  QQ<-cbind(QQ,Detritus)
  #Calculate exports
  #First sum catch
  Catch<-rowSums(model$Landings)
  #Add positive biomass accumulation terms
  Export<-Catch+(ifelse(model$BA>0,model$BA*model$Biomass,0))
  Export<-Export[1:57]
  Export[57]<-GOM$PB[57]*GOM$Biomass[57]
  #Calculate respiration
  #Assume detritus, discards have 0 respiration
  Resp<-((1-model$Unassim)*model$QB-model$PB)*model$Biomass
  Resp<-ifelse(Resp>0,Resp,0)
  Resp<-Resp[1:57]
  Resp[57]<-0
  #Deal with Primary Production
  #First, estimate GROSS production = Imports
  #P/B in Ecopath model gives NET production
  #Ratio of gross:net is going to be fixed based on EMAX
  gross_net<-4101.9/3281.5
  gross<-gross_net*model$PB[1]*model$Biomass[1]
  Resp[1]<-gross-(model$PB[1]*model$Biomass[1])
  #Calculate imports
  #Negative biomass accumulation terms
  #Gross primary production
  BA_Biomass<-abs(ifelse(model$BA<0,model$BA*model$Biomass,0))
  EE_Biomass<-ifelse(model$EE>1,(model$EE-1)*model$Biomass,0)
  Import<-BA_Biomass+EE_Biomass
  Import[1]<-gross
  Import<-Import[1:57]
  #Trim biomass
  Biomass<-model$Biomass[1:57]
  #Pack the model directly and store
  alt.networks[[i]]<-enaR::pack(flow = QQ,
             input = Import,
             export = Export,
             living = c(rep(TRUE,56),rep(FALSE,1)),
             respiration = Resp,
             storage = Biomass)
}

#Calculate network analysis outputs for original model (balanced)
#Pull diet matrix
diet<-GOM$DC
#Get consumption values by DC*QB*Biomass
QQ<-matrix(nrow = (nliving + ndead + 1),ncol=nliving)
for (j in 1:nliving){
  QQ[,j]<-diet[,j]*GOM$QB[j]*GOM$Biomass[j]
  }
#Ignore Imports
QQ<-QQ[1:(nliving+1),]
colnames(QQ)<-groups[1:nliving]
rownames(QQ)<-groups[1:(nliving+ndead)]
#Sum discards
#Discards<-rowSums(GOM$Discards)
#Discards<-Discards[1:58]
#Calculate flow to detritus
M0<-GOM$PB*(1-GOM$EE)
Detritus<-(M0*GOM$Biomass+GOM$QB*GOM$Biomass*GOM$Unassim)*GOM$DetFate[,1]
#Detritus<-GOM$QB*GOM$Biomass*GOM$Unassim
Detritus<-Detritus[1:(nliving+ndead)]
#Deal with flow to detritus from discards
#Should be equal to all flow to discards minus consumption by SeaBirds(45)
#DetInDisc<-sum(Discards)
#Detritus[58]<-DetInDisc-QQ[58,45]
#Flow to detritus from detritus = 0
Detritus[(nliving+1)]<-0
#Bind diet matrix (QQ) with flow to detritus, discards
QQ<-cbind(QQ,Detritus)
#Calculate exports
#First sum catch
Catch<-rowSums(GOM$Landings)
#Add positive biomass accumulation terms
Export<-Catch+(ifelse(GOM$BA>0,GOM$BA*GOM$Biomass,0))
Export<-Export[1:(nliving+ndead)]
for (i in 1:ndead){
  Export[nliving+i]<-GOM$PB[(nliving+i)]*GOM$Biomass[(nliving+i)]
}
#Calculate respiration
#Assume detritus, discards have 0 respiration
Resp<-((1-GOM$Unassim)*GOM$QB-GOM$PB)*GOM$Biomass
Resp<-ifelse(Resp>0,Resp,0)
Resp<-Resp[1:(nliving+ndead)]
Resp[(nliving+1):(nliving+ndead)]<-0
#Deal with Primary Production
#First, estimate GROSS production = Imports
#P/B in Ecopath model gives NET production
#Ratio of gross:net is going to be fixed based on EMAX
gross_net<-4101.9/3281.5
gross<-gross_net*GOM$PB[1]*GOM$Biomass[1]
Resp[1]<-gross-(GOM$PB[1]*GOM$Biomass[1])
#Calculate imports
#Negative biomass accumulation terms
#Gross primary production
Import<-abs(ifelse(GOM$BA<0,GOM$BA*GOM$Biomass,0))
Import[1]<-gross
Import<-Import[1:(nliving+ndead)]
#Trim biomass
Biomass<-GOM$Biomass[1:(nliving+ndead)]
#Pack the model directly and store
orig.network<-enaR::pack(flow = QQ,
                        input = Import,
                        export = Export,
                        living = c(rep(TRUE,nliving),rep(FALSE,ndead)),
                        respiration = Resp,
                        storage = Biomass)

#Information analysis of original model
info.orig<-enaAscendency(orig.network)
info.orig
control.orig<-enaControl(orig.network, balance.override = T)
mti.orig<-enaMTI(orig.network)

#Information analyes of suite of models
info<-lapply(alt.networks,enaAscendency)



#Generate flow models for all alternates
alt.flows<-lapply(alt.networks,enaFlow,balance.override=T)
flow.orig<-enaFlow(orig.network,balance.override=T)

#Pick out relative ascendancy metric
ASC.CAP<-c()
for (i in 1:length(alt.networks)){
  ASC.CAP[i]<-info[[i]][[7]]
}

#Pick out relative redundancy metric
OH.CAP<-c()
for (i in 1:length(alt.models)){
  OH.CAP[i]<-info[[i]][[8]]
}

R<-c()
for (i in 1:length(alt.networks)){
  R[i]<-info[[i]][[9]]
}


#Analyze original model
ASC.orig<-info.orig[7]

#Pick out absolute ascendancy metric
ASC<-c()
for (i in 1:length(alt.models)){
  ASC[i]<-info[[i]][[5]]
}

#storage analyses
storage.orig<-enaStorage(orig.network,balance.override = TRUE)
storage.orig$ns


#save outputs
save(info, file = "outputs/GOM_sense_enaR.RData")
save(alt.networks, file = "outputs/GOM_sense_enaR_networks.RData")

