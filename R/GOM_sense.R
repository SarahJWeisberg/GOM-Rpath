#Title: GOM Ecosense

# Purpose: This script uses Ecosense simplified Bayesian synthesis to
#           generate plausible versions of the GOM food web (1980-85)
#           These plausible webs incorporate parameter uncertainty, as
#           determined by data pedigree.

# DataFiles:"GOM_params_Rpath.RData"; "GOM_Rpath.RData"

# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu

# Thu Feb 22 10:20:36 2024 ------------------------------



#Required packages--------------------------------------------------------------
library(here); library(data.table); library(Rpath)

#Source code from sense_beta branch of Rpath Repo
library(devtools)
source_url('https://raw.githubusercontent.com/NOAA-EDAB/Rpath/sense_beta/R/ecosense.R')

#Load balanced model
load(here("outputs/GOM_params_Rpath.RData"))
load(here("outputs/GOM_Rpath.RData"))

#Set up model with group names and types
groups<-as.vector(GOM$Group)

#Count number of each group type
nliving <- nrow(GOM.params$model[Type <  2, ])
ndead   <- nrow(GOM.params$model[Type == 2, ])
#Identify index of pp
pp<-which(groups=="Phytoplankton")

#Fix PB/QB pedigree values so that Respiration cannot <0
#Exclude pp and detritus, which do not technically respire in Rpath model
Unassim<-GOM$Unassim[1:(nliving+ndead)]
QB<-GOM$QB[1:(nliving+ndead)]
PB<-GOM$PB[1:(nliving+ndead)]
#Identify which groups violate this rule
fixers<-which((1-Unassim)*QB*(1-GOM.params$pedigree[,QB]) < (1+GOM.params$pedigree[, PB])*PB)
#exclude Phytoplankton & Detritus
fixers <- fixers[!fixers %in% c(pp,(nliving+ndead))]

#adjust QB and PB values where needed
for (i in 1:length(fixers)){
  to_fix<-fixers[i]
  Resp_edge<-PB[to_fix]/((1-Unassim[to_fix])*QB[to_fix])
  QB_ped<-GOM.params$pedigree[to_fix,QB]
  PB_ped<-GOM.params$pedigree[to_fix,PB]
  limit<-(1-QB_ped)/(1+PB_ped)
  while(Resp_edge>limit){
    if(QB_ped >= PB_ped){
      QB_ped <- QB_ped - 0.1
    }
    else{
      PB_ped <- PB_ped - 0.1
    }
    limit<-(1-QB_ped)/(1+PB_ped)
  }
  GOM.params$pedigree[to_fix,PB := PB_ped]
  GOM.params$pedigree[to_fix,QB := QB_ped]
}

#set detrital PB, QB to 0 - testing
# GOM.params$pedigree[57,PB := 0]
# GOM.params$pedigree[57,QB := 0]

# #Fix PB/QB pedigree values so that GE cannot >1
# GE<-GOM$GE
# limits<-(1-GOM.params$pedigree[,QB])/(1+GOM.params$pedigree[, PB])
# #Identify which groups violate this rule
# which(limits<GE[1:58])
# 
# #Manually adjust groups as needed
# #Adjust bacteria pedigree
# GOM.params$pedigree[2,QB :=0.6]
# GOM.params$pedigree[2,PB :=0.6]
# #Adjust microzooplankton pedigree
# GOM.params$pedigree[3,QB :=0.5]
# GOM.params$pedigree[3,PB :=0.6]
# #Adjust GelZooplankton pedigree
# GOM.params$pedigree[4,QB :=0.6]
# GOM.params$pedigree[4,PB :=0.6]
# #Adjust LgCopepods pedigree
# GOM.params$pedigree[5,QB :=0.5]
# GOM.params$pedigree[5,PB :=0.5]
# #Adjust SmCopepods pedigree
# GOM.params$pedigree[6,QB :=0.6]
# GOM.params$pedigree[6,PB :=0.6]
# #Adjust Micronekton pedigree
# GOM.params$pedigree[7,QB :=0.4]
# GOM.params$pedigree[7,PB :=0.5]
# #Adjust OtherCeph pedigree
# GOM.params$pedigree[10,QB :=0.5]
# GOM.params$pedigree[10,PB :=0.6]
# #Adjust Macrobenthos pedigree
# GOM.params$pedigree[11,QB :=0.7]
# #Adjust OceanPout pedigree
# GOM.params$pedigree[13,PB :=0.5]
# #Adjust SmPelagics pedigree
# GOM.params$pedigree[14,QB :=0.4]
# GOM.params$pedigree[14,PB :=0.6]
# #Adjust SmFlatfishes pedigree
# GOM.params$pedigree[15,QB :=0.5]
# GOM.params$pedigree[15,PB :=0.6]
# #Adjust OtherShrimps pedigree
# GOM.params$pedigree[17,QB :=0.5]
# GOM.params$pedigree[17,PB :=0.6]
# #Adjust OtherPelagics pedigree
# GOM.params$pedigree[20,QB :=0.5]
# GOM.params$pedigree[20,PB :=0.4]
# #Adjust AtlHerring pedigree
# GOM.params$pedigree[21,QB :=0.2]
# GOM.params$pedigree[21,PB :=0.8]
# #Adjust Mesopelagics pedigree
# GOM.params$pedigree[22,QB :=0.3]
# GOM.params$pedigree[22,PB :=0.5]
# #Adjust Cod pedigree
# GOM.params$pedigree[24,QB :=0.4]
# #Adjust AtlMackerel pedigree
# GOM.params$pedigree[27,QB :=0.4]
# GOM.params$pedigree[27,PB :=0.7]
# #Adjust OtherDemersals pedigree
# GOM.params$pedigree[31,QB :=0.3]
# GOM.params$pedigree[31,PB :=0.4]
# #Adjust HMS pedigree
# GOM.params$pedigree[32,PB :=0.4]
# #Adjust OtherSkates pedigree
# GOM.params$pedigree[33,QB :=0.7]
# #Adjust Barndoor pedigree
# GOM.params$pedigree[35,QB :=0.2]
# GOM.params$pedigree[35,PB :=0.4]
# #Adjust Fourspot pedigree
# GOM.params$pedigree[36,QB :=0.3]
# GOM.params$pedigree[36,PB :=0.4]
# #Adjust Pollock pedigree
# GOM.params$pedigree[38,QB :=0.7]
# GOM.params$pedigree[38,PB :=0.4]
# #Adjust Goosefish pedigree
# GOM.params$pedigree[39,QB :=0.6]
# GOM.params$pedigree[39,PB :=0.2]
# #Adjust SilverHake pedigree
# GOM.params$pedigree[40,QB :=0.5]
# GOM.params$pedigree[40,PB :=0.7]
# #Adjust WhiteHake pedigree
# GOM.params$pedigree[41,QB :=0.7]
# #Adjust SpinyDogfish pedigree
# GOM.params$pedigree[42,QB :=0.4]
# #Adjust LittleSkate pedigree
# GOM.params$pedigree[44,QB :=0.4]
# #Adjust WinterFlounder pedigree
# GOM.params$pedigree[47,QB :=0.7]
# GOM.params$pedigree[47,PB :=0.6]
# #Adjust WitchFlounder pedigree
# GOM.params$pedigree[48,QB :=0.7]
# GOM.params$pedigree[48,PB :=0.7]
# #Adjust Sharks pedigree
# GOM.params$pedigree[51,QB :=0.6]
# GOM.params$pedigree[51,PB :=0.6]
# #Adjust Megabenthos pedigree
# GOM.params$pedigree[56,QB :=0.4]
# GOM.params$pedigree[56,PB :=0.7]

#Adjust SmCopepod biomass pedigree
#For OSM / WARMEM goals
#Keep phyto contant
#REco.params$pedigree[6,Biomass :=0.8]
#REco.params$pedigree[1,Biomass := 0.0]
#REco.params$pedigree[1,PB := 0.0]

#Also try keeping EVERYTHING CONSTANT but copepod groups
#REco.params$pedigree[c(1:4,7:58),Biomass :=0.0]
#REco.params$pedigree[c(1:4,7:58),PB :=0.0]
#REco.params$pedigree[c(1:4,7:58),QB :=0.0]
#REco.params$pedigree[c(1:4,7:58),Diet :=0.0]

#Set up sense runs
all_years <- 1:50
scene <- rsim.scenario(GOM, GOM.params, years = all_years)
orig.biomass<-scene$start_state$Biomass

# ----- Set up ecosense generator ----- #######################################
scene$params$BURN_YEARS <- 50
NUM_RUNS <- 2000
parlist <- as.list(rep(NA, NUM_RUNS))
kept <- rep(NA, NUM_RUNS)

fail_groups<-c()
set.seed(19)
for (irun in 1:NUM_RUNS){
  GOMsense <- copy(scene)
  # INSERT SENSE ROUTINE BELOW
  parlist[[irun]] <- GOMsense$params 		# Base ecosim params
  parlist[[irun]] <- rsim.sense(GOMsense, GOM.params)	# Replace the base params with Ecosense params  
  #GOMsense$start_state$Biomass <- parlist[[irun]]$B_BaseRef #took out this line on May 2, 2022
  parlist[[irun]]$BURN_YEARS <- 50			# Set Burn Years to 50
  GOMsense$params <- parlist[[irun]]
  GOMtest <- rsim.run(GOMsense, method = "RK4", years = all_years)
  # failList <- which((is.na(GOMtest$end_state$Biomass) | GOMtest$end_state$Biomass/orig.biomass > 1000 | GOMtest$end_state$Biomass/orig.biomass < 1/1000))
  # {if (length(failList)>0)
  # {cat(irun,": fail in year ",GOMtest$crash_year,": ",failList,"\n"); kept[irun] <- F; flush.console()}
  #   else 
  #   {cat(irun,": success!\n"); kept[irun]<-T;  flush.console()}}
  # failList<-as.data.frame(failList)
  # fail_groups<-rbind(fail_groups,failList)
  parlist[[irun]]$BURN_YEARS <- 1
}

fail_groups <- fail_groups %>% group_by(failList) %>% tally()
colnames(fail_groups)<-c("group_num","total")
groups_num<- as.data.frame(cbind(groups[1:nliving],seq(2,(nliving+1))))
colnames(groups_num) <-c("name","group_num")
groups_num$group_num<-as.numeric(groups_num$group_num)
fail_fin<-left_join(fail_groups,groups_num,by="group_num")

# KEPT tells you which ecosystems were kept
KEPT <- which(kept==T)
nkept <- length(KEPT)
nkept
GOM_sense <- parlist[KEPT]

save(GOM_sense, file = "outputs/GOM_sense_50k_resp_constrain.RData")


# ----- Examine results relative to starting model ----- #######################################
biomass_sense<-c()
#biomass distributions
for (i in 1:length(GOM_sense)){
  model<-model<-GOM_sense[[i]]
  biomass<-cbind(groups[1:(nliving+ndead)],model$B_BaseRef[2:(nliving+ndead+1)],rep(i,(nliving+ndead)))
  biomass_sense<-rbind(biomass_sense,biomass)
}
biomass_sense<-as.data.frame(biomass_sense)
colnames(biomass_sense)<-c("Group","Biomass","Model")
biomass_sense$Biomass<-as.numeric(biomass_sense$Biomass)

biomass_orig<-as.data.frame(cbind(groups[1:(nliving+ndead)],GOM$Biomass[1:(nliving+ndead)]))
colnames(biomass_orig)<-c("Group","Biomass_orig")
biomass_orig$Biomass_orig<-as.numeric(biomass_orig$Biomass_orig)

biomass_sense<-left_join(biomass_sense,biomass_orig,by="Group")
phyto<-biomass_sense %>% filter(Group == "Phytoplankton")


ggplot(data=biomass_sense,aes(x=Biomass))+
  geom_histogram(alpha = 0.4)+
  facet_wrap(vars(Group),nrow = 6,scales = "free")+
  geom_vline(data = biomass_orig, aes(xintercept = Biomass_orig),color="blue")

ggplot(data=phyto,aes(x=Biomass))+
  geom_density(alpha = 0.4,fill="orange")

biomass_cor<-cor(biomass_sense)
library(corrplot)
corrplot(biomass_cor,method = "color",diag=F,type="lower",sig.level = 0.05)
#which biomass distributions come out as NOT uniform
library(dgof)
ks<-c()
for(i in 1:(nliving+ndead)){
  group<-groups[i]
  biomass_group<-biomass_sense %>% filter(Group==group)
  test<-ks.test(biomass_group$Biomass,"punif",min(biomass_group$Biomass),max(biomass_group$Biomass))
  ks<-rbind(ks,c(group,test$p.value))
}
ks<-as.data.frame(ks)
colnames(ks)<-c("Group","p")
ks$p<-as.numeric(ks$p)

#PBS
pb_sense<-c()
#pb distributions
for (i in 1:length(GOM_sense)){
  model<-model<-GOM_sense[[i]]
  pb<-cbind(groups[1:(nliving+ndead)],model$PBopt[2:(nliving+ndead+1)],rep(i,(nliving+ndead)))
  pb_sense<-rbind(pb_sense,pb)
}
pb_sense<-as.data.frame(pb_sense)
colnames(pb_sense)<-c("Group","PB","Model")
pb_sense$PB<-as.numeric(pb_sense$PB)

#ks test
ks<-c()
for(i in 1:(nliving+ndead)){
  group<-groups[i]
  pb_group<-pb_sense %>% filter(Group==group)
  test<-ks.test(pb_group$PB,"punif",min(pb_group$PB),max(pb_group$PB))
  ks<-rbind(ks,c(group,test$p.value))
}
ks<-as.data.frame(ks)
colnames(ks)<-c("Group","p")
ks$p<-as.numeric(ks$p)

biomass_orig<-as.data.frame(cbind(groups[1:(nliving+ndead)],GOM$Biomass[1:(nliving+ndead)]))
colnames(biomass_orig)<-c("Group","Biomass_orig")
biomass_orig$Biomass_orig<-as.numeric(biomass_orig$Biomass_orig)

biomass_sense<-left_join(biomass_sense,biomass_orig,by="Group")

ggplot(data=pb_sense,aes(x=PB))+
  geom_histogram(alpha = 0.75,fill="#f8cda4ff")+
  facet_wrap(vars(Group),nrow = 6,scales = "free")

means<-pb_sense %>% group_by(Group) %>% mutate(mean_PB = mean(PB)) %>% select(Group,mean_PB) %>% 
  distinct()

#QBS
qb_sense<-c()
#qb distributions
for (i in 1:length(GOM_sense)){
  model<-model<-GOM_sense[[i]]
  qb<-cbind(groups[1:(nliving+ndead)],model$FtimeQBOpt[2:(nliving+ndead+1)],rep(i,(nliving+ndead)))
  qb_sense<-rbind(qb_sense,qb)
}
qb_sense<-as.data.frame(qb_sense)
colnames(qb_sense)<-c("Group","QB","Model")
qb_sense$QB<-as.numeric(qb_sense$QB)

biomass_orig<-as.data.frame(cbind(groups[1:(nliving+ndead)],GOM$Biomass[1:(nliving+ndead)]))
colnames(biomass_orig)<-c("Group","Biomass_orig")
biomass_orig$Biomass_orig<-as.numeric(biomass_orig$Biomass_orig)

biomass_sense<-left_join(biomass_sense,biomass_orig,by="Group")

qb_sense %>% filter(!Group %in% c("Phytoplankton","Detritus","BlackSeaBass"))%>%
ggplot(aes(x=QB))+
  geom_histogram(alpha = 0.75,fill="#b6d7a8ff")+
  facet_wrap(vars(Group),nrow = 6,scales = "free")
