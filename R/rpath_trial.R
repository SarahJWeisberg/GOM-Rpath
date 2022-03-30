#RPath model run - version 1
#Using all original data
#Run PREBAL diagnostics on original data to flag issues

# Tue Aug 24 10:09:05 2021 ------------------------------


#Load packages
library(Rpath); library(data.table);library(dplyr);library(here)

source(here("R/groups_fleets.R"))

#Set up model with group names and types
groups<-as.vector(groups_fleets$RPATH)
types<-c(1,rep(0,55),rep(2,2),rep(3,10))
REco.params<-create.rpath.params(group = groups,type=types)

#Fill in biomass estimates
source(here("R/EMAX_biomass_estimates.R"))

#biomass_80s<-na.omit(biomass_80s)

biomass<-left_join(groups_fleets,biomass_80s,by="RPATH")

#Remove barndoor
#biomass<-biomass[-35,]


#Turn biomass into vector
biomass<-as.vector(biomass$Biomass)

#Change barndoor to 0 biomass
biomass[35]<-0

#Fill model
REco.params$model[,Biomass:=biomass]


#Fill pb
source(here("R/biological_parameters.R"))
pb<-cbind(GOM.groups,PB)
pb<-left_join(groups_fleets,pb,by="RPATH")
pb<-as.vector(pb$PB)
REco.params$model[,PB:=pb]

#Fill qb
qb<-cbind(GOM.groups,QB)
qb<-left_join(groups_fleets,qb,by="RPATH")
qb<-as.vector(qb$QB)
REco.params$model[,QB:=qb]

#Fill biomass accumulation
source(here("R/Biomass_Accumulation.R"))
ba<-left_join(groups_fleets,biomass.accum,by="RPATH")
ba<-as.vector(ba$ba)
ba[is.na(ba)]<-0
ba[59:68]<-NA
REco.params$model[,BioAcc:=ba]

#Fill unassimilated consumption
REco.params$model[, Unassim := c(0,rep(0.4,5),rep(0.2, 50),rep(0,2), rep(NA, 10))]
#COME BACK TO THIS

#Detrital Fate
REco.params$model[, Detritus := c(rep(1, 56), rep(0, 12))]
REco.params$model[, Discards := c(rep(0, 56), rep(0,2),rep(1, 10))]

#Fisheries
#Landings
source(here("R/discards.R"))

#Fixed Gear
fixed<-left_join(groups_fleets,fixed,by="RPATH")
fixed<-as.vector(fixed$landings)
fixed[57:58]<-0
REco.params$model[, "Fixed Gear" := fixed]

#Large Mesh
lg_mesh<-left_join(groups_fleets,lg_mesh,by="RPATH")
lg_mesh<-as.vector(lg_mesh$landings)
lg_mesh[57:58]<-0
REco.params$model[, "LG Mesh" := lg_mesh]

#Other
other<-left_join(groups_fleets,other,by="RPATH")
other<-as.vector(other$landings)
other[57:58]<-0
REco.params$model[, "Other" := other]

#Small Mesh
sm_mesh<-left_join(groups_fleets,sm_mesh,by="RPATH")
sm_mesh<-as.vector(sm_mesh$landings)
sm_mesh[57:58]<-0
REco.params$model[, "SM Mesh" := sm_mesh]

#Scallop Dredge
scallop<-left_join(groups_fleets,scallop,by="RPATH")
scallop<-as.vector(scallop$landings)
scallop[57:58]<-0
REco.params$model[, "Scallop Dredge" := scallop]

#Trap
trap<-left_join(groups_fleets,trap,by="RPATH")
trap<-as.vector(trap$landings)
trap[57:58]<-0
REco.params$model[, "Trap" := trap]

#HMS.fleet
hms<-left_join(groups_fleets,hms,by="RPATH")
hms<-as.vector(hms$landings)
hms[57:58]<-0
REco.params$model[, "HMS Fleet" := hms]

#Pelagic
pelagic<-left_join(groups_fleets,pelagic,by="RPATH")
pelagic<-as.vector(pelagic$landings)
pelagic[57:58]<-0
REco.params$model[, "Pelagic" := pelagic]

#Other Dredge
other_dredge<-left_join(groups_fleets,other_dredge,by="RPATH")
other_dredge<-as.vector(other_dredge$landings)
other_dredge[57:58]<-0
REco.params$model[, "Other Dredge" := other_dredge]

#Clam
clam<-left_join(groups_fleets,clam,by="RPATH")
clam<-as.vector(clam$landings)
clam[57:58]<-0
REco.params$model[, "Clam Dredge" := clam]

#Fill in discards
#Fixed Gear
fixed.d<-left_join(groups_fleets,fixed.d,by="RPATH")
fixed.d<-as.vector(fixed.d$discards)
fixed.d[57:58]<-0
REco.params$model[, "Fixed Gear.disc" := fixed.d]

#Lg Mesh
lg_mesh.d<-left_join(groups_fleets,lg_mesh.d,by="RPATH")
lg_mesh.d<-as.vector(lg_mesh.d$discards)
lg_mesh.d[57:58]<-0
REco.params$model[, "LG Mesh.disc" := lg_mesh.d]

#Other
other.d<-left_join(groups_fleets,other.d,by="RPATH")
other.d<-as.vector(other.d$discards)
other.d[57:58]<-0
REco.params$model[, "Other.disc" := other.d]

#SM Mesh
sm_mesh.d<-left_join(groups_fleets,sm_mesh.d,by="RPATH")
sm_mesh.d<-as.vector(sm_mesh.d$discards)
sm_mesh.d[57:58]<-0
REco.params$model[, "SM Mesh.disc" := sm_mesh.d]

#Scallop Dredge
scallop.d<-left_join(groups_fleets,scallop.d,by="RPATH")
scallop.d<-as.vector(scallop.d$discards)
scallop.d[57:58]<-0
REco.params$model[, "Scallop Dredge" := scallop.d]

#Trap
trap.d<-left_join(groups_fleets,trap.d,by="RPATH")
trap.d<-as.vector(trap.d$discards)
trap.d[57:58]<-0
REco.params$model[, "Trap.disc" := trap.d]

#HMS
hms.d<-left_join(groups_fleets,hms.d,by="RPATH")
hms.d<-as.vector(hms.d$discards)
hms.d[57:58]<-0
REco.params$model[, "HMS Fleet.disc" := hms.d]

#Pelagic
pelagic.d<-left_join(groups_fleets,pelagic.d,by="RPATH")
pelagic.d<-as.vector(pelagic.d$discards)
pelagic.d[57:58]<-0
REco.params$model[, "Pelagic.disc" := pelagic.d]

#Other Dredge
other_dredge.d<-left_join(groups_fleets,other_dredge.d,by="RPATH")
other_dredge.d<-as.vector(other_dredge.d$discards)
other_dredge.d[57:58]<-0
REco.params$model[, "Other Dredge.disc" := other_dredge.d]

#Clam Dredge
clam.d<-c(rep(0,56),rep(0,2),rep(NA,10))
REco.params$model[, "Clam Dredge.disc" := clam.d]


#Complete diet table
source(here("R/diet.R"))

#Run diet filling
source(here("R/diet_filling.R"))

#Complete data pedigree
source(here('R/data_pedigree.R'))

#Run model
REco <- rpath(REco.params, eco.name = 'GOM Ecosystem')
REco

#### PREBAL ####

#Plot trophic level vs. log biomass
TL<-REco$TL
log_biomass<-log10(biomass)
prebal<-as.data.frame(cbind(TL,log_biomass))
row.names(prebal)<-groups
colnames(prebal)<-c("TL","log_biomass")

#Remove fleets, discards, detritus
prebal<-prebal[1:56,]

#Remove barndoor
prebal<-prebal[-35,]

#Remove AtlScallop
#prebal<-prebal[-19,]

#Linear regression
model1<-lm(log_biomass~TL,data=prebal)
summary(model1)

plot(log_biomass~TL,data=prebal,pch=1,cex=0.001)
abline(model1)
text(log_biomass~TL,data=prebal, labels=rownames(prebal), cex=0.5, font=2)

#Prediction intervals
newdata<-data.frame(TL=seq(min(TL),max(TL)))
#confidence.bands<-predict(eruption.lm,newdata,interval="confidence")
prediction.bands<-predict(model1,newdata,interval="predict")
#lines(newdata[,1],confidence.bands[,1],col=1)
#lines(newdata[,1],confidence.bands[,2],col=2)
#lines(newdata[,1],confidence.bands[,3],col=2)
lines(newdata[,1],prediction.bands[,2],col=3)
lines(newdata[,1],prediction.bands[,3],col=3)

#Plot SD lines
sd2 <- sd(abs(model1$residuals))*2
abline(model1$coefficients[1],model1$coefficients[2])
abline(model1$coefficients[1]+sd2,model1$coefficients[2])
abline(model1$coefficients[1]-sd2,model1$coefficients[2])

#Sum by quarter trophic level
prebal<-as.data.frame(REco$TL)
prebal<-cbind(prebal,biomass)
row.names(prebal)<-groups
colnames(prebal)<-c("TL","biomass")

#Remove fleets, discards, detritus
prebal<-prebal[1:56,]

#Remove barndoor
prebal<-prebal[-35,]

#Sum by quarter trophic level
prebal$rounded<-round(prebal$TL*4)/4
prebal<-aggregate(x=prebal$biomass,by=list(prebal$rounded),FUN=sum)
colnames(prebal)<-c("TL","biomass")
prebal$log_biomass<-log10(prebal$biomass)

#Linear regression
model1<-lm(biomass~TL,data=prebal)
summary(model1)

plot(biomass~TL,data=prebal,pch=19,cex=1)
abline(model1)

#Biomass ratios
#Use classification from starting parameters doc
class<-params_start[,c(1,2,4)]

#Remove SDemersals, Detritus, Discards
class<-class[-c(49,58,59),]

#Update class dataframe with new biomass estimates
biomass_new<-as.data.frame(cbind(groups,biomass),stringsAsFactors = F)
biomass_new<-biomass_new[1:56,]
colnames(biomass_new)<-c("RPATH","biomass_new")
biomass_new$biomass_new<-as.numeric(biomass_new$biomass_new)
class<-left_join(class,biomass_new,by="RPATH")

#Sum biomass by classification
class<-aggregate(x=class$biomass_new,by=list(class$Classification),FUN=sum)
colnames(class)<-c("Taxa","Biomass")

#Demersal and medium pelagics:small pelagics
DM<-sum(class$Biomass[which(class$Taxa == "Demersal (Round)" | class$Taxa == "Demersal (Flat)")])
MP<-class$Biomass[which(class$Taxa == "Pelagic (Medium; Round)")]
SP<-sum(class$Biomass[which(class$Taxa == "Pelagic (Small; Round)" | class$Taxa == "Pelagic (Small)")])

DMP_SP<-(DM+MP)/SP

#Small pelagics:zooplankton
ZP<-class$Biomass[which(class$Taxa == "Zooplankton")]
SP_ZP<-SP/ZP

#Zooplankton:phytoplankton
PP<-class$Biomass[which(class$Taxa == "Primary Producer")]
ZP_PP<-ZP/PP

#Small pelagics:phytoplankton
SP_PP<-SP/PP

#Demersal:benthic invertebrates
BI<-class$Biomass[which(class$Taxa == "Invertebrate (Benthic)")]
DM_BI<-DM/BI

#Sharks and HMS:small pelagics
SH<-class$Biomass[which(class$Taxa == "Shark")]
HMS<-class$Biomass[which(class$Taxa == "HMS")]
SHMS_SP<-(SH+HMS)/SP

#Marine mammals and birds:small pelagics
MB<-sum(class$Biomass[which(class$Taxa == "Mammal" | class$Taxa == "Whale" | class$Taxa == "Bird")])
MB_SP<-MB/SP

#Whales: zooplankton
W<-class$Biomass[which(class$Taxa == "Whale")]
W_ZP<-W/ZP

#Ratio between fish aggregate groups
#Demersal:pelagics
PL<-MP+SP
DM_PL<-DM/PL

#Flatfish:roundfish
FF<-class$Biomass[which(class$Taxa == "Demersal (Flat)")]
RF<-class$Biomass[which(class$Taxa == "Demersal (Round)")]+MP+SP
FF_RF<-FF/RF

#Small pelagics: all fish
AF<-DM+MP+SP+HMS+SH
SP_AF<-SP/AF

#Medium pelagics:all fish
MP_AF<-MP/AF

#HMS:all fish
HMS_AF<-HMS/AF

#Sharks:all fish
SH_AF<-SH/AF
                      
#Demersals:all fish
DM_AF<-DM/AF

#Ratio between invertebrate aggregate groups
PI<-class$Biomass[which(class$Taxa == "Invertebrate (Pelagic)")]
AI<-BI+ZP+PI

#Benthic invertebrate: all invertebrate
BI_AI<-BI/AI

#Zooplankton: all invertebrate
ZP_AI<-ZP/AI

#Pelagic invertebrate:all invertebrate
PI_AI<-PI/AI

#Other trophic ratios
#Zooplankton:benthos
ZP_BI<-ZP/BI

#Feeding guild ratios
class<-params_start[,c(1,2,5)]

#Remove SDemersals, Detritus, Discards
class<-class[-c(49,58,59),]

#Update class dataframe with new biomass estimates
biomass_new<-as.data.frame(cbind(groups,biomass),stringsAsFactors = F)
biomass_new<-biomass_new[1:56,]
colnames(biomass_new)<-c("RPATH","biomass_new")
biomass_new$biomass_new<-as.numeric(biomass_new$biomass_new)
class<-left_join(class,biomass_new,by="RPATH")

#Sum biomass by classification
class<-aggregate(x=class$biomass_new,by=list(class$Diet),FUN=sum)
colnames(class)<-c("Diet","Biomass")

#Benth:Pisc
Benth<-class$Biomass[which(class$Diet == "Benth")]
Pisc<-class$Biomass[which(class$Diet == "Pisc")]
Benth_Pisc<-Benth/Pisc
                      
#Benth:Plank
Plank<-class$Biomass[which(class$Diet == "Plank")]
Benth_Plank<-Benth/Plank

#Plank:Pisc
Plank_Pisc<-Plank/Pisc

#Vital rates across taxa
#Plot trophic level vs. log P/B
TL<-REco$TL
log_pb<-log10(pb)
prebal<-as.data.frame(cbind(TL,log_pb))
row.names(prebal)<-groups
colnames(prebal)<-c("TL","log_pb")

#Remove fleets, discards, detritus
prebal<-prebal[1:56,]

#Remove barndoor
prebal<-prebal[-35,]

#Remove AtlScallop
#prebal<-prebal[-19,]

#Linear regression
model1<-lm(log_pb~TL,data=prebal)
summary(model1)

plot(log_pb~TL,data=prebal,pch=1,cex=0.1)
abline(model1)
text(log_pb~TL,data=prebal, labels=rownames(prebal), cex=0.5, font=2)

#Vital rates across taxa
#Plot trophic level vs. log Q/B
TL<-REco$TL
log_qb<-log10(qb)
prebal<-as.data.frame(cbind(TL,log_qb))
row.names(prebal)<-groups
colnames(prebal)<-c("TL","log_qb")

#Remove fleets, discards, detritus
prebal<-prebal[1:56,]

#Remove phytoplankton, barndoor
prebal<-prebal[-c(1,35),]

#Remove AtlScallop
#prebal<-prebal[-19,]

#Linear regression
model1<-lm(log_qb~TL,data=prebal)
summary(model1)

plot(log_qb~TL,data=prebal,pch=1,cex=0.001)
abline(model1)
text(log_qb~TL,data=prebal, labels=rownames(prebal), cex=0.5, font=2)
