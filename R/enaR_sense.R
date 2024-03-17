#Title: enaR Ecosense combination

#Purpose: This script runs ecological network analysis (ENA) on Rpath models
#         that result from Ecosense runs.

# Author: S. Weisberg

# Contact details: sarah.j.weisberg@stonybrook.edu

# DataFile:"GOM_sense_Rpath_50k_2024.RData"

# Sun Mar 17 12:43:28 2024 ------------------------------


#Install packages
#Note that I had to pull from github; CRAN version is depreciated
library(devtools)
install.packages("gdata", repos = "https://packagemanager.posit.co/cran/2023-05-06")
install_github('SEELab/enaR', force = T)
library(gdata);library(enaR); library(sna); library(here); library(dplyr);library(Rpath);library(ggplot2);
library(tidyr);library(stats);library(forcats); library(ggbeeswarm); library(showtext);
library(RColorBrewer)

# Load initial model ---------------------------------------------------

#Load initial model
load(here("outputs/GOM_params_Rpath.RData"))
load(here("outputs/GOM_Rpath.RData"))
#Set up model with group names and types
groups<-as.vector(GOM$Group)

#Count number of each group type
nliving <- nrow(GOM.params$model[Type <  2, ])
ndead   <- nrow(GOM.params$model[Type == 2, ])

#Find index of pp groups
pp<- which(groups == "Phytoplankton")

# enaR on initial model ------------------------------------------------------

#Calculate network analysis outputs for original model (balanced)
#Pull diet matrix
diet<-GOM$DC
#Get consumption values by DC*QB*Biomass
QQ<-matrix(nrow = (nliving + ndead + 1),ncol=nliving)
for (j in 1:nliving){
  QQ[,j]<-diet[,j]*GOM$QB[j]*GOM$Biomass[j]
}
#Ignore Imports
QQ<-QQ[1:(nliving+ndead),]
colnames(QQ)<-groups[1:nliving]
rownames(QQ)<-groups[1:(nliving+ndead)]
#Calculate flow to detritus
M0<-GOM$PB*(1-GOM$EE)
Detritus<-(M0*GOM$Biomass+GOM$QB*GOM$Biomass*GOM$Unassim)*GOM$DetFate[,1]
#Detritus<-GOM$QB*GOM$Biomass*GOM$Unassim
Detritus<-Detritus[1:(nliving+ndead)]
#Note that I am ignoring discards here
#Flow to detritus from detritus = 0
Detritus[(nliving+1)]<-0
#Bind diet matrix (QQ) with flow to detritus, discards
QQ<-cbind(QQ,Detritus)
#Calculate exports
#First sum catch
Catch<-rowSums(GOM$Landings)
#Add positive biomass accumulation terms
Export<-Catch+(ifelse(GOM$BA>0,GOM$BA,0))
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
Import<-abs(ifelse(GOM$BA<0,GOM$BA,0))
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
info.orig <- as.data.frame(info.orig)
info.orig <- info.orig %>% mutate(A.internal.CAP = A.internal/CAP.internal)

#connectance
struc.orig<-enaStructure(orig.network)
#C value is directed, can also use this to calculate undirected connectance
ns<-as.data.frame(struc.orig$ns)
ns$L/(0.5*n*(n-1))

#FCI
flow.orig<-enaFlow(orig.network,balance.override = T)

ns.orig<-as.data.frame(flow.orig$ns)
flow.orig<-as.data.frame(cbind(groups[1:(nliving+ndead)],as.numeric(flow.orig$T))) %>% 
  rename(group=V1,orig=V2)


control<-enaControl(orig.network,balance.override = T)
sc<-as.data.frame(cbind(control$sc,groups[1:(nliving+ndead)]))
colnames(sc)<-c("sc","Group")

mti.orig<-enaMTI(orig.network,balance.override = T)
S<-enaStorage(orig.network,balance.override = T)

# enaR on all ecosense outputs ------------------------------------

#Load results of Ecosense, conversion of Rsim outputs to Rpath models
load(here("outputs/GOM_sense_50k_resp_constrain.Rdata"))

#calculate ena metrics from ecosense outputs
#older method I tried 
alt.networks<-as.list(rep(NA,length(GOM_sense)))

for (i in 1:length(GOM_sense)) {
  model<-GOM_sense[[i]]
  #Assign biomass, ignore Outside
  Biomass<-model$B_BaseRef[2:(nliving+ndead+1)]
  #Assign PB
  PB<-model$PBopt[2:(nliving+ndead+1)]
  #Assign QB
  QB<-model$FtimeQBOpt[2:(nliving+ndead+1)]
  QB[pp]<-0
  #Assign M0
  M0<-model$MzeroMort[2:(nliving+ndead+1)]
  #Assign diet
  #Remove first two entries which represent 'outside' flow to 'outside' and 'PP'
  PreyFrom<-model$PreyFrom[-c(1,2)]
  PreyTo<-model$PreyTo[-c(1,2)]
  predpreyQ<-model$QQ[-c(1,2)]
  #Fill consumption matrix
  QQ<-matrix(nrow = (nliving + ndead + 1),ncol=nliving)
  for (j in 1:length(PreyFrom)){
    prey<-PreyFrom[j]
    pred<-PreyTo[j]
    QQ[prey,pred]<-predpreyQ[j]
  }
  #convert NAs to 0s
  QQ[which(is.na(QQ)==T)]<-0
  #Ignore Imports
  QQ<-QQ[1:(nliving+ndead),]
  colnames(QQ)<-groups[1:nliving]
  rownames(QQ)<-groups[1:(nliving+ndead)]
  #Calculate flows to detritus
  Unassim<-model$UnassimRespFrac[2:(nliving+ndead+1)]
  DetFate<-model$DetFrac[2:(nliving+ndead+1)]
  Detritus<-(M0*Biomass+QB*Biomass*Unassim)*DetFate
  #Flow to detritus from detritus = 0
  Detritus[(nliving+ndead)]<-0
  #Bind diet matrix (QQ) with flow to detritus, discards
  QQ<-cbind(QQ,Detritus)
  #Calculate exports
  #First sum catch
  #Catch doesn't change in Ecosense so am pulling from original model
  Catch<-rowSums(GOM$Landings)[1:(nliving+ndead)]
  #Add positive biomass accumulation terms
  BA<-GOM$BA[1:(nliving+ndead)]
  Export<-Catch+(ifelse(BA>0,BA*Biomass,0))
  for (k in 1:ndead){
    Export[nliving+k]<-M0[(nliving+k)]*Biomass[(nliving+k)]
  }
  Export<-Export[1:(nliving+ndead)]
  #Calculate respiration
  #Assume detritus, discards have 0 respiration
  Resp<-((1-Unassim)*QB-PB)*Biomass
  Resp<-ifelse(Resp>0,Resp,0)
  Resp<-Resp[1:(nliving+ndead)]
  Resp[(nliving+1):(nliving+ndead)]<-0
  #Deal with Primary Production
  #First, estimate GROSS production = Imports
  #P/B in Ecopath model gives NET production
  #Ratio of gross:net is going to be fixed based on EMAX
  gross_net<-4101.9/3281.5
  gross<-gross_net*PB[pp]*Biomass[pp]
  Resp[pp]<-gross-(PB[pp]*Biomass[pp])
  Import<-abs(ifelse(BA<0,BA*Biomass,0)) 
  Import[pp]<-gross
  Import<-Import[1:(nliving+ndead)]
  alt.networks[[i]]<-enaR::pack(flow = QQ,
                                input = Import,
                                export = Export,
                                living = c(rep(TRUE,nliving),rep(FALSE,ndead)),
                                respiration = Resp,
                                storage = Biomass)
}
# run models, redo enaR ---------------------------------------------------

#set up runs
all_years <- 1:50
max<-max(all_years)
scene <- rsim.scenario(GOM, GOM.params, years = all_years)

#calculate ena metrics from ecosense runs
alt.networks<-as.list(rep(NA,length(GOM_sense)))

for (i in 1:length(GOM_sense)) {
  #run model
  scene$params<-GOM_sense[[i]]
  run <- rsim.run(scene, method = "RK4", years = all_years)
  #Assign biomass, ignore Outside
  Biomass<-run$end_state$Biomass[2:(nliving+ndead+1)]
  #Assign PB
  PB<-scene$params$PBopt[2:(nliving+ndead+1)]
  #Assign QB
  QB<-run[["annual_QB"]][max,][2:(nliving+ndead+1)]
  QB[pp]<-0
  #Assign M0
  M0<-scene$params$MzeroMort[2:(nliving+ndead+1)]
  #Assign diet
  #Remove first two entries which represent 'outside' flow to 'outside' and 'PP'
  PreyFrom<-scene$params$PreyFrom[-c(1,2)]
  PreyTo<-scene$params$PreyTo[-c(1,2)]
  predpreyQ<-run$annual_Qlink[max,][-c(1,2)]
  #Fill consumption matrix
  QQ<-matrix(nrow = (nliving + ndead + 1),ncol=nliving)
  for (j in 1:length(PreyFrom)){
    prey<-PreyFrom[j]
    pred<-PreyTo[j]
    QQ[prey,pred]<-predpreyQ[j]
  }
  #convert NAs to 0s
  QQ[which(is.na(QQ)==T)]<-0
  #Ignore Imports
  QQ<-QQ[1:(nliving+ndead),]
  colnames(QQ)<-groups[1:nliving]
  rownames(QQ)<-groups[1:(nliving+ndead)]
  #Calculate flows to detritus
  Unassim<-scene$params$UnassimRespFrac[2:(nliving+ndead+1)]
  DetFate<-scene$params$DetFrac[2:(nliving+ndead+1)]
  Detritus<-(M0*Biomass+QB*Biomass*Unassim)*DetFate
  #Flow to detritus from detritus = 0
  Detritus[(nliving+ndead)]<-0
  #Bind diet matrix (QQ) with flow to detritus, discards
  QQ<-cbind(QQ,Detritus)
  #Calculate exports
  #First sum catch
  #Catch doesn't change in Ecosense so am pulling from original model
  Catch<-run$annual_Catch[max,][2:(nliving+ndead+1)]
  #Add positive biomass accumulation terms
  BA<-GOM$BA[1:(nliving+ndead)]
  Export<-Catch+(ifelse(BA>0,BA*Biomass,0))
  for (k in 1:ndead){
    Export[nliving+k]<-M0[(nliving+k)]*Biomass[(nliving+k)]
  }
  Export<-Export[1:(nliving+ndead)]
  #Calculate respiration
  #Assume detritus, discards have 0 respiration
  Resp<-((1-Unassim)*QB-PB)*Biomass
  Resp<-ifelse(Resp>0,Resp,0)
  Resp<-Resp[1:(nliving+ndead)]
  Resp[(nliving+1):(nliving+ndead)]<-0
  #Deal with Primary Production
  #First, estimate GROSS production = Imports
  #P/B in Ecopath model gives NET production
  #Ratio of gross:net is going to be fixed based on EMAX
  gross_net<-4101.9/3281.5
  gross<-gross_net*PB[pp]*Biomass[pp]
  Resp[pp]<-gross-(PB[pp]*Biomass[pp])
  Import<-abs(ifelse(BA<0,BA*Biomass,0)) 
  Import[pp]<-gross
  Import<-Import[1:(nliving+ndead)]
  alt.networks[[i]]<-enaR::pack(flow = QQ,
                                input = Import,
                                export = Export,
                                living = c(rep(TRUE,nliving),rep(FALSE,ndead)),
                                respiration = Resp,
                                storage = Biomass)
}
# analyses & plots --------------------------------------------------------

#Run ascendancy analysis on all networks
#alt.info<-lapply(alt.networks,enaAscendency)
info<-c()
for (i in 1:length(alt.networks)){
  asc<-enaAscendency(alt.networks[[i]])
  info<-rbind(info,asc)
}
info<-as.data.frame(info)

#classify models as low or high based on lit values (Ulanowicz 2014)
info<-info %>% mutate(A.CAP.internal=A.internal/CAP.internal) %>% 
  mutate(range = ifelse(ASC.CAP<0.36,"low",ifelse(ASC.CAP>0.44,"high","norm")))

ns<-c()
for (i in 1:length(alt.networks)){
  asc<-enaFlow(alt.networks[[i]],balance.override = T)
  ns<-rbind(ns,asc$ns)
}
ns<-as.data.frame(ns)

#pull nodal flows out of each alternate model
flow_T<-c()
for (i in 1:length(alt.networks)){
  alt.flow<-enaFlow(alt.networks[[i]],balance.override=T)
  flow_group<-cbind(groups[1:57],alt.flow$T)
  flow_T<-rbind(flow_T,flow_group)
}
#bind with original values
flow_T <- as.data.frame(flow_T)
rownames(flow_T)<-c()
colnames(flow_T)<-c("group","flow")
flow_T$flow<-as.numeric(flow_T$flow)
flow_T$model<-rep(1:length(GOM_sense),each = (nliving+ndead))

#get means and 90th CI for each group's flow
flow_avg<-flow_T %>% group_by(group) %>% 
  mutate(avg = mean(flow),lower = quantile(flow,0.05),upper = quantile(flow,0.95)) %>%
  select(group,avg,upper,lower) %>% distinct 

#compare alternate flows with starting model
flow_comp <- left_join(flow_avg,flow.orig,by="group") %>% mutate(orig = as.numeric(orig)) %>%
  mutate(diff_orig = (avg-orig)/orig) %>% mutate(test = ifelse(orig<lower | orig > upper,T,F)) %>%
  mutate(sign = ifelse(diff_orig>0,"pos","neg")) 

#classify models by ASC cutoff
high_ASC<-flow_T %>% filter (model %in% which(info$range == "high"))
low_ASC<-flow_T %>% filter (model %in% which(info$range == "low"))
norm_ASC<-flow_T %>% filter (model %in% which(info$range == "norm"))

flow_T<- flow_T %>% 
  mutate(range = ifelse(model %in% which(info$range == "high"),"high",ifelse(model %in% which(info$range == "low"),"low","norm")))

#t-tests for flow differences
#probably a more elegant way to do w
p_values<-c()
for(i in 1:(nliving+ndead)){
  g<-groups[i]
  high<-high_ASC %>% filter(group == g)
  low<-low_ASC %>% filter(group == g)
  norm<-norm_ASC %>% filter(group == g)
  test<-t.test(high$flow,low$flow)
  out<-cbind(test$p.value,g,"high_low")
  p_values<-rbind(out,p_values)
  test<-t.test(high$flow,norm$flow)
  out<-cbind(test$p.value,g,"high_norm")
  p_values<-rbind(out,p_values)
  test<-t.test(norm$flow,low$flow)
  out<-cbind(test$p.value,g,"norm_low")
  p_values<-rbind(out,p_values)
}
p_values<-as.data.frame(p_values) %>% rename(p = V1, comp=V3)
p_values$adjusted<-p.adjust(p_values$p, method = "bonferroni")

p_values<-p_values %>% filter(adjusted<0.05)

norm_low<-flow_T %>% filter(group %in% p_values$g[which(p_values$comp == "norm_low")],range %in% c("norm","low")) 


#plotting
font_add_google("Roboto","Roboto")
roboto<-"Roboto"
showtext_auto()
my.pal<-brewer.pal(8,"Set2")

ggplot(info,aes(x=ASC.CAP,y=ELD))+
  geom_point()

ggplot(info,aes(x=TD,y=ELD))+
  geom_point(size=1)+
  # geom_vline(xintercept = 2)+
  # geom_vline(xintercept = 4.5)+
  # geom_hline(yintercept = 3)+
  labs(x="Trophic Depth",y="Effective Link Density")+
  theme(axis.title = element_text(size=14,family = "Roboto"),
        axis.text = element_text(size=10,family="Roboto"))+
  geom_point(x=info.orig$TD,y=info.orig$ELD,color=my.pal[4],size=3)
  
ggplot(info,aes(x=ASC.CAP*100))+
  geom_density(fill= "#f8cda4ff",alpha=0.75)+
  geom_vline(xintercept=info.orig$ASC.CAP*100,color=my.pal[4],linetype="dashed",linewidth=1)+
  labs(x="Relative Efficiency (%)",y="density")+
  theme(axis.title = element_text(size=14,family = "Roboto"),
        axis.text = element_text(size=10,family="Roboto"))+
  annotate("text",x=42.5,y=0.06,label="n = 2000",size=6,family="Roboto")

ggplot(info,aes(A.CAP.internal*100))+
  geom_density(fill= "#b6d7a8ff",alpha=0.75)+
  geom_vline(xintercept=info.orig$A.internal.CAP*100,color=my.pal[4],linetype="dashed",linewidth=1)+  
  labs(x="Relative Efficiency - Internal (%)",y="density")+
  theme(axis.title = element_text(size=14,family = "Roboto"),
        axis.text = element_text(size=10,family="Roboto"))+
  annotate("text",x=38.5,y=0.06,label="n = 1787",size=6,family="Roboto")

ggplot(ns_GOM,aes(x=FCI))+
  geom_density(fill= "magenta",alpha=0.75)+
  geom_vline(xintercept=ns.orig$FCI,color="black",linetype="dashed",linewidth=1)+  
  labs(x="FCI",y="density")+
  theme(axis.title = element_text(size=14,family = "Roboto"),
        axis.text = element_text(size=10,family="Roboto"))


ggplot(info,aes(x=ASC.CAP*100,fill=range))+
  geom_histogram(alpha=0.8,bins =100,show.legend = F)+
  scale_fill_manual(values = c(my.pal[1],my.pal[3],my.pal[2]))+ 
  #geom_vline(xintercept=info.orig[7]*100,color=my.pal[4],linetype="dashed",linewidth=1)+
  labs(x="Relative Efficiency (%)",y="count")+
  theme(axis.title = element_text(size=14,family = "Roboto"),
        axis.text = element_text(size=10,family="Roboto"))+
  annotate("segment",x=32,y=0,xend = 32,yend = 20,color=my.pal[4],linetype="dashed",linewidth=1)+
  annotate("text",x=33,y=25,label="n = 406",size=5,family="Roboto")+
  annotate("text",x=41.5,y=37,label="n = 1348",size=5,family="Roboto")+
  annotate("text",x=46.25,y=5,label="n = 33",size=5,family="Roboto")


norm_low %>% filter(group %in% p_values$g) %>%
  mutate(range=ifelse(range=="norm","mid",range)) %>%
  ggplot(aes(x=range,y=flow,color=range))+  
  ggbeeswarm::geom_quasirandom(
    size = 1, width = .2, alpha = .25,
  ) +
  scale_color_manual(values=c(my.pal[3],my.pal[2]))+
  stat_summary(
    fun = mean, geom = "point", 
    shape = 95, size = 35
  ) + 
  # ggbeeswarm::geom_quasirandom(
  #   size = 2, width = .33, shape = 1, color = "black", stroke = .25
  # )+
  facet_wrap(~group,scales = "free")+
  theme(legend.position = "none")+  
  theme(axis.title = element_text(size=14,family = "Roboto"),
        axis.text = element_text(size=10,family="Roboto"))
 

high_low<-flow_T %>% filter(group %in% p_values$g[which(p_values$comp == "high_low")],range %in% c("high","low")) 

high_low %>% mutate(range=fct_relevel(range,"low","high")) %>% filter(group %in% p_values$g) %>%
  ggplot(aes(x=range,y=flow,color=range))+  
  scale_color_manual(values=c(my.pal[3],my.pal[1]))+
  ggbeeswarm::geom_quasirandom(
    size = 1, width = .2, alpha = .25
  ) +
  stat_summary(
    fun = mean, geom = "point", 
    shape = 95, size = 35
  ) + 
  # ggbeeswarm::geom_quasirandom(
  #   size = 2, width = .33, shape = 1, color = "black", stroke = .25
  # )+
  facet_wrap(~group,scales = "free")+
  theme(axis.title = element_text(size=14,family = "Roboto"),
        axis.text = element_text(size=10,family="Roboto"))+
  theme(legend.position = "none")

flow_comp %>% 
  filter(abs(diff_orig) <=0.05) %>%
  mutate(group=fct_reorder(group,diff_orig))%>%
ggplot()+
  (aes(x=reorder(group,diff_orig),y=diff_orig*100,fill=sign)) +
  geom_bar(stat = "identity",show.legend = F,alpha=0.75) +
  #scale_y_continuous(breaks = seq(-25, 10, 2)) +
  scale_fill_manual(values = c("#034e7b","#99000d")) +
  labs(y = "Flow difference (%)", x = "") +   
  theme(axis.title = element_text(size=12,family = "Roboto"),
        axis.text = element_text(size=8,family="Roboto"))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#think about comparing sense outputs with unbounded versions
flow_unbound$test<-"unbounded"
flow_T$test<-"bounded"
flow_comp<-rbind(flow_T,flow_unbound)
ggplot(data=flow_comp,aes(x=flow,fill=test))+
  geom_density(alpha = 0.4)+
  facet_wrap(vars(group),nrow = 6,scales = "free")

#which flow distributions are not the same
ks_flow<-c()
for(i in 1:(nliving+ndead)){
  bound<-flow_T %>% filter(group == groups[i])
  unbound<-flow_unbound %>% filter(group == groups[i])
  test<-ks.test(bound$flow,unbound$flow)
  ks_flow<-rbind(ks_flow,c(groups[i],test$p.value))
}
ks_flow<-as.data.frame(ks_flow)
colnames(ks_flow)<-c("Group","p")
ks_flow$p<-as.numeric(ks_flow$p)


#compare to control
ks_flow<-left_join(ks_flow,sc,by="Group")
ks_flow$sc<-as.numeric(ks_flow$sc)

#visualize results
ks_flow<- ks_flow %>% mutate(quant = ifelse(p<quantile(p,0.1),10,
                                            ifelse(p<quantile(p,0.2),20,
                                                   ifelse(p<quantile(p,0.3),30,
                                                          ifelse(p<quantile(p,0.4),40,
                                                                 ifelse(p<quantile(p,0.5),50,
                                                                        ifelse(p<quantile(p,0.6),60,
                                                                               ifelse(p<quantile(p,0.7),70,
                                                                                      ifelse(p<quantile(p,0.8),80,
                                                                                             ifelse(p<quantile(p,0.9),90,100))))))))))
ks_vis<-ks_flow %>% mutate(sign = ifelse(sc>0,"pos","neg"))%>% group_by(quant) %>% tally(sign=="pos") %>%
  mutate(prop=n/5.7)

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

#Pick out absolute ascendancy metric
ASC<-c()
for (i in 1:length(alt.models)){
  ASC[i]<-info[[i]][[5]]
}

#storage analyses
storage.orig<-enaStorage(orig.network,balance.override = TRUE)
storage.orig$ns

#Generate flow models for all alternates
alt.flows<-lapply(alt.networks,enaFlow,balance.override=T)
flow.orig<-enaFlow(orig.network,balance.override=T)

#save outputs
save(info, file = "outputs/GOM_sense_enaAscendancy.RData")
save(alt.networks, file = "outputs/GOM_sense_enaR_networks.RData")

