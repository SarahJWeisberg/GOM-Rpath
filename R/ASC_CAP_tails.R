#analysis of tails of distribution

# Tue Nov  8 14:10:32 2022 ------------------------------

#Load packages
install.packages("GGally")
library(GGally)

#Load balanced model
load(here("outputs/GOM_params_Rpath.RData"))
load(here("outputs/GOM_Rpath.RData"))

#Load files
load(here("outputs/GOM_sense_Rpath_50k.RData"))
load(here("outputs/GOM_sense_enaR.RData"))
load(here("outputs/GOM_sense_enaR_networks.RData"))

#Generate flow models for all alternates
alt.flows<-lapply(alt.networks,enaFlow,balance.override=T)
#flow.orig<-enaFlow(orig.network,balance.override=T)

##Pick out relative ascendancy metric
ASC.CAP<-c()
for (i in 1:length(alt.models)){
  ASC.CAP[i]<-info[[i]][[7]]
}

#find 5th, 9th quantile
#based on ASC.CAP all values
low_tail<-quantile(ASC.CAP, probs = 0.05)
high_tail<-quantile(ASC.CAP, probs = 0.95)

#basic density plot
ASC.CAP<-as.data.frame(ASC.CAP)
ggplot(data = ASC.CAP, aes(x=ASC.CAP)) +
  geom_density(color="magenta",fill=4, alpha=0.7)+
  geom_vline(xintercept = low_tail, color = 'black')+
  geom_vline(xintercept = high_tail, color = 'black')
  
#isolate 'extreme' models
low.ASC<-alt.models[which(ASC.CAP$ASC.CAP <= low_tail)]
high.ASC<-alt.models[which(ASC.CAP$ASC.CAP >= high_tail)]

#isolate 'extreme' models as flows
low.ASC.flow<-alt.flows[which(ASC.CAP$ASC.CAP <= low_tail)]
high.ASC.flow<-alt.flows[which(ASC.CAP$ASC.CAP >= high_tail)]

#Extract all flows from these models
flows.high<-data.frame(matrix(nrow = 58,ncol=length(high.ASC.flow)))
for(i in 1:length(high.ASC.flow)){
  flow<-high.ASC.flow[[i]]
  flows.high[,i]<-flow$T
}
rownames(flows.high)<-groups[1:58] #come back and fix
flows.high$means<-rowMeans(flows.high)

flows.low<-data.frame(matrix(nrow = 58,ncol=length(low.ASC.flow)))
for(i in 1:length(low.ASC.flow)){
  flow<-low.ASC.flow[[i]]
  flows.low[,i]<-flow$T
}
rownames(flows.low)<-groups[1:58]
flows.low$means<-rowMeans(flows.low)


#all flows
flows.all<-data.frame(matrix(nrow = 58,ncol = length(alt.flows)))
for(i in 1:length(alt.flows)){
  flow<-alt.flows[[i]]
  flows.all[,i]<-flow$T
}
rownames(flows.all)<-groups[1:58]
flows.all$means<-rowMeans(flows.all)

#Repeat with biomass

#Extract all biomass from these models
biomass.high<-data.frame(matrix(nrow = 58,ncol=length(high.ASC)))
for(i in 1:length(high.ASC)){
  biomass.high[,i]<-high.ASC[[i]]$Biomass[1:58]
}
rownames(biomass.high)<-groups[1:58]
biomass.high$means<-rowMeans(biomass.high)

biomass.low<-data.frame(matrix(nrow = 58,ncol=length(low.ASC.flow)))
for(i in 1:length(low.ASC.flow)){
  biomass.low[,i]<-low.ASC[[i]]$Biomass[1:58]
}
rownames(biomass.low)<-groups[1:58]
biomass.low$means<-rowMeans(biomass.low)


#all biomasses
biomass.all<-data.frame(matrix(nrow = 58,ncol=length(alt.flows)))
for(i in 1:length(alt.flows)){
  biomass.all[,i]<-alt.models[[i]]$Biomass[1:58]
}
rownames(biomass.all)<-groups[1:58]
biomass.all$means<-rowMeans(biomass.all)

#Graph anomalies
#Start with high ASC models
anom.high<-(flows.high$means*100/flows.all$means)-100
anom.high<-as.data.frame(cbind(groups[1:58],anom.high))
colnames(anom.high)<-c("group","anomaly")
anom.high$anomaly<-as.numeric(anom.high$anomaly)
anom.high$sign<-ifelse(anom.high$anomaly>0,"pos","neg")

ggplot(anom.high)+
  (aes(group,anomaly,fill=sign)) +
  geom_bar(stat = "identity",show.legend = F) +
  scale_y_continuous(breaks = seq(-100, 100, 20)) +
  scale_fill_manual(values = c("#034e7b","#99000d")) +
  labs(y = "Flow anomaly (%)", x = "") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

anom.high.select<-filter(anom.high,group == "Cod" | group == "AtlHerring" |
                           group == "Bacteria" |
                           group == "LgCopepods" | group == "SmCopepods")
positions<-c("Bacteria","SmCopepods","LgCopepods","AtlHerring","Cod")

ggplot(anom.high.select)+
  (aes(x=group,anomaly,fill=sign)) +
  scale_x_discrete(limits=positions)+
  geom_bar(stat = "identity",show.legend = F) +
  #scale_y_continuous(breaks = seq(-100, 100, 20)) +
  scale_fill_manual(values = c("#034e7b","#99000d")) +
  labs(y = "Flow anomaly (%)", x = "") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("High Efficiency Models")


#Low ASC models
anom.low<-(flows.low$means*100/flows.all$means)-100
anom.low<-as.data.frame(cbind(groups[1:58],anom.low))
colnames(anom.low)<-c("group","anomaly")
anom.low$anomaly<-as.numeric(anom.low$anomaly)
anom.low$sign<-ifelse(anom.low$anomaly>0,"pos","neg")

ggplot(anom.low)+
  (aes(group,anomaly,fill=sign)) +
  geom_bar(stat = "identity",show.legend = F) +
  scale_y_continuous(breaks = seq(-100, 100, 20)) +
  scale_fill_manual(values = c("#034e7b","#99000d")) +
  labs(y = "Flow anomaly (%)", x = "") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

anom.low.select<-filter(anom.low,group == "Cod" | group == "AtlHerring" |
                          group == "Bacteria" |
                          group == "LgCopepods" | group == "SmCopepods")
positions<-c("Bacteria","SmCopepods","LgCopepods","AtlHerring","Cod")

ggplot(anom.low.select)+
  (aes(group,anomaly,fill=sign)) +
  scale_x_discrete(limits=positions)+
  geom_bar(stat = "identity",show.legend = F) +
  scale_y_continuous(breaks = seq(-100, 100, 20)) +
  scale_fill_manual(values = c("#034e7b","#99000d")) +
  labs(y = "Flow anomaly (%)", x = "") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Low Efficiency Models")

#Look at biomass anomalies
#Start with high ASC models
anom.high<-(biomass.high$means*100/biomass.all$means)-100
anom.high<-as.data.frame(cbind(groups[1:58],anom.high))
colnames(anom.high)<-c("group","anomaly")
anom.high$anomaly<-as.numeric(anom.high$anomaly)
anom.high$sign<-ifelse(anom.high$anomaly>0,"pos","neg")

ggplot(anom.high)+
  (aes(group,anomaly,fill=sign)) +
  geom_bar(stat = "identity",show.legend = F) +
  scale_y_continuous(breaks = seq(-100, 100, 20)) +
  scale_fill_manual(values = c("#034e7b","#99000d")) +
  labs(y = "Flow anomaly (%)", x = "") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

anom.high.select<-filter(anom.high,group == "Cod" | group == "AtlHerring" |
                           group == "Bacteria" |
                           group == "LgCopepods" | group == "SmCopepods" | group == "AmLobster")

positions<-c("Bacteria","SmCopepods","LgCopepods","AtlHerring","Cod","AmLobster")

ggplot(anom.high.select)+
  (aes(group,anomaly,fill=sign)) +
  geom_bar(stat = "identity",show.legend = F) +
  scale_x_discrete(limits=positions)+
  #scale_y_continuous(breaks = seq(-100, 100, 20)) +
  scale_fill_manual(values = c("#034e7b","#99000d")) +
  labs(y = "Biomass anomaly (%)", x = "") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("High Efficiency Models")


#Low ASC models
anom.low<-(biomass.low$means*100/biomass.all$means)-100
anom.low<-as.data.frame(cbind(groups[1:58],anom.low))
colnames(anom.low)<-c("group","anomaly")
anom.low$anomaly<-as.numeric(anom.low$anomaly)
anom.low$sign<-ifelse(anom.low$anomaly>0,"pos","neg")

ggplot(anom.low)+
  (aes(group,anomaly,fill=sign)) +
  geom_bar(stat = "identity",show.legend = F) +
  scale_y_continuous(breaks = seq(-100, 100, 20)) +
  scale_fill_manual(values = c("#034e7b","#99000d")) +
  labs(y = "Flow anomaly (%)", x = "") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

anom.low.select<-filter(anom.low,group == "Cod" | group == "AtlHerring" |
                          group == "Bacteria" |
                          group == "LgCopepods" | group == "SmCopepods" | group == "AmLobster")

ggplot(anom.low.select)+
  (aes(group,anomaly,fill=sign)) +
  geom_bar(stat = "identity",show.legend = F) +
  scale_x_discrete(limits=positions)+
  #scale_y_continuous(breaks = seq(-100, 100, 20)) +
  scale_fill_manual(values = c("#034e7b","#99000d")) +
  labs(y = "Biomass Anomaly (%)", x = "") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Low Efficiency Models")



    