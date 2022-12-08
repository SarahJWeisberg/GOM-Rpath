#Title: 

#Purpose: This script allows for visualization of Ecosense output models.
#         Specifically, it looks at how EEs of all groups in model ensemble
#         compare to initial balanced model.

#Datafile:

#Author: Sarah J. Weisberg
#Contact details: sarah.j.weisberg@stonybrook.edu

# Mon Nov  7 12:15:01 2022 ------------------------------


#Load needed packages
library(here); library(dplyr); library(tidyr); library(ggplot2)

#Can't put all species/groups on the same plot
#Bin based on TL in original model
#Load original model
load(here("outputs/GOM_Rpath.RData"))
load(here("outputs/GOM_sense_50k.RData"))
load(here("outputs/GOM_sense_Rpath_50k.RData"))

#Pull out TLs & EEs, assign to 4 bins (56/4 = 14 per bin)
#Ignoring fleets for now
groups<-GOM$Group[1:56]
TL<-as.data.frame(cbind(GOM$TL[1:56],GOM$EE[1:56],groups))
colnames(TL) <- c("TL","EE_orig","groups")
TL<-TL %>% mutate(TL_bin = ntile(TL, n=4))

#pull EEs from Ecosense output
#ensure tidy formatting
EE<-c()
for (i in 1:length(alt.models)){
  model<-alt.models[[i]]
  EE_i<-cbind(groups,model$EE[1:56], rep(i,56))
  EE<-rbind(EE,EE_i)
  rownames(EE)<-NULL
}

#EE<-c()
#for (i in 1:length(alt.models)){
 # model<-alt.models[[i]]
  #EE_i<-model$EE[1:56]
  #EE<-rbind(EE,EE_i)
#}

EE<-as.data.frame(EE)
colnames(EE)<-c("groups", "EE","model")
EE$EE<-as.numeric(EE$EE)
EE<- left_join(EE,TL,by="groups")

#plot proportion of models kept by various EE filters
totals<-c()
for(i in 0:10){
  x<-1+0.5*i
  EE_sub<-EE %>% filter(EE<=x) %>% count(model) %>% filter(n==56)
  y<-cbind(x,length(EE_sub$model)/length(alt.models))
  totals<-rbind(totals,y)
}
totals<-as.data.frame(totals)
colnames(totals)<-c("threshold","percent_below")

#plot results
ggplot(data = totals,aes(x=threshold,y=percent_below))+
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks = seq(1,6,0.5))+
  scale_y_continuous(breaks=seq(0,0.8,0.1))
  

#max EE = 245 --> Megabenthos

#plot
#one subset
#horizontal lines for orig EE
hline<-EE_sub %>% select(-EE) %>% distinct()
hline$EE_orig<-as.numeric(hline$EE_orig)

ggplot(data=EE_sub,aes(x=groups,y=EE))+
  #geom_boxplot(outlier.shape = NA)+
  geom_jitter(position = position_jitter(width = 0.15))+
  geom_hline(yintercept = 1,color="red")+
  geom_point(data=hline, aes(x=groups,y=EE_orig), shape=95, size=20,color="blue")

#loop over all subsets
for (i in 1:4) {
  EE_sub<-EE %>% filter(TL_bin == i)
  #horizontal lines for orig EE
  hline<-EE_sub %>% select(-EE) %>% distinct()
  hline$EE_orig<-as.numeric(hline$EE_orig)
  #plotting
  print(ggplot(data=EE_sub,aes(x=groups,y=EE))+
          geom_jitter()+
          geom_hline(yintercept = 1,color="red")+
          geom_point(data=hline, aes(x=groups,y=EE_orig), shape=95, size=20,color="blue")
        )
}

#Try plotting as density plots
#one subset
#vertical line for original EE
vline<-EE_sub %>% select(-EE) %>% distinct()
vline$EE_orig<-as.numeric(vline$EE_orig)

ggplot(data=EE_sub,aes(x=EE))+
  #geom_boxplot(outlier.shape = NA)+
  geom_density(alpha = 0.4)+
  facet_wrap(vars(groups),nrow = 3,scales = "free")+
  geom_vline(xintercept = 1,color="red")+
  geom_vline(data = vline, aes(xintercept = EE_orig), color = "blue")

#loop over all subsets
for (i in 1:4) {
  EE_sub<-EE %>% filter(TL_bin == i)
  #horizontal lines for orig EE
  vline<-EE_sub %>% select(-EE) %>% distinct()
  vline$EE_orig<-as.numeric(vline$EE_orig)
  #plotting
  print(ggplot(data=EE_sub,aes(x=EE))+
          #geom_boxplot(outlier.shape = NA)+
          geom_density(alpha = 0.4)+
          facet_wrap(vars(groups),nrow = 3,scales = "free")+
          geom_vline(xintercept = 1,color="red")+
          geom_vline(data = vline, aes(xintercept = EE_orig), color = "blue"))
}




