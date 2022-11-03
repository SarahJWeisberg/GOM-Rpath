#Title: 

#Purpose: This script allows for visualization of Ecosense output models.
#         Specifically, it looks at how EEs of all groups in model ensemble
#         compare to initial balanced model.

#Datafile:

#Author: Sarah J. Weisberg
#Contact details: sarah.j.weisberg@stonybrook.edu

# Thu Nov  3 12:09:13 2022 ------------------------------

#Load needed packages
library(here); library(dplyr); library(tidyr); library(ggplot2)

#Can't put all species/groups on the same plot
#Bin based on TL in original model
#Load original model
load(here("outputs/GOM_Rpath.RData"))

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
  EE_i<-cbind(groups,model$EE[1:56])
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
colnames(EE)<-c("groups", "EE")
EE$EE<-as.numeric(EE$EE)
EE<- left_join(EE,TL,by="groups")


#EE_test<-EE %>% filter(groups %in% c("Phytoplankton","Bacteria","SmCopepods"))

#EE_test$EE<-as.numeric(EE_test$EE)

#plot
for (i in 1:4) {
  EE_sub<-EE %>% filter(TL_bin == i)
  #horizontal lines for orig EE
  hline<-EE_sub %>% select(-EE) %>% distinct()
  hline$EE_orig<-as.numeric(hline$EE_orig)
  #plotting
  print(ggplot(data=EE_sub,aes(x=groups,y=EE))+
    geom_jitter()+
    geom_hline(yintercept = 1,color="red")+
    geom_point(data=hline, aes(x=groups,y=EE_orig), shape=95, size=20,color="blue"))
}

ggplot(data=EE,aes(x=groups,y=EE))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter()

#horizontal lines for orig EE
hline<-EE_sub %>% select(-EE) %>% distinct()
hline$EE_orig<-as.numeric(hline$EE_orig)

ggplot(data=EE_sub,aes(x=groups,y=EE))+
  #geom_boxplot(outlier.shape = NA)+
  geom_jitter(position = position_jitter(width = 0.15))+
  geom_hline(yintercept = 1,color="red")+
  geom_point(data=hline, aes(x=groups,y=EE_orig), shape=95, size=20,color="blue")
