#Title: 

#Purpose: This script allows for visualization of Ecosense output models.
#         Specifically, it looks at how EEs of all groups in model ensemble
#         compare to initial balanced model.

#Datafile:

#Author: Sarah J. Weisberg
#Contact details: sarah.j.weisberg@stonybrook.edu

# Thu Nov  3 12:09:13 2022 ------------------------------

#Load needed packages
library(here); library(dplyr); library(tidyr)

#Can't put all species/groups on the same plot
#Bin based on TL in original model
#Load original model
load(here("outputs/GOM_Rpath.RData"))

#Pull out TLs & EEs, assign to 4 bins (56/4 = 14 per bin)
#Ignoring fleets for now
TL<-as.data.frame(cbind(GOM$TL[1:56],GOM$EE[1:56]))
colnames(TL) <- c("TL","EE")
TL<-TL %>% mutate(TL_bin = ntile(TL, n=4))

#pull EEs from Ecosense output
EE<-c()
for (i in 1:length(alt.models)){
  model<-alt.models[[i]]
  EE_i<-model$EE
  EE<-rbind(EE,EE_i)
}

