#MDS
#Go back and pull / save proper files from tails script

#all biomasses
biomass.all<-data.frame(matrix(nrow = 58,ncol=length(alt.flows)))
for(i in 1:length(alt.flows)){
  biomass.all[,i]<-alt.models[[i]]$Biomass[1:58]
}
rownames(biomass.all)<-groups[1:58]