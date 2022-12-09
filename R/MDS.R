#MDS
#Go back and pull / save proper files from tails script

#all biomasses
#excluding discards and detritus just for fun
biomass.all<-data.frame(matrix(ncol = 56,nrow=length(alt.flows)))
for(i in 1:length(alt.flows)){
  biomass.all[i,]<-alt.models[[i]]$Biomass[1:56]
}
colnames(biomass.all)<-groups[1:56]

#isolate extremes
low<-which(ASC.CAP$ASC.CAP <= low_tail)
high<-which(ASC.CAP$ASC.CAP>=high_tail)

#name columns
rownames(biomass.all)<-paste(rep("model",length(alt.flows)),1:length(alt.flows))

d<-dist(biomass.all)
fit<-cmdscale(d,eig=TRUE,k=2)

x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS", pch=19,col = ifelse(x %in% x[low],"red",ifelse(x %in% x[high],"blue","black")))

#repeat for flows
#all flows
flows.all<-data.frame(matrix(ncol = 57,nrow = length(alt.flows)))
for(i in 1:length(alt.flows)){
  flow<-alt.flows[[i]]
  flows.all[i,]<-flow$T
}
colnames(flows.all)<-groups[1:57]
rownames(flows.all)<-paste(rep("model",length(alt.flows)),1:length(alt.flows))

d<-dist(flows.all)
fit<-cmdscale(d,eig=TRUE,k=2)

x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS", pch=19,col = ifelse(x %in% x[low],"red",ifelse(x %in% x[high],"blue","black")))

