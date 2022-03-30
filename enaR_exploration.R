#Getting familiar with enaR
#Looking at balanced GOM model, EMAX GOM model

#Author: Sarah J. Weisberg

# Tue Oct 12 15:40:06 2021 ------------------------------


#install.packages("enaR")

#Install package
#Note that I had to pull from github; CRAN version is depreciated
#install.packages("xlsx")
library(devtools);library(xlsx)
install_github('SEELab/enaR')
library(enaR)
install.packages("sna")
library(sna)


#going to try and build a network model from my rpath_balanced_3
#Sean don't make fun of me!
#Simplest translation / understanding of flow, respiration
#building on MDMAR02 example

#Need to calculate respiration, exports, detritus input
#To do so, need to have pull unassim, M0, and F (fishing mortality)

#Count number of each group type
#ngroups <- nrow(REco.params)
nliving <- nrow(REco.params$model[Type <  2, ])
ndead   <- nrow(REco.params$model[Type == 2, ])
#ngear   <- nrow(REco.params$model[Type == 3, ])

#Pull diet matrix
diet<-REco$DC

#Get consumption values by DC*QB*B
QQ<-matrix(nrow = (nliving + ndead + 1),ncol=nliving)

for (i in 1:nliving){
  QQ[,i]<-diet[,i]*qb[i]*biomass[i]
}
#Ignore Imports
QQ<-QQ[1:58,]

colnames(QQ)<-groups[1:56]
rownames(QQ)<-groups[1:58]


#Sum discards
Discards<-rowSums(REco$Discards)
Discards<-Discards[1:58]

#Calculate flow to detritus
M0<-REco$PB*(1-REco$EE)
Detritus<-M0*biomass+qb*biomass*REco$Unassim
Detritus<-Detritus[1:58]

#Deal with flow to detritus from discards
#Should be equal to all flow to discards minus consumption by SeaBirds(45)
DetInDisc<-sum(Discards)
Detritus[58]<-DetInDisc-QQ[58,45]

#Flow to detritus from detritus = 0
Detritus[57]<-0

#Bind diet matrix (QQ) with flow to detritus, discards
QQ<-cbind(QQ,Detritus,Discards)
 
#Calculate exports
#First sum catch
Catch<-rowSums(REco$Landings)

#Add positive biomass accumulation terms
Export<-Catch+(ifelse(REco$BA>0,REco$BA,0))
Export<-Export[1:58]

#Other ideas for export - all BA terms, or none
#Export<-Catch+REco$BA
#Export<-Export[1:58]
#Export<-Catch[1:58]

#Calculate respiration
#Assume detritus, discards have 0 respiration
Resp<-((1-REco$Unassim)*qb-pb)*biomass
Resp<-Resp[1:58]
Resp[57:58]<-0

#Deal with Primary Production
#First, estimate GROSS production = Imports
#P/B in Ecopath model gives NET production
#Ratio of gross:net is going to be fixed based on EMAX
gross_net<-4101.9/3281.5
gross<-gross_net*pb[1]*biomass[1]
Resp[1]<-gross-(pb[1]*biomass[1])

#Calculate imports
#Negative biomass accumulation terms
#Gross primary production
Import<-abs(ifelse(REco$BA<0,REco$BA,0))
Import[1]<-gross
Import<-Import[1:58]

#Translate biomass
Biomass<-biomass[1:58]

#Now need to get this into enaR format
#First method = dumb = put into csv
#Bind all outputs
#REco.enam<-cbind(QQ,DetIn,Disc,Catch,Resp)
#REco.enam<-rbind(REco.enam,Import,Biomass)
#save output
#write.csv(REco.enam,"outputs/GOM_enaR_test.csv")

#Second method = less dumb
#Pack the model directly
GOM.flow<-pack(flow = QQ,
               input = Import,
               export = Export,
               living = c(rep(TRUE,56),rep(FALSE,2)),
               respiration = Resp,
               storage = Biomass)

#Look at model summary
F.GOM<-enaFlow(GOM.flow,balance.override = T)
F.GOM$ns

A1<-enaAscendency(GOM.flow)
A1

#Loading existing models
#Specifically look at EMAX GoM model
#Hope that will help me understand how to map from EcoPath
data("troModels")
GOM<-troModels$"Gulf of Maine"
summary(GOM)

F<-enaFlow(GOM)
attributes(F)

A<-enaAscendency(GOM)
A

ssCheck(GOM)

C<-enaControl(GOM)
C$sc

#Fiddle with formatting -- get to match their example
#save and reupload as XLSX
m <- read.enam("GOM_enaR_test.xlsx")
ssCheck(m)

F1<-enaFlow(m,balance.override = T)
F1

A1<-enaAscendency(GOM)
A1

A2<-enaAscendency(m)
A2

mti<-enaMTI(m)

n<-unpack(m)

#Playing with plotting
#Copying code from vignette

## Set colors to use
my.col <- c('red','yellow',rgb(204,204,153,maxColorValue=255),'grey22')
## Extract flow information for later use.
F <- as.matrix(m,attrname='flow')
## Get indices of positive flows
f <- which(F!=0, arr.ind=T)
opar <- par(las=1,bg=my.col[4],xpd=TRUE,mai=c(1.02, 0.62, 0.82, 0.42))

set.seed(2)
set.seed(2)
plot(m,
     ## Scale nodes with storage
     vertex.cex=log(m%v%'storage'),
     ## Add node labels
     #label= m%v%'vertex.names',
     #boxed.labels=FALSE,
     #label.cex=0.65,
     #label.col='white',
     ## Make rounded nodes
     vertex.sides=45,
     ## Scale arrows to flow magnitude
     edge.lwd=log(abs(F[f])),
     edge.col=my.col[3],
     vertex.col=my.col[1],
     vertex.border = my.col[3],
     vertex.lty = 1,
     xlim=c(-3,3),ylim=c(-4,4))
## Lastly, remove changes to the plotting parameters
rm(opar)

set.seed(2)
plot(m)
p

#try other plotting approaches used in enaR vignette / paper
b <- sna::betweenness(m)
## Get vertex names
nms <- m%v%'vertex.names'
show(nms)
nms[b<=(0.1*max(b))] <- NA
set.seed(2)
opar <- par(xpd=TRUE,mfrow=c(1,1))
## Create target plot showing only
## labels of most central nodes
sna::gplot.target(m,b,
                  edge.col="grey",
                  label=nms,
                  circ.lab = F)



