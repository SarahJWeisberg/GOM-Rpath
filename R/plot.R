#code to visualize GOM food web
#uses network model

# Fri Mar 15 15:37:46 2024 ------------------------------

#load required packages
library(igraph)
library(ggplot2)
library(ggnetwork)
library(viridis)
library(devtools)
#need to install an older version of gdata to get enaR to work
install.packages("gdata", repos = "https://packagemanager.posit.co/cran/2023-05-06") 
install_github('SEELab/enaR', force = T)
library(gdata)
library(enaR)

#Load initial model
load(here("outputs/GOM_params_Rpath.RData"))
load(here("outputs/GOM_Rpath.RData"))

#Count number of each group type
#ngroups <- nrow(GOM.params)
nliving <- nrow(GOM.params$model[Type <  2, ])
ndead   <- nrow(GOM.params$model[Type == 2, ])

#run enaR_sense.R to get orig.network
#pull network structure
A<-enaStructure(orig.network)$A
g<-igraph::graph_from_adjacency_matrix(A)

#get x and y coordinates
#derived from the plotfw function (https://rfrelat.github.io/BalticFoodWeb.html)
nylevel<-7 #determines number of levels along y-axis
n <- igraph::vcount(g) #number of vertices
tl<-GOM$TL[1:(nliving+ndead)] #pull trophic level calculations from rpath
bks <- c(0.9, seq(1.9, max(tl), length.out = nylevel))
ynod <- cut(tl, breaks = bks, include.lowest = TRUE, 
            labels = 1:(length(bks)-1)) #assign group to a y-level
maxx <- max(table(ynod)) #looks for max # of groups at any y-level
xnod <- rep(0,n)
for (i in 1:nylevel){
  l <- sum(ynod==i)
  
  ltr <- (l/maxx)**(1/2)*maxx
  if (l>1) {
    xnod[ynod==i] <- seq(-ltr,ltr,length.out = l)
  } else {
    xnod[ynod==i] <- 0
  }
}

coo <- cbind(xnod,tl) #these inform x and y coordinates

#make manual adjustments if necessary
#eg., scootch Bacteria to the left to get more space in the bottom of the web
coo[2,1]<-(-10)
coo[31,1]<-7
coo[48,1]<-16

#use ggnetwork to create a network geometry
#storage and flows from original model
n<-ggnetwork(orig.network,layout=coo,weights="flow")
#bind with TL info
TL<-as.data.frame(cbind(GOM$TL,groups)) %>% rename(vertex.names=groups,TL=V1)
TL<-TL %>% mutate(TL = as.numeric(TL)) %>% mutate(TL = round(TL,2))
n<-left_join(n,TL,by="vertex.names")

#adjust Detritus storage so it doesn't swamp everything
det<-max(GOM$Biomass)
n <- n %>% mutate(storage_adjust = ifelse(storage == det,100,storage))

#plot!
GOM_web<-ggplot(n,aes(x, y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(length = unit(7, "pt"), type = "open"),
             curvature = 0.15,position="jitter",
             aes(color=TL,linewidth=flow)) +
  scale_color_gradientn(colors = turbo(6))+ #sets color scale
  scale_linewidth(range = c(0.15,9))+ #sets arrow width range
  geom_nodelabel(aes(label=vertex.names,size=storage_adjust),show.legend = F) +
  scale_size(range=c(2.5,10))+ #sets label size range
  guides(linewidth="none")+
  annotate("text",x=0.025,y=0.975,label="GOM",size=9)+
  theme_blank(legend.position=c(0.8,0.2))+
  theme(panel.background = element_rect(fill="#EEEEEEFF"),
        plot.background = element_rect(fill="#EEEEEEFF"),
        legend.background=element_rect(fill="#EEEEEEFF"))

GOM_web

ggsave(filename = "outputs/webplot_GOM.png",width = 35, height=25,units = c("cm"),plot=GOM_web,dpi=300)

