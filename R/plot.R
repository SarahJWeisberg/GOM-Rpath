#code to visualize GOM food web
#uses network model

# Tue Mar  5 13:29:10 2024 ------------------------------

#load required packages
library(igraph)
library(ggplot2)
library(ggnetwork)

#first run enaR_sense.R to get orig.network

#pull network structure
source(here("enaStructure.R"))
A<-enaStructure(orig.network)$A
g<-igraph::graph_from_adjacency_matrix(A)

#get x and y coordinates
#derived from the plotfw function (https://rfrelat.github.io/BalticFoodWeb.html)
nylevel<-7 #determines number of levels along y-axis
n <- vcount(g) #number of vertices
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

#scootch Bacteria to the left to get more space in the bottom of the web
#original x and y
coo[2,1]<-(-14)

#use ggnetwork to create a network geometry
#storage and flows from original model
n<-ggnetwork(orig.network,layout=coo,weights="flow")
#bind with TL info
TL<-as.data.frame(cbind(GOM$TL,groups)) %>% rename(vertex.names=groups,TL=V1)
TL<-TL %>% mutate(TL = as.numeric(TL)) %>% mutate(TL = round(TL,2))
n<-left_join(n,TL,by="vertex.names")

#set minimum size
n<-n %>% mutate(storage_adjust = ifelse(storage < 0.1,storage*16,storage))
#adjust Detritus storage so it doesn't swamp everything
n <- n %>% mutate(storage_adjust = ifelse(storage >50,sqrt(storage),storage_adjust))

#scootch Bacteria to the left to get more space in the bottom of the web
#original x and y
#x_bac<-n %>% filter(vertex.names == "Bacteria") %>% select(x) %>% distinct()
#y_bac<-n %>% filter(vertex.names == "Bacteria") %>% select(y) %>% distinct()
#n <- n %>% mutate(x = ifelse(vertex.names =="Bacteria", 0.1,x)) %>% 
 # mutate(xend = ifelse(xend == x_bac$x & yend == y_bac$y, 0.1,xend))

GOM_web<-ggplot(n,aes(x, y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(length = unit(7, "pt"), type = "open"),
             curvature = 0.175,position="jitter",
             aes(color=TL,linewidth=flow)) +
  scale_color_viridis(option = "turbo")+
  scale_linewidth(range = c(0.2,10))+
  geom_nodelabel(aes(label=vertex.names,size=(sqrt(storage_adjust))),show.legend = F) +
  guides(linewidth=F)+
  theme_blank()

GOM_web

ggsave(filename = "outputs/webplot_GOM.pdf", width = 40, height=25,units = c("cm"), plot=GOM_web,dpi=300)

ggsave(filename = "outputs/webplot_GOM.pdf",plot=GOM_web,dpi=300)
