##Exploring Gephi

# Fri Dec  9 15:26:55 2022 ------------------------------


install.packages("rgexf")
library(rgexf)
# Load libraries
library("igraph")
library("plyr")
library("tidyr")
library("dplyr")

#trying to modify code from: https://www.r-bloggers.com/2013/07/network-visualization-part-2-gephi/

#maybe need to pivot data first
flow.long<-pivot_longer(flow.matrix,cols=everything())
#add flow from
flow.long<-cbind(flow.long,rep(groups[1:57],each=57))
colnames(flow.long)<-c("flow.to","value","flow.from")
#reorder
flow.long<-flow.long[,c(3,1,2)]

#get rid of non-zero values as these are not real edges
flow.long<-flow.long %>% filter(value >0)

#turn into basic network (igraph)
net<-graph.data.frame(flow.long)
nodes_df<-data.frame(ID = c(1:length(groups[1:57])), NAME = groups[1:57])
edges_df<-as.data.frame(get.edges(net, c(1:ecount(net))))

#make node attribute df -- want to know biomass, TL
nodes_att<-data.frame(biomass = GOM$Biomass[1:57], TL =GOM$TL[1:57])

#make edges attribute df-- want to know flow value
edges_att<-data.frame(flow=flow.long$value)

# Define node/edge visual attributes - these attributes are the ones used for network visualization
#this probably is wrong!

# Assign visual attributes to nodes (colors have to be 4dimensional - RGBA)
nodes_att_viz <- list(size = nodes_size)
edges_att_viz <-list(size=edges_att$flow)

#
# Calculate node coordinate - needs to be 3D
#nodes_coord <- as.data.frame(layout.fruchterman.reingold(gD, weights = E(gD)$similarity, dim = 3, niter = 10000))
# We'll cheat here, as 2D coordinates result in a better (2D) plot than 3D coordinates
#nodes_coord <- as.data.frame(layout.fruchterman.reingold(net, weights = E(net)$similarity, dim = 2, niter = 10000))
#nodes_coord <- cbind(nodes_coord, rep(0, times = nrow(nodes_coord)))
nodes_att$biomass[57]<-20
nodes_size<-nodes_att$biomass
#set Detritus biomass to 20 (same as phyto) - just to see what happens

write.gexf(nodes = nodes_df, edges = edges_df, nodesAtt = nodes_att, edgesAtt = edges_att, edgesWeight = edges_att$flow,nodesVizAtt = nodes_att_viz, edgesVizAtt = edges_att_viz, 
           defaultedgetype = "undirected", output = "gom.gexf")
