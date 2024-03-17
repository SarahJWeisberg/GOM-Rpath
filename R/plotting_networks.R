#plotting experiments
# Wed Feb 28 14:17:25 2024 ------------------------------
library(igraph)
library(fluxweb)

A<-enaStructure(orig.network)$A
g<-igraph::graph_from_adjacency_matrix(A)

plot(g)

load("BalticFW.Rdata")

plotfw(g,edge.width=0.3, edge.arrow.size=0.3,nylevel = 7,ynum=5)
# Color the functional groups
colFG<- c("orange", "khaki", "blue", "green", "cyan")
# Assign the colors to each node (taxon)
info$colfg <- colFG[as.numeric(info$fg)]

#Visual representation parameter
Vscale <- 25 #multiplying factor
Vmin <- 4 #minimum size of the node
#scale the size of the node to the mean biomass
nodmax<-max(GOM$Biomass[1:56])
nodmax <- max(info$meanB)
sizeB <- (info$meanB/nodmax)*Vscale +Vmin
sizeG <- (GOM$Biomass[1:57]/nodmax)*Vscale +Vmin
#deal with detritus
sizeG[57]<-35

#used plotfw code to develop df with x and y positions
coo_df$size<-sizeG

ggplot(coo_df,aes(x=xnod,y=tl,size=size))+
  geom_point()

plotfw(g, col=info$colfg, size=sizeG,
       edge.width=0.3, edge.arrow.size=0.3,lab = groups[1:57],labcex = 1)

netmatrix <- get.adjacency(net, sparse=F)
fluxes <- fluxing(netmatrix, info$meanB, info$losses, 
                  info$efficiencies, ef.level="prey")

# Create a network with link weights
netLW <- graph_from_adjacency_matrix(fluxes, weighted=TRUE)

# Set visual parameters
Escale <- 15 #multiplying coefficient
Emin <- 0.1 #minimum width 

# Calculate the width of the arrows
# proportional to the square root of the fluxes
wid <- Emin+(sqrt(E(netLW)$weight)/max(sqrt(E(netLW)$weight))*Escale)

# Remove the border of the frame
V(netLW)$frame.color=NA

# Plot the network
plotfw(netLW,col=info$colfg,size=sizeB,
       lab = info$species,labcex = 0.5,
       edge.width=wid, edge.arrow.size=0.25)

# Load required packages
library(igraph)
library(ggraph)

# Sample data (nodes and edges)
nodes <- data.frame(name = c("Plant", "Herbivore", "Carnivore"),
                    group = c("Plants", "Animals", "Animals"))

edges <- data.frame(from = c("Plant", "Herbivore"),
                    to = c("Herbivore", "Carnivore"),
                    weight = c(1, 0.5))

# Create a graph object
graph <- graph_from_data_frame(edges, vertices = nodes, directed = TRUE)

# Plot using ggplot2
g <- ggraph(graph, layout = "circle") +
  geom_edge_link(aes(edge_width = weight), alpha = 0.5) +
  geom_node_point(aes(color = group), size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, size = 5) +
  theme_void()

# Print the plot
print(g)

#try using ggnetwork
ggplot(net, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "grey50") +
  geom_nodetext(aes(label = ggnetwork(net)$name),
                fontface = "bold")+
  theme_blank()

#this does seem to fix the vertices in the right places
test_matrix<-coo[1:14,]
ggplot(ggnetwork(emon[[1]], layout = test_matrix),aes(x, y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(length = unit(3, "pt"), type = "closed")) +
  geom_nodes() +
  theme_blank()
test<-ggnetwork(emon[[1]], layout = test_matrix)

n<-ggnetwork(orig.network,layout=coo_mat,weights="flow")
#bind with TL info
TL<-as.data.frame(cbind(GOM$TL,groups)) %>% rename(vertex.names=groups,TL=V1)
TL<-TL %>% mutate(TL = as.numeric(TL)) %>% mutate(TL = round(TL,2))
n<-left_join(n,TL,by="vertex.names")


#set minimum size
size_min<-0.1
n<-n %>% mutate(storage_adjust = ifelse(storage < 0.1,storage*25,storage))
#adjust Detritus storage so it doesn't swamp everything
n <- n %>% mutate(storage_adjust = ifelse(storage >50,sqrt(storage),storage_adjust))

# Set visual parameters
Escale <- 15 #multiplying coefficient
Emin <- 0.1 #minimum width 
Emin<-0.01
wid<-Emin+(sqrt(n$flow)/max(sqrt(n$flow),na.rm = T))

ggplot(n,aes(x, y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(length = unit(4, "pt"), type = "open"),
             curvature = 0.2,position="jitter",
             aes(color=TL,linewidth=flow)) +
  scale_color_viridis(option = "turbo")+
  #scale_color_tron()+
  scale_linewidth(range = c(0.2,3))+
  geom_nodelabel(aes(label=vertex.names,size=(sqrt(storage_adjust))),show.legend = F) +
  #geom_nodes(aes(size=sqrt(storage)))+
  guides(linewidth=F)+
  theme_blank()

GOM_web

ggsave(filename = "outputs/webplot_GOM.png", plot=GOM_web)

