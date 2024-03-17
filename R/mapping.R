#generate map for pub

remotes::install_github('NEFSC/NEFSC-Spatial')
#remotes::install_github('James-Thorson-NOAA/FishStatsUtils')
#remotes::install_github("NOAA-EDAB/ecodata")
library(NEFSCspatial)
library(sf)
library(sars)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rmapshaper)
library(marmap)
library(RColorBrewer)
library(ggspatial)
library(gridExtra)

plot(NEFSCspatial::BTS_Strata)
bts<-NEFSCspatial::BTS_Strata

MAB <- c(1010:1080, 1100:1120, 1600:1750, 3010:3450, 3470, 3500, 3510)
GB  <- c(1090, 1130:1210, 1230, 1250, 3460, 3480, 3490, 3520:3550)
GOM <- c(1220, 1240, 1260:1290, 1360:1400, 3560:3830)

bts_area<- bts %>% 
  mutate(EPU = ifelse(STRATA %in% MAB,"MAB",ifelse(STRATA %in% GB,"GB",ifelse(STRATA %in% GOM,"GOM","other"))))

theme_set(theme_bw())

#load in countries for plotting 
world <- ne_countries(scale = "medium", returnclass = "sf")

#generate region outline polygons
Region <- bts_area %>% filter(!EPU == "other") %>%
  st_make_valid() %>% st_buffer(0) %>% group_by(EPU) %>% summarize()
Region <- ms_filter_islands(Region, min_area=10000000000) #get rid of that weird one in the gulf of maine 

# call in bathymetric data
# convert  bathymetric contours into dataframe
bathy <- getNOAA.bathy(-80, -64.5, 34, 46, resolution=1); bathydf <- as.xyz(bathy) 
#ignore positive depth (land)
bathy_sea <- bathydf; bathy_sea$V3[bathy_sea$V3 > 1] <- 0
#ignore depths greater than 500m
bathy_shelf<-bathy_sea %>% filter(V3 > -500) 

#make table of summary stats for each model
#GOM
load(here("outputs/GOM_params_Rpath.RData"))
load(here("outputs/GOM_Rpath.RData"))
GOM<-cbind(nrow(GOM.params$model[Type <  2, ]),nrow(GOM.params$model[Type == 2, ]),nrow(GOM.params$model[Type == 3, ]))
#GB
load(here("outputs/GB.params.bal.rda"))
GB<-cbind(nrow(GB.params.bal$model[Type <  2, ]),1,nrow(GB.params.bal$model[Type == 3, ]))
#MAB
load("~/Desktop/MAB-Rpath/outputs/MAB_params_Rpath_no_disc.RData")
MAB<-cbind(nrow(MAB.rpath.params$model[Type <  2, ]),nrow(MAB.rpath.params$model[Type == 2, ]),nrow(MAB.rpath.params$model[Type == 3, ]))
summary_stats<-data.frame(rbind(GOM,GB,MAB))
rownames(summary_stats)<-c("GOM","GB","MAB")
colnames(summary_stats)<-c("Living Groups","Detrital Groups","Fleets")
#get rid of detrital groups for visuals
summary_stats<-summary_stats %>% dplyr::select(-`Detrital Groups`)
tt<-ttheme_default(core=list(bg_params = list(fill=c("#377EB8","#FF7F00","#E41A1C"),alpha=0.7)))

#plot  
NEUS <- ggplot() +
  geom_sf(data = world)+
  geom_raster(data = bathy_shelf, aes(x = V1, y = V2, fill = V3)) +
  scale_fill_gradient(low="#586575", high="#daedf7",
                      breaks = c(-400,-250,-100),labels=c(400,250,100))+
  geom_sf(data = Region, fill = NA, linewidth = .5, aes(color=EPU))+
  scale_color_manual(values = c("#FF7F00","#377EB8","#E41A1C"))+
  geom_sf(data = world)+
  coord_sf(xlim =c(-78, -65.5), ylim = c(35, 45)) + #zoomed to Hatteras and N
  labs(x = NULL, y = NULL,fill= "Depth (m)") +
  guides(color="none",fill=guide_colorbar(reverse = T))+
  annotate("text",x=-72.5,y=38.5,label="MAB",size=4)+
  annotate("text", x = -71.5, y = 43, label = "GOM", size = 4)+
  annotate("text", x = -66, y = 41, label = "GB", size = 4)+
  annotation_custom(tableGrob(summary_stats,theme = tt),xmin=-69, xmax=-65.5, ymin=36.25, ymax=38)+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white")) +
  theme(plot.margin = margin(0, 0, 0, 0, "cm"),
        panel.grid.major = element_blank(),
        #panel.border = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.85,0.1),
        legend.title = element_text(face="italic"),
        legend.direction = "horizontal")

NEUS
