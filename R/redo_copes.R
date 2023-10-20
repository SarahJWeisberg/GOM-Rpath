# Title: RPath model balancing
# Purpose: Copepod groups are rearranged
#         from EMAX categorization.

# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu


# Sat Jan 28 11:09:21 2023 ------------------------------

#Reallocate Copepod Biomass
#Lg Copepods
biomass[5]<- 9.196
#SmCopepods
biomass[6]<-13.162
#Fill model
GOM.params$model[,Biomass:=biomass]

#Diet proportion
#Special groups (based on lit review: AmLobster, AtlHerring, AtlMackerel, RiverHerring, SmCopepods)
#For most groups reallocate based on est. proportion of Calanus in original LgCope group
cal_lg<-9.196/11.955
#For AtlHerring, use ratio of Calanus to all copepods
cal_all<-9.196/(9.196+13.162)
#need to get rid of NAs first
GOM.params$diet[6,2:57]<-replace(GOM.params$diet[6,2:57],is.na(GOM.params$diet[6,2:57]),0)
GOM.params$diet[5,2:57]<-replace(GOM.params$diet[5,2:57],is.na(GOM.params$diet[5,2:57]),0)

#Shift AmLobster[12] predation
#Shift 0.05% from Macrobenthos[11] to LgCopepods[5]
GOM.params$diet[11,13]<-GOM.params$diet[11,13]-0.005
GOM.params$diet[5,13]<-GOM.params$diet[5,13]+0.005
#Shift 0.05% from Macrobenthos[11] to SmCopepods[6]
GOM.params$diet[11,13]<-GOM.params$diet[11,13]-0.005
GOM.params$diet[6,13]<-GOM.params$diet[6,13]+0.005

#Shift AtlHerring[21] predation
GOM.params$diet[6,22]<-GOM.params$diet[6,22]+GOM.params$diet[5,22]*(1-cal_all)
GOM.params$diet[5,22]<-GOM.params$diet[5,22]*cal_all

#Shift AtlMackerel[27] predation
#Remove 6% from Macrobenthos[11]
#Remove 6% from Micronekton[7]
#Remove 18% from LgCopepods[5]
#Move 30% to SmCopepods[6]
GOM.params$diet[11,28]<-GOM.params$diet[11,28]-0.06
GOM.params$diet[7,28]<-GOM.params$diet[7,28]-0.06
GOM.params$diet[5,28]<-GOM.params$diet[5,28]-0.18
GOM.params$diet[6,28]<-GOM.params$diet[6,28]+0.3

#Shift RiverHerring[18] predation
GOM.params$diet[6,19]<-GOM.params$diet[5,19]*0.75
GOM.params$diet[5,19]<-GOM.params$diet[5,19]*0.25

#Shift SmCopepod[6] predation
GOM.params$diet[5,7]<-GOM.params$diet[6,7]*0.05
GOM.params$diet[6,7]<-GOM.params$diet[6,7]*0.95

#Shift 3% predation from Microzooplankton[3] to Bacteria[2]
GOM.params$diet[2,7]<-GOM.params$diet[2,7]+0.03
GOM.params$diet[3,7]<-GOM.params$diet[3,7]-0.03

#Shift LgCopepod[5] predation
#Shift 1% predation from Microzooplankton[3] to Bacteria[2]
GOM.params$diet[2,6]<-GOM.params$diet[2,6]+0.01
GOM.params$diet[3,6]<-GOM.params$diet[3,6]-0.01

#For rest of groups, add portion to SmCope consumption
GOM.params$diet[6,c(2:5,7:11,13:17,19:20,22:26,28:57)]<-
  GOM.params$diet[6,c(2:5,7:11,13:17,19:20,22:26,28:57)]+
  GOM.params$diet[5,c(2:5,7:11,13:17,19:20,22:26,28:57)]*(1-cal_lg)
GOM.params$diet[5,c(2:5,7:11,13:17,19:20,22:26,28:57)]<-
  GOM.params$diet[5,c(2:5,7:11,13:17,19:20,22:26,28:57)]*cal_lg


#Shift AtlHerring predation


check.rpath.params(GOM.params)
 