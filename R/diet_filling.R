#Diet filling
#Brute force population of diet matrix

#Last modified: Tue Jul 13 17:44:42 2021 ------------------------------


library(readr);library(here)
#GOM_Diet_Matrix <- read_csv("outputs/GOM_Diet_Matrix.csv")

#Remove extraneous columns
diet<-GOM.diet


for (i in 1:length(diet$Rpred)){
  temp_group<-diet$Rprey[i]
  REco.params$diet[Group==temp_group,(diet$Rpred[i]):=diet$preyper[i]]
}

REco.params$diet[REco.params$diet==0]<-NA

#write.csv(REco.params$diet,"diet_filled.csv")


