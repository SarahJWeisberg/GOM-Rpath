#Diet filling
#Brute force population of diet matrix

#Last modified: Tue Jul 13 17:44:42 2021 ------------------------------


library(readr);library(here)
#GOM_Diet_Matrix <- read_csv("outputs/GOM_Diet_Matrix.csv")

#Rename
diet<-GOM.diet

#Change "Discard" to Discards"
diet[Rprey=="Discard",Rprey:="Discards"]

for (i in 1:length(diet$Rpred)){
  temp_group<-diet$Rprey[i]
  REco.params$diet[Group==temp_group,(diet$Rpred[i]):=diet$preyper[i]]
}

#write.csv(REco.params$diet,"diet_filled.csv")


