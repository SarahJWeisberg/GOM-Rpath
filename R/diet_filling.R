#Diet filling
#Brute force population of diet matrix

#Last modified: Tue Jul 13 17:44:42 2021 ------------------------------


library(readr);library(here)

#Rename
diet<-GOM.diet

#Change "Discard" to Discards"
diet[Rprey=="Discard",Rprey:="Discards"]

for (i in 1:length(diet$Rpred)){
  temp_group<-diet$Rprey[i]
  GOM.params$diet[Group==temp_group,(diet$Rpred[i]):=diet$preyper[i]]
}

#write.csv(REco.params$diet,"diet_filled.csv")


