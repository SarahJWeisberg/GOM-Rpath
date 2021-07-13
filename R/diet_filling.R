#Diet filling
#Brute force population of diet matrix

#Last modified: Tue Jul 13 17:44:42 2021 ------------------------------


library(readr);library(here)
GOM_Diet_Matrix <- read_csv("outputs/GOM_Diet_Matrix.csv")

#Remove extraneous columns
diet<-GOM_Diet_Matrix[,-c(1,5)]

#Loop
for (i in 1:length(diet$Rpred)){
  temp_group<-diet$Rprey[i]
  test[Group==temp_group,(diet$Rpred[i]):=diet$preyper[i]]
}


for (i in 1:length(diet$Rpred)){
  temp_group<-diet$Rprey[i]
  REco.params$diet[Group==temp_group,(diet$Rpred[i]):=diet$preyper[i]]
}

test[is.na(test)]<-0

write.csv(test,"diet_filled.csv")


