####################################
#PREAMBULE
#---
#This script allows to collect data from json and build a whole dataframe
#which will be used for classifications
####################################

#Selection of world data
#JSON to CSV (to convert the string values to int values)
#Here we combine two json files. One can change the data to load by remplacing values 
#and removing the "bind" function that help to combine our two json files.
library(jsonlite)
data = fromJSON("data/position.json")
data = sapply(data,rbind)
data2 = fromJSON("data/dataset.json")
data2 = sapply(data2,rbind)

write.csv(rbind(data,data2), "data/world_data.csv")


#-------------------------------
#Working with the csv file
dataset <- read.csv("data/world_data.csv", na.strings="", header=T)
head(dataset)
dataset <- dataset[,-1]
head(dataset)

#Loop for correct timestamp in "dataset" (export from "data" to "dataset")
x<-c()
for (i in 1:length(dataset[,2]))
{
  dataset[i,2] <- dataset[i,2]/1000
  x <- c(x,dataset[i,2])
  #This operation can be long
}
y <- c(as.POSIXct(as.numeric(as.character(x)), origin='1970-01-01'))
#Y should normally be an array of date strings

#Recreate a dataframe from scratch
id <- dataset$userId
date <- y
lat <- dataset$lat
lng <- dataset$lng
mode <- dataset$mode

transportation_mode <- c()
#Make correspondance btw mode number and transportation name
for (i in 1:length(mode))
{
  if(mode[i] != 0)
  {
    temp1 <- mode[i]  
    temp2 <- switch(temp1,"Biking","On foot","Still","Unclassified","NONE","Tilting","Fast Walk","Run","Entering Geofence","Exiting Geofence","tracking service started","tracking service stopped")
    transportation_mode <- c(transportation_mode,temp2)
  }
  else
  {
    transportation_mode <- c(transportation_mode,"Vehicule")
  }
}

mobility <- data.frame(id,date,lat,lng,mode,transportation_mode)

#DATASET SUCCESSFULLY CREATED
#-------------------------------

library(data.table)
#We now want to select the data from world's area
world_data = mobility
world_data = unique(setDT(world_data), by = c("id", "date"))

world_data=as.data.frame(world_data)
