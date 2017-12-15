#Selection of Compi??gne's data only
#JSON to CSV (to convert the string values to int values)
library(jsonlite)
data = fromJSON("/Users/julesvercoustre/Documents/UTC/TX/TX/data/dataset.json")
data = sapply(data,rbind)
head(data)
write.csv(data, "data/mobilite_compiegne.csv")

#-------------------------------
#Working with the csv file
dataset <- read.csv("data/mobilite_compiegne.csv", na.strings="", header=T)
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
dataset <- mobility
#DATASET SUCCESSFULLY CREATED
#-------------------------------


#We now want to select the data from Compiegne's area
#require(ggmap)
compiegne_data <- dataset[which(round(dataset$lat,1)==49.4 | round(dataset$lng)==2.8),]
#compiegne_GPS <- c(lat=49.39222, lon=2.789286)
#compiegne_map=get_map(location=compiegne_GPS,zoom=12)
#ggmap(compiegne_map)
#ggmap(compiegne_map)+geom_point(data=compiegne_data, aes(x=compiegne_data$lng,y=compiegne_data$lat), col="blue", size=1)
compiegne_data_vehicule <- compiegne_data[compiegne_data$transportation_mode == "Vehicule",]
compiegne_data = unique(setDT(compiegne_data), by = c("id", "date"))




