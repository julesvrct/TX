#install.packages("jsonlite")
library(jsonlite)
a<-read_json("data/lines.json", simplifyVector = TRUE)
b<-a[a$number=="5",]
ligne5<-as.data.frame(as.data.frame(as.data.frame(as.data.frame(b$directions)$shape)$features)$geometry)$coordinates


#ggplot2 doesn't know how to deal with data of class matrix i.e ligne5[[1]]
##So we have to convret into table
#ligne5_lon_part1 <- ligne5[[1]][,1]
#ligne5_lat_part1 <- ligne5[[1]][,2]
#Iterate method for plotting red dots of bus line
ligne5_lon_part1 <- c()
ligne5_lat_part1 <- c()
for(i in 1:length(ligne5))
{
  ligne5_lon_part1 <- c(ligne5_lon_part1,ligne5[[i]][,1])
  ligne5_lat_part1 <- c(ligne5_lat_part1,ligne5[[i]][,2])
}
ligne5_part1_coord <- data.frame(ligne5_lon_part1,ligne5_lat_part1)
require(ggmap)
ligne5_part1=c(lat=49.41255914140619, lon=2.814817428588867)
ligne5_map=get_map(location=ligne5_part1,zoom=14)
ggmap(ligne5_map)
#From here needs to be launched "compy_data.R" script
source('functions/compy_data.R')
ggmap(ligne5_map)+ geom_point(data=ligne5_part1_coord, aes(x=ligne5_part1_coord$ligne5_lon_part1,y=ligne5_part1_coord$ligne5_lat_part1), col="red", size=0.5, pch=1)+ geom_point(data=compiegne_data, aes(x=compiegne_data$lng,y=compiegne_data$lat), col="blue", size=0.5, pch=1)

###### Use of UTM coordinates  ######
#"31" est le fuseaux francais
#http://www.ign.fr/sites/all/files/geodesie_projections.pdf
#https://www.maptools.com/tutorials/utm/why_use_utm
source('functions/LongLatToUTM.R')
position <- LongLatToUTM(ligne5_lon_part1,ligne5_lat_part1,31)
plot(position$X, position$Y)


####### Classification sur tout les points de compiegne ######
source('functions/classification.R')
dataphone = data.frame(X=compiegne_data$lng,Y=compiegne_data$lat)
dataligne = data.frame(X=ligne5_part1_coord$ligne5_lon_part1, Y=ligne5_part1_coord$ligne5_lat_part1)
dataphone = LongLatToUTM(dataphone$X,dataphone$Y,31)
dataligne = LongLatToUTM(dataligne$X, dataligne$Y, 31)
dataphone = dataphone[,-1]
dataligne = dataligne[,-1]
y = classification(dataligne,dataphone)

on_bus_line = y
as.data.frame(table(y))
points = data.frame(X=dataphone$X,Y=dataphone$Y,on_line=on_bus_line)
points[points$on_line==1,]

#Plotting points on line
j<-100
a<-as.numeric(dataligne$Y[j]-dataligne$Y[j+1])/(dataligne$X[j]-dataligne$X[j+1])
b<- dataligne$Y[j+1]-a*dataligne$X[j+1]

plot(dataphone$X,dataphone$Y, col="red" ,xlim=c(484529,487349),ylim=c(5470540,5474348))
par(new=TRUE)
plot(dataligne$X,dataligne$Y, col="green",xlim=c(484529,487349),ylim=c(5470540,5474348))
par(new=TRUE)
curve(a*x+b,xlim=c(484529,487349),ylim=c(5470540,5474348))
par(new=TRUE)
curve(a*x+b+200,xlim=c(484529,487349),ylim=c(5470540,5474348))
par(new=TRUE)
curve(a*x+b-200,xlim=c(484529,487349),ylim=c(5470540,5474348))
par(new=TRUE)
abline(v=dataligne$X[j]-200)
par(new=TRUE)
abline(v=dataligne$X[j+1]+200)
par(new=TRUE)
plot(points[points$on_line==1,]$X,points[points$on_line==1,]$Y,col="blue" ,xlim=c(484529,487349),ylim=c(5470540,5474348))




######  Classification sur tout les points de compiegne classes VEHICULE  ######
source('functions/classification.R')
dataphone = data.frame(X=compiegne_data_vehicule$lng,Y=compiegne_data_vehicule$lat)
dataligne = data.frame(X=ligne5_part1_coord$ligne5_lon_part1, Y=ligne5_part1_coord$ligne5_lat_part1)
dataphone = LongLatToUTM(dataphone$X,dataphone$Y,31)
dataligne = LongLatToUTM(dataligne$X, dataligne$Y, 31)
dataphone = dataphone[,-1]
dataligne = dataligne[,-1]
y = classification(dataligne,dataphone)

on_bus_line = y
as.data.frame(table(y))
points = data.frame(X=dataphone$X,Y=dataphone$Y,on_line=on_bus_line)
points[points$on_line==1,]

#Plotting points on line
j<-100
a<-as.numeric(dataligne$Y[j]-dataligne$Y[j+1])/(dataligne$X[j]-dataligne$X[j+1])
b<- dataligne$Y[j+1]-a*dataligne$X[j+1]

plot(dataphone$X,dataphone$Y, col="red" ,xlim=c(484529,487349),ylim=c(5470540,5474348))
par(new=TRUE)
plot(dataligne$X,dataligne$Y, col="green",xlim=c(484529,487349),ylim=c(5470540,5474348))
par(new=TRUE)
curve(a*x+b,xlim=c(484529,487349),ylim=c(5470540,5474348))
par(new=TRUE)
curve(a*x+b+200,xlim=c(484529,487349),ylim=c(5470540,5474348))
par(new=TRUE)
curve(a*x+b-200,xlim=c(484529,487349),ylim=c(5470540,5474348))
par(new=TRUE)
abline(v=dataligne$X[j]-200)
par(new=TRUE)
abline(v=dataligne$X[j+1]+200)
par(new=TRUE)
plot(points[points$on_line==1,]$X,points[points$on_line==1,]$Y,col="blue" ,xlim=c(484529,487349),ylim=c(5470540,5474348))


