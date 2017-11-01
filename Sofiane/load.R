library(sp)
library(rgdal)
library(plotrix)
library(jsonlite)
packages <- c("jsonlite", "dplyr", "purrr")

purrr::walk(packages, library,
            character.only = TRUE, warn.conflicts = FALSE)
winners <- fromJSON("dataset1.json", flatten=TRUE)
data<-as.data.frame(winners) 

a<-read_json("lines.json", simplifyVector = TRUE)

b<-a[a$number=="5",]

ligne5<-as.data.frame(as.data.frame(as.data.frame(as.data.frame(b$directions)$shape)$features)$geometry)$coordinates

datacompi<-data[which(round(data$lat,1)==49.4 | round(data$lng)==2.8),]

lignec<-rbind(as.data.frame(ligne5[1]),as.data.frame(ligne5[2]),as.data.frame(ligne5[3]),as.data.frame(ligne5[4]),as.data.frame(ligne5[5]),as.data.frame(ligne5[6]),as.data.frame(ligne5[7]),as.data.frame(ligne5[8]),as.data.frame(ligne5[9]),as.data.frame(ligne5[10]),as.data.frame(ligne5[11]),as.data.frame(ligne5[12]),as.data.frame(ligne5[13]),as.data.frame(ligne5[14]),as.data.frame(ligne5[15]),as.data.frame(ligne5[16]),as.data.frame(ligne5[17]),as.data.frame(ligne5[18]))

coordon<-LongLatToUTM(lignec$X1,lignec$X2,31)
coordophone<-LongLatToUTM(datacompi$lng,datacompi$lat,31)


classif<-fonc(coordon,coordophone)
