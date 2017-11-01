setwd("D:/TX")
getwd()  

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


winners <- fromJSON("dataset1.json", flatten=TRUE)
lines<- read_json("lines.json", simplifyMatrix = TRUE,simplifyVector = TRUE,flatten=TRUE, simplifyDataFrame = TRUE)
colnames(winners)
data<-as.data.frame(winners) 

###userId 59703cd6c9e77c0001b821f3
val<-as.character(data[data$userId=="59703cd6c9e77c0001b821f3",]$date)
date<-as.POSIXct(as.numeric(as.character(val))/1000,origin="1970-01-01",tz="GMT")
dataUser<-data[data$userId=="59703cd6c9e77c0001b821f3",]
dataUserd<-dataUser[which(format(date, format = "%m/%d")=="08/08"),]
mydf <- structure(list(longitude = dataUserd$lng, latitude =dataUserd$lat), .Names = c("longitude", 
                                                                                       "latitude"), class = "data.frame", row.names = c(NA, -182L))
xy <- mydf[,c(1,2)]
spdf <- SpatialPointsDataFrame(coords = xy, data = mydf, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

coo<-LongLatToUTM(xy$longitude,xy$latitude,33)
plot(coo$X,coo$Y)


packages <- c("jsonlite", "dplyr", "purrr")

purrr::walk(packages, library,
            character.only = TRUE, warn.conflicts = FALSE)

vars <- setdiff(names(lines), c("photos", "features"))
data<-lines
data <- map_at(data, vars, unlist) %>% tibble::as_tibble(.)

names(a)

fonc<-function(dataligne,dataphone)
{
  n<-dim(dataphone)[1]
  m<-dim(dataligne)[1]
  y<-vector("numeric",n)
  d<-200
  for (i in 1:n)
  {
    for (j in 2:m-1)
    { 
      if((dataligne$X[j+1]==dataligne$X[j])){ 
        
        
      }
      else{
        a<-as.numeric(dataligne$Y[j]-dataligne$Y[j+1])/(dataligne$X[j]-dataligne$X[j+1])
        b<- dataligne$Y[j+1]-a*dataligne$X[j+1]
        if((dataphone$Y[i]<=dataphone$X[i]*a+b+d )&& (dataphone$Y[i]>=dataphone$X[i]*a+b-d) )
        { 
          if(dataligne$X[j+1]>=dataligne$X[j])
          { 
            if(between(dataphone$X[i],dataligne$X[j]-d,dataligne$X[j+1]+d))
            { 
              y[i]=1
              break
            }
            
          }
          if(dataligne$X[j+1]<=dataligne$X[j])
          { 
            if(between(dataphone$X[i],dataligne$X[j+1]-d,dataligne$X[j]+d))
            { 
              y[i]=1
              break
            }
            
          }
        }
      }
    } 
    
  }
  
  return (y);
}


a<-read_json("lines.json", simplifyVector = TRUE)

b<-a[a$number=="5",]
ligne5<-as.data.frame(as.data.frame(as.data.frame(as.data.frame(b$directions)$shape)$features)$geometry)$coordinates

datacompi<-data[which(round(data$lat,1)==49.4 | round(data$lng)==2.8),]

lignec<-rbind(as.data.frame(ligne5[1]),as.data.frame(ligne5[2]),as.data.frame(ligne5[3]),as.data.frame(ligne5[4]),as.data.frame(ligne5[5]),as.data.frame(ligne5[6]),as.data.frame(ligne5[7]),as.data.frame(ligne5[8]),as.data.frame(ligne5[9]),as.data.frame(ligne5[10]),as.data.frame(ligne5[11]),as.data.frame(ligne5[12]),as.data.frame(ligne5[13]),as.data.frame(ligne5[14]),as.data.frame(ligne5[15]),as.data.frame(ligne5[16]),as.data.frame(ligne5[17]),as.data.frame(ligne5[18]))

coordon<-LongLatToUTM(lignec$X1,lignec$X2,31)
coordophone<-LongLatToUTM(datacompi$lng,datacompi$lat,31)

j<-100
a<-as.numeric(dataligne$Y[j]-dataligne$Y[j+1])/(dataligne$X[j]-dataligne$X[j+1])
b<- dataligne$Y[j+1]-a*dataligne$X[j+1]


plot(coordophone$X,coordophone$Y, col="red" ,xlim=c(484529,487349),ylim=c(5470540,5474348))
par(new=TRUE)
plot(coordon$X,coordon$Y, col="green",xlim=c(484529,487349),ylim=c(5470540,5474348))
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
plot(dataphone[which(classif==1),]$X,dataphone[which(classif==1),]$Y,col="blue" ,xlim=c(484529,487349),ylim=c(5470540,5474348))

summary(coordon)


summary(datacompi)
table(datacompi$userId)
i<-2
j<-3
sqrt((coordon$X[i]-coordon$X[j])*(coordon$X[i]-coordon$X[j])+(coordon$Y[i]-coordon$Y[j])*(coordon$X[i]-coordon$X[j]))
segments(coordon$X[i], coordon$Y[i], coordon$X[j], coordon$Y[j], col = par("fg"), lty = par("lty"), lwd = par("lwd"))
lines(coordon$X,coordon$Y)
classif<-fonc(coordon,coordophone)
dataphone<-coordophone
dataligne<-coordon



y<-vector("numeric",n)
for (i in 2:n)
{
  for (j in 2:m-1)
  { if((dataligne$Y[j+1]==dataligne$Y[j])) 
    break
    else{
      a<-as.numeric(dataligne$Y[j]-dataligne$Y[j+1])/(dataligne$X[j]-dataligne$X[j+1])
      b<- dataligne$Y[j+1]-a*dataligne$X[j+1]
      if((dataphone$Y[i]<=dataphone$X[i]*a+b+d )&& (dataphone$Y[i]>=dataphone$X[i]*a+b-d) )
      { 
        if(dataligne$X[j+1]>=dataligne$X[j])
        { 
          if(between(dataphone$X[i],dataligne$X[j],dataligne$X[j+1]))
          { 
            y[i]=1
            break
          }
          
        }
        if(dataligne$X[j+1]<=dataligne$X[j])
        { 
          if(between(dataphone$X[i],dataligne$X[j+1],dataligne$X[j]))
          { 
            y[i]=1
            break
          }
          
        }
        print(a)}
    }}}
mode <- datacompi$mode

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

## Calcul des parametres
#Speed
speeddate<-as.POSIXct(as.numeric(as.character(datacompi$date))/1000,origin="1970-01-01",tz="GMT")
strptime(speeddate, "%y-%m-%d %H:%M:%S GMT")
help(as.POSIXct)
n<-dim(datacompi)[1]
### segment et user 
speeddist<- vector("numeric",n-1)
for (i in 2:n)
{
  j=i+1
  print(i)
  speeddist[i-1]=sqrt((coordophone$X[i]-coordophone$X[j])*(coordophone$X[i]-coordophone$X[j])+(coordophone$Y[i]-coordophone$Y[j])*(coordophone$X[i]-coordophone$X[j]))

}

###sEGMENTATION
for (i in 2:n)
{
l <- list(i = 1,b = "foo",c = 1:5)
}
n<-dim(datacompi)[1]
names(datacompi)
levels(factor(datacompi$userId))


users<-levels(factor(datacompi$userId))
phoneord<-phone[order(phone$user),]



phone<-cbind(dataphone,user=factor(datacompi$userId),date=as.POSIXct(as.numeric(as.character(datacompi$date))/1000,origin="1970-01-01",tz="GMT"),mode=datacompi$mode)

X <- split(phone, phone$user)
length(X)


#####
spl<-function(X)
{
for(j in 1:length(X))
  for (i in 1:dim(X[[j]]))
    if(as.character(phone$user[j]==users[i]))
      l<-list()
}
help(split)



cumsum(X[[1]]$mode == 10)
split(X[[1]], cumsum(X[[1]]$mode == 10))
