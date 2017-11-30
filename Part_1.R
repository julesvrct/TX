source('functions/compy_data.R')
source('functions/LongLatToUTM.R')
source('functions/vitesse_UTM.R')
source('functions/Angle.R')
library(plyr)

compiegne_data=unique(compiegne_data)
UTM <- LongLatToUTM(compiegne_data$lng,compiegne_data$lat,31)

compiegne_data$X<- UTM$X
compiegne_data$Y<- UTM$Y

X <- split(compiegne_data, compiegne_data$id, drop=TRUE)

trips<-list(data.frame())

for(i in 1:length(X))
{
  for ( j in 1:length((split(X[[i]], cumsum (X[[i]]$mode == 10)))))
  {
    trips[[j]] = split(X[[i]], cumsum (X[[i]]$mode == 10))[[j]]
  }
}

for (j in 1:length(trips)) 
{
  trips[[j]]=trips[[j]][-1,]
}

for (k in 1:length(trips))
{
  trips[[k]] = trips[[k]][order(trips[[k]]$date),]
}

# Trips est maintenant une liste de 350 objet. Chaque objet étant un data.frame

# A t'on vraiment besoin de créer un data.frame qui contient dans l'une de ses colonnes un data.frame de longueur différent pour chaque id de voyage ? 
# Non, on peut penser à un référencement par foreign key, le numéro de l'id correspond à l'index de l'élément de la liste

### GLOBAL FEATURES

info_trips = data.frame(id=c(1:length(trips)), mean_speed=NA, std_speed=NA, mode_speed=NA, top_speed=NA, range_speed=NA, perc_speed=NA, Intq_speed=NA,Skew_speed=NA,Kurtosis_speed=NA, Coefvar_speed=NA, Autocor_speed=NA ) #autres global features à rajouter dans le data.frame.

## Vitesse
trips_speeds = list(vector())
index = 0
for (t in trips)
{ 
  index = index + 1
  v = vector()
  
  
  for (i in 1:(length(t$id)-1))
  {
    
  if(is.null(vitesse_UTM(t[i,],t[i+1,]))  ) 
  {
    trips[[index]]=trips[[index]][-i,]
  }
  else if(dim(t)[1]<2)
  {
    trips[[index]]=trips[[index]][-i,]
  }
  else if((!is.na(vitesse_UTM(t[i,],t[i+1,])) && vitesse_UTM(t[i,],t[i+1,]) >120 ))
  {
    trips[[index]]=trips[[index]][-i,]
  }
  else
  {
    v = c(v,vitesse_UTM(t[i,],t[i+1,]))
  } 
  }
  
  trips_speeds[[index]] = v
} 


##Accceleration
trips_accel = list(vector())
index = 0
for (t in trips)
{  
  index = index + 1
  n=length(trips_speeds[[index]])
  m=length(trips[[index]])-1
  
  a= vector()
  for (k in 1:n-1)
  {
    if(0==0)
    {
      print(k)
      a=c(a,(trips_speeds[[index]][k+1]-trips_speeds[[index]][k])/(as.double((trips[[index]][k+2,2]-trips[[index]][k,2]), units='hours')/2))
    }
  }
  a[is.na(a)]<-0
  trips_accel[[index]]=a
  
}

##Turn angle 
course=list(vector())
turnrate=list(vector())
index=0
for (t in trips) {
  index=index+1
  m=dim(trips[[index]])[1]-1
  a=vector()
  for(k in 1:m)
  {
    print(k)
  a <- c(a,angle(c(trips[[index]]$lat[k + 1],trips[[index]]$lng[k + 1]), c(trips[[index]]$lat[k],trips[[index]]$lng[k])))
  
  }
  a[is.na(a)]<-0
  course[[index]]=a
  b=vector()
  for (k in 1:length(course[[index]])-1) {
    b=c(b,(course[[index]][k+1]-course[[index]][k])/(as.double((trips[[index]][k+1,2]-trips[[index]][k,2]), units='secs')))
  }
  b[is.na(b)]<-0
  turnrate[[index]]=b
}
### Distance / Distance la plus courte


sinuo=vector()
dist=list(vector())
index=0
for (t in trips) {
  index=index+1
  m=dim(trips[[index]])[1]
  a=vector()
  for(k in 1:m)
  {
    print(k)
    a <-  c(a,sqrt((trips[[index]]$X[k + 1]-trips[[index]]$X[k])^2+(trips[[index]]$Y[k + 1]-trips[[index]]$Y[k])^2))
    
  }
  a[is.na(a)]<-0
  dist[[index]]=a
  if(m==0)
    sinuo[index]=0
  else if(sqrt((trips[[index]]$X[m]-trips[[index]]$X[1])^2+(trips[[index]]$Y[m]-trips[[index]]$Y[1])^2)!=0)
  sinuo[index]=sum(dist[[index]])/sqrt((trips[[index]]$X[m]-trips[[index]]$X[1])^2+(trips[[index]]$Y[m]-trips[[index]]$Y[1])^2)
  else 
    sinuo[index]=0
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for (i in 1:nrow(info_trips))
{
  info_trips[i,2] = sum(trips_speeds[[i]])/length(trips_speeds[[i]])
  info_trips[i,3]=sd(trips_speeds[[i]])
  info_trips[i,4]=getmode(trips_speeds[[i]])
}


plot(info_trips$id,info_trips$mean_speed)
box = boxplot(info_trips$mean_speed)
summary(info_trips)

info_trips = info_trips[-which(is.na(info_trips$mean_speed)),]
summary(info_trips)


#require(ggmap)
#ligne5_part1=c(lat=49.41255914140619, lon=2.814817428588867)
#ligne5_map=get_map(location=ligne5_part1,zoom=14)
#ggmap(ligne5_map)
#ggmap(ligne5_map)+ geom_point(data=trips[[128]], aes(x=trips[[128]]$lng,y=trips[[128]]$lat,shape=label,label=format(date, '%H:%M:%S')), col="red", size=0.5, pch=1)+geom_text(data = trips[[128]], aes(x = lng, y = lat, label = format(date, '%H:%M:%S')), size = 3, vjust = 0, hjust = -0.5) 

