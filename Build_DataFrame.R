source('functions/world_data.R')
source('functions/LongLatToUTM.R')
source('functions/vitesse_UTM.R')
source('functions/Angle.R')
library(plyr)
library(e1071) 
world_data=unique(world_data)
UTM <- LongLatToUTM(world_data$lng,world_data$lat,31)

world_data$X<- UTM$X
world_data$Y<- UTM$Y

X <- split(world_data, world_data$id, drop=TRUE)

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



for (k in 1:length(trips))
{
  trips[[k]] = trips[[k]][-which(trips[[k]][,5]==3),]
  trips[[k]] = trips[[k]][-which(trips[[k]][,5]==9),]
}

n=vector()

for (k in 1:length(trips))
{
  if(nrow(trips[[k]])<2)
    n=c(n,k)
 
}
trips=trips[-n]
summary(trips)
tripssplited<-list(data.frame())
for(k in 1:length(trips))
{ 
  indice=vector()
  indice=1
  for(j in 1:(dim(trips[[k]])[1]-1))
  {
    if(trips[[k]][j,5]!=trips[[k]][j+1,5])
    {
      indice= c(indice,j)
    }
    
  }
  indice=c(indice,dim(trips[[k]])[1])
  
  tripssplited[[length(tripssplited)+1]]=trips[[k]][indice[1]:indice[2],]
  if(length(indice) > 2)
  {
    for(i in 2:(length(indice)-1))
    {
      tripssplited[[length(tripssplited)+1]]=trips[[k]][(indice[i]+1):indice[i+1],]
    }
  }
  
}
trips=tripssplited[-1]

n_2=vector()
for (k in 1:length(trips))
{
  if(nrow(trips[[k]])<2)
    n_2=c(n_2,k)
}
trips=trips[-n_2]

# Trips est maintenant une liste de 350 objet. Chaque objet étant un data.frame

# A t'on vraiment besoin de créer un data.frame qui contient dans l'une de ses colonnes un data.frame de longueur différent pour chaque id de voyage ? 
# Non, on peut penser à un référencement par foreign key, le numéro de l'id correspond à l'index de l'élément de la liste

### GLOBAL FEATURES


#autres global features à rajouter dans le data.frame.
##Mode trip


mode_trip=vector()
for(t in trips)
{
 
  mode_trip=c(mode_trip,as.numeric(t$mode)[1])
  
}
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

n_2=vector()
for (k in 1:length(trips_speeds))
{
  if(length(trips_speeds[[k]])<2)
    n_2=c(n_2,k)
}
trips_speeds=trips_speeds[-n_2]
trips = trips[-n_2]
mode_trip = mode_trip[-n_2]


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

n_2=vector()
for (k in 1:length(trips_accel))
{
  if(length(trips_accel[[k]])<2)
    n_2=c(n_2,k)
}
trips_accel=trips_accel[-n_2]
trips = trips[-n_2]
trips_speeds = trips_speeds[-n_2]
mode_trip = mode_trip[-n_2]

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
    sinuo[index]=sum(dist[[index]])/(1.3*sqrt((trips[[index]]$X[m]-trips[[index]]$X[1])^2+(trips[[index]]$Y[m]-trips[[index]]$Y[1])^2))
  else 
    sinuo[index]=0
}


getmode <- function(v) 
{
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



info_trips = data.frame(id=c(1:length(trips)), mean_speed=NA, std_speed=NA, mode_speed=NA, top1_speed=NA,top2_speed=NA,top3_speed=NA, min1_speed=NA,min2_speed=NA,min3_speed=NA, range_speed=NA, upq_speed=NA,lowq_speed=NA, Intq_speed=NA,Skew_speed=NA,Kurtosis_speed=NA, Coefvar_speed=NA
                        , mean_accel=NA, std_accel=NA, mode_accel=NA, top1_accel=NA,top2_accel=NA,top3_accel=NA, min1_accel=NA,min2_speed=NA,min3_accel=NA, range_accel=NA, upq_accel=NA,lowq_accel=NA, Intq_accel=NA,Skew_accel=NA,Kurtosis_accel=NA, Coefvar_accel=NA
                        , mean_turn=NA, std_turn=NA, mode_turn=NA, top1_turn=NA,top2_turn=NA,top3_turn=NA, min1_turn=NA,min2_turn=NA,min3_turn=NA, range_turn=NA, upq_turn=NA,lowq_turn=NA, Intq_turn=NA,Skew_turn=NA,Kurtosis_turn=NA, Coefvar_turn=NA,mode=NA
)



for (i in 1:nrow(info_trips))
{ 
  obj=trips_speeds
  info_trips[i,2] = sum(obj[[i]])/length(obj[[i]])
  info_trips[i,3]= sd(obj[[i]])
  info_trips[i,4]= getmode(obj[[i]])
  info_trips[i,5]= max((obj[[i]]))
  x = obj[[i]]
  n <- length(x)
  info_trips[i,6]= sort(x,partial=n-1)[n-1]
  if (length(obj[[i]]) > 2)
  {
    info_trips[i,7] = sort(x,partial=n-2)[n-2]
  }
  else
  {
    info_trips[i,7] = info_trips[i,6]
  }
  info_trips[i,8]= min((obj[[i]]))
  info_trips[i,9]= sort(x,partial=2)[2]
  if (length(obj[[i]]) > 2)
  {
    info_trips[i,10] = sort(x,partial=3)[3]
  }
  else
  {
    info_trips[i,10] = info_trips[i,9]
  }
  info_trips[i,11]= max((obj[[i]]))-min((obj[[i]]))
  info_trips[i,12]= quantile(obj[[i]])[[4]]
  info_trips[i,13]= quantile(obj[[i]])[[2]]
  info_trips[i,14]= quantile(obj[[i]])[[4]]-quantile(obj[[i]])[[2]]
  info_trips[i,15]= skewness(obj[[i]])
  info_trips[i,16]= kurtosis(obj[[i]])
  info_trips[i,17]= sd(obj[[i]])/mean(obj[[i]])
}


for (i in 1:nrow(info_trips))
{   
  obj=trips_accel
  x = obj[[i]]
  n <- length(x)
  info_trips[i,18] = sum(obj[[i]])/length(obj[[i]])
  info_trips[i,19]= sd(obj[[i]])
  info_trips[i,20]= getmode(obj[[i]]) #ne fonctionne pas
  info_trips[i,21]= max((obj[[i]]))
  info_trips[i,22]= sort(x,partial=n-1)[n-1]
  if (length(obj[[i]]) > 2)
  {
    info_trips[i,23]= sort(x,partial=n-2)[n-2]
  }
  else
  {
    info_trips[i,23] = info_trips[i,22]
  }
  info_trips[i,24]= min((obj[[i]]))
  info_trips[i,25]= sort(x,partial=2)[2]
  if (length(obj[[i]]) > 2)
  {
    info_trips[i,26]= sort(x,partial=3)[3]
  }
  else
  {
    info_trips[i,26] = info_trips[i,25]
  }
  info_trips[i,27]= max((obj[[i]]))-min((obj[[i]]))
  info_trips[i,28]= quantile(obj[[i]])[[4]]
  info_trips[i,29]= quantile(obj[[i]])[[2]]
  info_trips[i,30]= quantile(obj[[i]])[[4]]-quantile(obj[[i]])[[2]]
  info_trips[i,31]= skewness(obj[[i]])
  info_trips[i,32]= kurtosis(obj[[i]])
  info_trips[i,33]= sd(obj[[i]])/mean(obj[[i]])
}


for (i in 1:nrow(info_trips))
{
  obj=turnrate
  x = obj[[i]]
  n <- length(x)
  info_trips[i,34] = sum(obj[[i]])/length(obj[[i]])
  info_trips[i,35]= sd (obj[[i]])
  info_trips[i,36]= getmode(obj[[i]])
  info_trips[i,37]= max((obj[[i]]))
  info_trips[i,38]= sort(x,partial=n-1)[n-1]
  if (length(obj[[i]]) > 2)
  {
    info_trips[i,39]= sort(x,partial=n-2)[n-2]
  }
  else
  {
    info_trips[i,39] = info_trips[i,38]
  }
  info_trips[i,40]= min((obj[[i]]))
  info_trips[i,41]= sort(x,partial=2)[2]
  if (length(obj[[i]]) > 2)
  {
    info_trips[i,42]= sort(x,partial=3)[3]
  }
  else
  {
    info_trips[i,42] = info_trips[i,41]
  }
  info_trips[i,43]= max((obj[[i]]))-min((obj[[i]]))
  info_trips[i,44]= quantile(obj[[i]])[[4]]
  info_trips[i,45]= quantile(obj[[i]])[[2]]
  info_trips[i,46]= quantile(obj[[i]])[[4]]-quantile(obj[[i]])[[2]]
  info_trips[i,47]= skewness(obj[[i]])
  info_trips[i,48]= kurtosis(obj[[i]])
  info_trips[i,49]= sd(obj[[i]])/mean(obj[[i]])
}

for (i in 1:nrow(info_trips))
{
  obj=sinuo
  x = obj[[i]]
  n <- length(x)
  info_trips[i,50] = sum(obj[[i]])/length(obj[[i]])
  info_trips[i,51] = sd(obj[[i]])
  info_trips[i,52] = getmode(obj[[i]])
  info_trips[i,53] = max((obj[[i]]))
  info_trips[i,54] = sort(x,partial=n-1)[n-1]
  if (length(obj[[i]]) > 2)
  {
    info_trips[i,55] = sort(x,partial=n-2)[n-2]
  }
  else
  {
    info_trips[i,55] = info_trips[i,54]
  }
  info_trips[i,56] = min((obj[[i]]))
  info_trips[i,57] = sort(x,partial=2)[2]
  if (length(obj[[i]]) > 2)
  {
    info_trips[i,58] = sort(x,partial=3)[3]
  }
  else
  {
    info_trips[i,58] = info_trips[i,57]
  }
  info_trips[i,59]=max((obj[[i]]))-min((obj[[i]]))
  info_trips[i,60]=quantile(obj[[i]])[[4]]
  info_trips[i,61]=quantile(obj[[i]])[[2]]
  info_trips[i,62]=quantile(obj[[i]])[[4]]-quantile(obj[[i]])[[2]]
  info_trips[i,63]=skewness(obj[[i]])
  info_trips[i,64]=kurtosis(obj[[i]])
  info_trips[i,65]=sd(obj[[i]])/mean(obj[[i]])
}
  
info_trips$mode=mode_trip

plot(info_trips$id,info_trips$mean_speed)
box = boxplot(info_trips$mean_speed)
summary(info_trips)

#info_trips = info_trips[-which(is.na(info_trips$mean_speed)),]
#trips
#require(ggmap)
#ligne5_part1=c(lat=49.41255914140619, lon=2.814817428588867)
#ligne5_map=get_map(location=ligne5_part1,zoom=14)
#ggmap(ligne5_map)
#ggmap(ligne5_map)+ geom_point(data=trips[[128]], aes(x=trips[[128]]$lng,y=trips[[128]]$lat,shape=label,label=format(date, '%H:%M:%S')), col="red", size=0.5, pch=1)+geom_text(data = trips[[128]], aes(x = lng, y = lat, label = format(date, '%H:%M:%S')), size = 3, vjust = 0, hjust = -0.5) 