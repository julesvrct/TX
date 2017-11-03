source('functions/compy_data.R')
source('functions/LongLatToUTM.R')
source('functions/vitesse_UTM.R')
library(plyr)

UTM <- LongLatToUTM(compiegne_data$lng,compiegne_data$lat,31)
compiegne_data$lng <- UTM$X
compiegne_data$lat <- UTM$Y
compiegne_data = rename(compiegne_data, c("lat"="Y","lng"="X"))
compiegne_data = compiegne_data[c("id","date","X","Y","mode","transportation_mode")]
X <- split(compiegne_data, compiegne_data$id, drop=TRUE)

trips<-list(data.frame())

for(i in 1:length(X))
{
  for ( j in 1:length((split(X[[i]], cumsum (X[[i]]$mode == 10)))))
  {
    trips[[j]] = split(X[[i]], cumsum (X[[i]]$mode == 10))[[j]]
  }
}

# Trips est maintenant une liste de 350 objet. Chaque objet étant un data.frame

# A t'on vraiment besoin de créer un data.frame qui contient dans l'une de ses colonnes un data.frame de longueur différent pour chaque id de voyage ? 
# Non, on peut penser à un référencement par foreign key, le numéro de l'id correspond à l'index de l'élément de la liste

### GLOBAL FEATURES

info_trips = data.frame(id=c(1:length(trips)), mean_speed=NA) #autres global features à rajouter dans le data.frame.

## Vitesse
# Objet : A list of vector that contain speed btw each point

trips_speeds = list(vector())
index = 0
for (t in trips)
{
  index = index + 1
  v = vector()
  for (i in 1:(length(t$id)-1))
  {
    v = c(v,vitesse_UTM(t[i,],t[i+1,]))
  }
  trips_speeds[[index]] = v
} 
# A noter que certaines vitesse sont aberrante (800km/h) du fait que par exemple on a ce genre de données:
# un temps très resséré pour 2 points très éloignés (voir ci-dessous élements 1 et 2)

#trips[[146]]
#id                date      lat      lng mode transportation_mode
#33819 59096f9dc9e77c0001b5a200 2017-09-12 18:22:54 49.40659 2.793882   10    Exiting Geofence
#33820 59096f9dc9e77c0001b5a200 2017-09-12 18:22:55 49.40680 2.793413    3               Still
#33821 59096f9dc9e77c0001b5a200 2017-09-12 18:27:00 49.40680 2.793413    3               Still
#33822 59096f9dc9e77c0001b5a200 2017-09-12 18:27:58 49.40680 2.793413    9   Entering Geofence

#Du coup :

#trips_speeds[[146]]
#[1] 806.1009   0.0000   0.0000

for (i in 1:nrow(info_trips))
{
  info_trips[i,2] = sum(trips_speeds[[i]])/length(trips_speeds[[i]])
}

#Lorsque l'on fait 'mean(info_trips[,2])' on obtient une vitesse moyenne pour les voyages de 758.2359, aberrant !
#Il s'agit de nettoyer les données:

plot(info_trips$id,info_trips$mean_speed)
box = boxplot(info_trips$mean_speed)






