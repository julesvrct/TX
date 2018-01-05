####################################
#PREAMBULE
#---
#This script is used for finding patterns in data
#At the end of the script there is a function that can be used for all kind of users and whoch finds patterns
####################################

load("data/environment.RData")
library("dbscan")
Tlim=0.25
Vlim=0.8
userspattern=X
c=vector()
for( i in 1:length(userspattern))
{   b=0
    for (t in userspattern[i])
    { 
      if(length(t$id)==1)
      
    {
      b=1
    }
      
    }
    if(b==1)
      c=c(c,i)
}
c
userspattern=userspattern[-c]

for(k in 1:length(userspattern))
{ userspattern[[k]]$diff=0
userspattern[[k]]$speed=0
  for (i in 2:dim(userspattern[[k]][1]))
  { userspattern[[k]][i,9]=as.double(userspattern[[k]][i,2]-userspattern[[k]][i-1,2], units='hours')
    userspattern[[k]][i,10]= vitesse_UTM(userspattern[[k]][i-1,],userspattern[[k]][i,])
  }
}
user=86



# function point of interest


pattern <- function(user,Tlim,Vlim) 
  {
        v=which(userspattern[[user]]$speed<Vlim)
        vs=split(v, cumsum(c(1, diff(v) != 1)))
        
        pointofinterest=data.frame(id=NA,lat=NA,lng=NA,diff=NA)
        index=0
        for(i in 1:length(vs))
        {
        if(sum(userspattern[[user]][vs[[i]],]$diff)>Tlim)
        { index=index+1
          pointofinterest[index,1]=as.character(userspattern[[user]]$id[1])
          pointofinterest[index,2]=(mean(userspattern[[user]][vs[[i]],]$lat))
          pointofinterest[index,3]=(mean(userspattern[[user]][vs[[i]],]$lng))
          pointofinterest[index,4]=(sum(userspattern[[user]][vs[[i]],]$diff))
        
        }
        }
        
        obj=dbscan(as.matrix(pointofinterest[,c(2,3)]), 0.01, minPts = 5,weights =pointofinterest[,4] )
        plot(pointofinterest[,c(2,3)], col=obj$cluster)
        
        hullplot(pointofinterest[,c(2,3)], obj)
        obj$cluster
        return (pointofinterest)
    }

pattern(user,Tlim,Vlim)
