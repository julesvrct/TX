#install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)
direction <- function(p1, p2) 
{
  dTeta = log(tan((p2$lat/2)+(pi/4))/tan((p1$lat/2)+(pi/4)))
  print(dTeta)
  dLon = abs(p1$lng-p2$lng);
  print(dLon)
  teta = atan2(dLon,dTeta);
  print(teta)
  direction = round(NISTradianTOdeg(teta));
  print(direction)
  return(direction) #direction in degree
  
}