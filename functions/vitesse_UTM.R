vitesse_UTM <- function(p1,p2)
{
  dist = sqrt(((p2$X-p1$X)^2)+((p2$Y-p1$Y)^2))
  time_s = (p2$date - p1$date) #/ 1000.0 if timestamp in millisecond
  speed_mps = dist / as.double(time_s, units='secs')
  speed_kph = (speed_mps * 3600.0) / 1000.0

  return(speed_kph)
  
}