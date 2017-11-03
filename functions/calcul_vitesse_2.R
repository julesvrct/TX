vitesse <- function(p1,p2)
{
  dist = distance(p1$lat, p1$lng, p2$lat, p2$lng)
  time_s = (p2$date - p1$date) #/ 1000.0 if timestamp in millisecond
  print(time_s)
  
  speed_mps = dist / as.double(time_s, units='secs')
  speed_kph = (speed_mps * 3600.0) / 1000.0
  
  return(speed_kph)
}
