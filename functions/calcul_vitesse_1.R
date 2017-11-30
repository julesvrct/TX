distance <- function(lat1,lon1,lat2,lon2) 
{
  
  # Convert degrees to radians
  lat1 = lat1 * pi / 180.0;
  lon1 = lon1 * pi / 180.0;
  
  lat2 = lat2 * pi / 180.0;
  lon2 = lon2 * pi / 180.0;
  
  # radius of earth in metres
  r = 6378100;
  
  # P
  rho1 = r * cos(lat1);
  z1 = r * sin(lat1);
  x1 = rho1 * cos(lon1);
  y1 = rho1 * sin(lon1);
  
  # Q
  rho2 = r * cos(lat2);
  z2 = r * sin(lat2);
  x2 = rho2 * cos(lon2);
  y2 = rho2 * sin(lon2);
  
  # Dot product
  dot = (x1 * x2 + y1 * y2 + z1 * z2);
  cos_theta = dot / (r * r);
  
  theta = acos(cos_theta);
  
  # Distance in Metres
  result = r*theta
  
  return(result)
}