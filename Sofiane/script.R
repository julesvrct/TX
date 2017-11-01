fonc<-function(dataligne,dataphone)
{
  n<-dim(dataphone)[1]
  m<-dim(dataligne)[1]
  y<-vector("numeric",n)
  d<-200
  for (i in 2:n)
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
