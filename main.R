trips<-list(data.frame());
for(i in 1:length(X))
{
  
  trips[[i]] = split(X[[i]], cumsum (X[[i]]$mode == 10))
}
trips[[1]][1]
