trips<-list(data.frame());
for(i in 1:length(X))
{
  for ( j in 1:length((split(X[[i]], cumsum (X[[i]]$mode == 10)))))
  {
    trips[[j]] = split(X[[i]], cumsum (X[[i]]$mode == 10))[[j]]
  }
}
