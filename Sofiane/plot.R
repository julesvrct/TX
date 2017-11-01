j<-100
a<-as.numeric(dataligne$Y[j]-dataligne$Y[j+1])/(dataligne$X[j]-dataligne$X[j+1])
b<- dataligne$Y[j+1]-a*dataligne$X[j+1]


plot(coordophone$X,coordophone$Y, col="red" ,xlim=c(484529,487349),ylim=c(5470540,5474348))
par(new=TRUE)
plot(coordon$X,coordon$Y, col="green",xlim=c(484529,487349),ylim=c(5470540,5474348))
par(new=TRUE)
curve(a*x+b,xlim=c(484529,487349),ylim=c(5470540,5474348))
par(new=TRUE)
curve(a*x+b+200,xlim=c(484529,487349),ylim=c(5470540,5474348))
par(new=TRUE)
curve(a*x+b-200,xlim=c(484529,487349),ylim=c(5470540,5474348))
par(new=TRUE)
abline(v=dataligne$X[j]-200)
par(new=TRUE)
abline(v=dataligne$X[j+1]+200)
par(new=TRUE)
plot(dataphone[which(classif==1),]$X,dataphone[which(classif==1),]$Y,col="blue" ,xlim=c(484529,487349),ylim=c(5470540,5474348))
