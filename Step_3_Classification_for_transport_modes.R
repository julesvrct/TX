####################################
#PREAMBULE
#---
#This script allows to build the classifier for the transportation modes
####################################

# We load data environment objetc "info_trips" obtained from "Step_1_building_final_dataframe
load(file = "data/info_trips")

library(foreach) # doMC needs it
library(doMC) # for parralel processing
library(caret)
library(corrplot)
registerDoMC(cores = 2) # 5 cores pour parralel processing

data_train = info_trips[,1:50]

# Remove class 1 and 4
id_4 = data_train[data_train$mode==4,]$id
id_1 = data_train[data_train$mode==1,]$id
data_train = data_train[-c(id_1,id_4),]

data_train$mode = as.factor(data_train$mode)
  
n = dim(data_train)[1]
p = dim(data_train)[2]-1
k = length(levels(data_train$mode))
c.formule = mode ~ .

c.train.data = data_train



row.has.na <- apply(c.train.data, 1, function(x){any(is.na(x))})
sum(row.has.na)
c.train.data <- c.train.data[!row.has.na,]

########## Analyse données ##########
boxplot(c.train.data[,-1])
apply(c.train.data[,-1], 2, mean)
apply(c.train.data[,-1], 2, sd)
# On scale 
c.train.data[2:ncol(c.train.data)-1] <- scale(c.train.data[1:ncol(c.train.data)-1])
# => Données centrées réduites
boxplot(c.train.data[,-1])
apply(c.train.data[,-1], 2, mean)
apply(c.train.data[,-1], 2, sd)

# Proportion des classes
c.class.proportions = table(c.train.data$mode) / n * 100
c.class.proportions

n = dim(c.train.data)[1]
p = dim(c.train.data)[2]-1
k = length(levels(c.train.data$mode))
c.formule = mode ~ .


cors = cor(subset(c.train.data, select = -mode))
corrplot(cors, type="upper", order="hclust", tl.col="black", tl.srt=45, tl.cex = 0.5)



########### Essai de modèles #############
c.fitControl <- trainControl(
  ## K-fold CV
  method = "repeatedcv",
  number = 5, #10
  ## repeated 
  repeats = 3,
  verboseIter = TRUE) #10

c.fitControlRandomTuning <- trainControl(
  ## K-fold CV
  method = "repeatedcv",
  number = 5,
  repeats = 3,
  search = "random",
  verboseIter = TRUE)

##########  Random forest
# method = 'rf'
# Type: Classification, Regression
# Tuning parameters:
# mtry (#Randomly Selected Predictors)
# Number of variables randomly sampled as candidates at each split. 
# Note that the default values are different for classification (sqrt(p) 
# where p is number of variables in x) and regression (p/3)
# Required packages: randomForest
c.best.mtry = round(sqrt(p))
c.random.forest.grid = expand.grid(mtry = seq(max(1, c.best.mtry-5), c.best.mtry+5, 1))
c.random.forest.grid
c.random.forest.fit = caret::train(c.formule,
                                   data = c.train.data,
                                   method = "rf",
                                   trControl = c.fitControl,
                                   tuneGrid = c.random.forest.grid,
                                   ntree = 1000
                                   )
c.random.forest.fit
plot(c.random.forest.fit)



#############  SVM 

#### Support Vector Machines with Linear Kernel
# method = 'svmLinear'
# Type: Regression, Classification
# Tuning parameters:
# C (Cost)
# The margin is smaller for larger e. Hence larger values of C focus 
# attention more on points near the decision boundary, while smaller 
# values involve data further away.
# Required packages: kernlab
# install.packages("kernlab")
library(kernlab)
c.svm.linear.grid = expand.grid(
  C = c(0.1, 1, 10, 20, 30)
)
c.svm.linear.grid
c.svm.linear.fit.grid.tuning = caret::train(c.formule,
                                            data = c.train.data,
                                            method = "svmLinear",
                                            trControl = c.fitControl,
                                            tuneGrid = c.svm.linear.grid
)
c.svm.linear.fit.grid.tuning
plot(c.svm.linear.fit.grid.tuning)
# Grid autour du meilleur hyper-parametre C
c.svm.linear.grid.2 = expand.grid(
  C = seq(max(0, c.svm.linear.fit.grid.tuning$bestTune$C-1), c.svm.linear.fit.grid.tuning$bestTune$C+1, .1)
)
c.svm.linear.grid.2
c.svm.linear.fit.grid.tuning.2 = caret::train(c.formule,
                                              data = c.train.data,
                                              method = "svmLinear",
                                              trControl = c.fitControl,
                                              tuneGrid = c.svm.linear.grid.2
)
c.svm.linear.fit.grid.tuning.2
plot(c.svm.linear.fit.grid.tuning.2)




#### Support Vector Machines with Polynomial Kernel
# method = 'svmPoly'
# Type: Regression, Classification
# Tuning parameters:
# degree (Polynomial Degree)
# scale (Scale)
# Utilisation de la fonction kernlab::polydot(degree = param$degree, scale = param$scale, offset = 1) 
# pour construire le kernel
# degree = The degree of the polynomial
# scale = The scaling parameter of the polynomial and tangent kernel is a convenient way 
# of normalizing patterns without the need to modify the data itself
# In this interpretation, scale is related to how spread out your data points are. 
# If they are very far from each other (which would happen in a very high dimensional 
# space for example), then you don't want the kernel to drop off quickly, so you would 
# use a small gamma.
# default would be 1/p
# C (Cost)
# Required packages: kernlab

## Avec random tuning
#e.svm.poly.fit.random.tuning = caret::train(e.formule,
#                                            data = c.train.data,
#                                            method = "svmPoly",
#                                            trControl = e.fitControlRandomTuning,
#                                            tuneLength = 15
#)
#e.svm.poly.fit.random.tuning
#plot(e.svm.poly.fit.random.tuning)

## Avec grid tuning
# Grid avec degre 1 à 4, et Cost = c(0.1, 1, 2, 5, 10, 50, 100)
# Scale constant à meilleure valeur théorique = 1/p
# Tuning de scale une fois le cost et degree choisis
c.svm.poly.grid = expand.grid(
  degree = seq(1,4,1),
  scale = 1/p,
  C = c(0.1, 1, 10, 100)
)
c.svm.poly.grid
c.svm.poly.fit.grid.tuning = caret::train(c.formule,
                                          data = c.train.data,
                                          method = "svmPoly",
                                          trControl = c.fitControl,
                                          tuneGrid = c.svm.poly.grid
)
c.svm.poly.fit.grid.tuning
plot(c.svm.poly.fit.grid.tuning)

# Tester une dernière grid de cost pour degree = best degree et un intervalle autour du best cost
c.svm.poly.grid.cost.fixed.degree = expand.grid(
  degree = c.svm.poly.fit.grid.tuning$bestTune$degree,
  scale = 1/p,
  C = seq(max(0, c.svm.poly.fit.grid.tuning$bestTune$C-0.5), c.svm.poly.fit.grid.tuning$bestTune$C+0.5, .1)
)
c.svm.poly.grid.cost.fixed.degree
c.svm.poly.fit.grid.tuning.cost.fixed.degree = caret::train(c.formule,
                                                            data = c.train.data,
                                                            method = "svmPoly",
                                                            trControl = c.fitControl,
                                                            tuneGrid = c.svm.poly.grid.cost.fixed.degree
)
c.svm.poly.fit.grid.tuning.cost.fixed.degree
plot(c.svm.poly.fit.grid.tuning.cost.fixed.degree)

# Grid sur scale pour améliorer le modèle
c.svm.poly.grid.scale = expand.grid(
  degree = c.svm.poly.fit.grid.tuning.cost.fixed.degree$bestTune$degree,
  scale = seq(1/(4*p), 3/p, 0.001),
  C = c.svm.poly.fit.grid.tuning.cost.fixed.degree$bestTune$C
)
c.svm.poly.grid.scale
c.svm.poly.fit.grid.tuning.scale = caret::train(c.formule,
                                                data = c.train.data,
                                                method = "svmPoly",
                                                trControl = c.fitControl,
                                                tuneGrid = c.svm.poly.grid.scale
)
c.svm.poly.fit.grid.tuning.scale
plot(c.svm.poly.fit.grid.tuning.scale)



# Support Vector Machines with Radial Basis Function Kernel <=> Gaussian kernel
# method = 'svmRadial'
# Type: Regression, Classification
# Tuning parameters:
# sigma (Sigma)
# C (Cost)
# Required packages: kernlab

# Sans tuning
c.svm.gaussian.fit = caret::train(c.formule,
                                  data = c.train.data,
                                  method = "svmRadial",
                                  trControl = c.fitControl
)
c.svm.gaussian.fit
plot(c.svm.gaussian.fit)

# Grid tuning autour du sigma trouvé précédement
c.svm.gaussian.grid = expand.grid(
  sigma = seq(max(0, c.svm.gaussian.fit$bestTune$sigma - 3*c.svm.gaussian.fit$bestTune$sigma), 3*c.svm.gaussian.fit$bestTune$sigma, c.svm.gaussian.fit$bestTune$sigma),
  C = c(1, 5, 10)
)
c.svm.gaussian.grid
c.svm.gaussian.fit.grid.tuning = caret::train(c.formule,
                                              data = c.train.data,
                                              method = "svmRadial",
                                              trControl = c.fitControl,
                                              tuneGrid = c.svm.gaussian.grid
)
c.svm.gaussian.fit.grid.tuning
plot(c.svm.gaussian.fit.grid.tuning)

# Intervalle de tuning autour de cost
c.svm.gaussian.grid.cost.tuning = expand.grid(
  sigma = c.svm.gaussian.fit.grid.tuning$bestTune$sigma,
  C = seq(max(0, c.svm.gaussian.fit.grid.tuning$bestTune$C-0.5), c.svm.gaussian.fit.grid.tuning$bestTune$C+1, 0.1)
)
c.svm.gaussian.grid.cost.tuning
c.svm.gaussian.fit.grid.tuning.cost = caret::train(c.formule,
                                                   data = c.train.data,
                                                   method = "svmRadial",
                                                   trControl = c.fitControl,
                                                   tuneGrid = c.svm.gaussian.grid.cost.tuning
)
c.svm.gaussian.fit.grid.tuning.cost
plot(c.svm.gaussian.fit.grid.tuning.cost)




###Résultats


c.rf.final = c.random.forest.fit
c.rf.final$results$Accuracy[which(is.nan(c.rf.final$results$Accuracy))] = 0
c.rf.final.index = which(c.rf.final$results$Accuracy == max(c.rf.final$results$Accuracy))[1]
c.rf.final.per.error = round((1-c.rf.final$results$Accuracy[c.rf.final.index])*100, 2)
c.rf.final.per.error.ci = c(
  round((1-(c.rf.final$results$Accuracy[c.rf.final.index] + c.rf.final$results$AccuracySD[c.rf.final.index]))*100, 2),
  round((1-(c.rf.final$results$Accuracy[c.rf.final.index] - c.rf.final$results$AccuracySD[c.rf.final.index]))*100, 2)
)

c.svm.linear.final = c.svm.linear.fit.grid.tuning.2
c.svm.linear.final$results$Accuracy[which(is.nan(c.svm.linear.final$results$Accuracy))] = 0
c.svm.linear.final.index = which(c.svm.linear.final$results$Accuracy == max(c.svm.linear.final$results$Accuracy))[1]
c.svm.linear.final.per.error = round((1-c.svm.linear.final$results$Accuracy[c.svm.linear.final.index])*100, 2)
c.svm.linear.final.per.error.ci = c(
  round((1-(c.svm.linear.final$results$Accuracy[c.svm.linear.final.index] + c.svm.linear.final$results$AccuracySD[c.svm.linear.final.index]))*100, 2),
  round((1-(c.svm.linear.final$results$Accuracy[c.svm.linear.final.index] - c.svm.linear.final$results$AccuracySD[c.svm.linear.final.index]))*100, 2)
)

c.svm.poly.final = c.svm.poly.fit.grid.tuning.scale
c.svm.poly.final$results$Accuracy[which(is.nan(c.svm.poly.final$results$Accuracy))] = 0
c.svm.poly.final.index = which(c.svm.poly.final$results$Accuracy == max(c.svm.poly.final$results$Accuracy))[1]
c.svm.poly.final.per.error = round((1-c.svm.poly.final$results$Accuracy[c.svm.poly.final.index])*100, 2)
c.svm.poly.final.per.error.ci = c(
  round((1-(c.svm.poly.final$results$Accuracy[c.svm.poly.final.index] + c.svm.poly.final$results$AccuracySD[c.svm.poly.final.index]))*100, 2),
  round((1-(c.svm.poly.final$results$Accuracy[c.svm.poly.final.index] - c.svm.poly.final$results$AccuracySD[c.svm.poly.final.index]))*100, 2)
)

c.svm.gaussian.final = c.svm.gaussian.fit.grid.tuning.cost
c.svm.gaussian.final$results$Accuracy[which(is.nan(c.svm.gaussian.final$results$Accuracy))] = 0
c.svm.gaussian.final.index = which(c.svm.gaussian.final$results$Accuracy == max(c.svm.gaussian.final$results$Accuracy))[1]
c.svm.gaussian.final.per.error = round((1-c.svm.gaussian.final$results$Accuracy[c.svm.gaussian.final.index])*100, 2)
c.svm.gaussian.final.per.error.ci = c(
  round((1-(c.svm.gaussian.final$results$Accuracy[c.svm.gaussian.final.index] + c.svm.gaussian.final$results$AccuracySD[c.svm.gaussian.final.index]))*100, 2),
  round((1-(c.svm.gaussian.final$results$Accuracy[c.svm.gaussian.final.index] - c.svm.gaussian.final$results$AccuracySD[c.svm.gaussian.final.index]))*100, 2)
)
