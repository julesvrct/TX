load("environment.RData")
library(foreach) # doMC needs it
library(doMC) # for parralel processing
library(caret)
registerDoMC(cores = 2) # 5 cores pour parralel processing

data_train = info_trips[,1:50]
data_train$mode = as.factor(data_train$mode)
n = dim(data_train)[1]
p = dim(data_train)[2]-1
data_train.formule = mode ~ .

row.has.na <- apply(data_train, 1, function(x){any(is.na(x))})
sum(row.has.na)
data_train <- data_train[!row.has.na,]

fitControl <- trainControl(
  ## K-fold CV
  method = "repeatedcv",
  number = 5, #10
  ## repeated 
  repeats = 3) #10

##########  Random forest

# method = 'rf'
# Type: Classification, Regression
# Tuning parameters:
# mtry (#Randomly Selected Predictors)
# Number of variables randomly sampled as candidates at each split. 
# Note that the default values are different for classification (sqrt(p) 
# where p is number of variables in x) and regression (p/3)
# Required packages: randomForest
library(randomForest)
random.forest.grid = expand.grid(mtry = seq(1, round(sqrt(p))*2, 1))
random.forest.grid

random.forest.fit = train(data_train.formule,
                          data = data_train,
                          method = "rf",
                          trControl = fitControl,
                          tuneGrid = random.forest.grid,
                          ntree = 100)
random.forest.fit


##########  SVM 
#install.packages("kernlab")
library(kernlab)

# Support Vector Machines with Linear Kernel
# method = 'svmLinear'
# Type: Regression, Classification
# Tuning parameters:
# C (Cost)
# Required packages: kernlab
svm.linear.fit = train(data_train.formule,
                       data = data_train,
                       method = "svmLinear",
                       trControl = fitControl)
svm.linear.fit
