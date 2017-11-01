##########  BUS or CAR classifier ##########

### DATA PRE-PROCESSING ###

### FEATURE EXTRACTION  ###

  #Need of feature engineering = process of using domain knowledge to create features to enable machine learning methods to work well
  #Two categories of features = Global & Local
  #Global features refer to descriptive statistics for the entire trajectory, which makes trajectories more comparable
  #Local features extracted by profile decomposition reveal more detail in movement behavior

##  GLOBAL FEATURES ##

# speed #
nlevels(compiegne_data$id)
table(compiegne_data$id)
id = '59046be7c9e77c0001b582e4'
compiegne_data_id = compiegne_data[compiegne_data$id==id,]
source('functions/calcul_vitesse_1.R')
source('functions/calcul_vitesse_2.R')
v = vitesse(compiegne_data_id[1,],compiegne_data_id[2,])
#if 'still' then v=0, else ...


# acceleration  #
# a = v_f - v_i / t


# direction #
source('direction.R')
d = direction(compiegne_data_id[3,],compiegne_data_id[4,])
print(d)

# sinuosity #
#Sinuosity measures the deviation of a line from the shortest path, calculated by dividing total length by shortest possible path.  


####
X <- split(compiegne_data, compiegne_data$id, drop=TRUE)
X
nrow(X[[1]])
nb_trips = cumsum (X[[1]]$mode == 10)
trips = split(X[[1]], cumsum (X[[1]]$mode == 10))




