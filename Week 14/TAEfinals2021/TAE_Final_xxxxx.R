
# Please report your name and ID in the two lines below
# Name: Feng Zhengqing Mark
# ID: 1004314

# Load all packages
library(caTools)      # For properly splitting data into training and test sets
library(rpart)        # For CARTs
library(rpart.plot)   # For visualizing CARTs
library(randomForest) # For Random Forests
library(caret)        # For multiple ML functions
library(flexclust)    # For processing the results of cluster analysis
library(e1071)        # Naive Bayes Classifier 


## Question 1 [2 points] ####################################################

## Task 1 ###################################################################

# Consider a hypothetical scenario in which a bank decides to adopt a data-driven algorithm to evaluate 
# credit card applications. The information used by the algorithm encompasses a broad spectrum of data, 
# such as the applicant's gender, employment, assets owned, or salary. Identify one unintentional ethical 
# issue that might surface from the application of such algorithm. Justify your answer in one or two sentences.

# WRITE YOUR ANSWER HERE

# Using this data, banks creating a data-driven algorithm to evaluate credit cards applications can start to create customer profiling.
# Then, in the even they want to sell their new credit card to their existing users, they can use this profiling
# and purposefully target wealthier individuals who can afford the new credit card.

## Task 2 ###################################################################

# Consider a hypothetical modeling scenario in which you have solved a binary classification problem with 
# the aid of a CART. Is it possible to evaluate the Out-Of-Bag (OOB) error? Justify your answer in one or two sentences.

# WRITE YOUR ANSWER HERE 

# No. Out-of-bag error is obtain through bagging.. which is obtain through bootstrapping. As CART does not involve bootstrapping, we can't obtain OOB error.


#############################################################################


## Question 2 [16 points] ###################################################

# Our primary goal in this exercise is to understand the relationship between the first six variables 
# and `time_in_affairs`, and then develop data-driven models that predict it. 
# Before carrying out these tasks, we load the data:

# Remove all variables from the R environment to create a fresh start
rm(list=ls())

# Create dataframe
exm <- read.csv("extramarital.csv")

## Task 1 ###################################################################

# A person has an affair if the variable `time_in_affairs` is larger than 0. In the dataset, 
# what percentage of the readers have had affairs?

# WRITE YOUR ANSWER HERE

table(as.integer(exm$time_in_affairs > 0))

# 0    1 
# 4313 2053

2053/(2053+4313) #0.3224945

# 32.2% of readers had affairs
  
## Task 2 ###################################################################
  
# For each value, or level, of the variable `marriage_rating`, compute the number of readers who have had 
# an affair and the number of readers who have not had an affair. Based on this, do you think that readers 
# who had affairs tended to be less happy with their marriage than readers who did not have affairs? 
# Justify your answer in one sentence.

# WRITE YOUR ANSWER HERE

tapply(as.integer(exm$time_in_affairs > 0), exm$marriage_rating, mean)

#      1         2         3         4         5 
# 0.7474747 0.6350575 0.5508560 0.3229260 0.1814456

# Yes, readers who tend to had affairs tend to be less happy with their marriage. 74.7% of readers with marriage rating 1 has affairs, while only 18.1% of readers with marriage rating 5 had affairs. 
  
# Task 3 ####################################################################

# For each value, or level, of the variable `yrs_married`, compute the number of readers who have had 
# an affair and the number of readers who have not had an affair. Based on this, do you think that readers 
# who had affairs tended to be married for fewer years than readers who did not have affairs? 
# Justify your answer in one sentence.

# WRITE YOUR ANSWER HERE

tapply(as.integer(exm$time_in_affairs > 0), exm$yrs_married, mean)

#     0.5        2.5          6          9         13       16.5         23 
# 0.04324324 0.21583088 0.33917616 0.42192691 0.41186441 0.45110024 0.42540074

# No, readers who had affairs tend to be married for more years, than readers married for fewer years.


# Task 4 ####################################################################

# Calculate the correlation between each variable in the dataframe `exm` and `time_in_affairs`. 
# Does the value of the correlation reflect the answers you gave in Task 2 and Task 3?

# WRITE YOUR ANSWER HERE

# get the correlation matrix -> remove diagonal (all ones)
# -> then find the maximum (most positive correlation)

# ?cor

cor(exm[,1:7])
cor(exm[,1:7]) - diag(rep(1, 7)) # remove diagonals
max(cor(exm[,1:7]) - diag(rep(1, 7))) # 0.8940818

# most positive correlation between yrs_married and age

# Yes, marriage rating has a negative correlation to time in affairs, which supports Task 2 results
# Surprsingly,  yrs_married has a negative correlation to time in affairs, which does not support task 3 results

# Task 5 ####################################################################

# Suppose we want to build a regression model that predicts `time_in_affairs` as a function of the other variables. 
# Do you foresee any problem, or challenge, that we may face? 
# Justify your answer in one or two sentences. Hint: plot the variable `time_in_affairs`.

# WRITE YOUR ANSWER HERE

plot(exm$time_in_affairs)

# variable time_in_affairs takes on many different values, hence it cannot run regression model easily

# Task 6 ####################################################################

# We will now turn this problem into a multi-class classification problem by creating a new dependent variable. 
# Our new dependent variable, called `Affair`, will take three different values: `No`, `Minor`, and `Major`. 
# Create the variable by running the following code:

exm$affair <- factor(ifelse(exm$time_in_affairs == 0, "No", ifelse(exm$time_in_affairs >= 5, "Major", "Minor")))

# What is the fraction of readers that have had a major affair?

# WRITE YOUR ANSWER HERE

table(exm$affair)

# Major Minor    No 
# 149  1904  4313

149/(4313+1904+149) #0.02340559

# 0.02340559 fraction of readers had a major affairs

# Task 7 ####################################################################

# Randomly split the dataset `exm` into a training set, containing 70% of the observations, and a testing set, 
# containing 30% of the observations. Use the following code:

set.seed(100)                                   
spl   <- sample.split(exm$affair,SplitRatio=0.7) 
train <- subset(exm,spl==TRUE);             
test  <- subset(exm,spl==FALSE);  

# Build a CART model to predict `affair` using all of the other variables as independent variables except for 
# `time_in_affairs`. Use the training set to build the model and the default settings in fitting the CART model. 
# Before running the algorithm, set the seed of the R random number generator to 100 using `set.seed(100)`. 
# Which variable does the tree split on at the top node?

# WRITE YOUR ANSWER HERE

set.seed(100)
cart1 <- rpart(affair ~ marriage_rating +
                 age +
                 yrs_married +
                 religiosity +
                 education +
                 occupation ,data=train, method = "class")
cart1$frame["1", 1]

prp(cart1) # double check answer

# Variable marriage_rating splits on top node.

# Task 8 ####################################################################

# The CART model you just built never predicts one of the three outcomes. Which one?

# WRITE YOUR ANSWER HERE

# Based on the plot, Minor and No are predicted, Major outcome is never predicted

# Task 9 ####################################################################

# Consider a person who is 31 years old and is strongly religious. Would the CART model predict that he / she 
# would have no, minor, or major affair?

# WRITE YOUR ANSWER HERE

# The CART model is unable to predict the outcome because the variables split on are marriage_rating and yrs_married.

# Task 10 ###################################################################

# Make predictions on the test set, and then create a confusion matrix. What is the overall accuracy of the model?

# WRITE YOUR ANSWER HERE

predict_cart1 <- predict(cart1,newdata=test, type = "class")
#table(predict_cart1)

pred_table_cart1 <- table(test$affair, predict_cart1)
pred_table_cart1

# Actual       Predicted
#        Major Minor   No
# Major     0     4   41
# Minor     0   202  369
# No        0   119 1175

4+41+202+369+119+1175 # number of data in test set # 1910

# correct predictions are in diagonal entries

Accuracy <- (202+1175)/1910
Accuracy

# Accuracy of the model is 0.7209424

# Task 11 ###################################################################

# Look at the `cp` table created during the model identification process. Is it necessary to prune the tree? 
# If yes, what value of `cp` should we use? Explain your reasoning in one sentence.

# WRITE YOUR ANSWER HERE

printcp(cart1)

#     CP    nsplit rel error  xerror     xstd
# 1 0.084203      0   1.00000 1.00000 0.021714
# 2 0.033403      1   0.91580 0.91580 0.021192
# 3 0.010000      2   0.88239 0.89631 0.021058

# No, it is not necessary to prune the tree because the default cp value of 0.01 gives the smallest xerror
# However, we can consider building a deeper tree as the xerror seems to be going lower.

# Task 12 ###################################################################

# Now build a CART based on the answer you gave to the previous question (for example, you could prune 
# the original tree to build a smaller one, or decide to build a deeper tree). If you use the `rpart` function, 
# set the seed of the R random number generator to 100 using `set.seed(100)`. How many terminal leaves does the tree have?

# WRITE YOUR ANSWER HERE

#let's build a deeper tree... hopefully xerror will be smaller...
# we use cp = 0.0001

set.seed(100)
cart2 <- rpart(affair ~ marriage_rating +
                 age +
                 yrs_married +
                 religiosity +
                 education +
                 occupation ,data=train, method="class", cp = 0.0001)
cart2$frame["1", 1]
printcp(cart2)

#          CP nsplit rel error  xerror     xstd
# 1  0.08420320      0   1.00000 1.00000 0.021714
# 2  0.03340292      1   0.91580 0.91580 0.021192
# 3  0.00626305      2   0.88239 0.89631 0.021058
# 4  0.00278358      3   0.87613 0.89074 0.021019
# 5  0.00236604      6   0.86778 0.90118 0.021092
# 6  0.00208768     15   0.84551 0.90466 0.021116
# 7  0.00180932     20   0.83507 0.90327 0.021106
# 8  0.00173974     25   0.82603 0.91093 0.021159
# 9  0.00150777     30   0.81628 0.90814 0.021140
# 10 0.00139179     36   0.80724 0.90257 0.021102
# 11 0.00086987     42   0.79889 0.90466 0.021116
# 12 0.00069589     49   0.79054 0.91023 0.021154
# 13 0.00061857     65   0.77940 0.93946 0.021347
# 14 0.00046393     75   0.77314 0.94642 0.021391
# 15 0.00034795     81   0.77035 0.95338 0.021435
# 16 0.00029824     85   0.76896 0.96312 0.021496
# 17 0.00027836     92   0.76688 0.96799 0.021525
# 18 0.00023196    102   0.76409 0.97634 0.021576
# 19 0.00017397    111   0.76200 0.97634 0.021576
# 20 0.00013918    117   0.76061 0.97564 0.021571
# 21 0.00010000    127   0.75922 0.98191 0.021609

# the lowest xerror is the tree with xerror 0.89074
# prune to cp 0.00278358

set.seed(100)
cart3 <- rpart(affair ~ marriage_rating +
                 age +
                 yrs_married +
                 religiosity +
                 education +
                 occupation ,data=train, method="class", cp = 0.00278358)

prp(cart3) # 3 splits

# The tree has 4 terminal leaves

# Task 13 ###################################################################

# Using the new CART (created at Task 12), make predictions on the test set and then create a confusion matrix. 
# What is the overall accuracy of the model?

# WRITE YOUR ANSWER HERE

predict_cart3 <- predict(cart3,newdata=test, type = "class")

pred_table_cart3 <- table(test$affair, predict_cart3)
pred_table_cart3

#       Major Minor   No
# Major     0     3   42
# Minor     0   185  386
# No        0   106 1188

Accuracy <- (185+1188)/1910
Accuracy

# Accuracy has dropped to 0.7188482

# Task 14 ###################################################################

# Let's now move to a different model, Random Forests. Before running the algorithm, set the seed of the R 
# random number generator to 100 using `set.seed(100)`. Using the function `randomForest`, build a model 
# to predict `affair` using all of the other variables as independent variables except for `time_in_affairs`. 
# Use the default setting for the function `randomForest`. 
# What is the value of the Out-of-bag estimate of misclassification error?

# WRITE YOUR ANSWER HERE

set.seed(100)
forest <- randomForest(affair ~ marriage_rating +
                 age +
                 yrs_married +
                 religiosity +
                 education +
                 occupation ,data=train, method = "class")
forest

# randomForest(formula = affair ~ marriage_rating + age + yrs_married +      religiosity + education + occupation, data = train, method = "class") 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 2
# 
# OOB estimate of  error rate: 30.23%
# Confusion matrix:
#   Major Minor   No class.error
# Major     0    27   77   1.0000000
# Minor     3   490  840   0.6324081
# No        2   398 2619   0.1324942

# OOB misclassification error is 30.23%


# Task 15 ###################################################################

# Finally, use the model trained in the previous task to make predictions on the test set and create a confusion matrix. 
# What is the overall accuracy of the model?

# WRITE YOUR ANSWER HERE

predict_forest <- predict(forest,newdata=test, type = "class")

pred_table_forest <- table(test$affair, predict_forest)
pred_table_forest

#       Major Minor   No
# Major     0    10   35
# Minor     2   241  328
# No        0   160 1134

Accuracy <- (241+1134)/1910
Accuracy # 0.7198953

# Accuracy on test set is 0.7198953

# Task 16 ###################################################################

# Which is the most important variable (predictor) for the random forest?

# WRITE YOUR ANSWER HERE

varImpPlot(forest)
importance(forest)

#                 MeanDecreaseGini
# marriage_rating         218.4189
# age                     109.8472
# yrs_married             180.5721
# religiosity             108.1739
# education               115.4629
# occupation              116.5029

# marriage_rating is the most important variable.

#############################################################################


## Question 3 [12 points] ###################################################

# Our goal is to predict `RainTomorrow` using the other variables, or predictors, available in the dataset. 
# To that purpose, we will apply a technique known as cluster-then-predict.

# Remove all variables from the R environment to create a fresh start
rm(list=ls())

# Create dataframe
weatherAUS <- read.csv("weatherAUS.csv")

# Remove missing entries
weatherAUS <- na.omit(weatherAUS)

# Before beginning the exercise, we create a training and testing dataset, named `train` and `test`. 
# We then remove `RainTomorrow` (dependent variable) and `Date` from both sets, thereby creating 
# two additional datasets, `limitedTrain` and `limitedTest`. 
# Use the following code to carry out these operations:
  
# Create train and test datasets
idx <- c(1:2977)
spl <- c(1:2977)
spl[idx <= 2000] <- "TRUE"
spl[idx  > 2000] <- "FALSE"
train <- subset(weatherAUS,spl==TRUE)
test  <- subset(weatherAUS,spl==FALSE) 

# Create limitedTrain and limitedTest
limitedTrain <- train
limitedTrain$RainTomorrow  <- NULL
limitedTrain$Date  <- NULL
limitedTest <- test
limitedTest$RainTomorrow <- NULL
limitedTest$Date <- NULL

## Task 1 ###################################################################

# We are almost ready for the clustering process. The last pre-processing step is to normalize the data 
# by the mean and standard deviation of the variables in the training set. We can do this by using 
# the `preProcess()` function (`caret` package), which normalizes variables by subtracting the mean and dividing 
# by the standard deviation. Note that the function is also applied to the `limitedTest` dataset, 
# which will be used later. Use the following code:

# Normalize
preproc   <- preProcess(limitedTrain)
normTrain <- predict(preproc, limitedTrain)
normTest  <- predict(preproc, limitedTest)

# Why is it necessary to normalize the data? Justify your answer in one sentence.

# WRITE YOUR ANSWER HERE

# yay! caret function!!

# We normalise the data as larger values of the dataset can skew the calculation of euclidean distances, 
# hence skewing the clusters in its own favor.

## Task 2 ###################################################################

# What are the minimum and maximum values for the variable `MinTemp` in the original (`limitedTrain`) 
# and transformed (`normTrain`) training sets?

# WRITE YOUR ANSWER HERE

max(limitedTrain$MinTemp) # 28.3
max(normTrain$MinTemp) # 3.149369

min(limitedTrain$MinTemp) # -2.8
min(normTrain$MinTemp) # -1.962803

# Values obtained as seen above. 

# Task 3 ####################################################################

# We will now run the Hierarchical clustering algorithm on the normalized data (`normTrain` only). 
# The method requires us to first compute the Euclidean distance between the observations. 
# How many pairwise distances do we need to calculate? Suppose the dataset had 10,000 observations: 
# How many pairwise distances would we need to calculate?

# WRITE YOUR ANSWER HERE

distances <- dist(normTrain, method="euclidean")

# If dataset had 10,000 observations, then number of pairwise distances is n*(n-1) /2,
# where n = 10000
# number of pairwise distance is (10000*9999/2) = 49995000


# Task 4 ####################################################################

# Run the Hierarchical clustering algorithm using two different options for the linkage function: 
# `complete` and `ward.D2`. Each option yields a different dendrogram. How many clusters do we create 
# if we cut each dendrogram at a height equal to 10 units? Why is the number of clusters different? 
# Justify your answer in one sentence.

# WRITE YOUR ANSWER HERE

# ?hclust
# ?cutree

clusterWeather1 <- hclust(distances, method="ward.D2")
plot(clusterWeather1)
clusterWeather1d  <- cutree(clusterWeather1, h = 10)
table(clusterWeather1d) # 38 clusters in total

clusterWeather2 <- hclust(distances, method="complete")
plot(clusterWeather2)
clusterWeather2d  <- cutree(clusterWeather2, h = 10)
table(clusterWeather2d) # 5 clusters in total

# The difference in the number of clusters is due to the different methods used.
# Ward's method aims to find compact spherical clusters, while Complete finds similar clusters

# Task 5 ####################################################################

# Let's keep the results obtained with the the linkage function `ward.D2`. Cut the dendrogram and create 5 clusters. 
# Which cluster has the highest average value of the variable `MinTemp`? 
# Note: To answer this question, you can use either the normalized data or original data.

# WRITE YOUR ANSWER HERE

clusterWeather1e  <- cutree(clusterWeather1, k = 5) # keep ward.D2
table(clusterWeather1e) # 5 clusters

tapply(normTrain$MinTemp, clusterWeather1e, mean)
# 1          2          3          4          5 
# 1.1005241  0.0555095  0.7028689  0.1099238 -0.9285213

tapply(limitedTrain$MinTemp, clusterWeather1e, mean)
#     1         2         3         4         5 
# 15.835810  9.478443 13.416667  9.809474  3.492072 

# Cluster 1 has the highest average value of the variable MinTemp

# Task 6 ####################################################################

# We now move to the k-means algorithm: set the random seed to 100 (by using the command `set.seed(100)`), 
# and run k-means clustering with 3 clusters on `normTrain`, with 50 restarts of the algorithm (option `nstart`). 
# Store the result in an object called `km`. Which cluster has the smallest number of observations? 

# WRITE YOUR ANSWER HERE

set.seed(100)
km <- kmeans(normTrain, centers=3, nstart=50)
table(km$cluster)

#   1   2   3 
# 429 803 768 

# Cluster 1 has the smallest number of observations

# Task 7 ####################################################################

# We are almost ready for cluster-then-predict: use the following code to obtain training set and testing set 
# cluster assignments for our observations and to do the predictions:

km.kcca      <- as.kcca(km, normTrain)
clusterTrain <- predict(km.kcca)
clusterTest  <- predict(km.kcca, newdata=normTest)

# How many test-set observations were assigned to Cluster 1, 2, and 3 for `clusterTest`?

# WRITE YOUR ANSWER HERE

table(clusterTest)

# clusterTest
# 1   2   3 
# 173 335 469

# Cluster 1 has 173, Cluster 2 has 335, Cluster 3 has 469 test-set observations respectively.

# Task 8 ####################################################################

# Using the `subset()` function, build the dataframes `train1`, `train2`, and `train3`, containing 
# the elements in the original training set (`train`) assigned to clusters 1, 2, and 3, respectively. 
# Similarly build `test1`, `test2`, and `test3` from the original testing set (`test`). 
# Which training set has the highest number of rainy days?

# WRITE YOUR ANSWER HERE

train1 <- subset(train, clusterTrain == 1)
train2 <- subset(train, clusterTrain == 2)
train3 <- subset(train, clusterTrain == 3)

test1 <- subset(test, clusterTest == 1)
test2 <- subset(test, clusterTest == 2)
test3 <- subset(test, clusterTest == 3)

# Which training set has the highest number of rainy days?

table(train1$RainTomorrow)
# No Yes 
# 215 214

table(train2$RainTomorrow)
# No Yes 
# 689 114

table(train3$RainTomorrow)
# No Yes 
# 679  89

# train1 has the highest number of rainy days.

# Task 9 [2 points] #########################################################

# Build three Naive Bayes Classifiers, named `model1`, `model2`, and `model3`, 
# trained on `train1`, `train2`, and `train3`, respectively. When training the models, drop the variable `Date`. 
# Report the accuracy of each model on the corresponding testing set, that is, `test1`, `test2`, and `test3.`

# WRITE YOUR ANSWER HERE

model1 <- naiveBayes(as.factor(RainTomorrow) ~ MinTemp + MaxTemp + Rainfall +
                       WindSpeed9am + WindSpeed3pm + Humidity9am + Humidity3pm +
                       Pressure9am + Pressure3pm + Temp9am + Temp3pm,
                       data=train1)

PredictTest1 <- predict(model1, newdata = test1, type="class")
pred_table <- table(PredictTest1,test1$RainTomorrow)
Accuracy <- (pred_table[1,1]+pred_table[2,2])/sum(pred_table)
Accuracy # of model1 is 0.6936416

model2 <- naiveBayes(as.factor(RainTomorrow) ~ MinTemp + MaxTemp + Rainfall +
                       WindSpeed9am + WindSpeed3pm + Humidity9am + Humidity3pm +
                       Pressure9am + Pressure3pm + Temp9am + Temp3pm,
                     data=train2)

PredictTest2 <- predict(model2, newdata = test2, type="class")
pred_table <- table(PredictTest2,test2$RainTomorrow)
# pred_table
Accuracy <- (pred_table[1,1]+pred_table[2,2])/sum(pred_table)
Accuracy # of model2 is 0.8268657

model3 <- naiveBayes(as.factor(RainTomorrow) ~ MinTemp + MaxTemp + Rainfall +
                       WindSpeed9am + WindSpeed3pm + Humidity9am + Humidity3pm +
                       Pressure9am + Pressure3pm + Temp9am + Temp3pm,
                     data=train3)

PredictTest3 <- predict(model3, newdata = test3, type="class")
pred_table <- table(PredictTest3,test3$RainTomorrow)
pred_table
Accuracy <- (pred_table[1,1]+pred_table[2,2])/sum(pred_table)
Accuracy # of model3 is  0.8742004

# Task 10 [2 points] ########################################################

# Finally, compute the overall test-set accuracy of the cluster-then-predict approach.

# WRITE YOUR ANSWER HERE

AllPredictions <- c(PredictTest1, PredictTest2, PredictTest3)
ALlOutcomes <- c(test1$RainTomorrow,
                 test2$RainTomorrow,
                 test3$RainTomorrow) # actual results

pred_table <- table(AllPredictions,AllOutcomes)
pred_table

Accuracy <- (pred_table[1,1]+pred_table[2,2])/sum(pred_table)
Accuracy # overall accuracy is 0.8742004






############################################ END OF EXAM ###################################################