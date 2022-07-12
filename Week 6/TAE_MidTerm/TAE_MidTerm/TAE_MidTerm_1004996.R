
# Please report your name and ID in the two lines below
# Name: ...
# ID: ...

# Load all packages
library(caTools)      # For properly splitting data into training and test sets
library(ROCR)         # For calculating model performance measures


## Question 1 [2 points] ####################################################

## Task 1 ###################################################################

# Suppose that p_1 and p_2 are two principal components vectors (obtained via Principal Component Analysis), 
# with the eigenvalue of p_1 (lambda_1) larger than the eigenvalue of p_2 (lambda_2). Which of the following statements 
# are / is correct about p_1 and p_2? (You do not need to justify your answer.)
#
# 1. p_1 is orthogonal to p_2.
# 
# 2. p_1 is parallel to p_2.
# 
# 3. The variance along p_1 is bigger than the variance along p_2.
# 
# 4. The variance along p_2 is bigger than the variance along p_1.

# 3

## Task 2 ###################################################################

# Which of the following statements are / is correct (about different resampling methods)? 
# (You do not need to justify your answer.)
# 
# 1. Leave-one-out cross-validation (LOOCV) is a special case of k-fold cross-validation.
# 
# 2. k-fold cross-validation has higher variance than LOOCV when k < n.
# 
# 3. LOOCV tends to overestimate the test error rate in comparison to validation set approach.

# 2 


#############################################################################


## Question 2 [14 points] ###################################################

# The National Longitudinal Surveys (NLS) are a set of surveys designed to gather information at 
# multiple points in time on the labor market activities and other significant life events of several 
# groups of men and women. Because of its quality, breadth, and thoroughness, the NLS have become 
# some of the most analyzed data in the social sciences. In this question, we will study the data taken 
# from the website [https://www.bls.gov/nls/](https://www.bls.gov/nls/) to analyze the relationship 
# between wages and education, ability, and family characteristics. 
# The data are provided in the file `wages.csv`, which contains the following variables:
# 
# * `ID`: Identification number of the individual
# * `EDUC`: Education (0 = High School, 1 = College, 2 = Postgraduate)
# * `LOGWAGE`: Logarithm of hourly wages
# * `EXPER`: Labor market experience
# * `ABILITY`: Ability (measured from a standardized test score where a higher value indicates better ability)
# * `MOTHERED`: Mother’s education (years of schooling)
# * `FATHERED`: Father’s education (years of schooling)
# * `BRKNHOME`: Residence in a broken home (1 = yes, 0 = no)
# * `SIBLINGS`: Number of siblings.
# 
# In the dataset, the variables `EDUC`, `LOGWAGE`, and `EXPER` for an individual vary with time, 
# while the variables `ABILITY`, `MOTHERED`, `FATHERED`, `BRKNHOME` and `SIBLINGS` for an individual do not vary with time.
# 
# 
# Remove all variables from the R environment to create a fresh start
rm(list=ls())

## Task 1 ###################################################################

# Read the data into the dataframe `wages`. How many individuals are there in the dataset? 
# 
wages <- read.csv("wages.csv")

# WRITE YOUR ANSWER HERE
max(wages$ID)
#ans 2178
  
## Task 2 ###################################################################
  
# Identify the individual with the largest number of observations in this dataset.

# WRITE YOUR ANSWER HERE
table(max(nrow(wages$ID)))
table(wages$ID)
max(table(wages$ID))
#ans 15
  
# Task 3 ####################################################################

# Develop a linear regression model to predict the `LOGWAGE` variable using the variables 
# which are time invariant in the dataset (i.e., `ABILITY`, `MOTHERED`, `FATHERED`, `BRKNHOME`, and `SIBLINGS`). 
# What is the R-squared for the model?

# WRITE YOUR ANSWER HERE
model1  <- lm(LOGWAGE~ABILITY+MOTHERED+FATHERED+BRKNHOME+SIBLINGS,data=wages)
summary(model1)$r.squared
#ans 0.05406993

# Task 4 ####################################################################

# Now, develop a linear regression model to predict the `LOGWAGE` variable using the variables 
# that are time varying in the dataset (i.e., `EDUC` and `EXPER`). 
# What is the linear regression equation produced by the model?

# WRITE YOUR ANSWER HERE
model2  <- lm(LOGWAGE~EDUC+EXPER,data=wages)
summary(model2)
summary(model2)$coefficients
# ans 
# y = 1.9058640 + (0.2656628*EDUC) + (0.0343065*EXPER)

# Task 5 ####################################################################

# Which among the two models developed in Task 3 and Task 4 do you think is preferable? 
# Provide a clear justification.

# WRITE YOUR ANSWER HERE
summary(model1)$r.squared
summary(model1)$adj.r.squared
summary(model2)$r.squared #higher 
summary(model2)$adj.r.squared #higher
#ans 
#The model in task 4 is preferable as it has a higher r.squared and adj.r.squared

# Task 6 ####################################################################

# Develop a linear regression model to predict the `LOGWAGE` variable using both the time varying 
# and time invariant predictor variables. 
# Identify the variables which are not significant in the model at the 0.05 level.

# WRITE YOUR ANSWER HERE
model3  <- lm(LOGWAGE~ABILITY+MOTHERED+FATHERED+BRKNHOME+SIBLINGS+EDUC+EXPER,data=wages)
summary(model3)
p_val_1 <- summary(model3)$coefficients[,4]
names(p_val_1[p_val_1 > 0.05])

#ans "MOTHERED" is the only one not significant 

# Task 7 ####################################################################

# Which pair of variables in the dataset has the maximum correlation? 
# Use this result to provide a justification for your observation in Task 6.

# WRITE YOUR ANSWER HERE
#cor(wages[,'ABILITY','MOTHERED','FATHERED','BRKNHOME','SIBLINGS','EDUC','EXPER'])
library(psych)
pairs.panels(wages)

#MOTHERED has a high correlation with FATHERED at 0.68 
#Thereby only one variable can preferably be used 
#preferably create a model with these variables separately and see which performs better 


# Task 8 ####################################################################

# Use the adjusted R-squared value to identify which among the models developed in Task 3, Task 4, and Task 6 
# is the most suitable. We will work with this model from this point onward.

# WRITE YOUR ANSWER HERE
summary(model1)$adj.r.squared
summary(model2)$adj.r.squared
summary(model3)$adj.r.squared #task 6 has the highest value

# Task 9 ####################################################################

# Predict the hourly wage for someone who has been to high school when all other variables are at their average values 
# and when the person is not from a broken home. (Recall we predict the logarithm of hourly wages.)

# WRITE YOUR ANSWER HERE
wage_highschool <- subset(wages, wages$EDUC ==0 & wages$BRKNHOME ==0)
mean(wage_highschool$LOGWAGE)

#log value = 2.220994
exp(mean(wage_highschool$LOGWAGE))
# ans 9.216487 is the hourly wage 

# Task 10 ###################################################################

# Does the `LOGWAGE` variable have a negative dependence on any of the variables in the model? 
# If yes, provide an intuitive explanation for this result.
summary(model3)
# ans 
#LOGWAGE has a negative dependence on BRKNHOME. 
#A  person living in a broken home is less likely to afford a quality education and thereby they have a lower average wage


# Task 11 ###################################################################

# Estimate the percentage change in hourly wages that is associated with a change in the variable `BRKNHOME` 
# from 1 to 0 (or 0 to 1). To this purpose, you can use the regression coefficient of the selected model.

# WRITE YOUR ANSWER HERE
wage_highschool_BRKNHOME_yes <-  subset(wages, wages$EDUC ==0 & wages$BRKNHOME ==1)
wage_highschool_BRKNHOME_no <- subset(wages, wages$EDUC ==0 & wages$BRKNHOME ==0)

percentage <- (mean(wage_highschool_BRKNHOME_no$LOGWAGE) - mean(wage_highschool_BRKNHOME_yes$LOGWAGE)) / mean(wage_highschool_BRKNHOME_no$LOGWAGE)
percentage

#ans 0.02793806

# Task 12 ###################################################################

# We now consider variants of the model. One might suspect that the value of education is enhanced by greater ability. 
# We could examine this effect by introducing an interaction of the two variables in the equation by adding 
# the variable `EDUCABILITY = EDUC × ABILITY`. Add this variable to the model chosen at Task 8 and recompute the model. 
# Is there any indication that this term is significant in the model?

# WRITE YOUR ANSWER HERE
wages$EDUCABILITY <- wages$EDUC * wages$ABILITY
model4 <- lm(LOGWAGE~ABILITY+MOTHERED+FATHERED+BRKNHOME+SIBLINGS+EDUC+EXPER+EDUCABILITY,data=wages)
summary(model4)

# ans 
#the term EDUCABILITY is significant in as its p-value is 0.0040 (below 0.01) and helps enhance the model

# Task 13  [2 points] ########################################################

# We now consider another variant of the model. The current model assumes that the increment in the `LOGWAGE` variable is 
# the same at the different levels of education (high school, college, or postgraduate). We now build a more flexible model 
# that allows for these increments to be different for each level of education. Define a new variable `HS` that takes a value 
# of 1 if the highest degree of the individual is high school and 0 otherwise. 
# Similarly define two new binary variables `Col` and `Grad` that take a value of 1 if the highest degrees are college and 
# postgraduate degrees, respectively, and 0 otherwise. Replace `EDUC` in the model chosen at Task 8 with `Col` and `Grad` 
# (thereby making `HS` the base category) and recompute the model. What is the sum of squared errors in the model?

# WRITE YOUR ANSWER HERE
wages$HS <- wages$EDUC >= 0
wages$COL <- wages$EDUC >= 1
wages$GRAD <- wages$EDUC >= 2

model4 <- lm(LOGWAGE~ABILITY+MOTHERED+FATHERED+BRKNHOME+SIBLINGS+EXPER+EDUCABILITY+COL+GRAD,data=wages)
summary(model4)
# ans squared error is 0.485

#############################################################################


## Question 3 [14 points] ###################################################

# The data in this question are related to the direct marketing campaign of a Portuguese banking institution 
# collected over a period from 2008 to 2010 through the financial crisis. The marketing campaigns were based 
# on phone calls. Often, more than one contact to the same client was required. The goal is to predict the success 
# of telemarketing calls for selling bank long-term deposits, in order to assess if the bank term deposit would be 
# subscribed (1) or not (0). This is captured in the `subscribe` variable. 
# The data are provided in the file `bank.csv`, which contains the following variables:
#   
# * age: Age of client
# * job: Type of job
# * marital: Marital status
# * education: Education level
# * default: Does the client have credit in default? (yes or no)
# * balance: Balance in the bank for the client (in euros)
# * housing: Does the client have an housing loan? (yes or no)
# * loan: Does the client have a personal loan? (yes or no)
# * contact: Type of contact communication
# * day: Last day client was contacted during the current campaign
# * month: Last month client was contacted during the current campaign
# * duration: Contact duration of the last call with the client during the current campaign (in seconds)
# * campaign: Number of contacts performed during the current campaign
# * previous: Number of contacts performed before this campaign
# * poutcome: Outcome of the previous marketing campaign
# * subscribe: Has the client subscribed to a term deposit? (1 = yes or 0 = no)
# 
# 
# Remove all variables from the R environment to create a fresh start
rm(list=ls())

## Task 1 ###################################################################

# Read the dataset into the dataframe `bank`. Use a two-sided t-test to verify if there is evidence that 
# the average bank balance of married clients is significantly different from that of single clients. 
# What is the p-value of the test and your conclusion?
#   
bank <- read.csv("bank.csv")
t.test(bank$balance[bank$marital=='married'],bank$balance[bank$marital=='single'])
# p-value = 0.9784
#There is insufficient evidence at the 5% significance level to reject the null hypothesis


# WRITE YOUR ANSWER HERE

## Task 2 ###################################################################

# Use a two-sided t-test to verify if there is evidence that the average bank balance of married clients is 
# significantly different from that of divorced clients. What is the p-value of the test and your conclusion?

# WRITE YOUR ANSWER HERE
t.test(bank$balance[bank$marital=='married'],bank$balance[bank$marital=='divorced'])
# p-value = 0.002659
#There is sufficient evidence at the 5% significance level to reject the null hypothesis



# Task 3 ####################################################################

# We are interested in predicting the `subscribe` variable. Ensure that the package caTools is installed 
# and loaded before running these commands: 
#   
set.seed(2017)
spl <- sample.split(bank$subscribe, SplitRatio=0.80)
# 
# 
# Using the information contained in `spl`, we split the data frame into a training dataframe called `train` 
# using the observations for which `spl` is `TRUE` and a test dataframe called `test` using the observations 
# for which `spl` is `FALSE`. Why do we set the seed before splitting the data into a training and testing set?
# 
train <- subset(bank,spl==TRUE)
test  <- subset(bank,spl==FALSE)
# 
# 
# 1. It balances the independent variables between the training and testing sets.
# 2. It balances the dependent variable between the training and testing sets.
# 3. It ensures that the random split of the data into a training and test set is replicable. 
# 4. It ensures that the split of the data into a training and test set is random.

# 2

# Task 4 ####################################################################

# We will now build a logistic regression model to predict the `subscribe` variable using all the variables 
# in the training dataset. Identify all the variables that are identified to be significant at the 0.05 level.

# WRITE YOUR ANSWER HERE
model2_1 <- glm(subscribe ~., data = train, family = binomial)
summary(model2_1)
p_val_2_1 <- summary(model2_1)$coefficients[,4]
names(p_val_2_1[p_val_2_1 < 0.05])

# ans 
#[1] "(Intercept)"     "jobunemployed"   "loanyes"         "contactunknown" 
#[5] "monthjun"        "monthmar"        "monthnov"        "monthoct"       
#[9] "monthsep"        "duration"        "poutcomesuccess"


# Task 5 ####################################################################

# Use the model to estimate the average probability of clients subscribing to the term deposit in the training set. 
# Compare this value to the actual fraction of individuals with `subscribe = 1` in the training set.

# WRITE YOUR ANSWER HERE
x <- model2_1$coefficients[1]
x
exp(x)/(1+exp(x))
# ans  0.04553238 

# Task 6 ####################################################################

# The 2008 financial crisis strongly increased the pressure for Portuguese banks to increase long term deposits. 
# The telemarketing group was evaluating the strategy of increasing the contacts with clients who have already been 
# contacted multiple times in the current campaign. 
# Would you recommend this strategy to the bank? Justify your recommendation.

# WRITE YOUR ANSWER HERE
summary(model2_1)
# ans
# Not a good strategy 
# campaign is a negative coefficient with the model 
# increasing it will deccrease the probability of a subscription

# Task 7 ####################################################################

# Suppose you use a threshold of 0.5 to make predictions on the test set using the model estimated in Task 4. 
# What is the accuracy of the model on the test set?

# WRITE YOUR ANSWER HERE
pred <- predict(model2_1, newdata = test, type = "response")
table(pred >= 0.5, test$subscribe)
x <- sum(diag(table(pred >= 0.5, test$subscribe)))/nrow(test) 
x # ans 0.8772124

# Task 8 ####################################################################

# Compare this accuracy with a baseline model that predicts the majority outcome in the test set.

# WRITE YOUR ANSWER HERE
x <- sum(diag(table(pred, test$subscribe)))/nrow(test)
x #ans 0.001106195

# Task 9 ####################################################################

# Provide a graphical illustration of the Receiver Operating Characteristic (ROC) curve. Use only the `test` dataset.

# WRITE YOUR ANSWER HERE
library(ROCR)
ROCRpred <- prediction(pred,test$subscribe)
ROCRperf <- performance(ROCRpred,x.measure="fpr",measure="tpr")
plot(ROCRperf)
plot(ROCRperf, colorize=T,print.cutoffs.at=c(0,0.1,0.2,0.3,0.5,1), text.adj=c(-0.2,1.7))

# Task 10 ####################################################################

# What is the corresponding value of the Area Under the Curve (AUC)?

# WRITE YOUR ANSWER HERE
as.numeric(performance(ROCRpred,measure="auc")@y.values)
# ans 0.8559856

# Task 11 [2 points] ########################################################

# In some datasets such as this, the number of samples of the two possible outcomes of the dependent variable is unbalanced, 
# rather than distributed in equal parts. On fitting a logistic regression model to such data, it is invariably found that 
# the estimated predicted probabilities are quite high for the outcome of the dependent variable with more observations and 
# much lower for the outcome of the dependent variable with the lesser observations. 
# One goodness measure of fit that has been proposed to deal with this issue is to compute the difference of the following two numbers:
# 
# 1. Average predicted probability of subscription across individuals who subcribed
# 
# 2. Average predicted probability of subscription across individuals who did not subcribe
# 
# Calculate this metric on the `train` and `test` datasets. 
# How does it compare against the accuracy of two baseline models that predict the minority outcome in the test set?

# WRITE YOUR ANSWER HERE
test_subscribed <- subset(test, test$subscribe==1)
test_notsubscribed <- subset(test, test$subscribe==0)

pred_subscribed <- predict(model2_1, newdata = test_subscribed, type = "response")
CM1 <- table(pred_subscribed >= 0.5, test_subscribed$subscribe)
(CM1[1,1]/sum(CM1)) #lower accuracy than baseline model 

pred_notsubscribed <- predict(model2_1, newdata = test_notsubscribed, type = "response")
CM2 <- table(pred_notsubscribed >= 0.5, test_notsubscribed$subscribe)
(CM2[1,1]/sum(CM2)) #higher accuracy than baseline model 

# Task 12 [2 points] ########################################################

# One indicator of fit for a logistic regression model is the McFadden pseudo R-squared measure, which, in this case, is defined as 
# 1 - LogLikelihood of fitted model)/(LogLikelihood of best fit with only intercept).
# 
# What is the value of the McFadden pseudo R-squared measure on the training set?

# WRITE YOUR ANSWER HERE
#pred.out.link <- predict(train, type = "link")
#mod.out.null <- glm(Default~1, family = binomial, data = outSample)
model2_1 <- glm(subscribe ~., data = train, family = binomial)
mod_null <- glm(subscribe~1, family = binomial, data = train)
pR2.out <- 1 - logLik(model2_1)/logLik(mod_null)
pR2.out
#ans 0.3386835

############################################ END OF EXAM ###################################################