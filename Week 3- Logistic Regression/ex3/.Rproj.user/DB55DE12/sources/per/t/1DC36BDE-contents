#1
baseballlarge <- read.csv("baseballlarge.csv")
str(baseballlarge)

#a
#i
nrow(baseballlarge)
#ii
table(baseballlarge$Year)
length(table(baseballlarge$Year))
#iii
baseballlarge <- subset(baseballlarge, Playoffs == 1)
nrow(baseballlarge)
#iv
table(baseballlarge$Year)

#b
year_col <- baseballlarge$Year
baseballlarge$NumCompetiors <- table(year_col)[as.character(year_col)]
table(baseballlarge$NumCompetitiors)
table(baseballlarge$NumCompetitiors)['8']
unname(table(baseballlarge$NumCompetitors)["8"])

#c
baseballlarge$WorldSeries <- as.integer(baseballlarge$RankPlayoffs==1)
table(baseballlarge$WorldSeries)

#d
model_1_d <- glm(WorldSeries ~ Year, data = baseballlarge, family = binomial)
summary(model_1_d)
x <-summary(model_1_d)
x$iter # simply type "x$" and wait for RStudio to suggest things
#x$coefficients[row,col]
x$coefficients[2,4] #p-value for model 

#much faster way to compare all individual variables 
#create a for loop for all the variables 
p_val_1_d <- c()
model_list_1 <- list()
all_vars_1 <- c("Year", "RS", "RA", "W", "OBP", "SLG", "BA",
               "RankSeason", "NumCompetitors", "League") #change accordingly

#to train single-variable models                                                                                           
for (variable in all_vars_1) {
  
  model <- glm(as.formula(paste0("WorldSeries ~ ", variable)),
               data = baseballlarge, family = binomial)
  model_list_1[[variable]] <- model #save the trained model in the list
  # we are appending a named numeric variable, for reference later
  p_val_1_d <- c(p_val_1_d,
                 setNames(summary(model)$coefficients[2, 4], variable))  
}
p_val_1_d

#filter out columns with p-values below 0.05
sig_vars_1_d <- names(p_val_1_d[p_val_1_d < 0.05])
sig_vars_1_d

#e
sig_vars_1_e <- c(sig_vars_1_d, "W", "SLG") # add on manually considered W and SLG
formula_1 <- as.formula(paste0("WorldSeries ~ ",
                                paste0(sig_vars_1_e, collapse = "+")))
model_1_e <- glm(formula_1, data = baseballlarge, family = binomial)
summary(model_1_e)
p_val_1_e <- summary(model_1_e)$coefficients[,4]
p_val_1_e
names(p_val_1_e[p_val_1_e < 0.05])

#f
# run a correlation function 
corr_1 <- cor(baseballlarge[,sig_vars_1_d])
corr_1
#extract ones where correlation is greater than 0.8
diag(corr_1) <- 0  # self-correlation not relevant
row.names(which(corr_1 > 0.8, arr.ind = T))
#variables highly correlated are "NumCompetitors" "Year" 

#g
#build code to compare all different type of variables from the 4 variables 
var_combns_1 <- combn(sig_vars_1_d, 2)  # combinations of 2 variables
aic_table_1 <- data.frame(var_1 = character(), var_2 = character(),
                          aic = numeric())

#iterate through the pariwise combinations of variables
for (idx in 1:choose(length(sig_vars_1_d), 2)) {
  var_comb <- var_combns_1[,idx] #get the combination of variables
  
  model <- glm(formula(paste0("WorldSeries ~ ", paste0(var_comb, collapse = "+"))),
               data = baseballlarge, family = binomial)
  # note that rbind is relatively slow but this is not too important
  aic_table_1 <- rbind(aic_table_1,
                       data.frame(var1 = var_comb[1],
                                  var2 = var_comb[2],
                                  aic = summary(model)$aic))
  
}

#iterate through all the original variables (single variables)
for (model in model_list_1) {
  aic_table_1 <- rbind(aic_table_1,
                       data.frame(var1 = names(model$coefficients)[2],
                                  var2 = NA,  # NULL does not work
                                  aic = model$aic))
}
aic_table_1

#find model with minimun AIC
aic_table_1[which.min(aic_table_1[,3]),]

#h
#AIC with the smallest variable only has one variable


#use this to help you clear your environment :)
setdiff(ls(), ls(pattern = "SETUP"))
rm(list = setdiff(ls(), ls(pattern = "SETUP")))


#q2
Parole <- read.csv("Parole.csv")

#a
nrow(Parole)

#b
nrow()
nrow(subset(Parole, Parole$Violator==1))

#c
sapply(Parole, class)
# If there is a column with character type you wish to convert eg.State, use: Parole$State <- as.factor(Parole$State)
sapply(Parole[sapply(Parole, is.factor)], nlevels) #get the number of levels of each column

#ans State and Crime are the unordered factor variables 

#d

set.seed(144)
library(caTools)
s1 <- sample.split(Parole$Violator, SplitRatio = 0.7)
tr1 <- subset(Parole, s1 == TRUE)
te1 <- subset(Parole, s1 == FALSE)


#e
set.seed(144)
library(caTools)  # not required by this point
split <- sample.split(Parole$Violator, SplitRatio = 0.7)
train <- subset(Parole, split == TRUE)
test <- subset(Parole, split == FALSE)

model_2 <- glm(Violator ~ ., data = train, family = binomial)
summary(model_2)

#extracting the significant variables with p-value below 0.05
coef_table_2 <- summary(model_2)$coefficients #save the coefficients for later use
p_val_2_e <- coef_table_2[,4]
sig_vars_2 <- names(p_val_2_e[p_val_2_e <= 0.05])
sig_vars_2

#f
# Get the corresponding coefficient of the 'MultipleOffenses' variable (amount of increase in log odds if one comitted multiple offenses)
# Use row name to extract row (check spelling!); column 1 corresponds to coefficient column
coef_multiple <- coef_table_2["MultipleOffenses", 1]
exp(coef_multiple) # odds

#g
# Here we prep the coefficients we need
coef_2 <- coef_table_2 #includes many coefficients we don't need

# We want to remove the coefficients for StateLouisiana, StateOther, StateVirginia, CrimeDrugs, CrimeOther
coef_2 <- coef_2[!(startsWith(rownames(coef_2), "State")  |
                     startsWith(rownames(coef_2), "Crime")) |
                   rownames(coef_2) == "CrimeLarceny",]

# Or we can also choose the variables (rows) we want to keep
# keep_var <- c("(Intercept)","Male", "RaceWhite", "Age", "TimeServed", "MaxSentence", "MultipleOffenses", "CrimeLarceny")
# coef_2 <- coef_table_2[keep_var,]

# Note that we retain CrimeLarceny because our prisoner has that
# Also note that StateKentucky is being used as a reference
# and hence there is no coefficient

# Based on information given, define x_2
x_2 <- c(1,  # intercept
         1,  # male
         1,  # white
         50, # age
         3,  # time served
         12, # max sentence
         0,  # multiple offenses
         1)  # larceny
logodds_2 <- coef_2[,1] %*% x_2  # matrix mult
#logodds_2
#exp(logodds_2)
prob = exp(logodds_2)/ (1+exp(logodds_2))
prob

#h 
pred_2_h <- predict(model_2, newdata = test, type = "response")
max(pred_2_h)
#0.9072791

#i
pred_table_2 <- table((pred_2_h > 0.5), test$Violator)
pred_table_2

# sensitivity (true positive rate)
#The Sensitivity (True Positive Rate) is 12/(11+12)=0.521
pred_table_2[2,2]/sum(pred_table_2[,2])

# specificity (true negative rate)
#The Specificity (True Negative Rate) is 167/(167+12)=0.932
pred_table_2[1,1]/sum(pred_table_2[,1])

# accuracy (accuracy)
#The Accuracy is (167+12)/(167+11+12+12)=0.886
sum(diag(pred_table_2)/sum(pred_table_2))

#j
# To predict every parolee as a non-violator, model will have accuracy = total number of non-violators/total number of parolees
#The accuracy of a simple model that predicts that every parolee is a non-violator is 179/202=0.886.
table(test$Violator)[1]/nrow(test)

#m
suppressMessages(library(ROCR))  # suppressMessages not critical - this is used as loading this library prints dependencies loaded

# Make a prediction and find the model performance using predicted values
predrocr_2 <- prediction(pred_2_h, test$Violator)
auc_2 <- performance(predrocr_2, measure = "auc")@y.values[[1]]
auc_2 # AUC value (area under curve) - the closer to 1, the better

#END_________

setdiff(ls(), ls(pattern = "SETUP"))
rm(list = setdiff(ls(), ls(pattern = "SETUP")))

#q3

#a
germancredit <- read.csv("germancredit.csv")

set.seed(2019)
library(caTools)
spl <- sample.split(germancredit$resp, 0.75)

training <- subset(germancredit, spl == TRUE)
test <- subset(germancredit, spl == FALSE)

#b
mmodel_3_b <- glm(resp ~ 1, data = training, family = binomial)
x <- model_3_b$coefficients[1]  
x # intercept

#calculating probability 
exp(x) / (1+exp(x))

#c
# Variable 'resp' gives the credit rating of the respondents
n_good <- table(training$resp)["1"]
n_bad <- table(training$resp)["0"]

n_good/(n_good+n_bad) # same as probability of resp=1 in simple logistic regression model in (b)

#d
model_3_d <- glm(resp ~ ., data = training, family = binomial)
#summary(model_3_d)
p_val_3 <- summary(model_3_d)$coefficients[,4] #p values
sig_vars_3 <- names(p_val_3[p_val_3 <= 0.10]) #10% level
sig_vars_3

#e
summary(model_3_d)
-model_3_d$deviance/2


#f
# using the suffix 3_d because (d) will be the reference to this model
pred_3_d <- predict(model_3_d, newdata = test, type = "response")
# at threshold 0.5
table(pred_3_d >= 0.5, test$resp)


#g
# sensitivity (true positive rate)
pred_table[2,2]/sum(pred_table[,2])

# specificity (true negative rate)
pred_table[1,1]/sum(pred_table[,1])

# accuracy (accuracy)
sum(diag(pred_table)/sum(pred_table))

#h 
# using predictor vairables significant at 10% level
model_3_h <- glm(as.formula(
  paste0("resp ~ ",
         paste0(sig_vars_3, collapse = " + "),
         "- 1")  # intercept not significant at 3(d)
) , data = training, family = binomial)
model_3_h$aic # aic - the lower the better

#i 
#lower the AIC the better 

#j
# using the suffix 3_d because (d) will be the reference to this model
pred_3_h <- predict(model_3_h, newdata = test, type = "response")
# at threshold 0.5
table(pred_3_h >= 0.5, test$resp)

#k
#l
#m
library(ROCR)  # this time no messages as it is already loaded in this document

rocr_3_d <- prediction(pred_3_d, test$resp)
auc_3_d <- performance(rocr_3_d, measure = "auc")@y.values
auc_3_d[[1]] #auc for model d

rocr_3_h <- prediction(pred_3_h, test$resp)
auc_3_h <- performance(rocr_3_h, measure = "auc")@y.values
auc_3_h[[1]] #auc for model h

#ans) Model D is preferred cuz it has a hugher AUC

#n
model_3_d <- glm(resp ~ ., data = training, family = binomial)
#summary(model_3_d)

pred_3_d <- predict(model_3_d, newdata = test, type = "response")
table(pred_3_d >= 0.5, test$resp)

profit <- (table(pred_3_d >= 0.5, test$resp)[2,1])*(-300) + table(pred_3_d >= 0.5, test$resp)[2,2]*(100)
#profit <- sum(pred_table_3_d * matrix(c(0,-300,0,100), nrow = 2, ncol = 2))
profit

#o
# Sort predicted probability from high to low
sort_pred_3_d <- sort(pred_3_d, decreasing = TRUE)

# Get the duration of the last individual
lowest_prob <- sort_pred_3_d[length(sort_pred_3_d)]
lowest_prob
last_man <- names(lowest_prob) # index of row with lowest prob
germancredit[last_man, ]$dur # duration of credit

#p
# first we get the sorted predictions
# we need index.return to give the indices with respect to pred_3_d
# which derives from the test set
# there might be other ways of doing this, but this is probably the most convenient way
sort_pred_3_p <- sort(pred_3_d, decreasing = TRUE, index.return = TRUE)
#sort_pred_3_p

profit_pred_3 <- ifelse(test$resp[sort_pred_3_p$ix], 100, -300)
#profit_pred_3

cumulative_profit_3 <- cumsum(profit_pred_3)
#cumulative_profit_3

# then we simply return what was asked for in the question
which.max(cumulative_profit_3)
max(cumulative_profit_3) #the max amount of profits

#q
# Find the predicted probability of the individual with index 153 on sorted test set
# NOTE: individiual with 153 on sorted test set may not be index 153 on unsorted/raw test set
sort_pred_3_p$x[which.max(cumulative_profit_3)] 

# NOT RUN
# for testing only (model (h))
sort_pred_3_test <- sort(pred_3_h, decreasing = TRUE, index.return = TRUE)
profit_pred_3_test <- ifelse(test$resp[sort_pred_3_test$ix], 100, -300)
cumulative_profit_3_test <- cumsum(profit_pred_3_test)
max(cumulative_profit_3_test)  # 7900, for seed 2019

#END________
setdiff(ls(), ls(pattern = "SETUP"))
rm(list = setdiff(ls(), ls(pattern = "SETUP")))

#q4
pres <- read.csv("presidential.csv")

#a
sum(pres$WIN)
table(pres$WIN)
#both are tied and have 13 each

#b
sort(table(pres$DEM))
sort(table(pres$REP))

names(which.max(table(pres$DEM)))
max(table(pres$DEM))

names(which.max(table(pres$REP)))
max(table(pres$REP))

#c
x <- t.test(pres$GOOD[pres$INC==1],pres$GOOD[pres$INC==-1])
x$p.value

#d
pres$WININC <- as.integer(pres$INC==pres$WIN)

#e
table(pres$WININC)

#f
model1 <- glm(WININC~GROWTH,data=pres,family=binomial)
summary(model1)

llm1<-(model1$aic - 2*model1$rank)/2
llm1 #log likelihood

logLik(model1) #log likelihood

summary(model1)$coefficients[2,4] #p-value for GROWTH variable

#g
presmod<-pres
presmod$WIN <- as.integer(pres$WIN==1)
presmod$GROWTH <- pres$GROWTH*pres$INC

#i
presmod$GOOD <- pres$GOOD*pres$INC
presmod

model2 <- glm(WIN~INC+RUN+GROWTH+DUR+GOOD,data=presmod,family=binomial)
x <- summary(model2)
x$aic

#j
#intercept, INC, GOOD

#k
# Drop intercept, INC and GOOD
model3 <- glm(WIN~RUN+GROWTH+DUR-1, data=presmod, family=binomial)
summary(model3)
model3$aic

#l
#m
#n

#o
#Using our model we need variable for growth. US per capita GDP is supposed to shrink by at least 5% in
#2020. Since the incumbent is republican we have GROWTH = (-5)*(-1)=5

newdat <- data.frame(RUN=-1, GROWTH=5, DUR=0)
predict(model3, newdat, type="response")

# Or manually calculate
# data20 <- c(-1,5,-0)
# exp(sum(model3$coefficients * data20)) / (1 + exp(sum(model3$coefficients * data20)))











