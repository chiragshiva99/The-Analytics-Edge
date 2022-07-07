# W1: Numbers and vectors #######

## Concatenation ###############

# Concatenate (combine) numbers to form a vector
x<-c(1,-2,pi,55,45)
x

## Applying operations to a vector ##########
# term by term inverse, concatenate vectors, exponentiation
1/x
y<-c(x,0,x)
y

# You can overload the sum operator by recycling the shorter vector - mathematically 
# adding vectors of different sizes are not permitted 
# R will not alert about the error, it will proceed to concatenate x until it reaches the length of y
x+y

### Maximum entry in a concatenated vector ##########
max(x,y)

## Parallel maximum
pmax(x,y) # since x is shorter than y, x will be recycled until length of y is met

### Generating vectors using a variety of commands ##########
x <- -3:8
y <- seq(-3,8)

# using seq, we can also set the step, or length between points
z <- seq(-3,8,length=5)
z

rep(x, times=3)

# to repeat each element instead of the whole lists
rep(x, each=3)

## Differences in assignments ##########
# Using <- vs = vs ==.
exp(1:5)

# exp(a=1.5) # will not work
exp(a<-1:5)

# since TRUE = 1, below command will exponentiate each element to power of 1
exp(a==1:5)

## Factors ##########
ct <- c('jap','kor','sin','kor','jap','sin','sin')
class(ct)

# factored elements
fct <- factor(ct)
fct
as.factor(ct) # same output
levels(fct)

summary(fct)

table(fct)

## The function apply #######
income <- c(500,1200,4000,2300,2300,1234,1345)
tapply(income,fct,mean) # get mean income by fct
med <- data.frame(patient=1:100, age=rnorm(100,mean=60,sd=12), treatment=gl(2,50,labels=c("Treat","Control")))
med
tapply(med$age,med$treatment,max)

A <- matrix(1:25,5,5)
A
apply(A,1,max) # get max across rows(index=1)
apply(A,2,max) # get max across cols(index=2)
apply(A,1,which.max) # get the index of the max across rows(index=1)

## Matrix and arrays ##########
r<-matrix(3:8,nrow=3,ncol=2)
r
rownames(r)<-c("Jack","Jane","June")
colnames(r)<-c("Bananas","Pancakes")
r
dim(r) # dimension of r
r[2,2] # specific element
r[1,]  # get 1st row and every col
class(r)

### Other matrix operations #######
diag(5) # 5 by 5 matrix with 1 in the diagonal
cbind(c(1,2,3),c(4,5,6))
rbind(c(1,2,3),c(4,5,6))

### Matrix multiplication #######
y<-matrix(1:6,3,2)
z<-matrix(3:8,3,2)
y*z # multiplies entry by entry

# for matrix multiplication, need to transpose z first
y%*%t(z)

### Solutions of linear equations #######
a<-matrix(c(2,1,-1,2),2,2)
b<-c(4,4)

solve(a,b) # solves for aX=b
solve(a) # gets the inverse of matrix

### Eigen decomposition of a matrix ######
E<-eigen(a)
E
E$values
E$vectors

## Lists ##########
# List consists of an ordered collection of objects that can be of different or the same type.
Paul<-list(age=44,sex="M",affiliation=c("Atreides","Fremen"))
class(Paul)
Paul[1]
Paul[[3]]
Paul$affiliation
class(Paul[1]) # still "list", as stored as a list of lists
class(Paul[[1]]) # element is now numeric

Leia<-list(age=54,sex="F",affiliation=c("Rebel Alliance","New Republic"))
fchar<-c(Paul,Leia)
fchar


## Dataframes ############

# consider using inner_join() from dplyr to insert a new column
# ensure that both dataframes have a common variable, such as year
baseballlarge <- inner_join(baseballlarge,NoOfTeamsPlayoffs,by="Year")

# Dataframes are a tightly coupled collection of variables that share many of the properties of matrices 
# and lists and is the fundamental data structure that will be used in most of this course.
A<-data.frame(c(Paul,Leia))
A
A<-data.frame(name=c("Paul","Leia"),age=c(44,54),affiliation=c(2,2))
A
A$spouse<-c("Irulan","Han")
A
as.data.frame(fchar)

# Then we remove columns which do not have available data (remove "NA"). See socprog.Rmd for more details.
NAobject<-colnames(spi2020)[apply(is.na(spi2020),2,any)]
spi2020<-spi2020[,!names(spi2020)%in% NAobject]

# method to drop certain columns
spi2020core<-spi2020[,!names(spi2020)%in% c(bigdim,medcomp)]

nonvars <- c("year","songtitle","artistname","songID","artistID")
SongsTrain <- SongsTrain[,!names(SongsTrain) %in% nonvars]
SongsTest  <- SongsTest [,!names(SongsTest)  %in% nonvars]

# W1: t-tests ######

## T-test and finding confidence intervals #####
t.test(faithful1$waiting,mu=71)
t.test(faithful2$waiting,mu=71)

# One Sample t-test

# data:  faithful1$waiting
# t = -27.835, df = 96, p-value < 2.2e-16
# alternative hypothesis: true mean is not equal to 71
# 95 percent confidence interval:
#   53.31781 55.67189
# sample estimates:
#   mean of x 
# 54.49485 
 
 
# One Sample t-test

# data:  faithful2$waiting
# t = 19.837, df = 174, p-value < 2.2e-16
# alternative hypothesis: true mean is not equal to 71
# 95 percent confidence interval:
#   79.09425 80.88289
# sample estimates:
#   mean of x 
# 79.98857 

## Two sample t-test ######
t.test(faithful1$waiting,faithful2$waiting)

# Welch Two Sample t-test

# data:  faithful1$waiting and faithful2$waiting
# t = -34.161, df = 202.71, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -26.96519 -24.02226
# sample estimates:
#   mean of x mean of y 
# 54.49485  79.98857 

## Interpretations of t-test results ######

# 2. With 95% confidence, if the eruption time < 3, the average waiting time is between 53.32 and 55.67.
# 3. With 95% confidence, if the eruption time >= 3, the average waiting time is between 79.10 and 80.88.
# 4. With 95% confidence, the difference between the average waiting time for the two categories is between -26.97 and -24.02.

# Plotting of data using qplot and ggplot ##########

# See Week 1 folder and open who_class.Rmd and oldfaithful_class.Rmd

# W2: Linear Regression ########

## Adding a regression line using ggplot #########3
ggplot(wine, aes(VINT,LPRICE)) + geom_point(na.rm=T) + 
  geom_smooth(method="lm",na.rm=T,se=F)

## Scatter plot of all pairs ######

# Diagonal contains a histogram of pairs of variables
pairs.panels(wine,ellipses = F, lm = T, breaks=10, hist.col="blue")  ## uses the psych package
# or can use GGally
library(GGally)
ggpairs(wine_italy)

## Obtaining coefficients from lm ######

model4$coefficients
model4$residuals

## Getting p-values from model ######
summary(fit)$coefficients[,4] 

## Getting coef names from model #####
attr(modelall$coefficients, "names")[modelall$coefficients <= 0.05]
model4b$coef[2] # because coef of variable is 2nd after intercept

## Getting confidence intervals ########
confint(model4)
confint(model4,level=0.99)

## Making predictions #####
wineprediction7 <- predict(model7,newdata = winetest)

## Get significant variables #####
p_val_3 <- summary(model15)$coefficients[,4]
sig_vars_3 <- names(p_val_3[p_val_3<=0.1])

## SST and SSE values #######
## for sst, remember to use mean of the train, not test
wineprediction7 <- predict(model7,newdata = winetest)
sst <- sum(((winetest$LPRICE[1:2])-mean(winetrain$LPRICE))^2)
sse7 <- sum((wineprediction7[1:2]-winetest$LPRICE[1:2])^2)
1 - sse7/sst

## StepAIC #####
library(MASS)
mAIC <- lm(RS~OBP+SLG+BA+OPS,data=baseball2002)
stepAIC(mAIC) 
# AIC scores represent the resultant AIC score after removing respective variable

# W2 - PCA #########

## PCA without scaling #####

# Remember to take out the classes column before doing PCA

pr.out.ns=prcomp(spi2020, scale=F)
pr.out.ns$rotation     ### gives the weights for the PCAs (eigen vectors)
pr.out.ns$x[1:10,1:5]  ### gives PCA value for 1st 10 countries and first 5 PCAs - "equivalent to the Z matrix"

## Scree plot #####
plot(pr.out.ns, type="l", main="Scree plot")

## Plot cumulative proportional variance #####

pr.var.ns=pr.out.ns$sdev^2
pve.ns=pr.out.ns$sdev^2/sum(pr.out.ns$sdev^2)
plot(cumsum(pve.ns), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained",
     ylim=c(0,1),type='b')+abline(h=0.9,col="red")

## Amt of var explained by first PC #####
pve.ns[1]

## Significant loadings, country rankings #####

pr.out.ns$x        ### to see how the countries rank in each of the principal components
pr.out.ns$rotation ### gives the weights for the PCAs (eigen vectors)
sp.pc<-data.frame(pr.out.ns$x[,1:2])
sp.pc[order(-sp.pc$PC1),] ### ranking of the countries when ranked using PC1 - what is the best or worst according to PC1?
sp.pc[order(-sp.pc$PC2),]
co.sp.pc<-data.frame(pr.out.ns$rotation[,1:2])

View(co.sp.pc)

# see the factors that make up the first component
View(co.sp.pc[order(-co.sp.pc[,1]),][c(1:5,50:54),]) ### first component heavily weighted on greenhouse 
                                                      # gases factor and little of other factors

## Which components and which country?
sp4<-data.frame(pr.out$x[,1:4])
sp4[order(-sp4$PC1),] ### nordic countries are performing well for PC1
cosp4<-data.frame(pr.out$rotation[,1:4])
cosp4[order(-cosp4[,1]),]

## Proportion of variance #####
pr.var=pr.out$sdev^2
pr.var
pve=pr.var/sum(pr.var)
# use summary(pr.out.ns) to get proportion of variance explained

## Biplots #####
# need to create a separate dataframe for the classes of objects first
library(factoextra)
fviz_pca_biplot(iris.pca.ns, label="var",
                addEllipses = T, habillage = iris_sp)

## Choosing no of components #####
# plot cumsum of pve against required variability or
# do a scree plot with horizontal line at 1 (representing the eigenvalues)



# W3 - Logistic Regression #####

# Fitting a logistic regression model: `glm()` is a generalized linear model
# that can be used to fit a logistic regression model by choosing `family=binomial`
model3 <- glm(Field~Temp+Pres,data=orings,family=binomial)

## Predictions #####
predict(model4,newdata=orings[144,])                 # predicts on log(odds) scale
predict(model4,newdata=orings[144,],type="response") # predicts probability

## Plotting logistic curve #####
ggplot(orings,aes(Temp,Field)) + 
  geom_jitter(na.rm=T,height=0,width=2) +
  geom_smooth(method="glm",se=F,na.rm=T,fullrange=T,method.args = list(family = "binomial"))

## Confusion matrix #####
Pred <- predict(model4,newdata=orings,type="response")
t1<-table(Pred[1:138]>0.5,orings$Field[1:138])
t2<-table(Pred[1:138]>0.25,orings$Field[1:138])
t3<-table(Pred[1:138]>0.2,orings$Field[1:138])

# Accuracy - number of correct predictions
# Specificity - True Negative rate
# Sensitivity - True Positive rate

Spec_0.25<- t2[1,1]/sum(t2[,1])
Sens_0.25<- t2[2,2]/sum(t2[,2])
Acc_0.5<- (t2[1,1]+t2[2,2])/sum(t2)

## ROC #####
library(ROCR)
Pred <- predict(model4,newdata=orings,type="response")
ROCRpred <- prediction(Pred[1:138],orings$Field[1:138])
ROCRperf <- performance(ROCRpred,x.measure="fpr",measure="tpr")
plot(ROCRperf)
plot(ROCRperf, colorize=T,print.cutoffs.at=c(0,0.1,0.2,0.3,0.5,1), text.adj=c(-0.2,1.7))
as.numeric(performance(ROCRpred,measure="auc")@y.values)

## Create training and test sets #####
library(caTools)
set.seed(1)
split <- sample.split(framing1$TENCHD,SplitRatio=0.65)
# split
# length(split)
training <- subset(framing1,split==TRUE)
test <- subset(framing1,split==FALSE)
# test

## Log Likelihood #####
logLik(modelx)
# or we can also divide residual deviance by 2

## Compute probability #####
model3$coefficients
x<-t(model3$coefficients)%*%c(1,1,60,145,2,80)
x  ## log(odds) scale
exp(x)/(1+exp(x))  ## probabilities

## Get significant variables #####
p_val_3 <- summary(model15)$coefficients[,4]
sig_vars_3 <- names(p_val_3[p_val_3<=0.1])

## Get AIC ######
model$aic
extractAIC(model)

llm1<-(model1$aic - 2*model1$rank)/2
llm1 #log likelihood

## StepAIC #####
library(MASS)
mAIC <- lm(RS~OBP+SLG+BA+OPS,data=baseball2002)
stepAIC(mAIC) 
# AIC scores represent the resultant AIC score after removing respective variable

# Index.Return ######
# Look at W3:
# Exercise3.Rmd or Exercise3Retry.R

# W4 - Multinomial Logit #####

# Need to convert the choice variable to 0/1
oscars$Ch <- 2-oscars$Ch

## Read data set #####

# This creates a data set for applying the `mlogit` function where `choice` is a 
# variable indicating the choice mode (here `Ch`), `shape` is the shape of the data 
# frame (here "long" since each row is an alternative) and `alt.var` is the name of 
# the variable containing the alternative index (here `Mode`). We use `shape="wide"` 
# when there is one row for all the alternatives.

oscars <- read.csv("oscars.csv")
safety <- read.csv("safetydata.csv")
electricity <- read.csv("electricity.csv")
View(oscars)
View(safety)
View(electricity)

D1 <- mlogit.data(subset(oscarsPP, Year <=2006), choice="Ch", shape="long", 
                  alt.var = "Mode")
data1 <- mlogit.data(electricity, shape="wide", choice="choice", 
            varying=c(3:26), sep="",id.var = list(c("id")))
S <- dfidx(subset(safety, Task<=12), shape="wide", choice="Choice", 
           varying=c(4:83), sep="",idx = list(c("No", "Case")), 
           idnames = c(NA, "alt"))

###
# use dfidx when there is more than one index for the alternative, use idx and idnames
# use mlogit.data when there is only one index for the alts, use id.var or alt.var
# see above for differences in syntax when handling long or wide datasets


## Fit MNL model #####

# For Oscars data
oscars<- read.csv("oscars.csv")
oscars$Ch <- 2-oscars$Ch
library(mlogit)
oscarsPP <- subset(oscars, PP==1)
oscarsPP$GG <- oscarsPP$Gmc + oscarsPP$Gdr
View(oscarsPP)
MPP1 <- mlogit(Ch~Nom+Dir+GG+Aml+Afl+PGA+Days+Length-1, data=D1) 

# For Automobiles data
safety <- read.csv("safetydata.csv")
View(safety)
S <- dfidx(subset(safety, Task<=12), shape="wide", choice="Choice", 
           varying=c(4:83), sep="",idx = list(c("No", "Case")), 
           idnames = c(NA, "alt"))

# * reads a subset of the dataframe where we use the first 12 choice tasks for each customer 
#   in our training set (12 out of 19),  
# * indicates that the` shape` of the dataframe is `wide` (where each row is an observation),  
# * indicates that the variable indicating the choice made is defined by `Choice`,  
# * indicates that the separator of the variable name and the alternative name is *blank* 
#   indicated by "" (this helps to guess the variables and the alternative name),  
# * indicates that the attributes are in columns 4 to 83 by the parameter specification 
#   `varying =c(4:83)` (helps indicate the variable indices that are alternative specific),  
# * indicates that `idx` identifies the indices in the data set,
# * Specifies the index for alternatives with the parameter `idnames`. Notice that we indicate 
#   the first one as `NA` which means it relates to the indices given in `idx` and additionally  
#   the second one `alt` specifies the four alternatives.  

## Predictions #####

ActualChoice <- subset(safety, Task<=12)[,"Choice"]
P <- predict(M, newdata=S) # use the logit dataset
PredictedChoice <- apply(P,1,which.max)

P <- predict(model1a, newdata=heatinglogit)
apply(P,2,mean)*900

# For forming models using the coefficients, see line 393 of Oscars.Rmd

## Quality of Fit #####
LL0 <- 56*log(1/5) # n=56, k=5
LLbeta = as.numeric(MPP2$logLik)
LLR = 1-LLbeta/LL0 # residual deviance
LLR
p=4 # number of predictors
AIC = -2*LLbeta+2*p
AIC

## MNL with norm dist #####
M1 <- mlogit(Choice~CC+GN+NS+BU+FA+LD+BZ+FC+FP+RP+PP+KA+SC+TS+NV+MA+LB+AF+HU+Price-1, 
             data=S, rpar=c(CC='n', GN='n', NS='n', BU='n',FA='n',LD='n',BZ='n',
                            FC='n',FP='n',RP='n',PP='n',KA='n',SC='n',TS='n',NV='n',
                            MA='n',LB='n',AF='n',HU='n',Price='n'), 
             panel = TRUE, print.level=TRUE)

# Hence log-likelihood for the Mixed logit model is higher than than of the MNL model;
# moreover, the AIC for Mixed logit model is lower than that of the MNL model. 
# Therefore from this perspective the Mixed logit model is preferred.

## Panel #####
# specify panel=T in mlogit() model

## WTP #####
wtp<- as.numeric(-M$coefficients["CC"]/M$coefficients["Price"])
wtp

## Distribution of variables #####
summary(model2c)$summary.rpar["wk",c("Mean","Min.","Max.")]

## ASC #####
# to normalise an alternative, use reflevel in mlogit()
modelQ1_3 <- mlogit(depvar ~ ic + oc, data = dataheat, reflevel = "hp")
# This forces hp to be the reference level and the other alternative 
# specific constants are relative to this

# use pnorm to get probability 

# we can also get the std dev of the variables using
model2a$coefficients["sd.cl"]


# W5 - Model Selection #####

## Best subsets #####
library(leaps)
modelbest<-regsubsets(Salary~.,hitters,nvmax=19) ## We will go up to 19 variables
summary(modelbest)

## Forward ####
modelfwd<-regsubsets(Salary~.,data=hitters,nvmax=19,method="forward")

## Backward #####
modelbcwd<-regsubsets(Salary~.,data=hitters,nvmax=19,method="backward")

## Plot models #####
plot(modelbest)  ### Plot with Bayesian Information Criterion - more negative is better
plot(modelbest, scale = "adjr2")
plot(summary(modelbest)$rsq)
plot(summary(modelbest)$rss)
plot(summary(modelbest)$adjr2)

## Choosing model #####
which.max(summary(modelbest)$adjr2) # best model using adjr2 score, code is to find the iteration (which is 11 here)
coef(modelbest,11) # id = 11, choosing M_11 among the 19 models

## Training and validation sets #####

# * `model.matrix` creates the **X**-matrix. Then we create a loop to find the coefficients for the best subset of 
# size `i` for `i` running from 1 to 19. We use the coefficients to predict the *Y*-values and compute MSE for 
# the *validation set*

set.seed(5)
train=sample(c(TRUE,FALSE), nrow(hitters),rep=TRUE)   ## Training set 
valset = (!train)       ## Validation set

model_valset=regsubsets(Salary~.,data=hitters[train,],nvmax=19)  ### best subset on training set
summary(model_valset)

valset.mat=model.matrix(Salary~.,data=hitters[valset,])  ### Creates the X-matrix

MSE.val = rep(NA,19)
for(i in 1:19){
  coefi=coef(model_valset, id=i)   ## Selects coefficients of the best subset of size i
  pred=valset.mat[,names(coefi)]%*%coefi  ## Predicts y-values
  MSE.val[i]= mean((hitters$Salary[valset]-pred)^2) ## computes MSE for the model on validation set.
}

## Prediction #####

predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars =names(coefi)
  mat[,xvars]%*%coefi
}

pred<- predict.regsubsets(model_valset,hitters[valset,],id=10)
MSE.valset10<- mean((hitters$Salary[valset]-pred)^2)


## Subset selection with k-fold cross validation ####
k=10
set.seed (1)
folds=sample(1:k,nrow(hitters),replace=TRUE)
cv.errors=matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))
folds
cv.errors

# Now we write a for loop that performs cross-validation. In the `j`-th fold, the
# elements of folds that equal `j` are in the test set, and the remainder are in
# the training set. We make our predictions for each model size (using our
# new `predict.regsubsets` function), compute the test errors on the appropriate subset,
# and store them in the appropriate slot in the matrix `cv.errors`.

for(j in 1:k){
  best.cv=regsubsets(Salary~.,data=hitters[folds !=j,],nvmax =19)
  for(i in 1:19) {
    pred=predict.regsubsets(best.cv,hitters[folds ==j,], id=i)
    cv.errors[j,i]=mean((hitters$Salary[folds ==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors, 2, mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors ,type='b')
which.min(mean.cv.errors)

# W5 - LASSO #####
# For explanation, refer to hitters.Rmd in W5

library(glmnet)
# remember to use original dataset and not train/test
X <- model.matrix(Salary~.,hitters)
y <- hitters$Salary
# str(X)

# We now choose a range for the lambda parameters and create a training and test set. 
# We then build the LASSO model on this data. The output from the model provides the Df 
# (number of non-zero coefficients), %Dev and Lambda values. The deviance measure is given
# as 2(loglike_sat - loglike), where loglike_sat is the log-likelihood for the saturated 
# model (a model with a free parameter per observation). Null deviance is defined to be 
# 2(loglike_sat - loglike(NULL)) where the NULL model refers to the intercept model only.
# The deviance ratio is dev.ratio=1-deviance/nulldev. As lambda decreases, the dev.ratio
# increases (more importance given to model fit than model complexity).

grid<-10^seq(10,-2, length=100)
set.seed(1)
train <- sample(1:nrow(X),nrow(X)/2)
test <- -train
modellasso <- glmnet(X[train,],y[train],lambda=grid)
summary(modellasso)
modellasso
deviance(modellasso) # log likelihood squared of model
plot(modellasso,xvar="lambda",label=TRUE)

## Predictions in LASSO #####
predictlasso1 <- predict(modellasso,newx=X[test,],s=100)
mse100<-mean((predictlasso1-y[test])^2)

predictlasso1a <- predict(modellasso,newx=X[test,],s=100,exact=T,x=X[train,],y=y[train])
mse100e<-mean((predictlasso1a-y[test])^2) # exact to use s=100 even if not in fitting algorithm

## Cross validation ####

set.seed(1)
# ?cv.glmnet
cvlasso <- cv.glmnet(X[train,],y[train])
cvlasso$glmnet.fit
optlambda<- cvlasso$lambda.min
optlambda
plot(cvlasso$lambda,cvlasso$cvm)
predictlassocv <- predict(modellasso,s=22.18,newx=X[test,])
mean((predictlassocv-y[test])^2)
coef(modellasso,s=optlambda)
coef(modelbest,10) # see if it matches with the above or not

set.seed(1)
model4g <- cv.glmnet(X[trainid,],y[trainid],lambda = grid,nfolds = 10)
model4g$lambda.min
coef(model4g,s=model4g$lambda.min)
predict4g <- predict(model4g,newx = X[testid,],s=model4g$lambda.min)
test_error <- mean((predict4g-y[testid])^2)
test_error

## LASSO with/without grid ####
x <- as.matrix(eg1[,c(2:42)])
grid<-10^seq(10,-2, length=100)
model3 <- glmnet(x,eg1$y,lambda=grid)
model3
model4 <- glmnet(x,eg1$y)

?coef
coef(model4, s=0.005) # s is the value of lambda
# setting lambda to be 1 will only result in intercept value


model4

# ?glmnet
model4$lambda
model4$df
model4$beta["EquipInv",]
model4$beta["YrsOpen",]
#model4$beta["Confucian",]
model4$beta["Abslat",]
plot(model4,xvar="lambda")
model4$beta
model4$beta !=0