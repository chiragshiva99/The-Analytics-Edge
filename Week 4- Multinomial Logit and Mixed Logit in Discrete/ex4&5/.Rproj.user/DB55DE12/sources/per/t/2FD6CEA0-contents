rm(list=ls())
# install.packages("mlogit")
library(mlogit)

#1
heating <- read.csv("Heating.csv")
#head(heating)

#a
dataheat <- mlogit.data(heating,        # data.frame of data
                        choice = "depvar",  # column name of choice
                        shape = "wide",     # wide means each row is an observation
                        # long if each row is an alternative
                        varying = c(3:12),  # indices of varying columns for each alternative,
                        sep = "."           # not necessary but still good to be clear
)

modelQ1_1 <- mlogit(depvar ~ ic + oc - 1, dataheat)  # -1 means no intercept

#i
modelQ1_1$coefficients
#probability of choosing that system decreases if theres an increase i9n
#installation costs and operarting system 

#ii
summary(modelQ1_1)
#both p-values are significant as their p-values are almost 0

#iii
#actual shares 
table(heating$depvar)/nrow(heating)
# vs predicted shares (average)
predQ1_1<- predict(modelQ1_1, newdata = dataheat)
apply(predQ1_1, 2, mean)

#iv
#calculating the ratio of coefficients
wtp <- as.numeric(coef(modelQ1_1)["oc"]/coef(modelQ1_1)["ic"])
wtp
#according to the model, the decision makers are willing to pay $0.739 in 
#installation cost for a $1 reduction in operating cost

#b
dataheat$lcc <- dataheat$ic + (dataheat$oc /0.12)
modelQ1_2 <-mlogit(depvar ~ lcc - 1, dataheat)

logLik(modelQ1_1)
logLik(modelQ1_2)

#the log likelihood is better in a than in b 
# lower the -ve value of log likelihood, the better

#c
modelQ1_3 <- mlogit(depvar ~ ic + oc, data = dataheat, reflevel = "hp")
# This forces hp to be the reference level and the other alternative specific constants are relative to this
summary(modelQ1_3)

#i
predQ1_3<- predict(modelQ1_3, newdata=dataheat)
shareQ1_3<- apply(predQ1_3,2,mean)
shareQ1_3

#ii
wtp <- as.numeric(coef(modelQ1_3)["oc"]/coef(modelQ1_3)["ic"])
wtp
#alternative way to calculate wtp
unname(modelQ1_3$coefficients["oc"]/modelQ1_3$coefficients["ic"])

#an extra down payment of $4.56 for a $1 saving in annual operating costs

#iii
modelQ1_3 <- mlogit(depvar ~ ic + oc, data = dataheat, reflevel = "hp")
summary(modelQ1_3)
# the intercept for gr is 0.308. 
# in the new model, all the specific constants are reduced by 0.308

### Check that you are right. compare it with the reflvevel of gr
modelQ1_4 <- mlogit(depvar ~ ic + oc, data = dataheat, reflevel = "gr")
summary(modelQ1_4)

#d
#i
#create new installation cost
dataheat$iic <- dataheat$ic / dataheat$income

modelQ1_5 <- mlogit(depvar ~ oc + iic, dataheat)
summary(modelQ1_5)
#the log likelihood function here is a larger negative value and thereby worse 

#ii
modelQ1_6 <- mlogit(depvar ~ oc + ic | income, dataheat) # | used to enter alternate-specific argument
summary(modelQ1_6)

#e
#i
heating1 <- heating
heating1$ic.hp <- 0.9*heating1$ic.hp
dataheat1 <- mlogit.data(heating1, 
                         choice = "depvar", 
                         shape = "wide", 
                         varying = c(3:12)
                         )

predQ1_3a <- predict(modelQ1_3, newdata = dataheat1)
shareQ1_3a <- apply(predQ1_3a, 2, mean)
shareQ1_3a

#The share of houses with heat pumps rises from 0.055 to 0.0645

#ii

#idk this is too difficult 


#2
library(mlogit)
electricity <- read.csv("Electricity.csv")
# str(electricity)
dim(electricity)

#a
# choices made captured in 'id' variable
electricity_data<-mlogit.data(electricity, 
                              id.var = "id", #col name of the individual index
                              choice = "choice", #col name 
                              varying = c(3:26), #3rd to 26th col
                              shape = "wide", 
                              sep = "")

# mlogit model without intercepts, normal distribution for 6 parameters
modelQ2_1<- mlogit(choice~pf+cl+loc+wk+tod+seas-1,
                   electricity_data, 
                   rpar=c(pf='n',cl='n',loc='n',wk='n',tod='n', seas='n'), #a named vector whose names are the random parameters and values the distribution
                   panel=T,
                   print.level=T)

summary(modelQ2_1)

#i
#look at the coefficient estimate of cl 
modelQ2_1$coefficients["cl"] # estimated mean coeff for contract length
as.numeric(modelQ2_1$coefficients["cl"]/modelQ2_1$coefficients["pf"])

#ii
modelQ2_1$coefficients[c("cl", "sd.cl")]
# negative estimated price coefficient indicates that people are more likely to say "no" to longer contracts

pnorm(0, 
      as.numeric(modelQ2_1$coefficients["cl"]), # mean
      as.numeric(modelQ2_1$coefficients["sd.cl"]) # sd
)

#b
pnorm(0, # P(x < 0)
      as.numeric(modelQ2_1$coefficients["pf"]), # mean
      as.numeric(modelQ2_1$coefficients["sd.pf"]) # sd
)
1-pnorm(0, 
        as.numeric(modelQ2_1$coefficients["pf"]), # mean
        as.numeric(modelQ2_1$coefficients["sd.pf"]) # sd
)

## old model:
# modelQ2_1<- mlogit(choice~pf+cl+loc+wk+tod+seas-1,
#                    electricity_data, 
#                    rpar=c(pf='n',cl='n',loc='n',wk='n',tod='n', seas='n'),
#                    panel=T,print.level=T)

## new model
modelQ2_2<- mlogit(choice~pf+cl+loc+wk+tod+seas-1,
                   electricity_data,
                   rpar=c(cl='n',loc='n',wk='n',tod='n', seas='n'), # now price no longer follows normal distribution
                   panel=T,print.level=T)
summary(modelQ2_2)

modelQ2_2$coefficients["pf"]

#comparison of model 1 and model 2
modelQ2_1$logLik
modelQ2_2$logLik
#model 1 is better as it has a smaller negative value 

#c
modelQ2_3<-mlogit(choice~pf+cl+loc+wk+tod+seas-1,
                  electricity_data, 
                  rpar=c(cl='n',loc='n',wk='u',tod='n', seas='n'), # now we set 'wk' to follow a uniform distribution
                  panel=T,print.level=T)

summary(modelQ2_3)

# extract this information from the model summary
summary(modelQ2_3)$summary.rpar["wk", c("Mean", "Min.", "Max.")]

# and find the estimated price coefficient
modelQ2_3$coefficients["pf"]


#q3
















