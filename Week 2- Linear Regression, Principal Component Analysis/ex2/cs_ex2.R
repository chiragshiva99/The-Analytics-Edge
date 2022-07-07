#q1
auto <- read.csv("Auto.csv")
str(auto) #horsepower is a STRING. Change it to numeric 

#a 
#horsepower variable needs to be changed because it is shown as a  chr format. 
# convert it into num format 
auto$horsepower <- as.numeric(as.character(auto$horsepower))
model1<- lm(mpg~horsepower, data=auto)
summary(model1)

#b 
# yes there is a strong relationship. p value is almost 0. thereby can reject null hypothesis 

#c
predict(model1, newdata = data.frame(horsepower=98), interval = c('confidence'), level= .99)

#d
cor(auto$horsepower, auto$mpg, use = "pairwise.complete.obs")^2

#e
library(ggplot2)
# scatter plot and line 
ggplot(auto,aes(horsepower,mpg))+geom_point(na.rm=T)+geom_smooth(method="lm",na.rm=T,se=F)

#f
library(ggfortify)
autoplot(model1)
#a good linear fit should have residuals randomly scattered 
#the normal qq plot isnt entirely a straight line too 


#2

#a
library(psych)
#pairs.panels(auto)
pairs.panels(auto,ellipses = F, lm =T, breaks=10, hist.col="blue")

#b
#create a new model to exclude last column 
auto1 <- subset(auto, select = -c(name))
str(auto1)
cor(auto1)
cor(auto1, use="complete.obs")

#c
model2<- lm(mpg~., data=auto1)
summary(model2)
# overall p-value is close to 0 and thereby can reject null hypothesis
# the ones with 3 stars are better
# coefficient is positive and p value is close to 0

#3
# ans for b0 b1 b2 

# b 
cor(x1,x2)
ggplot(cbind(x1,x2),aes(x1,x2))+geom_point()

#c
model3 <- lm(y~x1+x2)
summary(model3)
# we can reject the numm hypothesis for b1 but not for b2
# based on the 5% significance level 

#d
model4 <- lm(y~x1)
summary(model4)

#e
model5 <- lm(y~x2)
summary(model5)

#f
#possibility of multicolinearity 
# can reject b0 and b1 individually 
# but when put toogether, we cant do so 

#4 
boston <- read.csv("Boston.csv")
colnames(boston)

#a
model1 <- lm(medv~crim, data=boston)
summary(model1)

model2 <- lm(medv~zn, data=boston)
summary(model2)

model3 <- lm(medv~indus, data=boston)
summary(model3)

model4 <- lm(medv~chas, data=boston)
summary(model4)

model5 <- lm(medv~nox, data=boston)
summary(model5)

model6 <- lm(medv~rm, data=boston)
summary(model6)

model7 <- lm(medv~age, data=boston)
summary(model7)

model8 <- lm(edv~dis, data=boston)
summary(model8)

model9 <- lm(medv~rad, data=boston)
summary(model9)

model10 <- lm(medv~tax, data=boston)
summary(model10)

model11 <- lm(medv~ptratio, data=boston)
summary(model11)

model12 <- lm(medv~black, data=boston)
summary(model12)

model13 <- lm(medv~lstat, data=boston)
summary(model13)

ggplot(boston,aes(lstat,medv))+geom_point(na.rm=T)+geom_smooth(method="lm",na.rm=T,se=F)

#b
modelall<- lm(medv~., data=boston)
summary(modelall)

attr(modelall$coefficients, "names")[modelall$coefficients <= 0.05]

#c
#take in the respective coefficients for all the models 
Ind <- c(model1$coef[2], model2$coef[2], model3$coef[2], model4$coef[2], model5$coef[2],
         model6$coef[2], model7$coef[2], model8$coef[2], model9$coef[2], model10$coef[2],
         model11$coef[2], model12$coef[2], model13$coef[2])
#take in all coefficients from the main model
All <- modelall$coef[2:14]
#visualise
ggplot(cbind(Ind,All),aes(Ind,All)) 
+ geom_point()+geom_smooth(method="lm",se=F)
+ggtitle("Coefficient relationship") 
+ xlab("Simple linear regression") 
+ ylab("Multiple linear regression")

#d
modelpoly2<- lm(medv~poly(lstat,2, raw=TRUE), data=boston)
summary(modelpoly2)
modelpoly3 <- lm(medv~poly(lstat,3,raw=TRUE), data = boston)
summary(modelpoly3)
modelpoly4 <- lm(medv~poly(lstat,4,raw=TRUE), data = boston)
summary(modelpoly4)
modelpoly5 <- lm(medv~poly(lstat,5,raw=TRUE), data = boston)
summary(modelpoly5)
modelpoly6 <- lm(medv~poly(lstat,6,raw=TRUE), data = boston)
summary(modelpoly6) #after degree 5 the models dont

boston$pr1 <- predict(model13,newdata=boston)
boston$pr5 <- predict(modelpoly5,newdata=boston)

ggplot(boston)+geom_point(aes(lstat,medv))+geom_line(aes(lstat,pr1),color="blue",size=2)+geom_line(aes(lstat,pr5),color="red",linetype="solid",size=2)


#5
wine<-read.csv("winedata.csv")
str(wine)

#a
#create new colums
wine$age91 <- 1991 - wine$vintage
wine$age92 <- 1992 - wine$vintage

mean(subset(wine$price91, wine$age91 >= 15 ))

#b

mean(subset(wine$price91,
            wine$hrain < mean(wine$hrain) 
            &
            wine$tempdiff < mean(wine$tempdiff)))

#c
train <- subset(wine, wine$vintage <= 1981)
model1 <- lm(log(price91)~age91, data= train)
summary(model1)

#d
confint(model1, level = .99)

#e
#testing the model
test<-subset(wine,vintage>=1982)
predtest<-predict(model1,newdata=test)
predtest

log(test$price91)

sse<-sum((log(test$price91)-predtest)^2)
sse

sst<-sum((log(test$price91)-mean(log(train$price91)))^2)
sst

sst-sse #test R^2

#f
# Training and testing r^2 is higher in this data set than the previous one
# This dataset is exaplined better 

#g
# use the training dataset
model2<-lm(log(price91)~temp+hrain+wrain+tempdiff+age91,data=train)
summary(model2)

#h
#compare adjusted r^2 accordingly 

#i
#compare the coefficients and check for significance level 

#j 
#the ones with the lower higher p value 
model2<-lm(log(price91)~temp+hrain+wrain+tempdiff+age91,data=train)
summary(model2)

# 6

#a
summary(iris)
dim(iris)
str(iris)

#b 
iris_data<-iris[,-5] #[ROW, COLUMN]
iris_sp<-iris[,5]

#c
library(psych)
pairs.panels(iris_data, ellipses = F, lm =T, breaks=10, hist.col="blue")
#check the row and column with the higher numbers

#d (i)
pr_out <- prcomp(iris_data, scale= F)
summary(pr_out)
#from data all of them are satisfied

pr_out$sdev

pve<-pr_out$sdev^2/sum(pr_out$sdev^2)
cpve<-cumsum(pve)
pve
cpve
#cumulative percentage of variance
plot(cpve,xlab="Principal components",type="l",ylim=c(0.7,1))

#d (ii)
library(factoextra)
fviz_pca_biplot(pr_out, label = "var", habillage=iris_sp)
fviz_pca_biplot(pr_out, label = "var", habillage=iris_sp,addEllipses=TRUE, ellipse.level=0.95)

#e
#standardizing the dataset

#7
wineitaly<-read.csv("wine_italy.csv",sep=",",head=T)
str(wineitaly)

#a 
wine_it<-wineitaly[,-1]
wine_cl<-wineitaly[,1]

pairs.panels(wine_it, ellipses = F, lm =T, breaks=10, hist.col="blue")

#b
pr_wine<-prcomp(wine_it,scale=T)
summary(pr_wine)
pr_wine$sdev

#c
fviz_pca_biplot(pr_wine, label = "var", habillage=wine_cl)
fviz_pca_biplot(pr_wine, label = "var", habillage=wine_cl,addEllipses=TRUE, ellipse.level=0.95)

pr_wine$rotation[order(pr_wine$rotation[,1],decreasing=T),1:2] ## order according to PC1
pr_wine$rotation[order(pr_wine$rotation[,2],decreasing=T),1:2] ## order according to PC2

#d
#check for variance for values greater than one at 80% of the total variation of the data 
pr_wine$sdev^2

pve_w<- pr_wine$sdev^2/sum(pr_wine$sdev^2)
cpve_w<- cumsum(pve_w)
pve_w

cpve_w
plot(cpve_w,xlab="Principal components",type="l",ylim=c(0,1))+abline(h=0.8,col="red")+abline(v=5,col="blue")
plot(pr_wine,type="l",ylim=c(0,5),main="Scree plot")+abline(h=1,col="red") 






