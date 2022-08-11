rm(list=ls())

# Load Libraries 
library(tm)
library(NLP)
library(SnowballC)
library(wordcloud)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)

# Read csv
twitter <- read.csv("twitter.csv",stringsAsFactors=FALSE)
head(twitter)
str(twitter)
summary(twitter)

# Data Prep
#checking negative tweets 
twitter$Neg <- as.factor(twitter$sentiment <= 2)
table(twitter$Neg)

# Data pre-processing using tm library
?Corpus
?VectorSource

corpus <- Corpus(VectorSource(twitter$tweet))
corpus[[1]]
as.character(corpus[[1]])
as.character(corpus[[2664]])

getTransformations()

# 2.1 converting text to lower case 
corpus <- tm_map(corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
corpus <- tm_map(corpus, content_transformer(function(x)    iconv(enc2utf8(x), sub = "bytes")))

corpus <- tm_map(corpus, content_transformer(tolower))
as.character(corpus[[1]])

# 2.2 remove stopwords
stopwords(kind = "en")
corpus <- tm_map(corpus, removeWords, stopwords(kind = "en"))
as.character(corpus[[1]])
#manually removing words
corpus <- tm_map(corpus,removeWords,c("drive","driving","driver","self-driving","car","cars"))
as.character(corpus[[1]])

# 2.3 remove punctuations
corpus <- tm_map(corpus,removePunctuation)
as.character(corpus[[1]])

# 2.4 Stemming using SnowballC package
corpus <- tm_map(corpus,stemDocument)
as.character(corpus[[1]])
twitter$tweet[1] #check to compare how much has the tweet changed

# 2.5 Create DTM 
?DocumentTermMatrix
dtm <- DocumentTermMatrix(corpus)
dtm #cols with all characters 
dtm[1,]
inspect(dtm[1,])

# 2.6 Removing sparse terms
?findFreqTerms
dtm[,"accid"]
findFreqTerms(dtm,lowfreq=50) #words with a higher frequency

dtm[,"accid"]
dtm[,"awesom"]

?removeSparseTerms
dtm <- removeSparseTerms(dtm,0.995)
dtm 


# 3. Prep model for learning 
twittersparse <- as.data.frame(as.matrix(dtm))
# str(twittersparse)
# colnames(twittersparse)
colnames(twittersparse) <- make.names(colnames(twittersparse))
colnames(twittersparse)

# Get word counts in decreasing order
word_freqs = sort(colSums(twittersparse), decreasing=TRUE)
# Create data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=unname(word_freqs))
# Plot wordcloud
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))


twittersparse$Neg <- twitter$Neg
str(twittersparse)


# 4 Train and test a classifier
set.seed(123)
spl <- sample.split(twittersparse$Neg,SplitRatio=0.7)
train <- subset(twittersparse,spl==TRUE)
test <- subset(twittersparse,spl==FALSE)

# 4.1 Logistc regression
model1 <- glm(Neg~.,data=train,family=binomial)
summary(model1)
predict1 <- predict(model1,newdata=test,type="response") #using the test datset 
table(predict1>=0.5,test$Neg)

# 4.2 CART
model2 <- rpart(Neg~.,data=train)
summary(model2)
prp(model2,type=4,extra=4) # Here, TRUE indicates Neg = 1, FALSE indicates Neg = 0.
predict2 <- predict(model2,newdata=test,type="class")
table(predict2,test$Neg)

model2a <- rpart(Neg~.,data=train,cp=10^-6) 
prp(model2a,type=4,extra=4) 
printcp(model2a)

model2b <- prune(model2a,cp=0.00092251) # To prune, we use the smallest value of xerror
prp(model2b,type=4,extra=4) 

predict2b <- predict(model2b,newdata=test,type="class") #making predictions using the test dataset
table(predict2b,test$Neg)


# 4.3 Random Forests

# We use the default parameters to fit the model (500 trees)
model3 <- randomForest(Neg~.,data=train) 
summary(model3)
model3

predict3 <- predict(model3,newdata=test,type="class")
table(predict3,test$Neg)



