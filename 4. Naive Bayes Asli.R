library(e1071)
library(naivebayes)
library(caret)
library(tm)

#NAIVE BAYES MURNI

df<-read.csv("file:///D:/PROJECT/EL-CREATIVEON/05.01.2020 - REZA - Sentimen Analisis/Code/Code Fix/Result/Scoring Asli Poin 4.csv", stringsAsFactors = FALSE)

df$klasifikasi <- as.factor(df$klasifikasi)

corpus <- Corpus(VectorSource(df$text))
inspect(corpus[1:3])


dtm <- DocumentTermMatrix(corpus)


#Split Data Training & Testing
n<-round(nrow(df)*0.8)
n

set.seed(100)
samp<-sample(1:nrow(df),n)

df.train<-df[samp,]
df.test<-df[-samp,]
write.csv(df.train,file = 'D://PROJECT//EL-CREATIVEON//05.01.2020 - REZA - Sentimen Analisis//Code//Code Fix//Result//Naive Bayes Asli 8020 Poin 1 Train.csv')
write.csv(df.test,file = 'D://PROJECT//EL-CREATIVEON//05.01.2020 - REZA - Sentimen Analisis//Code//Code Fix//Result//Naive Bayes 8020 Poin 1 Test.csv')

dtm.train <- dtm[samp,]
dtm.test <- dtm[-samp,]

corpus.train <- corpus[samp]
corpus.test <- corpus[-samp]

dim(dtm.train)

tenfreq <- findFreqTerms(dtm.train, 10)
length((tenfreq))
View(tenfreq)
write.csv(tenfreq,file = 'D://PROJECT//EL-CREATIVEON//05.01.2020 - REZA - Sentimen Analisis//Code//Code Fix//Result//Naive Bayes Asli 8020 Poin 2.csv')

# Use only 10 most frequent words (tenfreq) to build the DTM

dtm.train.nb <- DocumentTermMatrix(corpus.train, control=list(dictionary = tenfreq))
dim(dtm.train.nb)
## [1]  32 21

dtm.test.nb <- DocumentTermMatrix(corpus.test, control=list(dictionary = tenfreq))

dim(dtm.train.nb)

# Function to convert the word frequencies to yes (presence) and no (absence) labels
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

# Apply the convert_count function to get final training and testing DTMs
trainNB <- apply(dtm.train.nb, 2, convert_count)
testNB <- apply(dtm.test.nb, 2, convert_count)
View(testNB)
write.csv(trainNB,file = 'D://PROJECT//EL-CREATIVEON//05.01.2020 - REZA - Sentimen Analisis//Code//Code Fix//Result//Naive Bayes Asli 8020 Poin 3 Train.csv')
write.csv(testNB,file = 'D://PROJECT//EL-CREATIVEON//05.01.2020 - REZA - Sentimen Analisis//Code//Code Fix//Result//Naive Bayes Asli 8020 Poin 3 Test.csv')

# Train the classifier
naive=system.time( classifier <- naiveBayes(trainNB, df.train$klasifikasi))
head(naive)

# Use the NB classifier we built to make predictions on the test set.
system.time( pred <- predict(classifier, newdata=testNB) )

# Create a truth table by tabulating the predicted class labels with the actual class labels 
table("Predictions"= pred,  "Actual" = df.test$klasifikasi)
pred.vs.actual<-data.frame(pred,act=df.test$klasifikasi)
head(pred.vs.actual,15)
write.csv(pred.vs.actual,file = 'D://PROJECT//EL-CREATIVEON//05.01.2020 - REZA - Sentimen Analisis//Code//Code Fix//Result//Naive Bayes Asli 8020 Poin 5 Test.csv')

# Prepare the confusion matrix
conf.mat <- confusionMatrix(pred, df.test$klasifikasi)
conf.mat

conf.mat$byClass

conf.mat$overall['Accuracy']

