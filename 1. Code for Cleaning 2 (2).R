#library(devtools)
#install_github("nurandi/katadasaR")

library(tm)
library(textclean)
library(dplyr)
library(katadasaR)
library(tokenizers)


jokowi1 = read.csv("file:///D:/PROJECT/EL-CREATIVEON/05.01.2020 - REZA - Sentimen Analisis/Code/Data/jokowi1.csv", header=T)
jokowi2 = read.csv("file:///D:/PROJECT/EL-CREATIVEON/05.01.2020 - REZA - Sentimen Analisis/Code/Data/jokowi2.csv", header=T)
jokowi3 = read.csv("file:///D:/PROJECT/EL-CREATIVEON/05.01.2020 - REZA - Sentimen Analisis/Code/Data/jokowi3.csv", header=T)
data.cleaning=rbind(jokowi1,jokowi2,jokowi3)
data.cleaning=distinct(data.cleaning)
head(data.cleaning,3)

data.tweets = data.frame(data.cleaning$text)
View(head(data.tweets,15))

#find RT
retweet=grepl("(RT|via)((?:\\b\\W*@\\w+)+)", data.cleaning$text, ignore.case=TRUE, data.cleaning$text)
score.retweet=ifelse(retweet==TRUE,print(0.5),print(1))
score.retweet = cbind(data.tweets,score.retweet)
View(head(score.retweet,15))

##hanya ambil data tweet saja
data.cleaning.text=score.retweet$data.cleaning.text
data.cleaning.text=Corpus(VectorSource(data.cleaning.text))
inspect(data.cleaning.text[1:15])

##Cleaning data
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
clean <- tm_map(data.cleaning.text, removeURL)

removeNL <- function(y) gsub("\n", " ", y)
clean <- tm_map(clean, removeNL)

removepipe <- function(z) gsub("<[^>]+>", "", z)
clean <- tm_map(clean, removepipe)
inspect(clean[1:15])

remove.retweet <- function(z) gsub("RT @\\S+", "", z)
clean <- tm_map(clean, remove.retweet)
inspect(clean[1:15])

clean <- tm_map(clean, tolower)
inspect(clean[1:15])

remove.mention <- function(z) gsub("@\\S+", "", z)
clean <- tm_map(clean, remove.mention)
inspect(clean[1:15])

remove.hashtag <- function(z) gsub("#\\S+", "", z)
clean <- tm_map(clean, remove.hashtag)
inspect(clean[1:15])

removeamp <- function(y) gsub("&amp;", "", y)
clean <- tm_map(clean, removeamp)
inspect(clean[1:15])

removetitik3 <- function(y) gsub("[[:punct:]]", "", y)
clean <- tm_map(clean, removetitik3)
inspect(clean[1:15])

remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
clean <- tm_map(clean,remove.all)
inspect(clean[1:15])

#remove extra whitespace (spasi)
clean <- tm_map(clean, stripWhitespace)
inspect(clean[1:15])


#load slangword
slang <- read.csv("file:///D:/PROJECT/EL-CREATIVEON/05.01.2020 - REZA - Sentimen Analisis/Code/Code/slangword_list.csv", header=T)
old_slang <- as.character(slang$old) 
new_slang <- as.character(slang$new)

slangword <- function(x) Reduce(function(x,r) gsub(slang$old[r],slang$new[r],x,fixed=T),
                                seq_len(nrow(slang)),x)
clean <- tm_map(clean,slangword)
inspect(clean[1:15])

#Stemming
stemming=data.frame(text=unlist(sapply(clean, `[`)), stringsAsFactors=F)
stemming=lapply(stemming, as.character)

stemming.word <- function(x){
  paste(lapply(x,katadasar),collapse = " ")}

stemming <- lapply(tokenize_words(stemming$text[]), stemming.word)
head(stemming,15)


#Tokenization
library(tokenizers)
tokenization <- tokenize_words(stemming)
head(tokenization,15)

##load stopword-ID
stopwordID <- "file:///D:/PROJECT/EL-CREATIVEON/05.01.2020 - REZA - Sentimen Analisis/Code/Code/id.stopwords.02.01.2016.txt"

##membaca baris stopwordID
StopwordID<-readLines(stopwordID)
stopword.char=as.character(tokenization)
stopword <- tokenize_words(stopword.char, stopwords = StopwordID)
head(stopword,15)

## save data
savedata=as.character(stopword)
savedata = gsub("[[:punct:]]", "", savedata)
savedata=gsub(", "," ",savedata)
savedata=gsub("cc "," ",savedata)
dataframe=data.frame(tweets=unlist(sapply(savedata, `[`)),score.retweet=score.retweet$score.retweet)
View(dataframe)
write.csv(dataframe,file = 'D://PROJECT//EL-CREATIVEON//05.01.2020 - REZA - Sentimen Analisis//Code//Result//TweetClean.csv')

###------------CARI KATA------------
library(tm)
clean.for.find.word=read.csv('D://PROJECT//EL-CREATIVEON//05.01.2020 - REZA - Sentimen Analisis//Code//Result//TweetClean.csv', header = T)
View(clean.for.find.word)
clean.for.find.word=clean.for.find.word$tweets
find.word<- Corpus(VectorSource(clean.for.find.word))

writeLines(strwrap(find.word[[1]], 100))

myCorpus=find.word
tdm <- TermDocumentMatrix(myCorpus,
                          control = list(wordLengths = c(1, Inf)))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 5)
df <- data.frame(term = names(term.freq), freq = term.freq)
df <- df[with(df, order(-freq)), ]
n=dim(df)[1]
df <- data.frame(no=1:n, df)
View(df)

