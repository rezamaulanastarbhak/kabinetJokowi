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
write.csv(data.tweets,file = 'D://PROJECT//EL-CREATIVEON//05.01.2020 - REZA - Sentimen Analisis//Code//Code Fix//Result//Cleaning Weighted Poin 2.csv')


#find RT
retweet=grepl("(RT|via)((?:\\b\\W*@\\w+)+)", data.cleaning$text, ignore.case=TRUE, data.cleaning$text)
score.retweet=ifelse(retweet==TRUE,print(0),print(1))
score.retweet = cbind(data.tweets,score.retweet)
View(head(score.retweet,15))
write.csv(score.retweet,file = 'D://PROJECT//EL-CREATIVEON//05.01.2020 - REZA - Sentimen Analisis//Code//Code Fix//Result//Cleaning Weighted Poin 3.csv')

##hanya ambil data tweet saja
data.cleaning.text=score.retweet$data.cleaning.text
data.cleaning.text=Corpus(VectorSource(data.cleaning.text))
inspect(data.cleaning.text[1:15])
data.tweet.clean=data.frame(text=unlist(sapply(data.cleaning.text, `[`)), stringsAsFactors=F)
write.csv(clean.html,file = 'D://PROJECT//EL-CREATIVEON//05.01.2020 - REZA - Sentimen Analisis//Code//Code Fix//Result//Cleaning Weighted Poin 4.csv')


##Cleaning data
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
clean <- tm_map(data.cleaning.text, removeURL)

removeNL <- function(y) gsub("\n", " ", y)
clean <- tm_map(clean, removeNL)

removepipe <- function(z) gsub("<[^>]+>", "", z)
clean <- tm_map(clean, removepipe)
inspect(clean[1:15])
clean.html=data.frame(text=unlist(sapply(clean, `[`)), stringsAsFactors=F)
write.csv(clean.html,file = 'D://PROJECT//EL-CREATIVEON//05.01.2020 - REZA - Sentimen Analisis//Code//Code Fix//Result//Cleaning Weighted Poin 5.csv')

clean <- tm_map(clean, tolower)
inspect(clean[1:15])
clean.tolower=data.frame(text=unlist(sapply(clean, `[`)), stringsAsFactors=F)
write.csv(clean.tolower,file = 'D://PROJECT//EL-CREATIVEON//05.01.2020 - REZA - Sentimen Analisis//Code//Code Fix//Result//Cleaning Weighted Poin 6.csv')

remove.mention <- function(z) gsub("@\\S+", "", z)
clean <- tm_map(clean, remove.mention)
inspect(clean[1:15])
clean.mention=data.frame(text=unlist(sapply(clean, `[`)), stringsAsFactors=F)
write.csv(clean.mention,file = 'D://PROJECT//EL-CREATIVEON//05.01.2020 - REZA - Sentimen Analisis//Code//Code Fix//Result//Cleaning Weighted Poin 7.csv')

remove.hashtag <- function(z) gsub("#\\S+", "", z)
clean <- tm_map(clean, remove.hashtag)
inspect(clean[1:15])
clean.hashtag=data.frame(text=unlist(sapply(clean, `[`)), stringsAsFactors=F)
write.csv(clean.hashtag,file = 'D://PROJECT//EL-CREATIVEON//05.01.2020 - REZA - Sentimen Analisis//Code//Code Fix//Result//Cleaning Weighted Poin 8.csv')

removeamp <- function(y) gsub("&amp;", "", y)
clean <- tm_map(clean, removeamp)

removetitik3 <- function(y) gsub("[[:punct:]]", "", y)
clean <- tm_map(clean, removetitik3)
inspect(clean[1:15])
clean.punct=data.frame(text=unlist(sapply(clean, `[`)), stringsAsFactors=F)
write.csv(clean.punct,file = 'D://PROJECT//EL-CREATIVEON//05.01.2020 - REZA - Sentimen Analisis//Code//Code Fix//Result//Cleaning Weighted Poin 9.csv')

remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
clean <- tm_map(clean,remove.all)
inspect(clean[1:15])
clean.all=data.frame(text=unlist(sapply(clean, `[`)), stringsAsFactors=F)
write.csv(clean.all,file = 'D://PROJECT//EL-CREATIVEON//05.01.2020 - REZA - Sentimen Analisis//Code//Code Fix//Result//Cleaning Weighted Poin 10.csv')

#remove extra whitespace (spasi)
clean <- tm_map(clean, stripWhitespace)
inspect(clean[1:15])
clean.space=data.frame(text=unlist(sapply(clean, `[`)), stringsAsFactors=F)
write.csv(clean.space,file = 'D://PROJECT//EL-CREATIVEON//05.01.2020 - REZA - Sentimen Analisis//Code//Code Fix//Result//Cleaning Weighted Poin 11.csv')


#load slangword
slang <- read.csv("file:///D:/PROJECT/EL-CREATIVEON/05.01.2020 - REZA - Sentimen Analisis/Code/Code Fix/slangword_list.csv", header=T)
old_slang <- as.character(slang$old) 
new_slang <- as.character(slang$new)

slangword <- function(x) Reduce(function(x,r) gsub(slang$old[r],slang$new[r],x,fixed=T),
                                seq_len(nrow(slang)),x)
clean <- tm_map(clean,slangword)
inspect(clean[1:15])
clean.slang=data.frame(text=unlist(sapply(clean, `[`)), stringsAsFactors=F)
write.csv(clean.slang,file = 'D://PROJECT//EL-CREATIVEON//05.01.2020 - REZA - Sentimen Analisis//Code//Code Fix//Result//Cleaning Weighted Poin 12.csv')

#Stemming
stemming=data.frame(text=unlist(sapply(clean, `[`)), stringsAsFactors=F)
stemming=lapply(stemming, as.character)

stemming.word <- function(x){
  paste(lapply(x,katadasar),collapse = " ")}

stemming <- lapply(tokenize_words(stemming$text[]), stemming.word)
head(stemming[1:15])
clean.stem=data.frame(text=unlist(sapply(stemming, `[`)), stringsAsFactors=F)
write.csv(clean.stem,file = 'D://PROJECT//EL-CREATIVEON//05.01.2020 - REZA - Sentimen Analisis//Code//Code Fix//Result//Cleaning Weighted Poin 13.csv')

#Tokenization
library(tokenizers)
library(stringi)
tokenization <- tokenize_words(stemming)
head(tokenization,15)

clean.token=stri_list2matrix(tokenization, byrow=TRUE)
View(clean.token)
write.csv(clean.token,file = 'D://PROJECT//EL-CREATIVEON//05.01.2020 - REZA - Sentimen Analisis//Code//Code Fix//Result//Cleaning Weighted Poin 14.csv')

##load stopword-ID
stopwordID <- "file:///D:/PROJECT/EL-CREATIVEON/05.01.2020 - REZA - Sentimen Analisis/Code/Code Fix/id.stopwords.02.01.2016.txt"

##membaca baris stopwordID
StopwordID<-readLines(stopwordID)
stopword.char=as.character(tokenization)
stopword <- tokenize_words(stopword.char, stopwords = StopwordID)
head(stopword,15)
clean.stopword=stri_list2matrix(stopword, byrow=TRUE)
View(clean.stopword)
write.csv(clean.stopword,file = 'D://PROJECT//EL-CREATIVEON//05.01.2020 - REZA - Sentimen Analisis//Code//Code Fix//Result//Cleaning Weighted Poin 15.csv')

## save data
savedata=as.character(stopword)
savedata=gsub("^c\\(|\\)$"," ",savedata)
savedata = gsub("[[:punct:]]", "", savedata)
savedata=gsub(", "," ",savedata)
dataframe=data.frame(tweets=unlist(sapply(savedata, `[`)),score.retweet=score.retweet$score.retweet)
View(dataframe)
write.csv(dataframe,file = 'D://PROJECT//EL-CREATIVEON//05.01.2020 - REZA - Sentimen Analisis//Code//Code Fix//Result//TweetClean Weighted.csv')

###------------CARI KATA------------
library(tm)
clean.for.find.word=read.csv('D://PROJECT//EL-CREATIVEON//05.01.2020 - REZA - Sentimen Analisis//Code//Code Fix//Result//TweetClean Weighted.csv', header = T)
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
write.csv(df,file = 'D://PROJECT//EL-CREATIVEON//05.01.2020 - REZA - Sentimen Analisis//Code//Code Fix//Result//Cleaning Weighted Poin 16.csv')
