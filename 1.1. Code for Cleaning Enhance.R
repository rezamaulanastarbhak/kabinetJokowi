
###------------CARI KATA------------
library(tm)
clean.for.find.word=read.csv('D://PROJECT//EL-CREATIVEON//05.01.2020 - REZA - Sentimen Analisis//Code//Result//TweetClean.csv', header = T)
View(clean.for.find.word)
clean.for.find.word$tweets=clean.for.find.word$tweets
find.word<- Corpus(VectorSource(clean.for.find.word$tweets))

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

#Tokenization
library(tokenizers)
stemming=lapply(find.word, as.character)
tokenization <- tokenize_words(stemming)
head(tokenization,15)

##load stopword-ID
stopwordID <- "file:///D:/PROJECT/EL-CREATIVEON/05.01.2020 - REZA - Sentimen Analisis/Code/Code/id.stopwords.02.01.2016.txt"

##membaca baris stopwordID
StopwordID<-readLines(stopwordID)
stopword.char=as.character(tokenization)
stopword <- tokenize_words(stopword.char, stopwords = StopwordID)
stopword=as.character(stopword)
stopword <- tokenize_words(stopword)

savedata=as.character(stopword)
savedata = gsub("[[:punct:]]", "", savedata)
savedata=gsub(", "," ",savedata)
savedata=gsub("cc "," ",savedata)
dataframe=data.frame(tweets=unlist(sapply(savedata, `[`)))

clean.for.find.word=dataframe$tweets
find.word<- Corpus(VectorSource(clean.for.find.word))

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

dataframe2=data.frame(tweets=unlist(sapply(savedata, `[`)),score.retweet=clean.for.find.word$score.retweet)
write.csv(dataframe2,file = 'D://PROJECT//EL-CREATIVEON//05.01.2020 - REZA - Sentimen Analisis//Code//Result//TweetClean2.csv')
