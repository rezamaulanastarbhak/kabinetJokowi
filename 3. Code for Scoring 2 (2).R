library(ggplot2)

kalimat2=read.csv('D://PROJECT//EL-CREATIVEON//05.01.2020 - REZA - Sentimen Analisis//Code//Result//TweetClean2.csv', header = T)

#ambil kata kata untuk skoring
positif <- scan("file:///D:/PROJECT/EL-CREATIVEON/05.01.2020 - REZA - Sentimen Analisis/Code/Code/s-pos.txt",what="character",comment.char=";") 
negatif <- scan("file:///D:/PROJECT/EL-CREATIVEON/05.01.2020 - REZA - Sentimen Analisis/Code/Code/s-neg.txt",what="character",comment.char=";") 

score.sentiment = function(kalimat2, positif, negatif, .progress='none')
{
  require(plyr)
  require(stringr)
  scores = laply(kalimat2, function(kalimat, positif, negatif) {
    list.kata = str_split(kalimat, '\\s+')
    kata2 = unlist(list.kata)
    positif.matches = match(kata2, positif)
    negatif.matches = match(kata2, negatif)
    positif.matches = !is.na(positif.matches)
    negatif.matches = !is.na(negatif.matches)
    score = sum(positif.matches) - (sum(negatif.matches))
    return(score)
  }, positif, negatif, .progress=.progress )
  scores.df = data.frame(score=scores, text=kalimat2)
  return(scores.df)
}

hasil = score.sentiment(kalimat2$tweets, positif, negatif)
View(hasil)

#CONVERT SCORE TO SENTIMENT
hasil$klasifikasi<- ifelse(hasil$score<0, "Negatif",ifelse(hasil$score>0,"Positif", "Netral"))
head(hasil$klasifikasi, 15)
View(hasil[c(2,3,1)])

#Score for Weighted
weighted.score=hasil$score*kalimat2$score.retweet

#CONVERT TO SENTIMENT WEIGHTED
klasifikasi.weighted<- ifelse(weighted.score<0, "Negatif",ifelse(weighted.score>0,"Positif", "Netral"))
head(klasifikasi.weighted,15)

#EXCHANGE ROW SEQUENCE
data <- hasil[c(2,3,1)]
data <- data.frame(data, klasifikasi.weighted,weighted.score, score.retweet=kalimat2$score.retweet)
View(data)
write.csv(data, file = "D:/PROJECT/EL-CREATIVEON/05.01.2020 - REZA - Sentimen Analisis/Code/Code/Scoring.csv")

#Visualisasi Sentimen
##Asli
frekuensi.asli=table(data$klasifikasi)
frekuensi.asli

frekuensi.asli=c(frekuensi.asli[[1]], frekuensi.asli[[2]], frekuensi.asli[[3]])
label.asli=c("Negatif", "Netral", "Positif")
df.asli=data.frame(label.asli,frekuensi.asli)
barchart.asli<-ggplot(df.asli, aes(x=label.asli, y=frekuensi.asli)) + geom_bar(stat="identity", colour = "Black") + xlab("")
barchart.asli


##Weighted
frekuensi.weighted=table(data$klasifikasi.weighted)
frekuensi.weighted

frekuensi.weighted=c(frekuensi.weighted[[1]], frekuensi.weighted[[2]], frekuensi.weighted[[3]])
label.weighted=c("Negatif", "Netral", "Positif")
df.weighted=data.frame(label.weighted,frekuensi.weighted)
barchart.weighted<-ggplot(df.weighted, aes(x=label.weighted, y=frekuensi.weighted)) + geom_bar(stat="identity", colour = "Black") + xlab("")
barchart.weighted