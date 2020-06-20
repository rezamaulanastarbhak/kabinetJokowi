library(ggplot2)

kalimat2=read.csv('file:///D:/PROJECT/EL-CREATIVEON/05.01.2020 - REZA - Sentimen Analisis/Code/Code Fix/Result/TweetClean Asli.csv', header = T)

#ambil kata kata untuk skoring
positif <- scan("file:///D:/PROJECT/EL-CREATIVEON/05.01.2020 - REZA - Sentimen Analisis/Code/Code Fix/s-pos.txt",what="character",comment.char=";") 
negatif <- scan("file:///D:/PROJECT/EL-CREATIVEON/05.01.2020 - REZA - Sentimen Analisis/Code/Code Fix/s-neg.txt",what="character",comment.char=";") 

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
write.csv(hasil,file = 'D://PROJECT//EL-CREATIVEON//05.01.2020 - REZA - Sentimen Analisis//Code//Code Fix//Result//Scoring Asli Poin 3.csv')

#CONVERT SCORE TO SENTIMENT
hasil$klasifikasi<- ifelse(hasil$score<0, "Negatif",ifelse(hasil$score>0,"Positif", "Netral"))
head(hasil$klasifikasi, 15)
View(hasil[c(2,3,1)])
write.csv(hasil[c(2,3,1)],file = 'D://PROJECT//EL-CREATIVEON//05.01.2020 - REZA - Sentimen Analisis//Code//Code Fix//Result//Scoring Asli Poin 4.csv')

#Visualisasi Sentimen
##Asli
frekuensi.asli=table(hasil$klasifikasi)
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