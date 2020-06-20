#require(devtools)
#install_github("lchiffon/wordcloud2")
library(wordcloud2)
library(tm)
library(ggplot2)

tweetclean<-read.csv("file:///D:/PROJECT/EL-CREATIVEON/05.01.2020 - REZA - Sentimen Analisis/Code/Code Fix/Result/TweetClean Weighted.csv", stringsAsFactors = FALSE)
corpus<-Corpus(VectorSource(tweetclean$tweets))

myCorpus=corpus
tdm <- TermDocumentMatrix(myCorpus,
                          control = list(wordLengths = c(1, Inf)))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 10)
df <- data.frame(term = names(term.freq), freq = term.freq)
df <- df[with(tweetclean, order(-term.freq)), ]
n=dim(df)[1]
df <- data.frame(no=1:n, df)
View(df)

#Wordcloud
words <- data.frame(word = df$term, freq = df$freq)
wordcloud2(words)

#Barchart 10 frekuensi terbanyak
barchart10<-ggplot(data=head(df,10), aes(x=term, y=freq)) + geom_bar(stat="identity") 
barchart10

#PieChart
retweet= table(tweetclean$score.retweet)
retweet
retweet[[1]]
x=c(retweet[[1]]/nrow(tweetclean), retweet[[2]]/nrow(tweetclean))
labels=c("Retweet", "Tweet")
vs=data.frame(labels,x)

# Create a basic bar
pie = ggplot(vs, aes(x=labels, y=x, fill=labels)) + geom_bar(stat="identity", width=1) + scale_fill_manual(values=c("#999999", "#E69F00"))
# Convert to pie (polar coordinates) and add labels
pie = pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(x*100), "%")), position = position_stack(vjust = 0.5))
# Remove labels and add title
pie = pie + labs(x = NULL, y = NULL, fill = NULL)
# Tidy up the theme
pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank())
pie
