tweets= read.csv(file.choose(), header = T)#twittes related info
tweets
str(tweets)

#build corpus-> collection of documents
#Text Mining
library(tm)
corpus<- iconv(tweets$text,to="utf-8")
corpus<-Corpus(VectorSource(corpus))
inspect(corpus[1:5])

#Clean Text
corpus<- tm_map(corpus,tolower)
inspect(corpus[1:5])

corpus<- tm_map(corpus,removePunctuation)
inspect(corpus)

corpus<- tm_map(corpus,removeNumbers)
inspect(corpus)

cleanset<- tm_map(corpus, removeWords, stopwords('english'))
cleanset
inspect(cleanset)

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset<- tm_map(cleanset,removeWords,c('aapl','apple'))
cleanset<- tm_map(cleanset,gsub,pattern='stocks',replacement='stock')

cleanset<-tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

#Term document matrix
tdm=TermDocumentMatrix(cleanset)
tdm

tdm <- as.matrix(tdm)
tdm
tdm[1:10,1:20]

#barplot
w= rowSums(tdm)
w
w=subset(w,w>=25)
w
barplot(w,las=2,col=rainbow(50))

#wordcloud
library(wordcloud)
w=sort(rowSums(tdm),decreasing=TRUE)
w
set.seed(222)
wordcloud(words = names(w),freq = w)
wordcloud(words = names(w),freq = w,max.words = 150)
wordcloud(words = names(w),freq = w,max.words = 150,random.order = F,min.freq = 10)
wordcloud(words = names(w),freq = w,max.words = 150,random.order = F,min.freq = 10,colors=brewer.pal(8,'Dark2'))
wordcloud(words = names(w),freq = w,max.words = 150,random.order = F,min.freq = 10,colors=brewer.pal(8,'Dark2'),scale = c(5,0.3))


#Wordcloud2
library(wordcloud2)
w<- data.frame(names(w),w)
colnames(w)<-c('word','freq')
wordcloud2(w,size = 0.5,shape = 'star')


#SENTIMENTAL ANALYSIS

library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

apple= read.csv(file.choose(), header = T)#twittes related info

str(tweets)

#build corpus-> collection of documents
library(tm)
tweets<- iconv(apple$text,to="UTF-8")
tweets
s<-get_nrc_sentiment(tweets)
s
head(s)

#barplot
library(barplot)
?barplot
barplot(colSums(s),
        las = 2 ,
        col = rainbow(10),
        ylab = "Count",
        main = "Sentiment Scores for Apple Tweets")


