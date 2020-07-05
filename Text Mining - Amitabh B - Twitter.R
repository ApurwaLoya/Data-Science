install.packages("twitteR")
library("twitteR")

install.packages("ROAuth")
library("ROAuth")

install.packages("base64enc")
library(base64enc)

install.packages("httpuv")
library(httpuv)

### https://apps.twitter.com/
cred <- OAuthFactory$new(consumerKey='P6W3njsfVAXkT8KfkiQxayTpM',
                         consumerSecret='1Tu9JJJArylRZXOmP3qgHqoscncBGYviA6ueNABnIVO3Tm7Ngx',
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

#save(cred, file="twitter authentication.Rdata")
#load("twitter authentication.Rdata")

setup_twitter_oauth("P6W3njsfVAXkT8KfkiQxayTpM", 
                    "1Tu9JJJArylRZXOmP3qgHqoscncBGYviA6ueNABnIVO3Tm7Ngx",
                    "2879802830-7p8uJSnMiE2n8NlnLSMbN4cChveXN8xpi2YMxPK", 
                    "gDMm4u17kwUndaqRBF8LEKactexUlLxy9rN4RNayyr0mZ")  

Tweets <- userTimeline('SrBachchan', n = 1000)

TweetsDF <- twListToDF(Tweets)
setwd("E:\\Data Science\\Assignments")
write.csv(TweetsDF, "Tweets_sarf.csv")
getwd()

Tweet <- read.csv(file.choose())
str(Tweet)

library(tm)
x<- as.character(Tweet$text)
x <- Corpus(VectorSource(x))
inspect(x[1:5])

x1 <- tm_map(x, tolower)
inspect(x1[1:5])

x1 <- tm_map(x, removePunctuation)
inspect(x1[1:5])

x1 <- tm_map(x1, removeNumbers)
inspect(x1[1:5])

x1 <- tm_map(x1, stripWhitespace)
inspect(x1[1:5])

x1 <- tm_map(x1, removeWords, stopwords('english'))
inspect(x1[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
x1 <- tm_map(x1, content_transformer(removeURL))
inspect(x1[1:5])

x1 <- tm_map(x1, removeWords, c("the",'can','will','thank'))
inspect(x1[1:5])

x1 <- tm_map(x1, stripWhitespace)
inspect(x1[1:5])

tdm <- TermDocumentMatrix(x1)
tdm <- as.matrix(tdm)
w1 <- rowSums(tdm)
tdm[1:10,1:20]

library(wordcloud)
windows()
wordcloud(words = names(w1), freq = w1)

w_sub1 <- sort(rowSums(tdm), decreasing = TRUE)
wordcloud(words = names(w_sub1), freq = w_sub1)

windows()
wordcloud(words = names(w1), freq = w1, random.order = F, colors = rainbow(20), scale=c(5,.2), rot.per = 0.6)

pos.words = scan(file.choose(), what="character", comment.char=";")	
neg.words = scan(file.choose(), what="character", comment.char=";") 
pos.words = c(pos.words,"wow", "kudos", "hurray") 

pos.matches = match(names(w_sub1), c(pos.words))
pos.matches = !is.na(pos.matches)
freq_pos <- w_sub1[pos.matches]
p_names <- names(freq_pos)
windows()
wordcloud(p_names,freq_pos,scale=c(3.5,.2),colors = rainbow(20))

neg.matches = match(names(w_sub1), c(neg.words))
neg.matches = !is.na(neg.matches)
freq_neg <- w_sub1[neg.matches]
n_names <- names(freq_neg)
windows()
wordcloud(n_names,freq_neg,scale=c(3.5,.2),colors = brewer.pal(8,"Dark2"))

library(syuzhet)
s<-get_nrc_sentiment(x)
head(s)
barplot(colSums(s), las = 2.5, col = rainbow(10),ylab = 'Count',main= 'Sentiment scores for Amitabh Bachchan Tweets')

