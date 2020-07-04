setwd("E:\\Data Science\\Assignments")

library(rvest)
library(XML)
library(magrittr)


aurl <- "https://www.imdb.com/title/tt10333912/reviews?ref_=tt_ov_rt"
IMDB_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>% html_nodes(".show-more__control") %>% html_text()
  IMDB_reviews <- c(IMDB_reviews,rev)
}
length(IMDB_reviews)

write.table(IMDB_reviews,"Gulabo Sitabo.txt",row.names = F)

x <- as.character(IMDB_reviews)

library(tm)
x <- Corpus(VectorSource(x))
inspect(x[1:5])

# Data Cleansing
x1 <- tm_map(x, tolower)
inspect(x1[1])

x1 <- tm_map(x, removePunctuation)
inspect(x1[1])

x1 <- tm_map(x1, removeNumbers)

x1 <- tm_map(x1, removeWords, stopwords('english'))
inspect(x1[1:5])
x1 <- tm_map(x1, tolower)

#striping white spaces
x1 <- tm_map(x1, stripWhitespace)
inspect(x1[1])

tdm <- TermDocumentMatrix(x1)
dtm <- t(tdm)
tdm <- as.matrix(tdm)

w <- rowSums(tdm)
w
w_sub <- subset(w, w >= 20)
w_sub
windows()
barplot(w_sub, las=3, col = rainbow(20))

x1<-tm_map(x1, removeWords, c('film','the','movie','will','this','also','one','gave'))
x1 <- tm_map(x1, stripWhitespace)

tdm <- TermDocumentMatrix(x1)
tdm <- as.matrix(tdm)
w1 <- rowSums(tdm)

library(wordcloud)
windows()
wordcloud(words = names(w1), freq = w1) 
w_sub1 <- sort(rowSums(tdm), decreasing = TRUE)
wordcloud(words = names(w_sub1), freq = w_sub1) 

windows()
wordcloud(words = names(w1), freq = w1, random.order = F, colors = rainbow(20), scale=c(2,.2), rot.per = 0.3)

pos.words = scan(file.choose(), what="character", comment.char=";")	
neg.words = scan(file.choose(), what="character", comment.char=";") 	

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
IMDB_rev <- read.delim('E:\\Data Science\\Assignments\\Gulabo Sitabo.TXT')
reviews <- as.character(IMDB_rev[-1,])
class(reviews)
s<-get_nrc_sentiment(reviews)
head(s)
barplot(colSums(s), las = 2, col = rainbow(10),ylab = 'Count',main= 'Sentiment scores for movie Gulabo Sitabo')
