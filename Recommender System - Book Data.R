
library(recommenderlab)
library(caTools)
library(Matrix)

book<- read.csv('E:\\Data Science\\Assignments\\books.csv')
books<-book[,-1]
str(books)
hist(books$Book.Rating)
book_matrix <- as(books, 'realRatingMatrix')
book_matrix@data

book_recomm_model1 <- Recommender(book_matrix, method="POPULAR")

recommended_items1 <- predict(book_recomm_model1, book_matrix, n=1)
as(recommended_items1, "list")



