#load necessary libraries
library(RWeka)
library(cluster)
library(arules)
library(arulesViz)
library(datasets)
library(readr)
library(tidyr)

#import data
originalData <- read.csv("/Users/aannick/Documents/Grad School/IST707/Project/goodreads_train.csv")

#PREPROCESSING FOR CLUSTERING

#Check if there are entries with NAs
colSums(is.na(preprocessedData))
#no NAs present in data. 

#Check data types
str(preprocessedData)

########################CLUSTER WITH ONLY RATING################################
clusterRating <- originalData[,c("book_id","rating")]
condensed_meanRating <- aggregate(clusterRating$rating,by=list(clusterRating$book_id),data=clusterRating,FUN=mean)

#change the book_id to the row values
rownames(condensed_meanRating) <- condensed_meanRating$Group.1
#get rid of the book_id column cause it's now the row label
condensed_meanRating$Group.1<-NULL
colnames(condensed_meanRating) <- c("rating")

#use the kMeans algorithm in R
model_r_bookDataRating <- kmeans(condensed_meanRating,5)
#plot kMeans algorithm
clusplot(condensed_meanRating,model_r_bookDataRating$cluster,color=TRUE,shade=TRUE,labels=2,lines=0) 

########################END CLUSTER WITH ONLY RATING################################

########################START CLUSTER WITH RATING, COMMENTS, VOTES##################

#Remove date_added, date_updated, read_at, started_at, review_id, review_text
clusterRatingVotesComments <- originalData[,c("book_id","rating","n_votes","n_comments")]

#keep only unique book_id values, but average the rating of all the instances of that particular book_id from the big dataset
#source: https://r-coder.com/aggregate-r/
condensed_meanRating <- aggregate(clusterRatingVotesComments$rating,by=list(clusterRatingVotesComments$book_id),data=clusterRatingVotesComments,FUN=mean)
condensed_meanVotes <- aggregate(clusterRatingVotesComments$n_votes,by=list(clusterRatingVotesComments$book_id),data=clusterRatingVotesComments,FUN=mean)
condensed_meanComments <- aggregate(clusterRatingVotesComments$n_comments,by=list(clusterRatingVotesComments$book_id),data=clusterRatingVotesComments,FUN=mean)
condensed <- merge(condensed_meanRating,condensed_meanVotes,by="Group.1")
colnames(condensed) <- c("Group.1","rating","votes")
condensed <- merge(condensed,condensed_meanComments,by="Group.1")
colnames(condensed) <- c("Group.1","rating","votes","comments")

#change the book_id to the row values
rownames(condensed) <- condensed$Group.1
#get rid of the book_id column cause it's now the row label
condensed$Group.1<-NULL

#testing condensed_mean code
#newVector <- preprocessedData[preprocessedData$book_id == 2, ]
#mean(newVector$rating)

#use the kMeans algorithm in R
model_r_bookDataAll <- kmeans(condensed,5)
#plot kMeans algorithm
clusplot(condensed,model_r_bookDataAll$cluster,color=TRUE,shade=TRUE,labels=2,lines=0) 

########################END CLUSTER WITH RATING, COMMENTS, VOTES##################
