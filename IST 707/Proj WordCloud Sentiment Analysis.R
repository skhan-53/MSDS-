
install.packages("wordcloud")
install.packages("qdap")
library(RColorBrewer)
library(qdap)
library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(ggplot2) 
library(ggthemes)
library(wordcloud)

install.packages("textdata")
library(textdata)


str(good_reads)

good_reads = read.csv("/Users/kevin/OneDrive/IST 707 Machine Personal/good_reads_small.csv",stringsAsFactors = F)
good_reads %>%
  summarize(average_rating = mean(review_rating), median_rating = median(review_rating))

ggplot(data=good_reads,aes(x=review_rating))+
  geom_bar(fill='sienna')+
  theme_economist()

good_reads%>%
  summarize(mean_character = mean(nchar(review_text)), median_character = median(nchar(review_text)))

good_reads%>%
  summarize(mean_words = mean(str_count(string = review_text,pattern = '\\S+')), median_words = median(str_count(string = review_text,pattern = '\\S+')))

good_reads%>%
  summarize(mean_sentences = mean(str_count(string = review_text,pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]")), median_sentences = median(str_count(string = review_text,pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]")))

good_reads %>%
  select(id,review_text) %>%
  unnest_tokens(output = word,input=review_text) %>%
  group_by(id) %>%
  count() %>%
  head()

positive_negative_words = good_reads%>%
  select(id,review_text)%>%
  unnest_tokens(output = word, input = review_text)%>%
  inner_join(get_sentiments('bing'))

head(positive_negative_words)


ggplot(positive_negative_words,aes(x=sentiment,fill=sentiment))+
  geom_bar()+
  guides(fill = F)+
  theme_economist()

Sentiment_of_Ratings = 
  good_reads %>%
  select(id,review_text,review_rating)%>%
  unnest_tokens(output=word,input=review_text)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(review_rating,sentiment)%>%
  summarize(amount = n())%>%
  mutate(proportion = amount/sum(amount))

Sentiment_of_Ratings %>%
  ggplot(aes(x=review_rating,y=proportion,fill=sentiment))+
  geom_col()+
  theme_economist()

good_reads %>%
  select(id,review_text) %>%
  unnest_tokens(output = word, input = review_text) %>%
  inner_join(get_sentiments('nrc')) %>%
  group_by(sentiment) %>%
  count()

good_reads %>%
  select(id,review_text)%>%
  unnest_tokens(output = word, input = review_text)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=reorder(sentiment,X = n),y=n,fill=sentiment))+
  geom_col()+
  guides(fill=F)+
  coord_flip()+
  theme_wsj()

wordcloudData1 = 
  good_reads%>%
  select(id,review_text)%>%
  unnest_tokens(output=word,input=review_text)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(freq = n())%>%
  arrange(desc(freq))%>%
  ungroup()%>%
  data.frame()

set.seed(617)
wordcloud(words = wordcloudData1$word,wordcloudData1$freq,scale=c(2,0.5),max.words = 200,colors=brewer.pal(9,"Spectral"))


wordcloudData2 = 
  good_reads%>%
  select(id,review_text)%>%
  unnest_tokens(output=word,input=review_text)%>%
  anti_join(stop_words)%>%
  inner_join(get_sentiments('bing'))%>%
  count(sentiment,word,sort=T)%>%
  spread(key=sentiment,value = n,fill=0)%>%
  data.frame()

rownames(wordcloudData2) = wordcloudData2[,'word']
wordcloudData2 = wordcloudData2[,c('positive','negative')]
wcd2<-as.matrix(wordcloudData2)

set.seed(617)
comparison.cloud(term.matrix = wcd2,scale = c(2,0.5),max.words = 200, rot.per=0)


