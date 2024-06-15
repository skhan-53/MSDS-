library(tidyverse) 
library(dplyr) 
library(arules)
library(arulesViz)

# Load data (100k rows)
goodreads <- read.csv("C:\\Users\\Administrator\\OneDrive\\Desktop\\Syracuse University\\Spring 2022 - Volume 2\\IST 707 (Applied Machine Learning)\\Project\\goodreads_100k.csv")

# Subset of relevant columns (rating and review_text)
data <- goodreads[,4:5]

# Discretize rating column, remove punctuation, and set review_text to factor
data$rating <- cut(data$rating, breaks = c(-1, 0, 1, 2, 3, 4, 5),
                   labels = c("zero", "one", "two", "three", "four", "five"))

data <- mutate(data, review_text = str_remove_all(review_text, "'|\\.|,"))

data$review_text <- factor(data$review_text)

# Generate rules
rules <- apriori(data, parameter = list(supp = 0.001, conf = 0.1, maxlen = 3))
rules <- sort(rules, by = "lift", decreasing = TRUE)
summary(rules)
inspect(rules)


rules2 <- apriori(data, parameter = list(supp = 0.0001, conf = 0.0001, 
                                         target = "rules", minlen = 2))
rules2 <- sort(rules2, by = "lift", decreasing = TRUE)
summary(rules2)
inspect(head(subset(rules2, size(rules2) == 2), 10))
inspect(rules2)