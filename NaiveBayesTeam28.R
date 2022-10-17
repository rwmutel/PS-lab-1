{r}
library(tidytext)
library(readr)
library(dplyr)
library(ggplot2)

naiveBayes <- setRefClass("naiveBayes",
  fields = c("fdict", "total_prob_pos", "total_prob_neu", "total_prob_neg"),
  methods = list(
    fit = function(X, y)
    {
      df <- data.frame(text=X,sentiment=y)
      stop_words <- strsplit(read_file("stop_words.txt"), split='\n')[[1]]
      stop_words = gsub("[\r]", "", stop_words)
      tidy_text <- unnest_tokens(df, 'token', 'text', token="words") %>% filter(!token %in% stop_words)
      prob_sentiments <- count(tidy_text, sentiment)
      total_prob_neg <<- prob_sentiments[1,]$n/sum(prob_sentiments$n)
      total_prob_neu <<- prob_sentiments[2,]$n/sum(prob_sentiments$n)
      total_prob_pos <<- prob_sentiments[3,]$n/sum(prob_sentiments$n)
      
      fdict <<- data.frame(
        word = count(tidy_text, token, sort=TRUE)$token
        #total = count(tidy_text, token, sort=TRUE)$n
      )
      positive <- tidy_text %>% filter(sentiment=="positive")  %>% count(token)
        total_pos = nrow(positive)
        neutral <- tidy_text %>% filter(sentiment=="neutral") %>% count(token)
        total_neu = nrow(neutral)
        negative <- tidy_text %>% filter(sentiment=="negative")  %>% count(token)
        total_neg = nrow(negative)
        total_all = n_distinct(tidy_text$token)
        
        fdict$prob_pos <<- seq(0,0, length=length(fdict$word))
        fdict$prob_neu <<- seq(0,0, length=length(fdict$word))
        fdict$prob_neg <<- seq(0,0, length=length(fdict$word))
        
        for(i in 1:1000){
          i_pos = positive[positive$token==fdict[i,"word"],]$n
          i_neu = neutral[neutral$token==fdict[i,"word"],]$n
          i_neg = negative[negative$token==fdict[i,"word"],]$n
          
          if (length(i_pos) == 0){
            i_pos <- integer(1)
            i_pos = 0
          }
          if (length(i_neu) == 0){
            i_neu <- integer(1)
            i_neu = 0
          }
          if (length(i_neg) == 0){
            i_neg <- integer(1)
            i_neg = 0
          }
          
          fdict[i,"prob_pos"] <<- (i_pos + 1) / (total_pos + 3)
          fdict[i,"prob_neu"] <<- (i_neu + 1) / (total_neu + 3)
          fdict[i,"prob_neg"] <<- (i_neg + 1) / (total_neg + 3)
        }    
      },

      predict = function(message)
      {
        tokens = read.table(text=message,col.names=c("word")) %>% filter(!word %in% stop_words)
        prob_pos = 1
        prob_neu = 1
        prob_neg = 1
        for (i in 1:nrow(tokens)) {
          if (length(fdict[fdict$word==tokens[i,"word"],]) > 0){
            prob_pos = prob_pos * fdict[fdict$word==tokens[i,"word"],]$prob_pos
            prob_neu = prob_neu * fdict[fdict$word==tokens[i,"word"],]$prob_neu
            prob_neg = prob_neg * fdict[fdict$word==tokens[i,"word"],]$prob_neg}
        }
        print(prob_pos*total_prob_pos)
        print(prob_neu*total_prob_neu)
        print(prob_neg*total_prob_neg)
      },

      score = function(X_test, y_test)
      {
        # TODO
      }
))

test_path <- "3-sentiment/test.csv"
train_path <- "3-sentiment/train.csv"
train <- read.csv(file = train_path, stringsAsFactors = FALSE)
test <- read.csv(file = test_path, stringsAsFactors = FALSE)

model = naiveBayes()
print(model$fdict)

