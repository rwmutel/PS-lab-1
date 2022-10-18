{r}
library(tidytext)
library(readr)
library(dplyr)
library(ggplot2)

{r}
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
                              total_prob_neg <<- prob_sentiments[1,"n"]/sum(prob_sentiments$n)
                              total_prob_neu <<- prob_sentiments[2,"n"]/sum(prob_sentiments$n)
                              total_prob_pos <<- prob_sentiments[3,"n"]/sum(prob_sentiments$n)
                              
                              fdict <<- data.frame(
                                word = count(tidy_text, token, sort=TRUE)$token
                                #total = count(tidy_text, token, sort=TRUE)$n
                              )
                              positive <- tidy_text %>% filter(sentiment=="positive")  %>% count(token, sort=TRUE)
                              total_pos = nrow(positive)
                              neutral <- tidy_text %>% filter(sentiment=="neutral") %>% count(token, sort=TRUE)
                              total_neu = nrow(neutral)
                              negative <- tidy_text %>% filter(sentiment=="negative")  %>% count(token, sort=TRUE)
                              total_neg = nrow(negative)
                              total_all = n_distinct(tidy_text$token)
                              
                              fdict$prob_pos <<- seq(0,0, length=length(fdict$word))
                              fdict$prob_neu <<- seq(0,0, length=length(fdict$word))
                              fdict$prob_neg <<- seq(0,0, length=length(fdict$word))
                              
                              for(i in 1:nrow(fdict)){
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
                                
                                fdict[i,"prob_pos"] <<- (i_pos + 1) / (total_pos + total_all)
                                fdict[i,"prob_neu"] <<- (i_neu + 1) / (total_neu + total_all)
                                fdict[i,"prob_neg"] <<- (i_neg + 1) / (total_neg + total_all)
                              }
                            },
                            predict = function(message)
                            {
                              tokens = data.frame(
                                word = unlist(strsplit(message, split=" "))
                              )
                              tokens <-  filter(tokens, !word %in% stop_words & word %in% fdict$word)
                              filtered_fdict <- filter(fdict, word %in% tokens$word)
                              prob_pos <- prod(filtered_fdict$prob_pos) * total_prob_pos
                              prob_neu <- prod(filtered_fdict$prob_neu) * total_prob_neu
                              prob_neg <- prod(filtered_fdict$prob_neg) * total_prob_neg
                              #print(prob_pos)
                              #print(prob_neu)
                              #print(prob_neg)
                              if (prob_pos > prob_neu & prob_pos > prob_neg){
                                return("positive")
                              }
                              else if(prob_pos > prob_neg){
                                return("neutral")
                              }
                              else{
                                return("negative")
                              }
                              
                            },
                            score = function(X_test, y_test)
                            {
                              df <- data.frame(X_test,y_test)
                              df$prediction = seq(0,0,length=nrow(df))
                              counter = 0
                              for(i in (1:nrow(df))){
                                df[i,"prediction"] = .self$predict(df[i,"X_test"])
                                if(df[i,"prediction"] == df[i,"y_test"]){
                                  counter = counter + 1
                                }
                              }
                              #print(df)
                              return(counter/nrow(df))
                            }
                          ))
test_path <- "3-sentiment/test.csv"
train_path <- "3-sentiment/train.csv"
train <- read.csv(file = train_path, stringsAsFactors = FALSE)
test <- read.csv(file = test_path, stringsAsFactors = FALSE)

model = naiveBayes()
model$fit(train$text, train$sentiment)
model$fdict
model$predict("Cramo slipped to a pretax loss of EUR 6.7 million from a pretax profit of EUR 58.9 million .")
#model$predict("According to Gran , the company has no plans to move all production to Russia , although that is where the company is growing .")
print("Model Accuracy is:")
model$score(test$text, test$sentiment)