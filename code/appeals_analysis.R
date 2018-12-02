# Purpose: A Characterizing Appeals Courts
# Date: November 2018
# Team:
library(tidytext)
library(tidyverse)
library(tm)
library(topicmodels)
library(magrittr)
library(tibble)
library(dplyr)
library(pROC)
library(ROCR)
# A)
# IMPORT DATA
appeals.data<-read_tsv('data/recent_opinions.tsv')
appeals.data<-appeals.data %>%  mutate(opinion_id = row_number())
  
data(stop_words)
#set the lexicon to custom
stopwords<- read.delim("data/custom_words.txt", header = FALSE)
stopwords<- tibble(word = stopwords$V1, lexicon = "custom")
stop_words_full <- bind_rows(stopwords, stop_words)
stop_words_full$word<- as.factor(stop_words_full$word)


# B) 
## a)
# Unnest the tokens and remove the custom stop words.
text_appeals <- appeals.data %>% unnest_tokens(word,text) %>% anti_join(stop_words_full)
#top 10 most frequent words 
top_10<-text_appeals %>% count(word) %>% arrange(desc(n)) %>% slice(1:10)
# top 10 in circuit 5
text_appeals %>% filter(circuit == 'fifth') %>% count(word) %>% arrange(desc(n)) %>% slice(1:10)
# top 10 in circuit 9
text_appeals %>% filter(circuit == 'ninth') %>% count(word) %>% arrange(desc(n)) %>% slice(1:10)



## b) Build a document-term tibble
# Convert from one-row-per-token data frame to one-row-per-document format 
# Retain the top 100 most frequent words
top_100<-text_appeals %>% count(word) %>% arrange(desc(n)) %>% slice(1:100)
most_freq_words<-top_100$word
text_appeals<-text_appeals %>% filter(word %in% most_freq_words)

# count number of occurances of each word per opinion_id.
appeals.tokens.count <- appeals.tokens %>% group_by(opinion_id) %>% count(word)
appeals.dtm <- appeals.tokens.count %>% cast_dtm(opinion_id, word, n)
appeals.dtm <- data.frame(as.matrix(appeals.dtm))

# count number of occurances of each word per opinion_id.
appeals.tokens.count <- appeals.tokens %>% group_by(opinion_id) %>% count(word)
appeals.dtm <- appeals.tokens.count %>% cast_dtm(opinion_id, word, n)
appeals.dtm <- data.frame(as.matrix(appeals.dtm))

# add opinion_id, year, and circuit into dtm. (each row is an opinion(belongs to one circuit), each column is one of the top 100 terms, )
all.opinions <- select(appeals.data, opinion_id, year, circuit)
accounted.opinions <- inner_join(all.opinions, appeals.tokens.count) %>%
  select(opinion_id, year, circuit) %>%
  group_by(opinion_id, year, circuit) %>% slice(1)

appeals.dtm$opinion_id <- accounted.opinions$opinion_id
appeals.dtm$circuit <- accounted.opinions$circuit

# some opinions were removed in the process; add those back in.
missing.opinions <- anti_join(all.opinions, data.frame(opinion_id=appeals.tokens.count$opinion_id))
appeals.dtm <- bind_rows(appeals.dtm, select(missing.opinions, opinion_id, circuit))

# split data into train and test.(randomly)
appeals.split <- split(appeals.dtm, sample(rep(c(1,2), nrow(appeals.dtm)/2 + 1), nrow(appeals.dtm)))
train.set <- appeals.split$`1`
test.set <- appeals.split$`2`

# clean up the variables
rm(accounted.opinions, all.opinions, appeals.tokens, appeals.tokens.count, custom_words, 
   missing.opinions, word.count, word.count.fifth, word.count.ninth, appeals.split)


## subpart c)
train_and_evaluate <- function(train.set, test.set) {
  logreg_model <- glm(circuit ~ ., family=binomial(link='logit'), data=train.set)
  test.predicted.probability <- predict(logreg_model, newdata = test.set, type='response')
  test.pred <- prediction(test.predicted.probability, test.set$circuit)
  test.perf <- performance(test.pred, "auc")
  cat('the auc score is ', 100*test.perf@y.values[[1]], "\n") 
  logreg_model
}

train_and_evaluate(train.set, test.set)

# Warning messages:
# 1: glm.fit: algorithm did not converge 
# 2: glm.fit: fitted probabilities numerically 0 or 1 occurred

### The data can be perfectly split because of the 'opinion_id' variable.
### As a result, there's no unique solution to the model, and the algorithm does not converge.
### Even in the presence of the convergence issue, the test AUC is extremely high at 99.6% because `opinion_id` is a perfect predictor.
### Looking at the fitted model, most coefficients are extremely large.
### Additionally, other than 'opinion_id' and the intercept, all other predictors have a p-value of near 1.

## subpart d)
train.set.rm.id <- subset(train.set, select = -c(opinion_id))
logreg_model <- train_and_evaluate(train.set.rm.id, test.set)

# assume 'largest' and 'smallest' refer to the magnitude of the coefficients.
sort(abs(coef(logreg_model)))

### The five words with the largest coefficients are: disposition, memorandum, precedent, california, provide
### The five words with the smallest coefficients are: argument, plaintiffs, evidence, process, discretion

### 'disposition' is a word that liberals like to use, so it makes sense that it is strongly (negatively) correlated with the ninth circuit court.

# clean up the variables
rm(appeals.tokens, dataset.split, logreg_model, test.set, train.set, train.set.rm.id, word.count)

## C) Bigrams
appeals.tokens <- appeals.data %>% 
  unnest_tokens(bigram, text, token="ngrams", n=2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word)

appeals.tokens$word <- with(appeals.tokens, paste0(word1, " ", word2))
appeals.tokens <- select(appeals.tokens, -c(word1, word2))

word.count <- count_most_freq_tokens(appeals.tokens)

dataset.split <- create_dtm_split(appeals.tokens, word.count, tf.idf=FALSE)
train.set <- dataset.split$`1`
test.set <- dataset.split$`2`

train.set.rm.id <- subset(train.set, select = -c(opinion_id))
logreg_model <- train_and_evaluate(train.set.rm.id, test.set) # AUC is 93.91556

# assume 'largest' and 'smallest' refer to the magnitude of the coefficients.
sort(abs(coef(logreg_model)))

### The five words with the largest coefficients are: unanimously.concludes, panel.unanimously, california.law, jurisdiction.pursuant, plea.conviction
### The five words with the smallest coefficients are: due.process, bad.faith, death.penalty, substantial.evidence, asylum.withholding

### 'unanimously.concludes' very negatively correlates with being in the fifth court, so I suppose
### the ninth court has much higher chances of the jury unanimously agreeing on the result.

# clean up the variables except appeals.tokens and word.count for part D).
rm(dataset.split, logreg_model, test.set, train.set, train.set.rm.id)

## D)

dataset.split <- create_dtm_split(appeals.tokens, word.count, tf.idf=TRUE)
train.set <- dataset.split$`1`
test.set <- dataset.split$`2`

train.set.rm.id <- subset(train.set, select = -c(opinion_id))
logreg_model <- train_and_evaluate(train.set.rm.id, test.set) # AUC is slightly higher at 94.35309

# assume 'largest' and 'smallest' refer to the magnitude of the coefficients.
sort(abs(coef(logreg_model)))

### The five words with the largest coefficients are: unanimously.concludes, panel.unanimously, california.law, texas.law, jurisdiction.pursuant
### The five words with the smallest coefficients are: cross.examination, summary.judgement, prima.facie, guilty.plea, immigration.appeals

### The top predictor is the same as in part C) when using term frequencies.
### 'unanimously.concludes' very negatively correlates with being in the fifth court, so I suppose
### the ninth court has much higher chances of the jury unanimously agreeing on the result.

# clean up the variables except appeals.tokens and word.count for part D).
rm(appeals.tokens, dataset.split, logreg_model, test.set, train.set, train.set.rm.id, word.count)

## E)

### Since tf-idf uses a weighting that depends on the full corpus, we should compute these weights
### on the training dataset and keep track of them as parameters of the model.
### In order to test on a new sample, we would then compute their tf-idf values based on these weights
### prior to feeding them into the classifier.
### This is similar to how normalization would work for real valued predictors.

## F)

appeals.tokens.supreme <- appeals.data %>% 
  unnest_tokens(bigram, text, token="ngrams", n=3) %>% 
  separate(bigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  filter(word1 == "supreme" | word2 == "supreme" | word3 == "supreme")
appeals.tokens.supreme$word <- with(appeals.tokens.supreme, paste0(word1, " ", word2, " ", word3))
appeals.tokens.supreme <- select(appeals.tokens.supreme, -c(word1, word2, word3))

top.supreme.trigrams.circuit5 <- appeals.tokens.supreme %>% filter(circuit == 'fifth') %>% count(word, sort = TRUE) 
top.supreme.trigrams.circuit5[1:10,]
#  A tibble: 10 x 2
# word                            n
# <chr>                       <int>
# 1 mississippi supreme court     370
# 2 louisiana supreme court       305
# 3 supreme court held            294
# 4 supreme court's decision      294
# 5 texas supreme court           286
# 6 supreme court precedent       143
# 7 supreme court decision        112
# 8 supreme court denied           85
# 9 mississippi supreme court's    64
# 10 supreme court explained       55

top.supreme.trigrams.circuit9 <- appeals.tokens.supreme %>% filter(circuit == 'ninth') %>% count(word, sort = TRUE) 
top.supreme.trigrams.circuit9[1:10,]

# # A tibble: 10 x 2
# word                           n
# <chr>                      <int>
# 1 california supreme court     762
# 2 supreme court held           359
# 3 supreme court's decision     307
# 4 supreme court precedent      293
# 5 arizona supreme court        279
# 6 nevada supreme court         244
# 7 established supreme court    187
# 8 supreme court denied         132
# 9 supreme court law            126
# 10 california supreme court's  122




