## Final version of appeals_analysis
# Purpose: A Characterizing Appeals Courts
# Date: November 2018
# Team: 
library(magrittr)
library(tibble)
library(tidytext)
library(dplyr)
library(tidyr)
library(ROCR)
# A)
# Importing the data in the file recent_opinions.tsv as a tibble called ‘appeals.data’. 
appeals.data <- read.delim("recent_opinions.tsv")
appeals.data$text <- as.character(appeals.data$text)
# Adding an ‘opinion_id’ column to appeals.data, which gives each row a unique id. 
appeals.data <- rowid_to_column(appeals.data, "opinion_id")
# Loading tidytext’s stop_words tibble.
data("stop_words")
# Adding the words in custom_words.txt to it to create a custom dictionary of stop words (you can set the lexicon as “custom” for these words).
custom_words <- read.table("custom_words.txt", col.names="word")
custom_words <- data.frame(word=custom_words, lexicon='custom')
stop_words <- rbind(custom_words, stop_words)
stop_words$word <- as.character(stop_words$word)

# B)

## subpart a)
appeals.tokens <- appeals.data %>% unnest_tokens(word, text) %>% anti_join(stop_words)
word.count <- appeals.tokens %>% count(word, sort = TRUE) 
word.count.fifth <- appeals.tokens %>% filter(circuit=='fifth') %>% count(word, sort=TRUE)
word.count.ninth <- appeals.tokens %>% filter(circuit=='ninth') %>% count(word, sort=TRUE)

paste0('Most frequent words in the ENTIRE corpus are: ', paste0(word.count[1:10,]$word, collapse=', '))
paste0('Most frequent words in the FIFTH circuit are: ', paste0(word.count.fifth[1:10,]$word, collapse=', '))
paste0('Most frequent words in the NINTH circuit are: ', paste0(word.count.ninth[1:10,]$word, collapse=', '))

## subpart b)
# retain only top 100 most frequent words.
most_freq_words <- word.count[1:100,]$word
appeals.tokens <- appeals.tokens %>% filter(word %in% most_freq_words)

# count number of occurances of each word per opinion_id.
appeals.tokens.count <- appeals.tokens %>% group_by(opinion_id) %>% count(word)
appeals.dtm <- appeals.tokens.count %>% cast_dtm(opinion_id, word, n)
appeals.dtm <- data.frame(as.matrix(appeals.dtm))

# add opinion_id, year, and circuit into dtm.
all.opinions <- select(appeals.data, opinion_id, year, circuit)
accounted.opinions <- inner_join(all.opinions, appeals.tokens.count) %>%
  select(opinion_id, year, circuit) %>%
  group_by(opinion_id, year, circuit) %>% slice(1)

appeals.dtm$opinion_id <- accounted.opinions$opinion_id
appeals.dtm$circuit <- accounted.opinions$circuit

# some opinions were removed in the process; add those back in.
missing.opinions <- anti_join(all.opinions, data.frame(opinion_id=appeals.tokens.count$opinion_id))
appeals.dtm <- bind_rows(appeals.dtm, select(missing.opinions, opinion_id, circuit))

# split data into train and test.
appeals.split <- split(appeals.dtm, sample(rep(1:2, nrow(appeals.dtm)/2 + 1), nrow(appeals.dtm)))
train.set <- appeals.split$`1`
test.set <- appeals.split$`2`

# clean up the variables
rm(accounted.opinions, all.opinions, appeals.tokens, appeals.tokens.count, custom_words, 
   missing.opinions, word.count, word.count.fifth, word.count.ninth, appeals.split)

## part c)
linreg_model <- glm(circuit ~., family=binomial(link='logit'), data=train.set)

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

## C)
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

