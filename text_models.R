getwd()
library(readxl)
library(dplyr)
library(tidytext)
library(ggplot2)
#### Reading files and preprocessing ---- 
# fit files
fit_unigram <- read.csv("Fit-word.csv", stringsAsFactors = F)
fit_bigram <- read.csv("Fit-bigram.csv", stringsAsFactors = F)
fit_trigram <- read.csv("Fit-trigram.csv", stringsAsFactors = F)

# price files
price_unigram <- read.csv("Price-word.csv", stringsAsFactors = F)
price_bigram <- read.csv("Price-bigram.csv", stringsAsFactors = F)
price_trigram <- read.csv("Price-trigram.csv", stringsAsFactors = F)

# quality files
quality_unigram <- read.csv("Quality-word.csv", stringsAsFactors = F)
quality_bigram <- read.csv("Quality-bigram.csv", stringsAsFactors = F)
quality_trigram <- read.csv("Quality-trigram.csv", stringsAsFactors = F)

# vendor files
vendor_unigram <- read.csv("Vendor-word2.csv", stringsAsFactors = F)
vendor_bigram <- read.csv("Vendor-bigram.csv", stringsAsFactors = F)
vendor_trigram <- read.csv("Vendor-trigram.csv", stringsAsFactors = F)

# Reading the training set
training_set <- read_excel("Replacement parts training set conflict-resolved reviews_CORRECT.xlsx")

###########################################################################################
#### converting the df text ----
system.time(txt <- unique(training_set$Text))
txt

system.time(txt_df_original <- data_frame(line=1:2262, txt=txt))
txt_df_original

# Split a column into tokens using the tokenizers package, splitting the table 
# into one-token-per-row. 
system.time(
  txt_df <- txt_df_original %>%
    unnest_tokens(output = word, input = txt)
)
# remove stop words from dataset
txt_df <- txt_df %>%
  anti_join(stop_words)

# most common words
eda_txt_df <- txt_df %>%
  count(word, sort = TRUE) 

# visualize words
ggplot(eda_txt_df %>% filter(n > 100), aes(word, n)) + 
  geom_col() +
  xlab(NULL) +
  coord_flip()

# get lexicons
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

# preparing bi-gram and tri-gram words from uni-grams
system.time(
  one_gram <- txt_df %>%
    unnest_tokens(output = one_gram, token = "ngrams", input = word, n = 1)
) 

system.time(
  two_gram <- txt_df %>%
    unnest_tokens(output = two_gram, token = "ngrams", input = word, n = 2)
) 

system.time(
  three_gram <- txt_df %>%
    unnest_tokens(output = three_gram, token = "ngrams", input = word, n = 3)
) 

# getting the word tags and scores - uni_gram and all tags
one_gram$fit <- fit_unigram$Wrong.Size[match(one_gram$one_gram, fit_unigram$Word)]
one_gram$price <- price_unigram$Overpriced[match(one_gram$one_gram, price_unigram$Word)]
one_gram$quality <- quality_unigram$Unsatisfied.With.Quality[match(one_gram$one_gram, quality_unigram$Word)]
one_gram$vendor <- vendor_unigram$Non.satisfaction.With.Vendor[match(one_gram$one_gram, vendor_unigram$Word)]

one <- one_gram %>%
  filter(!is.na(fit)) %>%
  group_by(line) %>%
  summarise(fit_sum = sum(fit),
            price_sum = sum(price),
            quality_sum = sum(quality),
            vendor_sum = sum(vendor))

sum(is.na(one))

sum(is.na(one_gram$fit))
sum(is.na(one_gram$price))
sum(is.na(one_gram$quality))
sum(is.na(one_gram$vendor))

# getting the word tags and scores - bi_gram and all tags
two_gram$fit <- fit_bigram$Wrong.Size[match(two_gram$two_gram, fit_bigram$Bigram)]
two_gram$price <- price_bigram$Overpriced[match(two_gram$two_gram, price_bigram$Bigram)]
two_gram$quality <- quality_bigram$Unsatisfied.With.Quality[match(two_gram$two_gram, quality_bigram$Bigram)]
two_gram$vendor <- vendor_bigram$Non.satisfaction.With.Vendor[match(two_gram$two_gram, vendor_bigram$Bigram)]

two <- two_gram %>%
  filter(!is.na(fit)) %>%
  group_by(line) %>%
  summarise(fit_sum = sum(fit),
            price_sum = sum(price),
            quality_sum = sum(quality),
            vendor_sum = sum(vendor))

sum(is.na(two))

sum(is.na(two_gram$fit))
sum(is.na(two_gram$price))
sum(is.na(two_gram$quality))
sum(is.na(two_gram$vendor))

# getting the word tags and scores - tri_gram and all tags
three_gram$fit <- fit_trigram$Wrong.Size[match(three_gram$three_gram, fit_trigram$Trigram)]
three_gram$price <- price_trigram$Overpriced[match(three_gram$three_gram, price_trigram$Trigram)]
three_gram$quality <- quality_trigram$Unsatisfied.With.Quality[match(three_gram$three_gram, quality_trigram$Trigram)]
three_gram$vendor <- vendor_trigram$Non.satisfaction.With.Vendor[match(three_gram$three_gram, vendor_trigram$Trigram)]

three_gram$price <- as.numeric(three_gram$price)

three <- three_gram %>%
  filter(!is.na(fit)) %>%
  group_by(line) %>%
  summarise(fit_sum = sum(fit),
            price_sum = sum(price),
            quality_sum = sum(quality),
            vendor_sum = sum(vendor))

sum(is.na(three))

sum(is.na(three_gram$fit))
sum(is.na(three_gram$price))
sum(is.na(three_gram$quality))
sum(is.na(three_gram$vendor))

# coercing the tri-gram scores of each category back to text_df
txt_df <- merge(txt_df, three, by = 'line')

# Data Visualization in Textual Analysis
## 1. word_frequency - uni_gram
ggplot(eda_txt_df %>% 
         filter(n > 200, 
                word != 'quot'), aes(word, n)) + 
  geom_col(fill = "deepskyblue4") +
  xlab(NULL) +
  coord_flip()

## 2. word_frequency - bi_gram
eda_txt_two <- two_gram %>%
  count(two_gram, sort=T)
ggplot(eda_txt_two %>% 
         filter(n > 20, 
                two_gram != 'quot'), aes(two_gram, n)) + 
  geom_col(fill = "deepskyblue4") +
  xlab(NULL) +
  coord_flip()

## 3. word_frequency - tri_gram
eda_txt_three <- three_gram %>%
  filter(!is.na(three_gram)) %>%
  count(three_gram, sort=T)
ggplot(eda_txt_three %>% 
         filter(n > 5, 
                three_gram != 'quot'), aes(three_gram, n)) + 
  geom_col(fill = "deepskyblue4") +
  xlab(NULL) +
  coord_flip()

# save.image("./text_models.RData")

#### User-defined function for categorization and sentiment analyses score ----
#### Preproc_txt ----
Preproc_txt<-function(x)
{
  # converting the df text
  txt <- unique(x$Text)
  txt_df <- data_frame(line=1:length(txt), txt=txt)
  
  # Split a column into tokens using the tokenizers package, splitting the table 
  # into one-token-per-row. 
  
  txt_df <- txt_df %>%
    unnest_tokens(output = word, input = txt)
  
  # remove stop words from dataset
  txt_df <- txt_df %>%
    anti_join(stop_words)
  
  return(txt_df)
}
#### AutoPred ----
AutoPred<-function(txt_df)
{
  # preparing bi-gram and tri-gram words from uni-grams
  one_gram <- txt_df %>%
    unnest_tokens(output = one_gram, token = "ngrams", input = word, n = 1)
  
  two_gram <- txt_df %>%
    unnest_tokens(output = two_gram, token = "ngrams", input = word, n = 2)
  
  three_gram <- txt_df %>%
    unnest_tokens(output = three_gram, token = "ngrams", input = word, n = 3)
  
  # getting the word tags and scores - uni_gram and all tags
  one_gram$fit <- fit_unigram$Wrong.Size[match(one_gram$one_gram, fit_unigram$Word)]
  one_gram$price <- price_unigram$Overpriced[match(one_gram$one_gram, price_unigram$Word)]
  one_gram$quality <- quality_unigram$Unsatisfied.With.Quality[match(one_gram$one_gram, quality_unigram$Word)]
  one_gram$vendor <- vendor_unigram$Non.satisfaction.With.Vendor[match(one_gram$one_gram, vendor_unigram$Word)]
  
  one <- one_gram %>%
    filter(!is.na(fit)) %>%
    group_by(line) %>%
    summarise(fit_sum = sum(fit),
              price_sum = sum(price),
              quality_sum = sum(quality),
              vendor_sum = sum(vendor))
  
  # getting the word tags and scores - bi_gram and all tags
  two_gram$fit <- fit_bigram$Wrong.Size[match(two_gram$two_gram, fit_bigram$Bigram)]
  two_gram$price <- price_bigram$Overpriced[match(two_gram$two_gram, price_bigram$Bigram)]
  two_gram$quality <- quality_bigram$Unsatisfied.With.Quality[match(two_gram$two_gram, quality_bigram$Bigram)]
  two_gram$vendor <- vendor_bigram$Non.satisfaction.With.Vendor[match(two_gram$two_gram, vendor_bigram$Bigram)]
  
  two <- two_gram %>%
    filter(!is.na(fit)) %>%
    group_by(line) %>%
    summarise(fit_sum = sum(fit),
              price_sum = sum(price),
              quality_sum = sum(quality),
              vendor_sum = sum(vendor))
  
  # getting the word tags and scores - tri_gram and all tags
  three_gram$fit <- fit_trigram$Wrong.Size[match(three_gram$three_gram, fit_trigram$Trigram)]
  three_gram$price <- price_trigram$Overpriced[match(three_gram$three_gram, price_trigram$Trigram)]
  three_gram$quality <- quality_trigram$Unsatisfied.With.Quality[match(three_gram$three_gram, quality_trigram$Trigram)]
  three_gram$vendor <- vendor_trigram$Non.satisfaction.With.Vendor[match(three_gram$three_gram, vendor_trigram$Trigram)]
  three_gram$price <- as.numeric(three_gram$price)
  
  three <- three_gram %>%
    filter(!is.na(fit)) %>%
    group_by(line) %>%
    summarise(fit_sum = sum(fit),
              price_sum = sum(price),
              quality_sum = sum(quality),
              vendor_sum = sum(vendor))
  
  # coercing the tri-gram scores of each category back to text_df
  txt_df <- merge(txt_df, three, by = 'line')
  
  
  data<-merge(one,two,by.x = 'line',by.y = 'line',all.x=TRUE)
  names(data)<-c('line','fit_1','price_1','quality_1','vendor_1','fit_2',
                 'price_2','quality_2','vendor_2')
  combined<-merge(data,three,by.x = 'line',by.y = 'line',all.x=TRUE)
  names(combined)<-c('line','fit_1','price_1','quality_1','vendor_1','fit_2',
                     'price_2','quality_2','vendor_2',
                     'fit_3','price_3','quality_3','vendor_3')
  
  
  combined$fit<-ifelse(combined$fit_3==0|is.na(combined$fit_3),
                       ifelse(combined$fit_2==0|is.na(combined$fit_2),
                              combined$fit_1,combined$fit_2),combined$fit_3)
  combined$price<-ifelse(combined$price_3==0|is.na(combined$price_3),
                         ifelse(combined$price_2==0|is.na(combined$price_2),
                                combined$price_1,combined$price_2),combined$price_3)
  combined$vendor<-ifelse(combined$vendor_3==0|is.na(combined$vendor_3),
                          ifelse(combined$vendor_2==0|is.na(combined$vendor_2),
                                 combined$vendor_1,combined$vendor_2),combined$vendor_3)
  combined$quality<-ifelse(combined$quality_3==0|is.na(combined$quality_3),
                           ifelse(combined$quality_2==0|is.na(combined$quality_2),
                                  combined$quality_1,combined$quality_2),combined$quality_3)
  data<-combined[,c('line','fit','price','quality','vendor')]
  
  
  return(data)
}
#### Flag Function ----
flagset = function(data)
{
  data$flag<-"NC"
  data%>%mutate(flag=ifelse((fit>vendor)&(fit>quality)&(fit>price),"F",
                            ifelse((vendor>fit)&(vendor>quality)&(vendor>price),"V",
                                   ifelse((quality>fit)&(quality>vendor)&(quality>price),"Q",
                                          ifelse((price>fit)&(price>vendor)&(price>quality),"P","NC")))))
}
##### Output Function ----
output<-flagset(AutoPred(Preproc_txt(training_set)))

txt_df_original <- merge(txt_df_original, output, by = c('line'))

#### Visualizations for each category ----
#### FIT ----
fit <- txt_df_original %>%
  filter(flag == "F")

#### FIT Valueboxes ----
fit_text_count <- fit %>%
  group_by(flag) %>%
  summarise(fit_sentiment_count = length(flag)) %>%
  select(fit_sentiment_count) %>%
  ungroup()

fit_text_mean <- fit %>%
  group_by(flag) %>%
  summarise(fit_sentiment_mean = mean(fit)) %>%
  select(fit_sentiment_mean) %>%
  ungroup()

fit_text_max <- fit %>%
  group_by(flag) %>%
  summarise(fit_sentiment_max = max(fit)) %>%
  select(fit_sentiment_max) %>%
  ungroup()

fit_text_min <- fit %>%
  group_by(flag) %>%
  summarise(fit_sentiment_min = min(fit)) %>%
  select(fit_sentiment_min) %>%
  ungroup()

#### FIT Wordclouds ----
fit_wc_uni <- fit[,1:2] %>%
  unnest_tokens(output = word, input = txt) %>%
  anti_join(stop_words) %>%
  count(word, sort=T) %>%
  filter(word != 'quot')

wordcloud(words = fit_wc_uni$word, 
          freq = fit_wc_uni$n,
          min.freq = 1,
          max.words = 200,
          random.order = FALSE,
          rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

fit_wc_bi <- fit[,1:2] %>%
  unnest_tokens(output = word, token = "ngrams", input = txt, n = 2) %>%
  anti_join(stop_words) %>%
  count(word, sort=T) %>%
  filter(word != 'quot')

wordcloud(words = fit_wc_bi$word, 
          freq = fit_wc_bi$n,
          min.freq = 1,
          max.words = 200,
          random.order = FALSE,
          rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

fit_wc_tri <- fit[,1:2] %>%
  unnest_tokens(output = word, token = "ngrams", input = txt, n = 3) %>%
  anti_join(stop_words) %>%
  count(word, sort=T) %>%
  filter(word != 'quot')

wordcloud(words = fit_wc_tri$word, 
          freq = fit_wc_tri$n,
          min.freq = 1,
          max.words = 200,
          random.order = FALSE,
          rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

#### FIT freq_counts ----
ggplot(fit_wc_uni %>% filter(n > 50), aes(word, n)) + 
  geom_col() +
  xlab(NULL) +
  coord_flip()

ggplot(fit_wc_bi %>% filter(n > 50), aes(word, n)) + 
  geom_col() +
  xlab(NULL) +
  coord_flip()

ggplot(fit_wc_tri %>% filter(n > 8), aes(word, n)) + 
  geom_col() +
  xlab(NULL) +
  coord_flip()

#### PRICE ----
price <- txt_df_original %>%
  filter(flag == "P")

#### price Valueboxes ----
price_text_count <- price %>%
  group_by(flag) %>%
  summarise(price_sentiment_count = length(flag)) %>%
  select(price_sentiment_count) %>%
  ungroup()

price_text_mean <- price %>%
  group_by(flag) %>%
  summarise(price_sentiment_mean = mean(price)) %>%
  select(price_sentiment_mean) %>%
  ungroup()

price_text_max <- price %>%
  group_by(flag) %>%
  summarise(price_sentiment_max = max(price)) %>%
  select(price_sentiment_max) %>%
  ungroup()

price_text_min <- price %>%
  group_by(flag) %>%
  summarise(price_sentiment_min = min(price)) %>%
  select(price_sentiment_min) %>%
  ungroup()

#### price Wordclouds ----
price_wc_uni <- price[,1:2] %>%
  unnest_tokens(output = word, input = txt) %>%
  anti_join(stop_words) %>%
  count(word, sort=T) %>%
  filter(word != 'quot')

wordcloud(words = price_wc_uni$word, 
          freq = price_wc_uni$n,
          min.freq = 1,
          max.words = 200,
          random.order = FALSE,
          rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

price_wc_bi <- price[,1:2] %>%
  unnest_tokens(output = word, token = "ngrams", input = txt, n = 2) %>%
  anti_join(stop_words) %>%
  count(word, sort=T) %>%
  filter(word != 'quot')

wordcloud(words = price_wc_bi$word, 
          freq = price_wc_bi$n,
          min.freq = 1,
          max.words = 200,
          random.order = FALSE,
          rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

price_wc_tri <- price[,1:2] %>%
  unnest_tokens(output = word, token = "ngrams", input = txt, n = 3) %>%
  anti_join(stop_words) %>%
  count(word, sort=T) %>%
  filter(word != 'quot')

wordcloud(words = price_wc_tri$word, 
          freq = price_wc_tri$n,
          min.freq = 1,
          max.words = 200,
          random.order = FALSE,
          rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

#### price freq_counts ----
ggplot(price_wc_uni %>% filter(n > 50), aes(word, n)) + 
  geom_col() +
  xlab(NULL) +
  coord_flip()

ggplot(price_wc_bi %>% filter(n > 50), aes(word, n)) + 
  geom_col() +
  xlab(NULL) +
  coord_flip()

ggplot(price_wc_tri %>% filter(n > 8), aes(word, n)) + 
  geom_col() +
  xlab(NULL) +
  coord_flip()

#### QUALITY ----
quality <- txt_df_original %>%
  filter(flag == "Q")

#### quality Valueboxes ----
quality_text_count <- quality %>%
  group_by(flag) %>%
  summarise(quality_sentiment_count = length(flag)) %>%
  select(quality_sentiment_count) %>%
  ungroup()

quality_text_mean <- quality %>%
  group_by(flag) %>%
  summarise(quality_sentiment_mean = mean(quality)) %>%
  select(quality_sentiment_mean) %>%
  ungroup()

quality_text_max <- quality %>%
  group_by(flag) %>%
  summarise(quality_sentiment_max = max(quality)) %>%
  select(quality_sentiment_max) %>%
  ungroup()

quality_text_min <- quality %>%
  group_by(flag) %>%
  summarise(quality_sentiment_min = min(quality)) %>%
  select(quality_sentiment_min) %>%
  ungroup()

#### quality Wordclouds ----
quality_wc_uni <- quality[,1:2] %>%
  unnest_tokens(output = word, input = txt) %>%
  anti_join(stop_words) %>%
  count(word, sort=T) %>%
  filter(word != 'quot')

wordcloud(words = quality_wc_uni$word, 
          freq = quality_wc_uni$n,
          min.freq = 1,
          max.words = 200,
          random.order = FALSE,
          rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

quality_wc_bi <- quality[,1:2] %>%
  unnest_tokens(output = word, token = "ngrams", input = txt, n = 2) %>%
  anti_join(stop_words) %>%
  count(word, sort=T) %>%
  filter(word != 'quot')

wordcloud(words = quality_wc_bi$word, 
          freq = quality_wc_bi$n,
          min.freq = 1,
          max.words = 200,
          random.order = FALSE,
          rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

quality_wc_tri <- quality[,1:2] %>%
  unnest_tokens(output = word, token = "ngrams", input = txt, n = 3) %>%
  anti_join(stop_words) %>%
  count(word, sort=T) %>%
  filter(word != 'quot')

wordcloud(words = quality_wc_tri$word, 
          freq = quality_wc_tri$n,
          min.freq = 1,
          max.words = 200,
          random.order = FALSE,
          rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

#### quality freq_counts ----
ggplot(quality_wc_uni %>% filter(n > 50), aes(word, n)) + 
  geom_col() +
  xlab(NULL) +
  coord_flip()

ggplot(quality_wc_bi %>% filter(n > 50), aes(word, n)) + 
  geom_col() +
  xlab(NULL) +
  coord_flip()

ggplot(quality_wc_tri %>% filter(n > 8), aes(word, n)) + 
  geom_col() +
  xlab(NULL) +
  coord_flip()







#### VENDOR ----
vendor <- txt_df_original %>%
  filter(flag == "V")

#### vendor Valueboxes ----
vendor_text_count <- vendor %>%
  group_by(flag) %>%
  summarise(vendor_sentiment_count = length(flag)) %>%
  select(vendor_sentiment_count) %>%
  ungroup()

vendor_text_mean <- vendor %>%
  group_by(flag) %>%
  summarise(vendor_sentiment_mean = mean(vendor)) %>%
  select(vendor_sentiment_mean) %>%
  ungroup()

vendor_text_max <- vendor %>%
  group_by(flag) %>%
  summarise(vendor_sentiment_max = max(vendor)) %>%
  select(vendor_sentiment_max) %>%
  ungroup()

vendor_text_min <- vendor %>%
  group_by(flag) %>%
  summarise(vendor_sentiment_min = min(vendor)) %>%
  select(vendor_sentiment_min) %>%
  ungroup()

#### vendor Wordclouds ----
vendor_wc_uni <- vendor[,1:2] %>%
  unnest_tokens(output = word, input = txt) %>%
  anti_join(stop_words) %>%
  count(word, sort=T) %>%
  filter(word != 'quot')

wordcloud(words = vendor_wc_uni$word, 
          freq = vendor_wc_uni$n,
          min.freq = 1,
          max.words = 200,
          random.order = FALSE,
          rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

vendor_wc_bi <- vendor[,1:2] %>%
  unnest_tokens(output = word, token = "ngrams", input = txt, n = 2) %>%
  anti_join(stop_words) %>%
  count(word, sort=T) %>%
  filter(word != 'quot')

wordcloud(words = vendor_wc_bi$word, 
          freq = vendor_wc_bi$n,
          min.freq = 1,
          max.words = 200,
          random.order = FALSE,
          rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

vendor_wc_tri <- vendor[,1:2] %>%
  unnest_tokens(output = word, token = "ngrams", input = txt, n = 3) %>%
  anti_join(stop_words) %>%
  count(word, sort=T) %>%
  filter(word != 'quot')

wordcloud(words = vendor_wc_tri$word, 
          freq = vendor_wc_tri$n,
          min.freq = 1,
          max.words = 200,
          random.order = FALSE,
          rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

#### vendor freq_counts ----
ggplot(vendor_wc_uni %>% filter(n > 50), aes(word, n)) + 
  geom_col() +
  xlab(NULL) +
  coord_flip()

ggplot(vendor_wc_bi %>% filter(n > 50), aes(word, n)) + 
  geom_col() +
  xlab(NULL) +
  coord_flip()

ggplot(vendor_wc_tri %>% filter(n > 8), aes(word, n)) + 
  geom_col() +
  xlab(NULL) +
  coord_flip()





