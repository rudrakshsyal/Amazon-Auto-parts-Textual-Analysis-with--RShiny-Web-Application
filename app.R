getwd()
library(readxl)
library(dplyr)
library(tidytext)
library(ggplot2)
library(SnowballC)
library(wordcloud)
library(tm)
library(shiny)
library(memoise)
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
# txt

system.time(txt_df_original <- data_frame(line=1:2262, txt=txt))
# txt_df_original

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

# wordcloud(words = fit_wc_uni$word, 
#           freq = fit_wc_uni$n,
#           min.freq = 1,
#           max.words = 200,
#           random.order = FALSE,
#           rot.per = 0.35,
#           colors = brewer.pal(8, "Dark2"))

fit_wc_bi <- fit[,1:2] %>%
  unnest_tokens(output = word, token = "ngrams", input = txt, n = 2) %>%
  anti_join(stop_words) %>%
  count(word, sort=T) %>%
  filter(word != 'quot')

# wordcloud(words = fit_wc_bi$word, 
#           freq = fit_wc_bi$n,
#           min.freq = 1,
#           max.words = 200,
#           random.order = FALSE,
#           rot.per = 0.35,
#           colors = brewer.pal(8, "Dark2"))

fit_wc_tri <- fit[,1:2] %>%
  unnest_tokens(output = word, token = "ngrams", input = txt, n = 3) %>%
  anti_join(stop_words) %>%
  count(word, sort=T) %>%
  filter(word != 'quot')

# wordcloud(words = fit_wc_tri$word, 
#           freq = fit_wc_tri$n,
#           min.freq = 1,
#           max.words = 200,
#           random.order = FALSE,
#           rot.per = 0.35,
#           colors = brewer.pal(8, "Dark2"))

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

# wordcloud(words = price_wc_uni$word, 
#           freq = price_wc_uni$n,
#           min.freq = 1,
#           max.words = 200,
#           random.order = FALSE,
#           rot.per = 0.35,
#           colors = brewer.pal(8, "Dark2"))

price_wc_bi <- price[,1:2] %>%
  unnest_tokens(output = word, token = "ngrams", input = txt, n = 2) %>%
  anti_join(stop_words) %>%
  count(word, sort=T) %>%
  filter(word != 'quot')

# wordcloud(words = price_wc_bi$word, 
#           freq = price_wc_bi$n,
#           min.freq = 1,
#           max.words = 200,
#           random.order = FALSE,
#           rot.per = 0.35,
#           colors = brewer.pal(8, "Dark2"))

price_wc_tri <- price[,1:2] %>%
  unnest_tokens(output = word, token = "ngrams", input = txt, n = 3) %>%
  anti_join(stop_words) %>%
  count(word, sort=T) %>%
  filter(word != 'quot')

# wordcloud(words = price_wc_tri$word, 
#           freq = price_wc_tri$n,
#           min.freq = 1,
#           max.words = 200,
#           random.order = FALSE,
#           rot.per = 0.35,
#           colors = brewer.pal(8, "Dark2"))

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

# wordcloud(words = quality_wc_uni$word, 
#           freq = quality_wc_uni$n,
#           min.freq = 1,
#           max.words = 200,
#           random.order = FALSE,
#           rot.per = 0.35,
#           colors = brewer.pal(8, "Dark2"))

quality_wc_bi <- quality[,1:2] %>%
  unnest_tokens(output = word, token = "ngrams", input = txt, n = 2) %>%
  anti_join(stop_words) %>%
  count(word, sort=T) %>%
  filter(word != 'quot')

# wordcloud(words = quality_wc_bi$word, 
#           freq = quality_wc_bi$n,
#           min.freq = 1,
#           max.words = 200,
#           random.order = FALSE,
#           rot.per = 0.35,
#           colors = brewer.pal(8, "Dark2"))

quality_wc_tri <- quality[,1:2] %>%
  unnest_tokens(output = word, token = "ngrams", input = txt, n = 3) %>%
  anti_join(stop_words) %>%
  count(word, sort=T) %>%
  filter(word != 'quot')

# wordcloud(words = quality_wc_tri$word, 
#           freq = quality_wc_tri$n,
#           min.freq = 1,
#           max.words = 200,
#           random.order = FALSE,
#           rot.per = 0.35,
#           colors = brewer.pal(8, "Dark2"))

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

# wordcloud(words = vendor_wc_uni$word, 
#           freq = vendor_wc_uni$n,
#           min.freq = 1,
#           max.words = 200,
#           random.order = FALSE,
#           rot.per = 0.35,
#           colors = brewer.pal(8, "Dark2"))

vendor_wc_bi <- vendor[,1:2] %>%
  unnest_tokens(output = word, token = "ngrams", input = txt, n = 2) %>%
  anti_join(stop_words) %>%
  count(word, sort=T) %>%
  filter(word != 'quot')

# wordcloud(words = vendor_wc_bi$word, 
#           freq = vendor_wc_bi$n,
#           min.freq = 1,
#           max.words = 200,
#           random.order = FALSE,
#           rot.per = 0.35,
#           colors = brewer.pal(8, "Dark2"))

vendor_wc_tri <- vendor[,1:2] %>%
  unnest_tokens(output = word, token = "ngrams", input = txt, n = 3) %>%
  anti_join(stop_words) %>%
  count(word, sort=T) %>%
  filter(word != 'quot')

# wordcloud(words = vendor_wc_tri$word, 
#           freq = vendor_wc_tri$n,
#           min.freq = 1,
#           max.words = 200,
#           random.order = FALSE,
#           rot.per = 0.35,
#           colors = brewer.pal(8, "Dark2"))

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

## 1. Load Libraries ----
library(htmltools)
library(lubridate)
library(dplyr)
library(reshape)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(zoo)
library(reshape2)
library(Hmisc)
library(DT)
library(extrafont)
# font_import()
# loadfonts(device = "win")
library(ggthemes)
library(htmlwidgets)
# update.packages(ask = FALSE)
# install.packages(
#   c("DT", "shiny")
#   type = "source",
#   repos = c("http://yihui.name/xran", "http://cran.rstudio.com")
# )
library(gtable)
library(grid)
library(plotly)
# library(sunburstR)
# devtools::install_github("carlganz/rintrojs")
library(rintrojs)
library(shinyWidgets)
library(data.table)
# install.packages("shinyjs")
library(shinyjs)
# devtools::install_github("yang-tang/shinyjqui")
# library(shinyjqui)


## 2. Reading the Process Folder files ----
ui <- dashboardPage(
  title = "Assortment Decision using Textual Analytics",
  skin = "black",
  
  dashboardHeader(
    #### Dropdown menu for messages ----
    dropdownMenu(type = "messages", badgeStatus = "success",
                 messageItem("FIT Reviews Team",
                             "Fit Reviews had high negative sentiment this week.",
                             time = "5 mins"
                 ),
                 messageItem("PRICE Reviews Team",
                             "Price Reviews had positive sentiment last week.",
                             time = "2 hours"
                 ),
                 messageItem("QUALITY Reviews Team",
                             "Quality Reviews were neutral this week.",
                             time = "1 Week"
                 ),
                 messageItem("VENDOR Reviews Team",
                             "Vendor Reviews totaled 45 this month",
                             time = "1 month"
                 )
    ),
    
    #### Dropdown menu for notifications ----
    dropdownMenu(type = "notifications", badgeStatus = "warning",
                 notificationItem(icon = icon("users"), status = "info",
                                  "5 new positive FIT reviews today"
                 ),
                 notificationItem(icon = icon("warning"), status = "danger",
                                  "10 recent negative PRICE reviews!"
                 ),
                 notificationItem(icon = icon("shopping-cart", lib = "glyphicon"),
                                  status = "success", "no negative reviews in Quality"
                 ),
                 notificationItem(icon = icon("user", lib = "glyphicon"),
                                  status = "danger", "Negtive Price reviews above average!"
                 )
    ),
    
    title = " Assortment Planning Dashboard ",
    titleWidth = 350,
    disable = F
  ),
    ### Dashboard Sidebar ----
  dashboardSidebar(
    width = 250,
    disable = F,
    
    sidebarMenu(
      id = "tabs",
      
      menuItem("1. Summary Statistics", icon = icon("search-plus"), startExpanded = T, 
               menuSubItem(" a. uni_gram", tabName = "uni", icon = icon("users")),
               menuSubItem(" b. bi_gram", tabName = "bi", icon = icon("bitcoin")),
               menuSubItem(" c. tri_gram", tabName = "tri", icon = icon("bar-chart"))
      ),
      
      menuItem("2. Fit", tabName = "fit", badgeLabel = "NEW", badgeColor = "red", icon = icon("inr")),
      menuItem("3. Price", tabName = "price", badgeLabel = "NEW", badgeColor = "red", icon = icon("inr")),
      menuItem("4. Quality", tabName = "quality", badgeLabel = "NEW", badgeColor = "yellow", icon = icon("book")),
      menuItem("5. Vendor", tabName = "vendor", badgeLabel = "NEW", badgeColor = "green", icon = icon("user-o")),
      menuItem("6. User Input Stats", tabName = "user", badgeLabel = "NEW", badgeColor = "blue", icon = icon("user-o")),
      
      hr('Slider inputs for n-gram wordclouds'),
      
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 50, value = 5),
      
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 500,  value = 200)
    )
    
  ),
  dashboardBody(
    tabItems(
    ############################ 1. Summary Statistics ----
    tabItem(
      tabName = "uni",
      
      fluidRow(
        valueBoxOutput("fit_1", width = 3),
        valueBoxOutput("price_1", width = 3),
        valueBoxOutput("quality_1", width = 3),
        valueBoxOutput("vendor_1", width = 3),
      
        valueBoxOutput("fit_2", width = 3),
        valueBoxOutput("price_2", width = 3),
        valueBoxOutput("quality_2", width = 3),
        valueBoxOutput("vendor_2", width = 3),
      
        box(width = 6, title =  " uni-gram freq count ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            column(6, plotlyOutput("freq_uni"))),
            
        box(width = 6, title =  " uni-gram word cloud ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            column(6, plotOutput("uni_cloud"))
        )
      )
      ),
    
    tabItem(
      tabName = "bi",
      fluidRow(
        
        valueBoxOutput("fit_3", width = 3),
        valueBoxOutput("price_3", width = 3),
        valueBoxOutput("quality_3", width = 3),
        valueBoxOutput("vendor_3", width = 3),
        
        valueBoxOutput("fit_4", width = 3),
        valueBoxOutput("price_4", width = 3),
        valueBoxOutput("quality_4", width = 3),
        valueBoxOutput("vendor_4", width = 3),
        
        box(width = 6, title =  " bi-gram freq count ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            column(6, plotlyOutput("freq_bi"))),
        
        box(width = 6, title =  " bi-gram word cloud ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            column(6, plotOutput("bi_cloud"))
        )
      )
      ),
    
    tabItem(
      tabName = "tri",
      fluidRow(
        
        valueBoxOutput("fit_5", width = 3),
        valueBoxOutput("price_5", width = 3),
        valueBoxOutput("quality_5", width = 3),
        valueBoxOutput("vendor_5", width = 3),
        
        valueBoxOutput("fit_6", width = 3),
        valueBoxOutput("price_6", width = 3),
        valueBoxOutput("quality_6", width = 3),
        valueBoxOutput("vendor_6", width = 3),
        
        box(width = 6, title =  " tri-gram freq count ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            column(6, plotlyOutput("freq_tri"))),
        
        box(width = 6, title =  " tri-gram word cloud ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            column(6, plotOutput("tri_cloud"))
        )
      )
    ),
    ############################ 2. FIT ----
    tabItem(
      tabName = "fit",
      
      fluidRow(
        valueBoxOutput("fit_count", width = 3),
        valueBoxOutput("fit_mean", width = 3),
        valueBoxOutput("fit_max", width = 3),
        valueBoxOutput("fit_min", width = 3),
        
        box(width = 6, title =  " FIT uni-gram frequency ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            
            column(6, plotlyOutput("fit_uni_freq"))
        ),
        box(width = 6, title =  " FIT uni-gram wordcloud ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            
            column(6, plotOutput("fit_uni_wc"))
        ),
        box(width = 6, title =  " FIT bi-gram frequency ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            
            column(6, plotlyOutput("fit_bi_freq"))
        ),
        box(width = 6, title =  " FIT bi-gram wordcloud ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            
            column(6, plotOutput("fit_bi_wc"))
        ),
        box(width = 6, title =  " FIT tri-gram frequency ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            
            column(6, plotlyOutput("fit_tri_freq"))
        ),
        box(width = 6, title =  " FIT tri-gram wordcloud ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            
            column(6, plotOutput("fit_tri_wc"))
        )
      )
),
    ############################ 3. PRICE ----
    tabItem(
      tabName = "price",
      
      fluidRow(
        valueBoxOutput("price_count", width = 3),
        valueBoxOutput("price_mean", width = 3),
        valueBoxOutput("price_max", width = 3),
        valueBoxOutput("price_min", width = 3),
        
        box(width = 6, title =  " price uni-gram frequency ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            
            column(6, plotlyOutput("price_uni_freq"))
        ),
        box(width = 6, title =  " price uni-gram wordcloud ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            
            column(6, plotOutput("price_uni_wc"))
        ),
        box(width = 6, title =  " price bi-gram frequency ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            
            column(6, plotlyOutput("price_bi_freq"))
        ),
        box(width = 6, title =  " price bi-gram wordcloud ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            
            column(6, plotOutput("price_bi_wc"))
        ),
        box(width = 6, title =  " price tri-gram frequency ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            
            column(6, plotlyOutput("price_tri_freq"))
        ),
        box(width = 6, title =  " price tri-gram wordcloud ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            
            column(6, plotOutput("price_tri_wc"))
        )
      )
    ),
    ############################ 4. QUALITY ----
    tabItem(
      tabName = "quality",
      
      fluidRow(
        valueBoxOutput("quality_count", width = 3),
        valueBoxOutput("quality_mean", width = 3),
        valueBoxOutput("quality_max", width = 3),
        valueBoxOutput("quality_min", width = 3),
        
        box(width = 6, title =  " quality uni-gram frequency ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            
            column(6, plotlyOutput("quality_uni_freq"))
        ),
        box(width = 6, title =  " quality uni-gram wordcloud ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            
            column(6, plotOutput("quality_uni_wc"))
        ),
        box(width = 6, title =  " quality bi-gram frequency ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            
            column(6, plotlyOutput("quality_bi_freq"))
        ),
        box(width = 6, title =  " quality bi-gram wordcloud ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            
            column(6, plotOutput("quality_bi_wc"))
        ),
        box(width = 6, title =  " quality tri-gram frequency ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            
            column(6, plotlyOutput("quality_tri_freq"))
        ),
        box(width = 6, title =  " quality tri-gram wordcloud ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            
            column(6, plotOutput("quality_tri_wc"))
        )
      )
    ),
    ############################ 5. VENDOR ----
    tabItem(
      tabName = "vendor",
      
      fluidRow(
        valueBoxOutput("vendor_count", width = 3),
        valueBoxOutput("vendor_mean", width = 3),
        valueBoxOutput("vendor_max", width = 3),
        valueBoxOutput("vendor_min", width = 3),
        
        box(width = 6, title =  " vendor uni-gram frequency ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            
            column(6, plotlyOutput("vendor_uni_freq"))
        ),
        box(width = 6, title =  " vendor uni-gram wordcloud ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            
            column(6, plotOutput("vendor_uni_wc"))
        ),
        box(width = 6, title =  " vendor bi-gram frequency ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            
            column(6, plotlyOutput("vendor_bi_freq"))
        ),
        box(width = 6, title =  " vendor bi-gram wordcloud ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            
            column(6, plotOutput("vendor_bi_wc"))
        ),
        box(width = 6, title =  " vendor tri-gram frequency ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            
            column(6, plotlyOutput("vendor_tri_freq"))
        ),
        box(width = 6, title =  " vendor tri-gram wordcloud ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            
            column(6, plotOutput("vendor_tri_wc"))
        )
      )
    ),
    ############################ 6. USER INPUT STATS ----
    tabItem(
      tabName = "user",
      
      fluidRow(
        box(width = 12, title =  " User Input Review ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            column(12,
        selectInput(inputId = "review", label = "Reviews: Multi-select Filter", 
                    choices = unique(substr(txt_df_original$txt,1,100)), 
                    selected = substr(txt_df_original$txt[1:3],1,100), 
                    multiple = T))
        )
      ),
      fluidRow(
        box(width = 12, title =  " Review Details ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
            column(12, DT::dataTableOutput("review_freq", width = 500))
        )
      )
    )

    #### Last set of brackets ---- 
)
)
)
## 4. server.R ----
server <- function(input, output, session){
  
  observeEvent(input$showSidebar, {
    shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
  })
  observeEvent(input$hideSidebar, {
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  })

  #### VALUEBOXES ----
  output$fit_1 <- renderValueBox({
    valueBox(round(mean(txt_df$fit_sum),2),
             
             " FIT TRI-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Mean of FIT sentiments",
             color = "purple")
  })
  output$price_1 <- renderValueBox({
    valueBox(round(mean(txt_df$price_sum, na.rm = T),2),
             
             " PRICE TRI-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Mean of PRICE sentiments",
             color = "purple")
  })
  output$quality_1 <- renderValueBox({
    valueBox(round(mean(txt_df$quality_sum, na.rm = T),2),
             
             " QUALITY TRI-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Mean of QUALITY sentiments",
             color = "purple")
  })
  output$vendor_1 <- renderValueBox({
    valueBox(round(mean(txt_df$vendor_sum, na.rm = T),2),
             
             " VENDOR TRI-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Mean of VENDOR sentiments",
             color = "purple")
  })

  output$fit_3 <- renderValueBox({
    valueBox(round(mean(txt_df$fit_sum),2),
             
             " FIT TRI-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Mean of FIT sentiments",
             color = "purple")
  })
  output$price_3 <- renderValueBox({
    valueBox(round(mean(txt_df$price_sum, na.rm = T),2),
             
             " PRICE TRI-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Mean of PRICE sentiments",
             color = "purple")
  })
  output$quality_3 <- renderValueBox({
    valueBox(round(mean(txt_df$quality_sum, na.rm = T),2),
             
             " QUALITY TRI-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Mean of QUALITY sentiments",
             color = "purple")
  })
  output$vendor_3 <- renderValueBox({
    valueBox(round(mean(txt_df$vendor_sum, na.rm = T),2),
             
             " VENDOR TRI-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Mean of VENDOR sentiments",
             color = "purple")
  })

  output$fit_5 <- renderValueBox({
    valueBox(round(mean(txt_df$fit_sum),2),
             
             " FIT TRI-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Mean of FIT sentiments",
             color = "purple")
  })
  output$price_5 <- renderValueBox({
    valueBox(round(mean(txt_df$price_sum, na.rm = T),2),
             
             " PRICE TRI-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Mean of PRICE sentiments",
             color = "purple")
  })
  output$quality_5 <- renderValueBox({
    valueBox(round(mean(txt_df$quality_sum, na.rm = T),2),
             
             " QUALITY TRI-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Mean of QUALITY sentiments",
             color = "purple")
  })
  output$vendor_5 <- renderValueBox({
    valueBox(round(mean(txt_df$vendor_sum, na.rm = T),2),
             
             " VENDOR TRI-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Mean of VENDOR sentiments",
             color = "purple")
  })
  
  output$fit_2 <- renderValueBox({
    valueBox(paste0(round(sd(txt_df$fit_sum)/1000,2),
                    " k"),
             
             " FIT TRI-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Std Dev of FIT sentiments",
             color = "olive")
  })
  output$price_2 <- renderValueBox({
    valueBox(paste0(round(sd(txt_df$price_sum, na.rm = T)/1000,2),
                    " k"),
             
             " PRICE TRI-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Std Dev of PRICE sentiments",
             color = "olive")
  })
  output$quality_2 <- renderValueBox({
    valueBox(paste0(round(sd(txt_df$quality_sum)/1000,2),
                    " k"),
             
             " QUALITY TRI-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Std Dev of QUALITY sentiments",
             color = "olive")
  })
  output$vendor_2 <- renderValueBox({
    valueBox(paste0(round(sd(txt_df$vendor_sum)/1000,2),
                    " k"),
             
             " VENDOR TRI-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Std Dev of VENDOR sentiments",
             color = "olive")
  })
  
  output$fit_4 <- renderValueBox({
    valueBox(paste0(round(sd(txt_df$fit_sum)/1000,2),
                    " k"),
             
             " FIT TRI-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Std Dev of FIT sentiments",
             color = "olive")
  })
  output$price_4 <- renderValueBox({
    valueBox(paste0(round(sd(txt_df$price_sum, na.rm = T)/1000,2),
                    " k"),
             
             " PRICE TRI-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Std Dev of PRICE sentiments",
             color = "olive")
  })
  output$quality_4 <- renderValueBox({
    valueBox(paste0(round(sd(txt_df$quality_sum)/1000,2),
                    " k"),
             
             " QUALITY TRI-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Std Dev of QUALITY sentiments",
             color = "olive")
  })
  output$vendor_4 <- renderValueBox({
    valueBox(paste0(round(sd(txt_df$vendor_sum)/1000,2),
                    " k"),
             
             " VENDOR TRI-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Std Dev of VENDOR sentiments",
             color = "olive")
  })

  output$fit_6 <- renderValueBox({
    valueBox(paste0(round(sd(txt_df$fit_sum)/1000,2),
                    " k"),
             
             " FIT TRI-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Std Dev of FIT sentiments",
             color = "olive")
  })
  output$price_6 <- renderValueBox({
    valueBox(paste0(round(sd(txt_df$price_sum, na.rm = T)/1000,2),
                    " k"),
             
             " PRICE TRI-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Std Dev of PRICE sentiments",
             color = "olive")
  })
  output$quality_6 <- renderValueBox({
    valueBox(paste0(round(sd(txt_df$quality_sum)/1000,2),
                    " k"),
             
             " QUALITY TRI-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Std Dev of QUALITY sentiments",
             color = "olive")
  })
  output$vendor_6 <- renderValueBox({
    valueBox(paste0(round(sd(txt_df$vendor_sum)/1000,2),
                    " k"),
             
             " VENDOR TRI-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Std Dev of VENDOR sentiments",
             color = "olive")
  })
  
  #### FREQUENCY CHARTS ---- 
  output$freq_uni <- renderPlotly({
    q <- (ggplot(eda_txt_df %>% 
                   filter(n > 200, 
                          word != 'quot'), aes(word, n)) + 
            geom_col(fill = "deepskyblue4") +
            xlab(NULL) +
            coord_flip() + 
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(q, width = 500, height = 400, tooltip = c("x", "y"))
  })
  output$uni_cloud <- renderPlot({
    
    eda_txt_df <- eda_txt_df %>%
      filter(!word %in% "quot")
    
    wordcloud(words = eda_txt_df$word, 
              freq = eda_txt_df$n,
              min.freq = input$freq,
              max.words = input$max, 
              random.order = FALSE,
              rot.per = 0.35,
              colors = brewer.pal(8, "Dark2"))
  },
  height = 400,
  width = 500)
  output$freq_bi <- renderPlotly({
    eda_txt_two <- data.frame(eda_txt_two %>% 
                      filter(!grepl("quot",two_gram)))
      
    q <- (ggplot(eda_txt_two %>% 
                   filter(n>20), 
                 aes(two_gram, n)) + 
            geom_col(fill = "deepskyblue4") +
            xlab(NULL) +
            coord_flip() + 
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(q, width = 500, height = 400, tooltip = c("x", "y"))
  })
  output$bi_cloud <- renderPlot({
    eda_txt_two <- data.frame(eda_txt_two %>% 
                      filter(!grepl("quot",two_gram)))
    
    wordcloud(words = eda_txt_two$two_gram, 
              freq = eda_txt_two$n,
              min.freq = input$freq,
              max.words = input$max, 
              random.order = FALSE,
              rot.per = 0.35,
              colors = brewer.pal(8, "Dark2"))
  },
  height = 400,
  width = 500)
  output$freq_tri <- renderPlotly({
    eda_txt_three <- data.frame(eda_txt_three %>% 
                                filter(!grepl("quot",three_gram)))
    
    q <- (ggplot(eda_txt_three %>% 
                   filter(n>6), 
                 aes(three_gram, n)) + 
            geom_col(fill = "deepskyblue4") +
            xlab(NULL) +
            coord_flip() + 
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(q, width = 500, height = 400, tooltip = c("x", "y"))
  })
  output$tri_cloud <- renderPlot({
    eda_txt_three <- data.frame(eda_txt_three %>% 
                      filter(!grepl("quot",three_gram)))
    
    wordcloud(words = eda_txt_three$three_gram, 
              freq = eda_txt_three$n,
              min.freq = input$freq,
              max.words = input$max, 
              random.order = FALSE,
              rot.per = 0.35,
              colors = brewer.pal(8, "Dark2"))
  },
  height = 400,
  width = 500)
  
  ##### FIT ---- 
  
  output$fit_uni_freq <- renderPlotly({
    
    q <- (ggplot(fit_wc_uni %>% filter(n > 50), aes(word, n)) + 
            geom_col(fill = "deepskyblue4") +
            xlab(NULL) +
            coord_flip() + 
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(q, width = 500, height = 400, tooltip = c("x", "y"))
  })
  output$fit_bi_freq <- renderPlotly({
    
    q <- (ggplot(fit_wc_bi %>% filter(n > 50), aes(word, n)) + 
            geom_col(fill = "deepskyblue4") +
            xlab(NULL) +
            coord_flip() + 
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(q, width = 500, height = 400, tooltip = c("x", "y"))
  })
  output$fit_tri_freq <- renderPlotly({
    
    q <- (ggplot(fit_wc_tri %>% filter(n > 8), aes(word, n)) + 
            geom_col(fill = "deepskyblue4") +
            xlab(NULL) +
            coord_flip() + 
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(q, width = 500, height = 400, tooltip = c("x", "y"))
  })

  output$fit_uni_wc <- renderPlot({
    
    wordcloud(words = fit_wc_uni$word, 
              freq = fit_wc_uni$n,
              min.freq = input$freq,
              max.words = input$max, 
              random.order = FALSE,
              rot.per = 0.35,
              colors = brewer.pal(8, "Dark2"))
  },
  height = 400,
  width = 500)
  output$fit_bi_wc <- renderPlot({
    
    wordcloud(words = fit_wc_bi$word, 
              freq = fit_wc_bi$n,
              min.freq = input$freq,
              max.words = input$max, 
              random.order = FALSE,
              rot.per = 0.35,
              colors = brewer.pal(8, "Dark2"))
  },
  height = 400,
  width = 500)
  output$fit_tri_wc <- renderPlot({
    
    wordcloud(words = fit_wc_tri$word, 
              freq = fit_wc_tri$n,
              min.freq = input$freq,
              max.words = input$max, 
              random.order = FALSE,
              rot.per = 0.35,
              colors = brewer.pal(8, "Dark2"))
  },
  height = 400,
  width = 500)
  
  output$fit_count <- renderValueBox({
    valueBox(paste0(round(fit_text_count,1)),
             
             " FIT n-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Count of FIT sentiments",
             color = "olive")
  })
  output$fit_mean <- renderValueBox({
    valueBox(paste0(round(fit_text_mean,1)),
             
             " FIT n-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Mean of FIT sentiments",
             color = "olive")
  })
  output$fit_max <- renderValueBox({
    valueBox(paste0(round(fit_text_max,1)),
             
             " FIT n-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Max of FIT sentiments",
             color = "olive")
  })
  output$fit_min <- renderValueBox({
    valueBox(paste0(round(fit_text_min,1)),
             
             " FIT n-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Min of FIT sentiments",
             color = "olive")
  })
  
  ##### price ---- 
  output$price_uni_freq <- renderPlotly({
    
    q <- (ggplot(price_wc_uni %>% filter(n > 50), aes(word, n)) + 
            geom_col(fill = "deepskyblue4") +
            xlab(NULL) +
            coord_flip() + 
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(q, width = 500, height = 400, tooltip = c("x", "y"))
  })
  output$price_bi_freq <- renderPlotly({
    
    q <- (ggplot(price_wc_bi %>% filter(n > 50), aes(word, n)) + 
            geom_col(fill = "deepskyblue4") +
            xlab(NULL) +
            coord_flip() + 
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(q, width = 500, height = 400, tooltip = c("x", "y"))
  })
  output$price_tri_freq <- renderPlotly({
    
    q <- (ggplot(price_wc_tri %>% filter(n > 8), aes(word, n)) + 
            geom_col(fill = "deepskyblue4") +
            xlab(NULL) +
            coord_flip() + 
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(q, width = 500, height = 400, tooltip = c("x", "y"))
  })
  
  output$price_uni_wc <- renderPlot({
    
    wordcloud(words = price_wc_uni$word, 
              freq = price_wc_uni$n,
              min.freq = input$freq,
              max.words = input$max, 
              random.order = FALSE,
              rot.per = 0.35,
              colors = brewer.pal(8, "Dark2"))
  },
  height = 400,
  width = 500)
  output$price_bi_wc <- renderPlot({
    
    wordcloud(words = price_wc_bi$word, 
              freq = price_wc_bi$n,
              min.freq = input$freq,
              max.words = input$max, 
              random.order = FALSE,
              rot.per = 0.35,
              colors = brewer.pal(8, "Dark2"))
  },
  height = 400,
  width = 500)
  output$price_tri_wc <- renderPlot({
    
    wordcloud(words = price_wc_tri$word, 
              freq = price_wc_tri$n,
              min.freq = input$freq,
              max.words = input$max, 
              random.order = FALSE,
              rot.per = 0.35,
              colors = brewer.pal(8, "Dark2"))
  },
  height = 400,
  width = 500)
  
  output$price_count <- renderValueBox({
    valueBox(paste0(round(price_text_count,1)),
             
             " price n-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Count of price sentiments",
             color = "olive")
  })
  output$price_mean <- renderValueBox({
    valueBox(paste0(round(price_text_mean,1)),
             
             " price n-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Mean of price sentiments",
             color = "olive")
  })
  output$price_max <- renderValueBox({
    valueBox(paste0(round(price_text_max,1)),
             
             " price n-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Max of price sentiments",
             color = "olive")
  })
  output$price_min <- renderValueBox({
    valueBox(paste0(round(price_text_min,1)),
             
             " price n-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Min of price sentiments",
             color = "olive")
  })
    
  ##### quality ---- 
  output$quality_uni_freq <- renderPlotly({
    
    q <- (ggplot(quality_wc_uni %>% filter(n > 50), aes(word, n)) + 
            geom_col(fill = "deepskyblue4") +
            xlab(NULL) +
            coord_flip() + 
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(q, width = 500, height = 400, tooltip = c("x", "y"))
  })
  output$quality_bi_freq <- renderPlotly({
    
    q <- (ggplot(quality_wc_bi %>% filter(n > 50), aes(word, n)) + 
            geom_col(fill = "deepskyblue4") +
            xlab(NULL) +
            coord_flip() + 
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(q, width = 500, height = 400, tooltip = c("x", "y"))
  })
  output$quality_tri_freq <- renderPlotly({
    
    q <- (ggplot(quality_wc_tri %>% filter(n > 8), aes(word, n)) + 
            geom_col(fill = "deepskyblue4") +
            xlab(NULL) +
            coord_flip() + 
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(q, width = 500, height = 400, tooltip = c("x", "y"))
  })
  
  output$quality_uni_wc <- renderPlot({
    
    wordcloud(words = quality_wc_uni$word, 
              freq = quality_wc_uni$n,
              min.freq = input$freq,
              max.words = input$max, 
              random.order = FALSE,
              rot.per = 0.35,
              colors = brewer.pal(8, "Dark2"))
  },
  height = 400,
  width = 500)
  output$quality_bi_wc <- renderPlot({
    
    wordcloud(words = quality_wc_bi$word, 
              freq = quality_wc_bi$n,
              min.freq = input$freq,
              max.words = input$max, 
              random.order = FALSE,
              rot.per = 0.35,
              colors = brewer.pal(8, "Dark2"))
  },
  height = 400,
  width = 500)
  output$quality_tri_wc <- renderPlot({
    
    wordcloud(words = quality_wc_tri$word, 
              freq = quality_wc_tri$n,
              min.freq = input$freq,
              max.words = input$max, 
              random.order = FALSE,
              rot.per = 0.35,
              colors = brewer.pal(8, "Dark2"))
  },
  height = 400,
  width = 500)
  
  output$quality_count <- renderValueBox({
    valueBox(paste0(round(quality_text_count,1)),
             
             " quality n-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Count of quality sentiments",
             color = "olive")
  })
  output$quality_mean <- renderValueBox({
    valueBox(paste0(round(quality_text_mean,1)),
             
             " quality n-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Mean of quality sentiments",
             color = "olive")
  })
  output$quality_max <- renderValueBox({
    valueBox(paste0(round(quality_text_max,1)),
             
             " quality n-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Max of quality sentiments",
             color = "olive")
  })
  output$quality_min <- renderValueBox({
    valueBox(paste0(round(quality_text_min,1)),
             
             " quality n-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Min of quality sentiments",
             color = "olive")
  })
    
  ##### vendor ---- 
  output$vendor_uni_freq <- renderPlotly({
    
    q <- (ggplot(vendor_wc_uni %>% filter(n > 50), aes(word, n)) + 
            geom_col(fill = "deepskyblue4") +
            xlab(NULL) +
            coord_flip() + 
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(q, width = 500, height = 400, tooltip = c("x", "y"))
  })
  output$vendor_bi_freq <- renderPlotly({
    
    q <- (ggplot(vendor_wc_bi %>% filter(n > 50), aes(word, n)) + 
            geom_col(fill = "deepskyblue4") +
            xlab(NULL) +
            coord_flip() + 
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(q, width = 500, height = 400, tooltip = c("x", "y"))
  })
  output$vendor_tri_freq <- renderPlotly({
    
    q <- (ggplot(vendor_wc_tri %>% filter(n > 8), aes(word, n)) + 
            geom_col(fill = "deepskyblue4") +
            xlab(NULL) +
            coord_flip() + 
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue())
    
    ggplotly(q, width = 500, height = 400, tooltip = c("x", "y"))
  })
  
  output$vendor_uni_wc <- renderPlot({
    
    wordcloud(words = vendor_wc_uni$word, 
              freq = vendor_wc_uni$n,
              min.freq = input$freq,
              max.words = input$max, 
              random.order = FALSE,
              rot.per = 0.35,
              colors = brewer.pal(8, "Dark2"))
  },
  height = 400,
  width = 500)
  output$vendor_bi_wc <- renderPlot({
    
    wordcloud(words = vendor_wc_bi$word, 
              freq = vendor_wc_bi$n,
              min.freq = input$freq,
              max.words = input$max, 
              random.order = FALSE,
              rot.per = 0.35,
              colors = brewer.pal(8, "Dark2"))
  },
  height = 400,
  width = 500)
  output$vendor_tri_wc <- renderPlot({
    
    wordcloud(words = vendor_wc_tri$word, 
              freq = vendor_wc_tri$n,
              min.freq = input$freq,
              max.words = input$max, 
              random.order = FALSE,
              rot.per = 0.35,
              colors = brewer.pal(8, "Dark2"))
  },
  height = 400,
  width = 500)
  
  output$vendor_count <- renderValueBox({
    valueBox(paste0(round(vendor_text_count,1)),
             
             " vendor n-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Count of vendor sentiments",
             color = "olive")
  })
  output$vendor_mean <- renderValueBox({
    valueBox(paste0(round(vendor_text_mean,1)),
             
             " vendor n-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Mean of vendor sentiments",
             color = "olive")
  })
  output$vendor_max <- renderValueBox({
    valueBox(paste0(round(vendor_text_max,1)),
             
             " vendor n-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Max of vendor sentiments",
             color = "olive")
  })
  output$vendor_min <- renderValueBox({
    valueBox(paste0(round(vendor_text_min,1)),
             
             " vendor n-GRAM ", 
             icon = icon("credit-card-alt"),
             subtitle = "Min of vendor sentiments",
             color = "olive")
  })
  
  #### Review Details ---- 
  
  output$review_freq <- DT::renderDataTable({
    
    user_input <- data.frame(line = txt_df_original$line,
                             uncategorized_review_text = substr(txt_df_original$txt,1,100),
                             word_count = nchar(txt_df_original$txt),
                             fit_sentiment = round(txt_df_original$fit,1),
                             price_sentiment = round(txt_df_original$price,1),
                             quality_sentiment = round(txt_df_original$quality,1),
                             vendor_sentiment = round(txt_df_original$vendor,1),
                             category_of_review = txt_df_original$flag)
    
    subset(user_input,user_input$uncategorized_review_text %in% input$review)
    
  })
  
}
## 5. Final EXECUTION ---- 
shinyApp(ui, server)
