###########################################################
## Loading all libraries and R data
###########################################################


# libraries
library(textreadr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(stringr)
library(ggplot2)
library(scales)
library(tm)


# R data
data(stop_words)



###########################################################
## Importing all files and consolidating into one dataframe
###########################################################

## for articles pre whatsapp clarification
# Importing all .txt files from one directory
setwd("/Users/anneliseneumann/Desktop/Text Analytics and NLP/Assignement 1/txt files whatsapp pre")
nm <- list.files(path="/Users/anneliseneumann/Desktop/Text Analytics and NLP/Assignement 1/txt files whatsapp pre")


# using read document to import the data:
my_txt_text <- do.call(rbind, lapply(nm, function(x) paste(read_document(file=x), collapse = " ")))


# restructuring data as a data frame
mydf_pre <- data_frame(release = "pre", location=c("Forbes", "Guardian", "Independent", "Wired"), text=my_txt_text)  # putting in location info and text col


# creating a tidy version of the df
tidy_pre <- mydf_pre %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  count(location, word, sort=TRUE) %>%
  mutate(word=reorder(word, n)) %>%
  group_by(location)


## for articles post whatsapp clarification
# Importing all .txt files from one directory
setwd("/Users/anneliseneumann/Desktop/Text Analytics and NLP/Assignement 1/txt files whatsapp")
nm <- list.files(path="/Users/anneliseneumann/Desktop/Text Analytics and NLP/Assignement 1/txt files whatsapp")


# using read document to import the data:
my_txt_text <- do.call(rbind, lapply(nm, function(x) paste(read_document(file=x), collapse = " ")))


# restructuring data as a data frame
mydf_post <- data_frame(release = "post", location=c("CNBC", "Guardian", "NYT", "Verge"), text=my_txt_text)  # putting in location info and text col


# creating a tidy version of the df
tidy_post <- mydf_post %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  count(location, word, sort=TRUE) %>%
  mutate(word=reorder(word, n)) %>%
  group_by(location)


## Consolidated
mydf <- rbind(mydf_pre, mydf_post)

# creating a tidy version of the df
tidy_df <- mydf %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  count(release, word, sort=TRUE) %>%
  mutate(word=reorder(word, n)) %>%
  group_by(release)



###########################################################
## Tokenizing and visualizing word frequency
###########################################################


# Exploring word frequencies
freq_hist <- tidy_df %>%
  top_n(10, n) %>%
  ungroup() %>%
  ggplot(aes(word, n, fill = release))+
    facet_wrap(~release, scales = "free_y")+
    geom_col(show.legend = F)+
    xlab(NULL)+
    coord_flip()

print(freq_hist)

# INSIGHTS - post
# Many of the top words, as expected, speak to the topic discussed in the article:
# "whatsapp", "facebook", "privacy". What is interesting to note, are where we see
# differences. The Guradian appears to be speaking directly to its readers, frequently
# # using "you're", and appear to have consulted with Mitnick (a hacker) for information.
# On the ohter hand, NYT references Zuckerberg 5 times, prehaps putting a lot of the
# responsibility on Facebook's CEO?


# Let's look at relative frequency to see if that generates any different insights


# Exploring word frequencies proportionally
prop_hist <- tidy_df %>%
  mutate(proportion = n/sum(n)) %>%
  top_n(10, proportion) %>%
  ungroup() %>%
  ggplot(aes(word, proportion, fill = release))+
  facet_wrap(~release, scales = "free_y")+
  geom_col(show.legend = F)+
  xlab(NULL)+
  coord_flip()

print(prop_hist)


# INSIGHTS
# Unsurprisingly perhaps, there is very little change to note


# INSIGHTS ARE IN THE DIFFERENCES



###########################################################
## Comparing before and after articles
###########################################################


# setting up the df as needed for visualization
corr <- tidy_post %>%
  mutate(proportion = n/sum(n)) %>%
  select(-n) %>%
  spread(location, proportion) %>%
  gather(location, proportion, `Guardian`, `NYT`, `Verge`)


# creating the correlograms
ggplot(corr, aes(x=proportion, y=`CNBC`, 
                      color = abs(`CNBC`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5)+
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslateblue", high = "lightsteelblue4")+
  facet_wrap(~location, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "CNBC", x=NULL)

# ANALYSIS
# There appears to be a stronger correlation between the words used by CNBC, NYT
# and the Verge, over the Guardian. However, the words which stand out are almost
# identical across all the article. Furthermore, they are more indicative of the
# topic discussed (of which we are already aware), rather than specific concerns
# being addressed.


# Checking numerical correlation
cor.test(data=corr[corr$location == "Guardian",],
         ~proportion + `CNBC`)
# 0.665

cor.test(data=corr[corr$location == "NYT",],
         ~proportion + `CNBC`)
# 0.782

cor.test(data=corr[corr$location == "Verge",],
         ~proportion + `CNBC`)
# 0.828



###########################################################
## Sentiment analysis
###########################################################


## with bing
tidy_pre %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(location) %>%
 # filter(n > 1) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = fct_reorder(word, n)) %>%
  arrange(desc(n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
    geom_bar(stat = "identity") +
    facet_wrap(~location, scales = "free_y") +
    ylab("Contribution to sentiment, bing") +
    coord_flip()


## with afinn
tidy_pre %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(location) %>%
  mutate(n = n*value) %>%
  filter(abs(n) > 1) %>%
  mutate(word = fct_reorder(word, n)) %>%
  arrange(desc(n)) %>%
  ggplot(aes(word, n, fill = ifelse(n<0, "red", "green"))) +
  geom_bar(stat = "identity", show.legend = F) +
  facet_wrap(~location, scales = "free_y") +
  ylab("Contribution to sentiment (afinn)") +
  coord_flip()


## with nrc
tidy_pre %>%
  inner_join(get_sentiments("nrc")) %>%
  group_by(location) %>%
  filter(n >= 5) %>%
  mutate(word = fct_reorder(word, n)) %>%
  arrange(desc(n)) %>%
  ggplot(x, aes(word, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  facet_wrap(~location, scales = "free_y") +
  ylab("Contribution to sentiment") +
  coord_flip()


###########################################################
## Document term matrix
###########################################################

tidy_df %>% cast_dtm(release, word, n)
# sparcity : 41%


###########################################################
## tf_idf
###########################################################

tidy_tf_idf <- tidy_df %>%
  bind_tf_idf(release, word, n) %>%
  arrange(desc(n))
tidy_tf_idf




