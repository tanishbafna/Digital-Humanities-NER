# Importing Libraries

library(stringr)
library(dplyr)
library(tidyr)
library(tidytext)
library(readr)
library(stringi)
library(gutenbergr)
library(ggiraphExtra)
library(ggplot2)
library(RColorBrewer)
library(scales)

source("testMirrors.r")
good_mirror <- findMirror()

#=========================

# TXT to CSV Function

readTextCorpus <- function(fileName, bookName, authorName) {
  
  out <- read.delim(fileName, header=FALSE, blank.lines.skip=FALSE, stringsAsFactors = FALSE, quote = "")
  colnames(out) <- "text"
  out <- mutate(out, author = authorName, title = bookName) #Adding Book and Author Column
  
}

#=========================

# Reading TXT Files

loadIntoCorpus <- function() {
  
  behn1<- readTextCorpus("text_(dutch_lover).txt", "The Dutch Lover", "Behn, Aphra")
  behn2<- readTextCorpus("text_(roundheads).txt", "The Roundheads", "Behn, Aphra")
  
  dryden1<- readTextCorpus("text_(amboyna).txt", "Amboyna", "Dryden, John")
  dryden2<- readTextCorpus("text_(aureng-zebe).txt", "Aureng-Zebe", "Dryden, John")
  
  wycherley1<- readTextCorpus("text_(gentleman_dancing).txt", "The Gentlemen Dancing-Master", "Wycherley, William")
  wycherley2<- readTextCorpus("text_(plain_dealer).txt", "The Plain Dealer", "Wycherley, William")
  
  # Combining dataframes.
  corpusDF <- rbind(behn1, behn2, dryden1, dryden2, wycherley1, wycherley2)
  
  return(corpusDF)
  
}

#=========================

# Read Corpus

# First 4 texts
corpus_first4 <- read.csv("backup_(corpus_gutenberg).csv")
corpus_first4$X <- NULL

# Last 6 texts
corpus_last6 <- loadIntoCorpus()

corpus <- rbind(corpus_first4, corpus_last6)
rm(corpus_first4, corpus_last6)

corpus_clean <- corpus %>%
  mutate(text = str_replace_all(text,"_"," "))

#=========================

# Read extracted entities from Backup (after chunk operations, fuzzy join and manual cleaning)

clean_entities <- read.csv("backup_(final_entities).csv", stringsAsFactors = FALSE)

clean_entities$year[clean_entities$title == "The Dutch Lover"] <- "1673"
clean_entities$year[clean_entities$title == "The Plain Dealer"] <- "1676"
clean_entities$year[clean_entities$title == "The Gentlemen Dancing-Master"] <- "1673"
clean_entities$year[clean_entities$title == "The Recruiting Officer"] <- "1706"
clean_entities$year[clean_entities$title == "The Roundheads"] <- "1682"
clean_entities$year[clean_entities$title == "The Way of the World"] <- "1700"
clean_entities$year[clean_entities$title == "Venice Preserved: A Tragedy"] <- "1682"
clean_entities$year[clean_entities$title == "Amboyna"] <- "1673"
clean_entities$year[clean_entities$title == "Love for Love: A Comedy"] <- "1695"
clean_entities$year[clean_entities$title == "Aureng-Zebe"] <- "1675"

clean_entities$title[clean_entities$title == "The Dutch Lover"] <- "The Dutch Lover - 1673"
clean_entities$title[clean_entities$title == "The Plain Dealer"] <- "The Plain Dealer - 1676"
clean_entities$title[clean_entities$title == "The Gentlemen Dancing-Master"] <- "The Gentlemen Dancing-Master - 1673"
clean_entities$title[clean_entities$title == "The Recruiting Officer"] <- "The Recruiting Officer - 1706"
clean_entities$title[clean_entities$title == "The Roundheads"] <- "The Roundheads - 1682"
clean_entities$title[clean_entities$title == "The Way of the World"] <- "The Way of the World - 1700"
clean_entities$title[clean_entities$title == "Venice Preserved: A Tragedy"] <- "Venice Preserved - 1682"
clean_entities$title[clean_entities$title == "Amboyna"] <- "Amboyna - 1673"
clean_entities$title[clean_entities$title == "Love for Love: A Comedy"] <- "Love for Love - 1695"
clean_entities$title[clean_entities$title == "Aureng-Zebe"] <- "Aureng-Zebe - 1675"

#=========================

# Character Occurrences

# Calculating total occurences of a character.
occurences <- clean_entities %>% count(title, kind, entity)

occurences %>%
  group_by(title) %>%
  filter(kind == "person") %>%
  top_n(10) %>% 
  mutate(entity = reorder(entity, n)) %>%
  ggplot(aes(entity, y = n, fill = title)) +
  geom_col() +
  facet_wrap( ~ title, scales = "free") +
  ggtitle("Top 10 Mentions (Characters)") +
  coord_flip()

#=========================

# Length of Plays

unnest_corpus <- corpus_clean %>%
  unnest_tokens(word, text)

# Calculating total words in a text
wordCount <- unnest_corpus %>%
  count(title)
colnames(wordCount) <- c("title", "totalWords")

ggplot(wordCount, aes(x = title, y = totalWords, fill = title)) +
  geom_col() +
  ggtitle("Length of Plays") + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#=========================

# Top 10 + and - words.
bing_word_counts <- unnest_corpus %>%
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  ggtitle("Top 10 +- Words") +
  coord_flip()

#=========================

# Top 10 words in restoration literature.

# Anti-joining stop words
word_freq <- unnest_corpus %>%
  anti_join(stop_words, by = c("word" = "word")) %>%
  filter(!(word %in% c("thee", "thou", "i'll", "nay", "tis", "ha", "enter", "exit", "scene", "act", "em", "mons", "thy") )) %>%
  count(word, sort = TRUE) %>%
  ungroup()

word_freq %>%
  mutate(word = reorder(word, n)) %>%
  top_n(10) %>%
  ggplot(aes(x = word, y = n, fill = word)) +
  geom_col() +
  ggtitle("Most frequent words") + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#=========================
  
# NLP, NER and Sentiment Analysis

entities_unnest <- clean_entities %>%
  unnest_tokens(word, words)

entities_sentiment <- entities_unnest %>%
  group_by(author, title) %>%
  inner_join(get_sentiments("nrc")) %>% 
  count(sentence_nr, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

entities_matches_sentiment <- entities_unnest %>%
  inner_join(entities_sentiment) %>%
  distinct_at(vars(-word)) %>%
  left_join(occurences, by=c("title", "entity", "kind"))

ner_total_sentiment <- entities_matches_sentiment %>% 
  group_by(author, title, entity, kind, year) %>%  
  summarise(total = mean(sentiment))  

#=========================

# Top 10 Locations

# Positive Locations
ner_total_sentiment %>%
  group_by(title) %>%
  filter(kind == "location") %>%
  top_n(10) %>% 
  mutate(entity = reorder(entity, (desc(total)))) %>%  
  ggplot(aes(entity, y = total, fill = title)) +
  geom_col() +
  facet_wrap( ~ title, scales = "free") +
  ggtitle("Top 10 Positive (Locations)") +
  coord_flip()

# Negative Locations
ner_total_sentiment %>%
  group_by(title) %>%
  filter(kind == "location") %>%
  top_n(-10) %>% 
  mutate(entity = reorder(entity, (desc(total)))) %>%  
  ggplot(aes(entity, y = total, fill = title)) +
  geom_col() +
  facet_wrap( ~ title, scales = "free") +
  ggtitle("Top 10 Negative (Locations)") +
  coord_flip()

#=========================

# Top 10 Characters

# Positive Charcters
ner_total_sentiment %>%
  arrange(year) %>%
  group_by(title) %>%
  filter(kind == "person") %>%
  top_n(10) %>% 
  mutate(entity = reorder(entity, total)) %>% 
  ggplot(aes(entity, y = total, fill = title)) +
  geom_col() +
  facet_wrap( ~ title, scales = "free") +
  ggtitle("Top 10 Positive (Characters)") +
  coord_flip()

# Negative Charcters
ner_total_sentiment %>%
  group_by(title) %>%
  filter(kind == "person") %>%
  top_n(-10) %>% 
  mutate(entity = reorder(entity, desc(total))) %>% 
  ggplot(aes(entity, y = total, fill = title)) +
  geom_col() +
  facet_wrap( ~ title, scales = "free") +
  ggtitle("Top 10 Negative (Characters)") +
  coord_flip()

#=========================